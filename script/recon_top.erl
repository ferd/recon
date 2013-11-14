%%% run with escript recon_top.erl
-module(recon_top).
-export([main/1]).
-mode(compile).

-define(PING_TIMEOUT, timer:seconds(10)).
-define(PIDLEN, 15).
-define(PORTLEN, 18).
-define(MINLEN, 15).
-define(KEYPCT, 0.25).

-record(state, {type,
                ref}).

main(Args) ->
    try
        maybe_help(Args),
        parse_args(Args),
        default_args(),
        connect(),
        ping(),
        run(type()),
        init:stop(0)
    catch
        invalid_remote ->
            io:format("Invalid remote node name.~n");
        missing_remote ->
            io:format("Missing remote node name.~n");
        invalid_type ->
            io:format("Type not supported (procs|inet|stats).~n");
        failed_connect ->
            io:format("Could not connect to remote node.~n");
        ping_timeout ->
            io:format("Recon not loaded on remote node.~n");
        {remote_exit,R} ->
            io:format("Remote process died for reason ~p.~n",[R]);
        {failed_dist,R} ->
            io:format("net_kernel could not be started for reason ~p.~n",[R]);
        C:R ->
            io:format("EXCEPTION: ~p:~p~n~p~n",[C,R, erlang:get_stacktrace()])
    end,
    halt(1).

help() ->
"Mandatory Arguments:
  -r nodename
    name of the remote node to connect to.

 Optional Arguments:
  -l nodename
    name of the local node. Defaults to `recon_top@hostname` where hostname
    refers to a full or short name node based on what is inferred by -r
    nodename if no @ can be found.
  -c cookie
    The cookie to use to connect remotely, if necessary.
  -t procs | inet | stats
    defaults to 'stats', the type of information to display.
  -i <number>
    number of milliseconds to wait between samplings. Defaults to 1000.
  -k <name>
    the key to display. Defaults to 'reductions' for processes, and
    'oct' for inet ports. Ignored if -t is 'stats'.
  -n <number>
    the number of processes or ports to include in the results.
    Defaults to 10, but is ignored if '-t' is stats.
  -h
    displays this message.
".

remote() -> get(remote).
remote(Val) -> put(remote, Val).

host() -> get(host).
host(Val) -> put(host, Val).

local() -> get(local).
local(Val) -> put(local, Val).

cookie() -> get(cookie).
cookie(Val) -> put(cookie, Val).

type() -> get(type).
type(Val) -> put(type, Val).

interval() -> get(interval).
interval(Val) -> put(interval, Val).

key() -> get(key).
key(Val) -> put(key, Val).

num() -> get(num).
num(Val) -> put(num, Val).

maybe_help([]) -> io:format(help());
maybe_help(_) -> ok.

parse_args([]) ->
    ok;
parse_args(["-r", Node | Rest]) ->
     case re:run(Node, "(.*)@(.*)", [{capture, [1,2], list}]) of
         {match,[_Name,Host]} ->
             host(Host),
             remote(list_to_atom(Node)),
             parse_args(Rest);
         nomatch ->
             throw(invalid_remote)
     end;
parse_args(["-l", Node | Rest]) ->
    case re:run(Node, "(.*)@(.*)", [{capture, [1,2], list}]) of
        {match, _} ->
            local(list_to_atom(Node)),
            parse_args(Rest);
        nomatch ->
            case {host(), get(checked)} of
                {undefined, true} ->
                    throw(missing_remote);
                {undefined, undefined} ->
                    put(checked, true),
                    parse_args(Rest++["-l", Node]);
                {Host, _} ->
                    local(list_to_atom(Node++"@"++Host)),
                    parse_args(Rest)
            end
    end;
parse_args(["-c", Cookie | Rest]) ->
    cookie(list_to_atom(Cookie)),
    parse_args(Rest);
parse_args(["-h"|Rest]) ->
    io:format(help()),
    parse_args(Rest);
parse_args(["-t", Type | Rest]) ->
    Atom = case Type of
               "procs" -> procs;
               "inet" -> inet;
               "stats" -> stats;
               _ -> throw(invalid_type)
           end,
    type(Atom),
    parse_args(Rest);
parse_args(["-i", Num | Rest]) ->
    interval(list_to_integer(Num)),
    parse_args(Rest);
parse_args(["-k", Key | Rest]) ->
    key(list_to_atom(Key)),
    parse_args(Rest);
parse_args(["-n", Num | Rest]) ->
    num(list_to_integer(Num)),
    parse_args(Rest).

default_args() ->
    case remote() of
        undefined -> throw(missing_remote);
        _ -> ok
    end,
    case {host(), local()} of
        {undefined, _} ->
             throw(missing_remote);
        {Host, undefined} ->
            local(list_to_atom("recon_top@"++Host));
        {_, _} ->
            ok
    end,
    case type() of
        undefined -> type(stats);
        _ -> ok
    end,
    case interval() of
        undefined -> interval(1000);
        _ -> ok
    end,
    case {key(), type()} of
        {_, stats} -> ok;
        {undefined, procs} -> key(reductions);
        {undefined, inet} -> key(oct);
        {_, _} -> ok
    end,
    case {num(), type()} of
        {_, stats} -> ok;
        {undefined, _} -> num(10);
        {_, _} -> ok
    end.

connect() ->
    case net_kernel:start([local(), nametype(local())]) of
        {ok, _} -> ok;
        Err -> throw({failed_dist, Err})
    end,
    case cookie() of
        undefined -> ok;
        Cookie ->  erlang:set_cookie(remote(), Cookie)
    end,
    case net_kernel:hidden_connect(remote()) of
        true -> ok;
        false -> throw(failed_connect)
    end.


ping() ->
    Ref = make_ref(),
    io:format("Confirming recon presence... "),
    rpc:call(remote(), recon_escript, ping, [self(), Ref]),
    receive
        Ref -> io:format("ok~n")
    after ?PING_TIMEOUT -> throw(ping_timeout)
    end.

run(stats) ->
    Ref = make_ref(),
    rpc:cast(remote(), recon_escript, stats, [interval(), self(), Ref]),
    init(stats, Ref);
run(procs) ->
    Ref = make_ref(),
    rpc:cast(remote(), recon_escript, procs, [key(), num(), interval(), self(), Ref]),
    init(procs, Ref);
run(inet) ->
    Ref = make_ref(),
    rpc:cast(remote(), recon_escript, inet, [key(), num(), interval(), self(), Ref]),
    init(inet, Ref).

nametype(Node) ->
    {match,[_,Host]} = re:run(atom_to_list(Node),
                              "(.*)@(.*)",
                              [{capture, [1,2], list}]),
    case re:run(Host, ".*\\..*") of
        nomatch -> shortnames;
        _ -> longnames
    end.

init(Type, Ref) ->
    process_flag(trap_exit, true),
    loop(#state{type=Type, ref=Ref}).

loop(State=#state{ref=Ref}) ->
    {ok, Cols} = io:columns(),
    {ok, Rows} = io:rows(),
    receive
        {Ref, Data} ->
            IoList = format(State, Cols, Rows, Data),
            io:format("\e[0;0H\e[2J"), % erase previous screen
            io:format("~s", [IoList]),
            loop(State);
        {'EXIT', _Pid, Reason} ->
            throw({remote_exit, Reason})
    end.

format(#state{type=stats}, Cols, Rows, Data) ->
   Mem = ["Memory (total: ",
             integer_to_list(proplists:get_value(memory_total, Data)), ")\n",
          pad("", Cols, "="), "\n",
          "Processes: ",
             to_io(proplists:get_value(memory_procs, Data),
                   (Cols div 2) - 12, " "),
          "Binary: ",
             to_io(proplists:get_value(memory_bin, Data),
                   (Cols div 2) - 9, " "),
          "\n",
          "ETS: ",
             to_io(proplists:get_value(memory_ets, Data),
                   (Cols div 2) - 6, " "),
          "Atoms: ",
             to_io(proplists:get_value(memory_atoms, Data),
                   (Cols div 2) - 8, " "),
          "\n\n",
          "Garbage Collections: ",
             to_io(proplists:get_value(gc_count, Data),
                   (Cols div 2) - 22, " "),
          "Words Reclaimed: ",
             to_io(proplists:get_value(gc_words_reclaimed, Data),
                   (Cols div 2) - 19, " "),
             "\n", pad("", Cols, "-"), "\n\n"],
   Procs = ["Processes (count: ",
             integer_to_list(proplists:get_value(process_count, Data)), ")\n",
            pad("", Cols, "="), "\n",
            "Run Queue: ",
             to_io(proplists:get_value(run_queue, Data),
                   (Cols div 2) - 12, " "),
            "Reductions: ",
             to_io(proplists:get_value(reductions, Data),
                   (Cols div 2) - 13, " "),
             "\n", pad("", Cols, "-"), "\n\n"],
    Misc = ["Misc", "\n",
             pad("", Cols, "="), "\n",
            "Error Logger Queue Length: ",
            integer_to_list(proplists:get_value(error_logger_queue_len, Data)),
            "\n",
            "Bytes in: ",
             to_io(proplists:get_value(bytes_in, Data),
                   (Cols div 2) - 12, " "),
            "Bytes out: ",
             to_io(proplists:get_value(bytes_out, Data),
                   (Cols div 2) - 13, " "),
             "\n", pad("", Cols, "-"), "\n\n"],
    kill_lines([Mem, Procs, Misc], Rows);
format(#state{type=procs}, Cols, Rows, Data) ->
    %% fixed size for pids, ?KEYPCT% for the key, the rest for details,
    %% with a floor size of ?MINLEN for the key
    PidLen = ?PIDLEN,
    Free = Cols - PidLen,
    KeyLen = max(?MINLEN, trunc(Free*?KEYPCT)),
    DetailsLen = Free-KeyLen,
    Lines = lists:map(fun({Pid, Key, Details}) ->
        [to_io(Pid, PidLen, " "),
         to_io(Key, KeyLen, " "),
         to_io(Details, DetailsLen),
         "\n"]
    end, Data),
    [titles(procs, {PidLen, KeyLen, DetailsLen})
     | lists:sublist(Lines, Rows-3)];
format(#state{type=inet}, Cols, Rows, Data) ->
    %% fixed size for pids, ?KEYPCT% for the key, the rest for details,
    %% with a floor size of ?MINLEN for the key
    PortLen = ?PORTLEN,
    Free = Cols - PortLen,
    KeyLen = max(?MINLEN, trunc(Free*?KEYPCT)),
    DetailsLen = Free-KeyLen,
    Lines = lists:map(fun({Port, Key, Details}) ->
        [to_io(Port, PortLen, " "),
         to_io(Key, KeyLen, " "),
         to_io(Details, DetailsLen),
         "\n"]
    end, Data),
    [titles(inet, {PortLen, KeyLen, DetailsLen})
     | lists:sublist(Lines, Rows-3)].

to_io(Term, Len) ->
    Text = if is_pid(Term); is_port(Term) -> % make local
                  re:replace(io_lib:format("~p",[Term]), "<[0-9]+", "<0");
              true ->
                  io_lib:format("~p",[Term])
           end,
    lists:sublist(
      re:replace(re:replace(Text, ",", ", ", [global]),
                 "\\s+",
                 " ",
                 [global, {return, list}]),
      Len).

to_io(Term, Len, Pad) ->
    pad(to_io(Term, Len), Len, Pad).

titles(procs, {Pid, Key, Details}) ->
    [pad("Pid", Pid, " "),
     to_io(key(), Key, " "),
     pad("Details", Details, " "),
     "\n",
     pad("", Pid+Key+Details, "=")];
titles(inet, {Port, Key, Details}) ->
    [pad("Ports", Port, " "),
     to_io(key(), Key, " "),
     pad("Details", Details, " "),
     "\n",
     pad("", Port+Key+Details, "=")].

pad(Text, Len, Pad) ->
    TextLen = iolist_size(Text),
    if TextLen =:= Len -> Text;
       TextLen < Len -> [Text, lists:duplicate(Len-TextLen, Pad)]
    end.

kill_lines(IoList, Len) ->
    Bin = iolist_to_binary(IoList),
    {Pos,_} = lists:last(lists:sublist(binary:matches(Bin,<<"\n">>), Len)),
    binary:part(Bin, 0, Pos).


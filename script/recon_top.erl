%%% run with escript recon_top.erl. Will load the recon_top module
%%% remotely to the other node.
-module(recon_top).
-export([main/1]).

main(Args) ->
    try
        parse_args(Args),
        default_args(),
        connect(),
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
    key(list_to_existing_atom(Key)),
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

run(stats) ->
    Ref = make_ref(),
    rpc:cast(remote(), recon_escript, stats, [interval(), self(), Ref]),
    loop(Ref);
run(procs) ->
    Ref = make_ref(),
    rpc:cast(remote(), recon_escript, procs, [key(), num(), interval(), self(), Ref]),
    loop(Ref);
run(inet) ->
    Ref = make_ref(),
    rpc:cast(remote(), recon_escript, inet, [key(), num(), interval(), self(), Ref]),
    loop(Ref).

nametype(Node) ->
    {match,[_,Host]} = re:run(atom_to_list(Node),
                              "(.*)@(.*)",
                              [{capture, [1,2], list}]),
    case re:run(Host, ".*\\..*") of
        nomatch -> shortnames;
        _ -> longnames
    end.

loop(Ref) ->
    process_flag(trap_exit, true),
    receive
        {Ref, IoList} ->
            io:format("\e[0;0H\e[2J"), % erase preious screen
            io:format("~s", [IoList]);
        {'EXIT', _Pid, Reason} ->
            throw({remote_exit, Reason})
    end,
    loop(Ref).

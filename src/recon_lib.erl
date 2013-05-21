-module(recon_lib).
-export([sliding_window/2, sample/2, count/1,
         port_list/1, port_list/2]).

-type diff() :: [{Key::term(), Diff::number(), Other::term()}].

%% Compare two samples and return a list based on some key
-spec sliding_window(First::diff(), Last::diff()) -> [diff()].
sliding_window(First, Last) ->
    Dict = lists:foldl(
        fun({Key, {Current, Other}}, Acc) ->
            dict:update(Key,
                        fun({Old,_Other}) -> {Current-Old, Other} end,
                        {Current, Other},
                        Acc)
        end,
        dict:from_list([{K,{V,O}} || {K,V,O} <- First]),
        [{K,{V,O}} || {K,V,O} <- Last]
    ),
    [{K,V,O} || {K,{V,O}} <- dict:to_list(Dict)].

%% Runs a fun over time and returns both samples.
-spec sample(Ms::non_neg_integer(), fun(() -> term())) ->
      {First::term(), Second::term()}.
sample(Delay, Fun) ->
    First = Fun(),
    timer:sleep(Delay),
    Second = Fun(),
    {First, Second}.

%% Counts similar entries in a dict
-spec count([term()]) -> [{Count::integer(), term()}].
count(Terms) ->
    Dict = lists:foldl(
        fun(Val, Acc) ->  dict:update_counter(Val, 1, Acc) end,
        dict:new(),
        Terms
    ),
    dict:to_list(Dict).

port_list(Attr) ->
    [{Port,Val} || Port <- erlang:ports(),
                   {_, Val} <- [erlang:port_info(Port, Attr)]].

port_list(Attr, Val) ->
    [Port || Port <- erlang:ports(),
             {Attr, Val} =:= erlang:port_info(Port, Attr)].


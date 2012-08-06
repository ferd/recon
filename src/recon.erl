-module(recon).
-export([reductions/2]).

-spec reductions(non_neg_integer(), pos_integer()) -> [term()].
reductions(Time, Num) ->
    lists:sublist(
        lists:usort(
            fun({_, {X,_,_}}, {_, {Y,_,_}}) -> X > Y end,
            dict:to_list(build(fetch(Time)))),
        Num).

fetch(Delay) ->
    Sample1 = sample(),
    timer:sleep(Delay),
    Sample2 = sample(),
    {Sample1, Sample2}.

build({Sample1, Sample2}) ->
    lists:foldl(
        fun({Pid, {Red, Current, Init}}, Dict) ->
                dict:update(Pid,
                            fun({OldRed,_,_}) -> {Red-OldRed, Current, Init} end,
                            {Red, Current, Init},
                            Dict)
        end,
        dict:from_list(Sample1),
        Sample2).

sample() ->
    [{Pid, {Reds, Curr, Init}}
     || Pid <- processes(),
        {_, Reds} <- [process_info(Pid, reductions)],
        {_, Curr} <- [process_info(Pid, current_function)],
        {_, Init} <- [process_info(Pid, initial_call)]].

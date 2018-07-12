-module(records1).
-author("bartlomiej.gorny@erlang-solutions.com").
%% API
-export([state/3, another/3]).

-record(state, {aaa, bbb, ccc}).
-record(another, {ddd, eee, fff}).

state(A, B, C) ->
    #state{aaa = A, bbb = B, ccc = C}.

another(D, E, F) ->
    #another{ddd = D, eee = E, fff = F}.
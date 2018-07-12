-module(records2).
-author("bartlomiej.gorny@erlang-solutions.com").
%% API
-export([state/3, another/4]).

-record(state, {one, two, three}).
-record(another, {one, two = something, three :: boolean(), four = 123 :: integer()}).

state(A, B, C) ->
    #state{one = A, two = B, three = C}.

another(A, B, C, D) ->
    #another{one = A, two = B, three = C, four = D}.
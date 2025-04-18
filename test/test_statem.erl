%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Mathias Green
%%% @doc
%%% Basic statem for testing purposes
%%% @end
%%% Created : 08 Jun 2019 by Mathias Green (flmath)
%%%-------------------------------------------------------------------
-module(test_statem).

-behaviour(gen_statem).


-include_lib("eunit/include/eunit.hrl").


-define(HeavyStateWindowLength, 300).

%% API
-export([start/0, stop/0]).
-export([get_state/0, switch_state/0]).
%% gen_statem callbacks
-export([init/1, callback_mode/0, light_state/3, heavy_state/3,
	 terminate/3, code_change/4]).
-define(NAME, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
start() ->
    gen_statem:start({local, ?NAME}, ?MODULE, [], []).

switch_state()->
    gen_statem:cast(?NAME, switch_state).

get_state()->
    gen_statem:call(?NAME, get_value).

stop() ->
    gen_statem:stop(?NAME).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->
    ct:pal("Starting ~p~n", [?MODULE]),
    {ok, light_state, #{iterator=>0}}.

callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
light_state(cast, switch_state, State) ->
    #{iterator:=Number}=State,
    traced_function(enter_heavy_state, Number),
    {next_state, heavy_state, State#{iterator:=Number+1}, ?HeavyStateWindowLength};
light_state({call, From}, get_value, State) ->    
    #{iterator:=Number} = State,     
    {next_state, light_state, State, [{reply, From, {ok, light_state, Number}}]}.

heavy_state(cast, switch_state, State) ->   
    #{iterator:=Number} = State,
    traced_function(enter_light_state, Number),
    {next_state, light_state, State#{iterator:=Number+1}};
heavy_state(timeout, _Event, State) ->   
    #{iterator:=Number} = State,
    traced_function(keep_heavy_state, Number),
    {next_state, heavy_state, State#{iterator:=Number+1}, ?HeavyStateWindowLength};
heavy_state({call, From}, get_value, State) ->  
    #{iterator:=Number} = State,       
    {next_state, heavy_state, State,
     [{reply, From, {ok, heavy_state, Number}}, 
      {timeout, ?HeavyStateWindowLength, back_to_heavy_state}]}.


%%--------------------------------------------------------------------
terminate(Reason, _StateName, _State) ->
    ct:pal("Terminate ~p ~p ~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

traced_function(_StateName, _Number)->
    ct:pal("log called from state ~p number ~p~n", [_StateName, _Number]).
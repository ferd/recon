%%%-------------------------------------------------------------------
%%% @author Roberto Aloi
%%%   [http://roberto-aloi.com]
%%% @doc Scheduler utilization helper module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(recon_scheduler).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , profile/0
        , stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ts0 = []}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

profile() ->
  gen_server:call(?SERVER, profile).

stop() ->
  gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  erlang:system_flag(scheduler_wall_time, true),
  Ts0 = get_schedulers_utilization(),
  {ok, #state{ts0 = Ts0}}.

handle_call(profile, _From, #state{ts0 = Ts0} = State) ->
  Ts1   = get_schedulers_utilization(),
  PSU   = per_scheduler_utilization(Ts0, Ts1),
  Total = total_utilization(Ts0, Ts1),
  Reply = [ {per_scheduler_utilization, PSU}
          , {total_utilization        , Total}
          ],
  {reply, Reply, State};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  erlang:system_flag(scheduler_wall_time, false),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_schedulers_utilization() ->
  lists:sort(erlang:statistics(scheduler_wall_time)).

per_scheduler_utilization(Ts0, Ts1) ->
  Fun = fun({{I, A0, T0}, {I, A1, T1}}) ->
            {I, (A1 - A0)/(T1 - T0)}
        end,
  lists:map(Fun, lists:zip(Ts0, Ts1)).

total_utilization(Ts0, Ts1) ->
  Fun = fun({{_, A0, T0}, {_, A1, T1}}, {Ai, Ti}) ->
            {Ai + (A1 - A0), Ti + (T1 - T0)}
        end,
  {A, T} = lists:foldl(Fun, {0, 0}, lists:zip(Ts0, Ts1)),
  A/T.

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

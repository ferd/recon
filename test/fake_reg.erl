%% @author <flmathematic@gmail.com>
%%  [https://flmath.github.io]
%% @doc
%% Minimal fake registration module for testing
%% purposes. This module simulates the behavior of the
%% registration in Erlang, allowing us to
%% test the functionality of our code without
%% relying on the actual Erlang registry.
%% @end
-module(fake_reg).
-behaviour(gen_server).

%% API for the gen_server
-export([start/0, stop/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% gen_registry callbacks
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Public API
%%%===================================================================

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, #{}, []).

stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_registry required callbacks
%%%===================================================================

register_name(Name, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {register, Name, Pid}).

unregister_name(Name) ->
    gen_server:call(?SERVER, {unregister, Name}).

whereis_name(Name) ->
    gen_server:call(?SERVER, {whereis, Name}).

send(Name, Msg) ->
    case whereis_name(Name) of
        undefined -> exit({badarg, {Name, Msg}});
        Pid when is_pid(Pid) -> Pid ! Msg, Pid
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(StateMap) ->
    {ok, StateMap}.

handle_call({register, Name, Pid}, _From, Registry) ->
    case maps:is_key(Name, Registry) of
        true -> {reply, {error, already_registered}, Registry};
        false -> {reply, yes, Registry#{Name => Pid}}
    end;

handle_call({unregister, Name}, _From, Registry) ->
    {reply, ok, maps:remove(Name, Registry)};

handle_call({whereis, Name}, _From, Registry) ->
    {reply, maps:get(Name, Registry, undefined), Registry};

handle_call(stop, _From, Registry) ->
    {stop, normal, ok, Registry};

handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% @doc Tests for scheduler utilization helper module
%%%
%%% @end
%%%============================================================================
-module(recon_scheduler_tests).

-include_lib("eunit/include/eunit.hrl").

start_link_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [ ?_assert(is_pid(whereis(recon_scheduler)))
   ]}.

stop_test_() ->
  {setup,
   fun()  -> {ok, _} = recon_scheduler:start_link(),
             ok      = recon_scheduler:stop()
   end,
   fun(_) -> ok end,
   [ ?_assertEqual(undefined, whereis(recon_scheduler))
   ]}.

profile_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [ ?_assert(is_list(recon_scheduler:profile()))
   ]}.

setup() ->
  {ok, _Pid} = recon_scheduler:start_link().

teardown(_) ->
  recon_scheduler:stop().

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

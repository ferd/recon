%%% @author <flmathematic@gmail.com>
%%%  [https://flmath.github.io]
%%% @doc
%%% `recon_trace_use_dbg' is a module that allows API of recon use dbg module
%%% The added value of this solution is more flexibility in the pattern matching
%%% you can pattern match any structure you BEAM can put into function guard.
-module(recon_plugins).
%% API

-type tspec() :: recon_trace_use_dbg:tspec().
-type device() :: atom() | pid() | standard_io | standard_error | user.
-type property() :: atom() | tuple().
-type proplist() :: [property()].
-type init() :: recon_trace_use_dbg:max_traces().
-type matchspecs() :: recon_trace_use_dbg:matchspecs().

-type filter_fun_type() :: fun((tspec() | [tspec()], init()) -> init()).

-callback filter_fun(matchspecs(), init(), device(), pid(), proplist()) -> filter_fun_type().
-callback is_plugin() -> boolean().
-callback start_value(matchspecs(), init()) -> {matchspecs(), map(), init()}.

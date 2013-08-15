recon
=====

Recon wants to be a set of tools usable in production to diagnose Erlang problems or inspect production environment safely.

To build the library:

    rebar compile

Documentation for the library can be obtained at http://ferd.github.io/recon/

Changelog
---------

- 0.4.1: fixed bug where nodes with lots of processes could see the GC call
  fail if said processes failed between long calls within the `bin_leak`
  function call.
- 0.4.0: fixed bug where nodes with lots of processes or ports could see their
  count or window functions fail because a process or socket closed between the
  time the function started and before it finished. This ends up changing the
  API in `recon_lib` for the window and count functions that take a specific
  pid as an argument.
- 0.3.1: factored out some logic from `recon:info/1` into `recon_lib:term_to_pid`
  and allowed arbitrary terms to be used for pids in `recon:get_state/1`.

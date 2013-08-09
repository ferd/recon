recon
=====

Recon wants to be a set of tools usable in production to diagnose Erlang problems or inspect production environment safely.

To build the library:

    rebar compile

Documentation for the library can be obtained at http://ferd.github.io/recon/

Changelog
---------

- 0.3.1: factored out some logic from `recon:info/1` into `recon_lib:term_to_pid`
  and allowed arbitrary terms to be used for pids in `recon:get_state/1`.

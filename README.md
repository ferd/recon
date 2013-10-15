recon
=====

Recon wants to be a set of tools usable in production to diagnose Erlang problems or inspect production environment safely.

To build the library:

    rebar compile

Documentation for the library can be obtained at http://ferd.github.io/recon/

It is recommended that you use tags if you do not want bleeding edge and development content for this library.

Changelog
---------

- 1.2.0:
  - add `recon_alloc:snapshot*` functions, which allow memory allocation
    snapshots to be taken, saved on disk, reloaded, and analyzed on-demand.
    Thanks to Lukas Larsson for this functionality.
- 1.1.0:
  - add `recon_lib:term_to_port` to convert a string back to a
    usable port.
  - add `recon:port_info/1` and `recon:port_info/2`
  - add `recon_alloc` module
- 1.0.0: add `info/2` and `info/4`. The `memory` info type thus gets renamed
  to `memory_used`, in order to avoid conflicts when picking between a type
  and a specific process attribute. Types exported by the module also get
  updated.
- 0.4.2: extended `app_deps.erl` to read apps/ directories for releases
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

recon
=====

Recon wants to be a set of tools usable in production to diagnose Erlang problems or inspect production environment safely.

To build the library:

    rebar3 compile

Documentation for the library can be obtained at http://ferd.github.io/recon/

It is recommended that you use tags if you do not want bleeding edge and development content for this library.

Current Status
--------------

[![Build Status](https://github.com/ferd/recon/workflows/build/badge.svg)](https://github.com/ferd/recon)

Versions supported: OTP-18 and up. Support of OTP-17 down to R15B02 is best effort. Builds with Rebar3 require OTP-18.3 and up because that's what the build tools support. Testing may eventually clamp up to OTP-supported releases (current and the two prior).

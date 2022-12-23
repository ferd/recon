# Recon

Recon is a library to be dropped into any other Erlang project,
to be used to assist DevOps people diagnose problems in production
nodes.

The source code can be obtained from [the github repo](https://github.com/ferd/recon).
A toy application to experiment with can be found at [recon_demo](https://github.com/ferd/recon_demo).

## Installing

Add recon to your project by adding it as a dependency. With rebar3 this is done as:

```
{deps, [recon]}.
```

If you are using releases, make sure to add recon to this list of applications bundled in:

```
{relx, [
    {release, {myrel, "0.1.0"},
      [myapp,
       recon, % <-- here
       sasl]
    },
    ...
]}.
```

## Running tests

Call `rebar3 check`

## Using it

Each module in the API describes how it should be used in a stand-alone manner. Insights about how to debug a production Erlang system, often relying on this library, can be found in the free online book [Erlang in Anger](https://www.erlang-in-anger.com/).

The library also contains a few basic scripts that have proven useful in the past but may not be as much today in its `script/` directory. These include ways to visualize application dependencies, and a few short scripts to extract high-level information from crashdumps.

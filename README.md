recon
=====

Recon wants to be a set of tools usable in production to diagnose Erlang problems or inspect production environment safely.

Functions
---------

Functions will be added as the need arise (see: not covered by other tools far and wide for the time being).

Reduction count
---------------

To get the number of reduction of a sliding window in time on a given node. The call to `recon:reductions(5000, 5)` will check for the biggest reduction users for 5 seconds, and return the 5 bigger consumers:

    1> recon:reductions(5000, 5).
    [{<0.32.0>,
      {2348,{dets,open_file_loop2,2},{proc_lib,init_p,5}}},
     {<0.48.0>,
      {1977,{recon,'-sample/0-lc$^1/1-1-',3},{erlang,apply,2}}},
     {<0.15.0>,{0,{gen_server,loop,6},{proc_lib,init_p,5}}},
     {<0.31.0>,{0,{gen_server,loop,6},{proc_lib,init_p,5}}},
     {<0.47.0>,{0,{gen_server,loop,6},{proc_lib,init_p,5}}}]
    2> sys:get_status(pid(0,32,0)).
    {status,<0.32.0>,
            {module,dets},
            [[{1336,3384},
              {'$ancestors',[dets_sup,kernel_safe_sup,kernel_sup,<0.9.0>]},
              {312,1336},
              {'$initial_call',{dets,init,2}},
              {1340,5432}],
             running,<0.30.0>,[],
             {head,512,1024,512,
                   {file_descriptor,prim_file,{#Port<0.760>,21}},
                   479,479,9,0,set,1,
                   {v,v,v,v,v,{l,55320},v,{...},...},
                   57043,
                   [{5,21},{6,75},{7,123},{8,76},{9,...}],
                   500,dirty,false,phash2,true,512,...}]}

This function is particularly useful when processes are short-lived, usually too short to inspect through other tools, in order to figure out what kind of code is taking up all the time on a given node.

It is important to see its use as a sliding window. A program's timeline during sampling might look like this:

    --w---- [Sample1] ---x-------------y----- [Sample2] ---z--->

Some processes will live between `w` and die at `x`, some between `y` and `z`, and some between `w` and `z`. These samples won't be too significant as they're incomplete. If the majority of your processes run in a time interval `x`...`y`, you should make sure that your sampling time is smaller than this, and do it many times. This will allow to take snapshots that are more representative, both for short-lived and long-lived processes, in order to observe a trend. Not doing this can skew the results: long-lived processes that have 10 times the time to accumulate reductions will look like a bottleneck when they're not one.

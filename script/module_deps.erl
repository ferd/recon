%#!/usr/bin/env escript
-module(module_deps).
-export([main/1]).

-spec main([string()]) -> ok.
main([DirName, OutName]) ->
    Find = "zsh -c 'noglob find " ++ DirName ++ " -name *.erl -o -name *.hrl'",
    File_Name_Stream = os:cmd(Find),
    File_Names = string:tokens(File_Name_Stream, " \n"),
    to_graphviz(read_deps(File_Names), OutName);
main(_) ->
    io:format("usage: module_deps.erl <source_dir_name> <out_file_name>").

-spec read_deps([string()]) -> [[{string(), string()}]].
read_deps(Files) ->
    [read_dep(File) || File <- Files].

-spec read_dep(string()) -> [{string(), string()}].
read_dep(File) ->
    Call_Stream = os:cmd("grep -oh '[_@a-zA-Z]\\{1,\\}:[_@a-zA-Z]\\{1,\\}' " ++ File),
    Calls = string:tokens(Call_Stream, " \n"),
    Mod_Using = filename:rootname(filename:basename(File)),
    MF_Pairs = [string:tokens(Call, ":") || Call <- Calls],
    [{Mod_Using, Mod_Used} || [Mod_Used, _Func] <- MF_Pairs].

-spec to_graphviz([{string(), string()}], file:name_all()) -> ok.
to_graphviz(Mod_Deps, OutName) ->
    All_Mods = lists:usort(lists:flatten(Mod_Deps)),
    Bytes = ["digraph G {\n",
        [io_lib:format("    ~p->~p~n", [Mod_Using, Mod_Used])
            || {Mod_Using, Mod_Used} <- All_Mods],
             "}"],
    file:write_file(OutName, Bytes),
    Name_Core = filename:rootname(OutName),
    os:cmd("dot " ++ OutName ++ " -Tpng -o " ++ Name_Core  ++ ".png"),
    ok.

%%% @author David Åberg <>
%%% @copyright (C) 2012, David Åberg
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2012 by David Åberg <>

-module('protoc-erl').

-export([main/1]).

main(Args) ->
    OptSpecList = [{file, undefined, undefined, string, "Input protobuf file"}],
    case getopt:parse(OptSpecList, Args) of
	{ok,{Opt,_Rest}} ->
	    case proplists:get_value(file,Opt) of
		undefined -> getopt:usage(OptSpecList, "protoc-erl");
		File -> 
		    protobuffs_compile:generate_source (File)
	    end;
	_ ->
	    getopt:usage(OptSpecList, "protoc-erl")
    end.



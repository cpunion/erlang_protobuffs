%%% @author David Åberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by David AAberg <davabe@hotmail.com>

-module(protobuffs_compile_tests).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").
 
%proper_specs_test() ->
%    ?assertEqual([],
%		 (proper:check_specs(protobuffs_compile, [long_result]))).

proper_lib_specs_test() ->
    ?assertEqual([],
		 (proper:check_specs(protobuffs_compile_lib, [long_result]))).

proper_module_test() ->
    ?assertEqual([],
		  proper:module(?MODULE, [long_result])).

setup() ->
    Modules = [protobuffs_io],
    meck:new(Modules),
    meck:expect(protobuffs_io, open,
		fun (_, _) -> {ok, in_file} end),
    meck:expect(protobuffs_io, close, fun (_) -> ok end),
    meck:expect(protobuffs_io, request,
		fun (_) -> {eof, dummy} end),
    meck:expect(protobuffs_io, compile_forms,
		fun (_, _) -> {ok, dummy, <<"Bytest">>, dummy} end),
    meck:expect(protobuffs_io, write_file,
		fun (_, _) -> ok end),
    meck:expect(protobuffs_io, format,
		fun (_, _, _) -> ok end),
    meck:expect(protobuffs_io, path_open,
		fun (Path, FileName, _) ->
			{ok, io_device, filename:join([Path, FileName])}
		end),
    Modules.

cleanup(Modules) -> meck:unload(Modules).

scan_file_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [?_assertMatch(ok,
		    (protobuffs_compile:scan_file(dummy_file))),
      ?_assertMatch(ok,
		    (protobuffs_compile:scan_file("dummy_file.proto")))]}.

scan_string_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [?_assertMatch(ok,
		    (protobuffs_compile:scan_string("", "dummy")))]}.

generate_source_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [?_assertMatch(ok,
		    (protobuffs_compile:generate_source(dummy_file))),
      ?_assertMatch(ok,
		    (protobuffs_compile:generate_source("dummy_file.proto")))]}.

parse_imports_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [?_assertMatch([],
		    (protobuffs_compile_lib:parse_imports([], dummy_path))),
      ?_assertMatch([{import, dummy_import_file}],
		    (protobuffs_compile_lib:parse_imports([{import,
							    dummy_import_file}],
							  "dummy_path"))),
      ?_assertMatch([what_ever],
		    (protobuffs_compile_lib:parse_imports([what_ever],
							  dummy_path)))]}.

parse_string_test_() ->
    [?_assertMatch({ok,[]},protobuffs_compile_lib:parse_string(""))].

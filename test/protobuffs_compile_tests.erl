%%% @author David Åberg <>
%%% @copyright (C) 2012, David Åberg
%%% @doc
%%%
%%% @end
%%% Created : 26 Apr 2012 by David Åberg <>

-module(protobuffs_compile_tests).

-compile(export_all).

%-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%proper_lib_specs_test_() ->
%    error_logger:tty(false),
%    {timeout, 30, 
%     ?_assertEqual([],
%		  (proper:check_specs(protobuffs_compile, [long_result])))}.

%proper_module_test_() ->
%    {timeout, 60,
%     ?_assertEqual([], proper:module(?MODULE, [long_result]))}.

filter_forms_test_() ->
    [].

scan_string_test_() ->
    {foreach, fun setup/0, fun teardown/1, 
     [fun test_simple_string/0]}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
setup() ->
    Mods = [protobuffs_io],
    meck:new(Mods),
    meck:expect(protobuffs_io, write_file, fun(_File, _Bytes) -> ok end),
    meck:expect(protobuffs_io, open, fun(_File, _Options) -> {ok, file_ref} end),
    meck:expect(protobuffs_io, close, fun(file_ref) -> ok end),
    meck:expect(protobuffs_io, format, fun(file_ref, _FormatString, _WriteFields) -> ok end),
    Mods.

teardown(Mods) ->
    meck:unload(Mods).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
test_simple_string() ->
    Message = 
"message Person 
{
    required string name = 1;
    required string address = 2;
    required string phone_number = 3;
    required int32 age = 4;
}",
    Result = protobuffs_compile:scan_string(Message,"simple"),
    [?assertEqual(ok,Result),
     ?assert(meck:called(protobuffs_io,open,["simple.hrl",'_'])),
     ?assert(meck:called(protobuffs_io,write_file,["simple.beam",'_'])),
     ?assert(meck:validate(protobuffs_io))].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

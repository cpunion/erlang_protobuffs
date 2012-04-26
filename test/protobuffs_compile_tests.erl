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

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
setup() ->
    meck:new(protobuff_io).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

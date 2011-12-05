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

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%proper_specs_test() ->
%    ?assertEqual([],
%		 (proper:check_specs(protobuffs_compile, [long_result]))).

proper_lib_specs_test() ->
    ?assertEqual([],
		 (proper:check_specs(protobuffs_compile_lib, [long_result]))).

proper_module_test() ->
    ?assertEqual([],
		  proper:module(?MODULE, [long_result])).

filter_forms_test_() ->
    [test_filter_attribute_file(),
     test_filter_attribute_module(),
     test_filter_attribute_export(),
     test_filter_attribute_record(),
     test_filter_function_encode1(),
     test_filter_function_encode2(),
     test_filter_function_encode_extensions(),
     test_filter_function_encode_pikachu(),
     test_filter_function_iolist(),
     test_function_enum_to_int(),
     test_function_int_to_enum(),
     test_function_decode_pikachu(),
     test_function_decode(),
     test_function_to_record(),
     test_function_decode_extentions(),
     test_function_extension_size(),
     test_function_has_extension(),
     test_function_get_extension(),
     test_function_set_extension()].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
test_filter_attribute_file() ->
    Basename = "test_module",
    L = 1,
    Enums = ignored,
    Messages = ignored,
    Acc = [],
                 
    Attribute = {attribute,L,file,{"DummyString",L}},
    Expected = {attribute,L,file,{"src/"++Basename++".erl",L}},
    [?_assertEqual([Expected],
		   protobuffs_compile_lib:filter_forms(Messages, 
						       Enums, 
						       [Attribute],
						       Basename,
						       Acc))].

test_filter_attribute_module() ->
    Basename = "test_module",
    Enums = ignored,
    Messages = ignored,
    Acc = [],
                 

    {ok,Attribute} = parse("-module(pokemon_pb)."),
    {ok,Expected} = parse("-module("++Basename++")."),
    [?_assertEqual([Expected],
		   protobuffs_compile_lib:filter_forms(Messages, 
						       Enums, 
						       [Attribute],
						       Basename,
						       Acc))].

test_filter_attribute_export() ->
    Name = "name",
    Fields = ignored,
    Extends = ignored,
    Messages = [{Name,Fields,Extends}],
    Basename = ignored,
    Enums = ignored,

    Acc = [],

    {ok,Attribute} = parse("-export([encode_pikachu/1, decode_pikachu/1])."),
    {ok,Expected} = parse("-export([encode_"++Name++"/1, decode_"++Name++"/1])."),
    [?_assertEqual([Expected],
     		   protobuffs_compile_lib:filter_forms(Messages, 
     						       Enums, 
     						       [Attribute],
     						       Basename,
     						       Acc))].

test_filter_attribute_record() ->
    Name = "name",
    Fields = [{1,optional,"int32","field1",none}],
    Messages1 = [{Name,Fields,ignored}],
    Messages2 = [{Name,Fields,disallowed}],
    Basename = ignored,
    Enums = ignored,

    Acc = [],

    RecordFmt = "-record(~s, {~s ~s}).",
	
    TemplateFunction = string_format(RecordFmt,["pikachu",
						"abc",
						", '$extensions' = dict:new()"]),
    ExpectedFunction1 = string_format(RecordFmt,[Name,
						"field1",
						", '$extensions' = dict:new()"]),
    ExpectedFunction2 = string_format(RecordFmt,[Name,
						"field1",
						""]),

    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction1} = parse(ExpectedFunction1),
    {ok,FilterdFunction2} = parse(ExpectedFunction2),
    
    [?_assertEqual([FilterdFunction1],
		   protobuffs_compile_lib:filter_forms(Messages1, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction2],
		   protobuffs_compile_lib:filter_forms(Messages2, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

test_filter_function_encode1() ->
    Messages = ignored,
    Enums = ignored,
    Basename = ignored,

    {ok,Function} = parse("encode(Record) -> encode(element(1, Record), Record)."),

    Acc = [],

    [?_assertEqual([Function],
		   protobuffs_compile_lib:filter_forms(Messages, 
						       Enums, 
						       [Function],
						       Basename,
						       Acc))].

test_filter_function_encode2() ->
    Name = "name",
    Fields = ignored,
    Extends = ignored,
    Messages = [{Name,Fields,Extends}],
    Enums = ignored,
    Basename = ignored,

    {ok,Function} = parse(
		      "encode(pikachu, Record) -> 
                         iolist_to_binary(iolist(pikachu, Record) ++ 
                                          encode_extensions(Record))."),

    {ok,FilterdFunction} = parse("encode("++Name++", Record) -> 
                         iolist_to_binary(iolist("++Name++", Record) ++ 
                                          encode_extensions(Record))."),
    
    Acc = [],
    
    [?_assertEqual([FilterdFunction],
		   protobuffs_compile_lib:filter_forms(Messages, 
						       Enums, 
						       [Function],
						       Basename,
						       Acc))].

test_filter_function_encode_extensions() ->
    Name = "name",
    Fields = ignored,
    Extends = ignored,
    Messages = [{Name,Fields,Extends}],
    Enums = ignored,
    Basename = ignored,

    {ok,Function} = parse(
		      "encode_extensions(#pikachu{'$extensions' = Extends}) ->
                           [pack(Key, Optionalness, Data, Type, Accer) ||
                                {Key, 
                                 {Optionalness, Data, Type, Accer}} <- 
                                    dict:to_list(Extends)];
                       encode_extensions(_) -> []."),
    
    {ok,FilterdFunction} = parse(
			    "encode_extensions(#"++Name++"{'$extensions' = Extends}) ->
                                [pack(Key, Optionalness, Data, Type, Accer) ||
                                      {Key, 
                                       {Optionalness, Data, Type, Accer}} <- 
                                         dict:to_list(Extends)];
                             encode_extensions(_) -> []."),
    
    Acc = [],
    
    [?_assertEqual([FilterdFunction],
		   protobuffs_compile_lib:filter_forms(Messages, 
						       Enums, 
						       [Function],
						       Basename,
						       Acc))].

test_filter_function_encode_pikachu() ->
    Name = "name",
    Fields = ignored,
    Extends = ignored,
    Messages = [{Name,Fields,Extends}],
    Enums = ignored,
    Basename = ignored,

    {ok,Function} = parse(
		 "encode_pikachu(Record) when is_record(Record, pikachu) ->
                      encode(pikachu, Record)."),
    {ok,FilterdFunction} = parse(
		 "encode_"++Name++"(Record) when is_record(Record, "++Name++") ->
                      encode("++Name++", Record)."),

    Acc = [],

    [?_assertEqual([FilterdFunction],
		   protobuffs_compile_lib:filter_forms(Messages, 
						       Enums, 
						       [Function],
						       Basename,
						       Acc))].

test_filter_function_iolist() ->
    Extends = ignored,
    Name = "name",
    Functions = [{1,optional,"int32","field1",none},
		 {2,optional,"int32","field2",none}],
    Messages = [{Name,Functions,ignored}],
    Enums = ignored,
    Basename = ignored,

    PackFmt = "pack(~w, ~w, with_default(Record#~s.~s, ~w), ~s, [])",
    IolistFmt = "iolist(~s, Record) -> [~s].",

    Pack = string_format(PackFmt,
			 [1, required, "pikachu", "abc", none, "string"]),

    Template = string_format(IolistFmt, ["pikachu", Pack]),
    {ok,Function} = parse(Template),

    FilterdPack =  string:join(
		     [string_format(PackFmt,
				    [Id, Tag, Name, FName, Default, FType]) || 
			 {Id,Tag,FType,FName,Default} <- Functions],", "),
    
    Expected = string_format(IolistFmt,
                             [Name, FilterdPack]),
    {ok,FilterdFunction} = parse(Expected),

    Acc = [],

    [?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       Enums, 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([[]],
    		   protobuffs_compile_lib:filter_forms([], 
    						       Enums, 
    						       [[]],
    						       Basename,
						       Acc))].

test_function_enum_to_int() ->
    Functions = ignored,
    Messages = ignored,
    Enums = [{enum,"enumname",1,val}],
    Basename = ignored,

    EnumToIntFmt = "enum_to_int(~p,~p) -> ~p.",
    EnumToInt = string:join([string_format(EnumToIntFmt,
                                           [pikachu, value, 1])],", "),

    FilterdEnumToInt = string:join(
			 [string_format(EnumToIntFmt,
			                [list_to_atom(ValName), EnumValue,
			                 IntValue]) || 
			     {enum,ValName,IntValue,EnumValue} <- Enums],", "),

    {ok,Function} = parse(EnumToInt),
    {ok,FilterdFunction} = parse(FilterdEnumToInt),

    Acc = [],

    [?_assertEqual([Function],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       Enums, 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_int_to_enum() ->
    Messages = ignored,
    Enums = [{enum,"enumname1",1,val1},{enum,"enumname2",2,val2}],
    Basename = ignored,
    
    IntToEnumFmt = "int_to_enum(~w,~w) -> ~w",
    IntToEnum = "int_to_enum(_,Val) -> Val.",

    ExpandedFunction = [string_format(IntToEnumFmt,
				      [list_to_atom(ValName), IntValue, EnumValue]) || 
			   {enum,ValName,IntValue,EnumValue} <- Enums],
    FilterdIntToEnum = string:join(ExpandedFunction++[IntToEnum],"; "),

    {ok,Function} = parse(IntToEnum),
    {ok,FilterdFunction} = parse(FilterdIntToEnum),
    
    Acc = [],
    
    [?_assertEqual([Function],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       Enums, 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_decode_pikachu() ->
    Name = "name",
    Fields = ignored,
    Extends = ignored,
    Messages = [{Name,Fields,Extends}],
    Basename = ignored,
    Acc = [],

    DecodePikachuFmt = "decode_~s(Bytes) when is_binary(Bytes) -> decode(~s, Bytes).",
    TemplateFunction = string_format(DecodePikachuFmt,["pikachu", "pikachu"]),
    ExpectedFunction = string_format(DecodePikachuFmt,[Name, Name]),
    
    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction} = parse(ExpectedFunction),

    [?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_decode() ->
    Name = "name",
    Fields = [{1,optional,"int32","field1",none},
	      {2,optional,"int32","field2",none}],
    Extends = ignored,
    Messages = [{Name,Fields,Extends}],
    Basename = ignored,
    Enums = [{enum,"enumname1",1,val1},{enum,"enumname2",2,val2}],
    Acc = [],

    TypesFmt = "{~w,~s,~s,[]}",
    DecodeFmt = "decode(~s, Bytes) when is_binary(Bytes) -> Types = [~s], Defaults = [{false, '$extensions', dict:new()}], Decoded = decode(Bytes, Types, Defaults), to_record(~s, Decoded).",
    
    TemplateTypes = string_format(TypesFmt,[1,"abc","int32"]),
    TemplateFunction = string_format(DecodeFmt,["pikachu",TemplateTypes,"pikachu"]),

    ExpectedTypes = string:join([string_format(TypesFmt,[Id,FName,Type])||{Id,_,Type,FName,_} <- Fields ],", "),
    ExpectedFunction = string_format(DecodeFmt,[Name,ExpectedTypes,Name]),
    
    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction} = parse(ExpectedFunction),

    [?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_to_record() ->
    Name = "name",
    Fields = [],
    Extends = ignored,
    Messages = [{Name,Fields,Extends}],
    Basename = ignored,
    Enums = [],
    Acc = [],

    ToRecordFmt = "to_record(~s, DecodedTuples) -> Record1 = lists:foldr(fun({_FNum, Name, Val}, Record) -> set_record_field(record_info(fields, ~s), Record, Name, Val) end, #~s{}, DecodedTuples), decode_extensions(Record1).",

    TemplateToRecord = string_format(ToRecordFmt,["pikachu","pikachu","pikachu"]),
    
    ExpectedToRecord = string_format(ToRecordFmt,[Name,Name,Name]),

    {ok,Function} = parse(TemplateToRecord),
    {ok,FilterdFunction} = parse(ExpectedToRecord),

    [?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_decode_extentions() ->
    Name = "name",
    Fields = [],
    Extends = [],
    Messages = [{Name,Fields,Extends}],
    Basename = ignored,
    Enums = [],
    Acc = [],
    
    DecodeExtensionsFmt = "decode_extensions(#~s{'$extensions' = Extensions} = Record) ->"
	++ " Types = [],"
	++" NewExtensions = decode_extensions(Types, dict:to_list(Extensions), []),"
	++ " Record#~s{'$extensions' = NewExtensions};"
	++ " decode_extensions(Record) -> Record.",

    TemplateFunction = string_format(DecodeExtensionsFmt,["pikachu","pikachu"]),
    ExpectedFunction = string_format(DecodeExtensionsFmt,[Name,Name]),
    
    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction} = parse(ExpectedFunction),

    [?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_extension_size() ->
    Name = "name",
    Fields = [],
    Extends = [],
    Messages1 = [{Name,Fields,[]}],
    Messages2 = [{Name,Fields,disallowed}],
    Messages3 = [{Name,Fields,Extends}],
    Basename = ignored,
    Enums = [],
    Acc = [],

    ExtensionsSizeFmt = "extension_size(#~s{'$extensions' = Extensions}) ->"
	" dict:size(Extensions);",
    ExtensionsSizeDefaultFmt = " extension_size(_) ->"
	" 0.",

    TemplateFunction = string_format(ExtensionsSizeFmt++ExtensionsSizeDefaultFmt,["pikachu"]),
    ExpectedFunction1 = string_format(ExtensionsSizeFmt++ExtensionsSizeDefaultFmt,[Name]),
    ExpectedFunction2 = ExtensionsSizeDefaultFmt,

    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction1} = parse(ExpectedFunction1),
    {ok,FilterdFunction2} = parse(ExpectedFunction2),
    
    [?_assertEqual([FilterdFunction1],
    		   protobuffs_compile_lib:filter_forms(Messages1, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction2],
    		   protobuffs_compile_lib:filter_forms(Messages2, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_has_extension() ->
    Name = "name",
    Fields = [],
    Extends = [{1, rule, "int32", "field1", []}],
    Messages1 = [{Name,Fields,[]}],
    Messages2 = [{Name,Fields,disallowed}],
    Messages3 = [{Name,Fields,Extends}],
    Basename = ignored,
    Enums = [],
    Acc = [],

    HasExtensionFmt = "has_extension(#~s{'$extensions' = Extensions}, ~s) -> "
	"dict:is_key(~s, Extensions); ",
    HasExtensionFmt1 = "has_extension(#~s{'$extensions' = Extensions}, ~w) ->"
	"dict:is_key(~w, Extensions); ",
    DefaultFmt = "has_extension(_Record, _FieldName) -> false.",
    
    TemplateFunction = string_format(HasExtensionFmt ++ DefaultFmt,
				     ["pikachu","FieldKey","FieldKey"]),
    ExpectedFunction = string_format(HasExtensionFmt ++ DefaultFmt,
				     [Name,"FieldKey","FieldKey"]),
    ExpectedFunction1 = DefaultFmt,
    ExpectedFunction2 = string_format(HasExtensionFmt1 ++ 
					  HasExtensionFmt ++ 
					  DefaultFmt,
				      [Name,1,1,Name,"field1","field1"]),

    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction} = parse(ExpectedFunction1),
    {ok,FilterdFunction1} = parse(ExpectedFunction2),

    [?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages1, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction],
    		   protobuffs_compile_lib:filter_forms(Messages2, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction1],
    		   protobuffs_compile_lib:filter_forms(Messages3, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

test_function_get_extension() ->
    Name = "name",
    Fields = [],
    Messages1 = [{Name,Fields,[]}],
    Messages2 = [{Name,Fields,disallowed}],
    Basename = ignored,
    Enums = [],
    Acc = [],

    GetExtensionFmt = "get_extension(_Record, _FieldName) ->"
	" undefined.",
    GetExtensionFmt1 = "get_extension(~s, ~s) when is_record(Record, ~s)->"
	" get_extension(Record, 1);",
    GetExtensionFmt2 = "get_extension(#~s{'$extensions' = Extensions}, Int)"
	" when is_integer(Int) ->"
	" case dict:find(Int, Extensions) of"
	" {ok, {_Rule, Value, _Type, _Opts}} ->"
	" {ok, Value};"
	" {ok, Binary} ->"
	" {raw, Binary};"
	" error ->"
	" undefined"
	" end;"
	" get_extension(_Record, _FieldName) ->"
	" undefined.",
    TemplateFunction = string_format(GetExtensionFmt1++GetExtensionFmt2,["Record","fieldatom","pikachu","pikachu"]),
    ExpectedFunction1 = string_format(GetExtensionFmt2,[Name]),
    ExpectedFunction2 = string_format(GetExtensionFmt,[]),

    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction1} = parse(ExpectedFunction1),
    {ok,FilterdFunction2} = parse(ExpectedFunction2),

    [?_assertEqual([FilterdFunction1],
    		   protobuffs_compile_lib:filter_forms(Messages1, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction2],
    		   protobuffs_compile_lib:filter_forms(Messages2, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].
    
test_function_set_extension() ->
    Name = "name",
    Fields = [],
    Extends = [{1, rule, "int32", "field1", []}],
    Messages1 = [{Name,Fields,[]}],
    Messages2 = [{Name,Fields,disallowed}],
    Messages3 = [{Name,Fields,Extends}],
    Basename = ignored,
    Enums = [],
    Acc = [],

    SetExtensionFmt1 = "set_extension(#~s{'$extensions' = Extensions} = Record, "
	"~s, Value) ->"
	"NewExtends = dict:store(1, {rule, Value, ~s, []}, Extensions),"
	"{ok, Record#~s{'$extensions' = NewExtends}};",
    SetExtensionFmt2 = "set_extension(Record, _, _) ->"
	"{error, Record}.",
	
    TemplateFunction = string_format(SetExtensionFmt1 ++ SetExtensionFmt2,["pikachu","fieldname","type","pikachu"]),
    ExpectedFunction1 = string_format(SetExtensionFmt2,[]),
    ExpectedFunction2 = string_format(SetExtensionFmt2,[]),
    ExpectedFunction3 = string_format(SetExtensionFmt1 ++ SetExtensionFmt2,[Name,"field1","int32",Name]),

    {ok,Function} = parse(TemplateFunction),
    {ok,FilterdFunction1} = parse(ExpectedFunction1),
    {ok,FilterdFunction2} = parse(ExpectedFunction2),
    {ok,FilterdFunction3} = parse(ExpectedFunction3),

    [?_assertEqual([FilterdFunction1],
    		   protobuffs_compile_lib:filter_forms(Messages1, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction2],
    		   protobuffs_compile_lib:filter_forms(Messages2, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc)),
     ?_assertEqual([FilterdFunction3],
    		   protobuffs_compile_lib:filter_forms(Messages3, 
    						       [], 
    						       [Function],
    						       Basename,
    						       Acc))].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

parse(String) -> 
    {ok,Tokens,_} = erl_scan:string(String), 
    erl_parse:parse_form(Tokens).

string_format(FmtString,String) ->
    lists:flatten(io_lib:format(FmtString,String)).

%%% @author David Åberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by David AAberg <davabe@hotmail.com>

-module(protobuffs_compile_lib_tests).

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
%		  (proper:check_specs(protobuffs_compile_lib, [long_result])))}.

%proper_module_test_() ->
%    {timeout, 60,
%     ?_assertEqual([], proper:module(?MODULE, [long_result]))}.

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
     test_function_set_extension(),
     test_function_collect_full_messages(),
     test_function_is_enum_type(),
     test_function_set_line_number(),
     test_function_resolve_types(),
     test_function_is_scalar_type(),
     test_function_generate_field_definitions()].

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
    %Enums = ignored,

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
    Name = "name",
    Functions = [{1,optional,"int32","field1",none},
		 {2,required,"int32","field2",none},
		 {3,optional,"string","field3","Test"},
		 {4,optional,"float","field4",1.0},
		 {5,optional,"int","field5",1}],
    Messages = [{Name,Functions,ignored}],
    Enums = ignored,
    Basename = ignored,

    PackFmt = "pack(~w, ~w, with_default(Record#~s.~s, ~p), ~s, [])",
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
	      {2,required,"int32","field2",1},
	      {3,repeated,"int32","field3",none},
	      {4,repeated_packed,"name","field4",none}],
    Messages1 = [{Name,Fields,ignored}],
    Messages2 = [{Name,Fields,disallowed}],
    Basename = ignored,
    Acc = [],

    TypesFmt = "{~w,~s,~s,[~s]}",
    DecodeFmt = "decode(~s, Bytes) when is_binary(Bytes) -> "
	"Types = [~s]," 
	"Defaults = ~s,"
	"Decoded = decode(Bytes, Types, Defaults),"
	"to_record(~s, Decoded).",
    
    TemplateTypes = string_format(TypesFmt,[1,
					    "abc",
					    "int32",
					    ""]),
    TemplateFunction = string_format(DecodeFmt,["pikachu",
						TemplateTypes,
						"[{false, '$extensions', dict:new()}]",
						"pikachu"]),
    
    ExpectedTypes = string_format(TypesFmt,[1,"field1","int32",""]) ++ "," ++
	string_format(TypesFmt,[2,"field2","int32",""]) ++ "," ++ 
	string_format(TypesFmt,[3,"field3","int32","repeated"]) ++ "," ++
	string_format(TypesFmt,[4,"field4","name","is_record, repeated_packed"]),
    ExpectedFunction1 = string_format(DecodeFmt,[Name,
						 ExpectedTypes,
						"[{2, field2, 1},{false, '$extensions', dict:new()}]",
						Name]),
    ExpectedFunction2 = string_format(DecodeFmt,[Name,
						ExpectedTypes,
						"[{2, field2, 1}]",
						Name]),
    
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

test_function_to_record() ->
    Name = "name",
    Fields = [],
    Extends = ignored,
    Messages1 = [{Name,Fields,Extends}],
    Messages2 = [{Name,Fields,disallowed}],
    Basename = ignored,
    Acc = [],

    ToRecordFmt = "to_record(~s, DecodedTuples) -> "
	"Record1 = lists:foldr(fun({_FNum, Name, Val}, Record) -> "
	"set_record_field(record_info(fields, ~s), Record, Name, Val) end, "
	"#~s{}, DecodedTuples), ~s.",

    TemplateToRecord = string_format(ToRecordFmt,["pikachu","pikachu","pikachu","decode_extensions(Record1)"]),
    
    ExpectedToRecord1 = string_format(ToRecordFmt,[Name,Name,Name,"decode_extensions(Record1)"]),
    ExpectedToRecord2 = string_format(ToRecordFmt,[Name,Name,Name,"Record1"]),

    {ok,Function} = parse(TemplateToRecord),
    {ok,FilterdFunction1} = parse(ExpectedToRecord1),
    {ok,FilterdFunction2} = parse(ExpectedToRecord2),

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

test_function_decode_extentions() ->
    Name = "name",
    Fields = [],
    Extends = [{1, rule, "int32", "field1", []}],
    Messages1 = [{Name,Fields,[]}],
    Messages2 = [{Name,Fields,disallowed}],
    Messages3 = [{Name,Fields,Extends}],
    Basename = ignored,
    Acc = [],
    
    DecodeExtensionsFmt = "decode_extensions(#~s{'$extensions' = Extensions} = Record) ->"
	" Types = ~s,"
	" NewExtensions = decode_extensions(Types, dict:to_list(Extensions), []),"
	" Record#~s{'$extensions' = NewExtensions};",
    DecodeExtensionsDefaultFmt = " decode_extensions(Record) -> Record.",

    TemplateFunction = string_format(DecodeExtensionsFmt++DecodeExtensionsDefaultFmt,
				     ["pikachu","[]","pikachu"]),
    ExpectedFunction1 = string_format(DecodeExtensionsFmt++DecodeExtensionsDefaultFmt,
				      [Name,"[]",Name]),
    ExpectedFunction2 = DecodeExtensionsDefaultFmt,
    ExpectedFunction3 = string_format(DecodeExtensionsFmt++DecodeExtensionsDefaultFmt,
				      [Name,"[{1, field1, int32, []}]",Name]),
    
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

test_function_extension_size() ->
    Name = "name",
    Fields = [],
    Extends = [{1, rule, "int32", "field1", []}],
    Messages1 = [{Name,Fields,[]}],
    Messages2 = [{Name,Fields,disallowed}],
    Messages3 = [{Name,Fields,Extends}],
    Basename = ignored,
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
    {ok,FilterdFunction3} = parse(ExpectedFunction1),
    
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

test_function_has_extension() ->
    Name = "name",
    Fields = [],
    Extends = [{1, rule, "int32", "field1", []}],
    Messages1 = [{Name,Fields,[]}],
    Messages2 = [{Name,Fields,disallowed}],
    Messages3 = [{Name,Fields,Extends}],
    Basename = ignored,
    Acc = [],

    HasExtensionFmt = "has_extension(#~s{'$extensions' = Extensions}, ~s) -> "
	"dict:is_key(~s, Extensions); ",
    HasExtensionFmt1 = "has_extension(#~s{'$extensions' = Extensions}, ~w) ->"
	"dict:is_key(~w, Extensions); ",
    DefaultFmt = "has_extension(_Record, _FieldName) -> false.",
    
    TemplateFunction = string_format(HasExtensionFmt ++ DefaultFmt,
				     ["pikachu","FieldKey","FieldKey"]),
%    ExpectedFunction = string_format(HasExtensionFmt ++ DefaultFmt,
%				     [Name,"FieldKey","FieldKey"]),
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

test_function_collect_full_messages() ->
    [?_assertEqual({collected,[],[],[]},
		   protobuffs_compile_lib:collect_full_messages([])),
     ?_assertEqual({collected,[],[{["MsgName"],[],disallowed}],[{["MsgName"],[]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, "MsgName", []}])),
     ?_assertEqual({collected,[],[{["MsgName"],[],disallowed}],[{["MsgName"],[]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, ["MsgName"], []}])),
     ?_assertEqual({collected,[],[{["MsgName"],[{5,optional,"Location","location",none}],disallowed}],[{["MsgName"],[]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, 
								  ["MsgName"], 
								  [{5,optional,"Location","location",none}]}])),
     ?_assertEqual({collected,[],[{["MsgName"],[],disallowed}],[{["MsgName"],[]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, ["MsgName"], [{a,1}]}])),
     ?_assertEqual({collected,[{enum,"MsgName_EnumName",1,value}],[{["MsgName"],[],disallowed}],[{["MsgName"],[]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, 
								  ["MsgName"],
								  [{enum, "EnumName", [{value, 1}]}]}])),
     ?_assertEqual({collected,[],[{["MsgName"],[],[]}],[{["MsgName"],[{1,10}]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, ["MsgName"], [{extensions, 1, 10}]}])),
     ?_assertEqual({collected,[],[{["SubMsg","MsgName"],[],disallowed},
				  {["MsgName"],[],disallowed}],
		    [{["SubMsg","MsgName"],[]},
		     {["MsgName"],[]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, ["MsgName"], [{message, "SubMsg", []}]}])),
     ?_assertEqual({collected,[],[],[]},
		   protobuffs_compile_lib:collect_full_messages([{enum, "EnumName", []}])),
     ?_assertEqual({collected,[{enum,"EnumName",1,value}],[],[]},
		   protobuffs_compile_lib:collect_full_messages([{enum, "EnumName", [{value, 1}]}])),
     ?_assertEqual({collected,[{enum,"EnumName",1,value}],[],[]},
		   protobuffs_compile_lib:collect_full_messages([{enum, "EnumName", [{value, 1},dummy]}])),
     ?_assertEqual({collected,[],[],[]},
		   protobuffs_compile_lib:collect_full_messages([{package, "PackageName"}])),
     ?_assertEqual({collected,[],[],[]},
		   protobuffs_compile_lib:collect_full_messages([{option,a,b}])),
     ?_assertEqual({collected,[],[],[]},
		   protobuffs_compile_lib:collect_full_messages([{import, "Filename"}])),
     ?_assertEqual({collected,[],[{["ExtendedName"],[],[]}],[{["ExtendedName"],[{1,10}]}]},
	      protobuffs_compile_lib:collect_full_messages([{message, ["ExtendedName"],[{extensions, 1, 10}]},
							    {extend, ["ExtendedName"],[]}])),
     ?_assertEqual({collected,[],[{["ExtendedName"],[],[]}],[{["ExtendedName"],[{1,10}]}]},
		   protobuffs_compile_lib:collect_full_messages([{message, ["ExtendedName"],[{extensions, 1, 10}]},
							    {extend, "ExtendedName",[dummy]}])),
     ?_assertEqual({collected,[],[{["ExtendedName"],[],[{1,a,b,"FieldName",d}]}],[{["ExtendedName"],[{1,10}]}]},
	      protobuffs_compile_lib:collect_full_messages([{message, ["ExtendedName"],[{extensions, 1, 10}]},
							    {extend, ["ExtendedName"],[{1,a,b,"FieldName",d}]}])),
     ?_assertEqual({collected,[],[{["ExtendedName"],[],[{1,a,b,"FieldName",d}]}],[{["ExtendedName"],[{1,10}]}]},
	      protobuffs_compile_lib:collect_full_messages([{message, ["ExtendedName"],[{extensions, 1, 10}]},
							    {extend, ["ExtendedName"],[{1,a,b,"FieldName",d}]}])),
     ?_assertThrow(out_of_range, protobuffs_compile_lib:collect_full_messages(
				   [{message, ["ExtendedName"],[{extensions, 1, 10}]},
				    {extend, ["ExtendedName"],[{15,a,b,"FieldName",d}]}])),
     ?_assertThrow(out_of_range, protobuffs_compile_lib:collect_full_messages(
				   [{message, ["ExtendedName"],[{extensions, 19000, 19100}]},
				    {extend, ["ExtendedName"],[{19050,a,b,"FieldName",d}]}])),
     ?_assertEqual({collected,[],[],[]}, protobuffs_compile_lib:collect_full_messages(
				   [dummy]))].

test_function_is_enum_type() ->
    [?_assertEqual(false,protobuffs_compile_lib:is_enum_type(dummy,[],dummy)),
     ?_assertEqual({true,["Enum"]},protobuffs_compile_lib:is_enum_type("Enum",[["Enum"]],[{dummy,"Enum"}])),
     ?_assertEqual({true,["Enum"]},protobuffs_compile_lib:is_enum_type("Enum",[["Test","Message"],["Enum"]],[{dummy,"Enum"}])),
     ?_assertEqual({true,["Enum","Message"]},protobuffs_compile_lib:is_enum_type("Message_Enum",[["Enum","Message"],["Test"]],[{dummy,"Message_Enum"}]))].

test_function_set_line_number() ->
    [?_assertEqual({dummy,1}, protobuffs_compile_lib:set_line_number(1,{dummy,2})),
     ?_assertEqual({dummy,1,[list]}, protobuffs_compile_lib:set_line_number(1,{dummy,2,[list]})),
     ?_assertEqual({dummy,1,{tuple}}, protobuffs_compile_lib:set_line_number(1,{dummy,2,{tuple}})),
     ?_assertEqual({dummy,1,1}, protobuffs_compile_lib:set_line_number(1,{dummy,2,1})),
     ?_assertEqual({dummy,1,{dummy,1},{dummy,1}}, 
		   protobuffs_compile_lib:set_line_number(1,{dummy,2,{dummy,2},{dummy,2}})),
     ?_assertEqual({dummy,1,atom}, protobuffs_compile_lib:set_line_number(1,{dummy,2,atom}))].

test_function_resolve_types() ->
    [?_assertEqual([],protobuffs_compile_lib:resolve_types ([],[])),
     ?_assertEqual([{"Type", [{1, rules, "Type", "Identifier", "Other"}],[]}],
		   protobuffs_compile_lib:resolve_types ([{["Type"], [{1, rules, "Type", "Identifier", "Other"}],[]}],[])),
     ?_assertEqual([{"Message_Type", [{1, rules, "double", "Identifier", "Other"}],[]}],
		   protobuffs_compile_lib:resolve_types ([{["Type","Message"], [{1, rules, "double", "Identifier", "Other"}],[]}],[])),
     ?_assertEqual([{"Type", [],[]}],
		   protobuffs_compile_lib:resolve_types ([{["Type"], [dummy],[]}],[])),
     ?_assertEqual([{"Type", [{1, rules, "Type", "Identifier", "Other"}],disallowed}],
		   protobuffs_compile_lib:resolve_types ([{["Type"], [{1, rules, "Type", "Identifier", "Other"}],disallowed}],[])),
     ?_assertEqual([{"Message_Type", [{1, rules, "Enum", "Identifier", "Other"}],disallowed}],
		   protobuffs_compile_lib:resolve_types ([{["Message_Type"], 
							   [{1, rules, "Enum", "Identifier", "Other"}],disallowed}],
							 [{1,"Enum"}])),
     ?_assertEqual([{"Type", [{1, rules, "Message_Enum", "Identifier", "Other"}],[]}],
		   protobuffs_compile_lib:resolve_types ([{["Type"], 
							   [{1, rules, "Message.Enum", "Identifier", "Other"}],[]}],
							 [{1,"Message_Enum"}])),
     ?_assertThrow({unknown_type, "Enum"},
		   protobuffs_compile_lib:resolve_types ([{["Type","Message"], 
							   [{1, rules, "Enum", "Identifier", "Other"}],disallowed}],
							 [dummy]))].
    
test_function_is_scalar_type() ->
    [?_assertEqual(true,protobuffs_compile_lib:is_scalar_type("double")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("float")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("int32")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("int64")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("uint32")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("uint64")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("sint32")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("sint64")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("fixed32")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("fixed64")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("sfixed32")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("sfixed64")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("bool")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("string")),
     ?_assertEqual(true,protobuffs_compile_lib:is_scalar_type ("bytes")),
     ?_assertEqual(false,protobuffs_compile_lib:is_scalar_type ("dummy"))].

test_function_generate_field_definitions() ->
    [?_assertEqual([],protobuffs_compile_lib:generate_field_definitions([])),
     ?_assertEqual(["Name = erlang:error({required, Name})"],
		   protobuffs_compile_lib:generate_field_definitions([{"Name", required, dummy}])),
     ?_assertEqual(["Name"],protobuffs_compile_lib:generate_field_definitions([{"Name", dummy, none}])),
     ?_assertEqual(["Name = \"Default\""],protobuffs_compile_lib:generate_field_definitions([{"Name", dummy, "Default"}]))].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

parse(String) -> 
    {ok,Tokens,_} = erl_scan:string(String), 
    erl_parse:parse_form(Tokens).

string_format(FmtString,String) ->
    lists:flatten(io_lib:format(FmtString,String)).

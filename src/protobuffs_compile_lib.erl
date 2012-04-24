%%% @author David Åberg <>
%%% @copyright (C) 2012, David Åberg
%%% @doc
%%%
%%% @end
%%% Created : 24 Apr 2012 by David Åberg <>
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(protobuffs_compile_lib).



-module(protobuffs_compile_lib).

-ifdef(TEST).
-compile(export_all).
-else.
-export([filter_forms/5]).
-export([collect_full_messages/1]).
-export([resolve_types/2]).
-endif.


-record(collected,{enum=[], msg=[], extensions=[]}).

-type message() :: {string(),list(),atom()|list()}.
-type function_tuple() :: {'function',integer(),atom(),integer(),list()}.
-type enum_tuple() :: {'enum',string(),integer(),atom()}.
-type attribute_tuple() :: {'attribute',integer(),atom(),any()}.
-type clause_tuple() :: {'clause',integer(),list(),list(),list()}.

-spec filter_forms([message()],[enum_tuple()],list(),string(),list()) -> list().
filter_forms(Msgs, Enums, [{attribute,L,file,{_,_}}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,file,{"src/" ++ Basename ++ ".erl",L}}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,module,pokemon_pb}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,module,list_to_atom(Basename)}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,export,[{encode_pikachu,1},{decode_pikachu,1}]}|Tail], Basename, Acc) ->
    Exports = lists:foldl(fun filter_export_decode_encode/2, [], Msgs),
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,export,Exports}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,record,{pikachu,_}}=RecordTemplate|Tail], Basename, Acc) ->
    Records = [filter_record(Extends, Fields, L, Name, RecordTemplate) || {Name, Fields,Extends} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Records ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [filter_encode_function(Clause, L, Name) || {Name, _, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, 
		 Enums, 
		 Tail, 
		 Basename, 
		 [expand_encode_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, 
	     Enums, 
	     [{function,L,encode_extensions,1,[EncodeClause,Catchall]}|Tail], 
	     Basename, 
	     Acc) ->
    NewEncodeClauses = [replace_atom(EncodeClause, pikachu, atomize(Name)) ||
			   {Name, _Fields, Extens} <- Msgs, Extens =/= disallowed],
    NewClauses = NewEncodeClauses ++ [Catchall],
    NewFunction = {function,L,encode_extensions,1,NewClauses},
    filter_forms(Msgs, Enums, Tail, Basename, [NewFunction | Acc]);

filter_forms(Msgs, Enums, [{function,L,iolist,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, 
		 [expand_iolist_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, 
	     Enums, 
	     [{function,L,decode_pikachu,1,[Clause]}|Tail], 
	     Basename, 
	     Acc) ->
    Functions = [begin
		     {function,
		      L,
		      list_to_atom("decode_" ++ string:to_lower(Name)),
		      1,
		      [replace_atom(Clause, pikachu, atomize(Name))]} 
		 end || {Name, _, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,decode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, 
		 Enums, 
		 Tail, 
		 Basename, 
		 [{function,L,decode,2,expand_decode_function(Msgs, L, Clause)}|Acc]);

filter_forms(Msgs, Enums, [{function,L,to_record,2,Clause}|Tail], Basename, Acc) ->
    filter_forms(Msgs, 
		 Enums, 
		 Tail, 
		 Basename, 
		 [{function,L,to_record,2,expand_to_record_function(Msgs, L, Clause)}|Acc]);

filter_forms(Msgs, Enums, [{function,L,enum_to_int,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, 
		 Enums, 
		 Tail, 
		 Basename, 
		 [{function,L,enum_to_int,2,expand_enum_to_int_function(Enums, L, Clause)}|Acc]);

filter_forms(Msgs, Enums, [{function,L,int_to_enum,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, 
		 Enums, 
		 Tail, 
		 Basename, 
		 [{function,L,int_to_enum,2,expand_int_to_enum_function(Enums, L, Clause)}|Acc]);

filter_forms(Msgs, 
	     Enums, 
	     [{function,L,decode_extensions,1,[Clause,Catchall]}|Tail],
	     Basename, 
	     Acc) ->
    NewClauses = filter_decode_extensions_clause(Msgs, Msgs, Clause, []),
    NewHead = {function,L,decode_extensions,1,NewClauses ++ [Catchall]},
    filter_forms(Msgs, Enums, Tail,Basename,[NewHead|Acc]);

filter_forms(Msgs, 
	     Enums, 
	     [{function,L,extension_size,1,[RecClause,CatchAll]}|Tail],
	     Basename, 
	     Acc) ->
    NewRecClauses = filter_extension_size(Msgs, RecClause, []),
    NewClauses = lists:reverse([CatchAll | NewRecClauses]),
    NewHead = {function,L,extension_size,1,NewClauses},
    filter_forms(Msgs, Enums, Tail, Basename, [NewHead|Acc]);

filter_forms(Msgs, 
	     Enums, 
	     [{function,L,has_extension,2,[FilterClause,CatchallClause]}|Tail],
	     Basename,
	     Acc) ->
    NewRecClauses = filter_has_extension(Msgs, FilterClause, []),
    NewClauses = lists:reverse([CatchallClause | NewRecClauses]),
    NewHead = {function,L,has_extension,2,NewClauses},
    filter_forms(Msgs, Enums, Tail, Basename, [NewHead | Acc]);

filter_forms(Msgs, 
	     Enums, 
	     [{function,L,get_extension,2,[AtomClause,IntClause,Catchall]}|Tail],
	     Basename,
	     Acc) ->
    NewAtomClauses = filter_get_extension_atom(Msgs,AtomClause,[]),
    NewRecClauses = filter_get_extension_integer(Msgs, IntClause, NewAtomClauses),
    NewClauses = lists:reverse([Catchall | NewRecClauses]),
    NewHead = {function,L,get_extension,2,NewClauses},
    filter_forms(Msgs,Enums, Tail, Basename, [NewHead | Acc]);

filter_forms(Msgs, 
	     Enums, 
	     [{function,L,set_extension,3,[RecClause,Catchall]}|Tail],
	     Basename, 
	     Acc) ->
    NewRecClauses = filter_set_extension(Msgs, RecClause, []),
    NewClauses = lists:reverse([Catchall | NewRecClauses]),
    NewHead = {function,L,set_extension,3,NewClauses},
    filter_forms(Msgs,Enums,Tail,Basename,[NewHead|Acc]);

filter_forms(Msgs, Enums, [Form|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [Form|Acc]);

filter_forms(_, _, [], _, Acc) -> lists:reverse(Acc).

-spec filter_encode_function(clause_tuple(),integer(),string()) -> function_tuple().
filter_encode_function(Clause, L, Name) ->
    {function, L, list_to_atom("encode_" ++ string:to_lower(Name)), 1,
     [replace_atom(Clause, pikachu, atomize(Name))]}.

-spec filter_export_decode_encode({string(),_,_},_) -> [{atom(),integer()}].
filter_export_decode_encode({Name, _, _}, Acc) ->
    [{list_to_atom("encode_" ++ string:to_lower(Name)), 1},
     {list_to_atom("decode_" ++ string:to_lower(Name)), 1} | Acc].

-spec filter_record(any(),[tuple()],integer(),string(),attribute_tuple()) -> attribute_tuple().
filter_record(Extends,Fields,L,Name,{attribute, L, record, {pikachu, [FieldTemplate|Extend]}}) ->
    OutFields = [string:to_lower(A) || {_, _, _, A, _} <- lists:keysort(1, Fields)],		   
    ExtendField = case Extends of
		      disallowed -> [];
		      _ -> Extend
		  end,
    Frm_Fields = [replace_atom(FieldTemplate,abc,list_to_atom(OutField)) || OutField <- OutFields] ++ ExtendField, 
    {attribute, L, record, {atomize(Name), Frm_Fields}}.

-spec filter_set_extension(list(),_,list()) -> list().
filter_set_extension([],_,Acc) ->
    Acc;
filter_set_extension([{_,_,disallowed}|Tail],Clause,Acc) ->
    filter_set_extension(Tail,Clause,Acc);
filter_set_extension([{MsgName,_,Extends}|Tail],Clause,Acc) ->
    {clause,L,[OldRecArg,_OldAtomArg,ValueArg],Gs,[OldSet,OldReturn]} = Clause,
    {match,L,{record,L,_,RecArgFields},RecVar} = OldRecArg,
    {match,L2,NewReturn,OldDictStore} = OldSet,
    {call,L2,DictStore,[_StoreKey,_StoreVal,StoreVar]} = OldDictStore,
    {tuple,L3,[Ok, OldReturnRec]} = OldReturn,
    {record,L3,ReturnRecVar,_OldName,Fields} = OldReturnRec,
    Folder = fun({Id, Rule, StrType, Name, Opts}, Facc) ->
		     Type = atomize(StrType),
		     FClause = {clause,
				L,
				[{match,
				  L,
				  {record,L,atomize(MsgName),RecArgFields},
				  RecVar
				 },
				 {atom,L,atomize(Name)},
				 ValueArg
				],
				Gs,
				[{match,
				  L2,
				  NewReturn,
				  {call,
				   L2,
				   DictStore,
				   [{integer,L2,Id},
				    {tuple,L2,
				     [set_line_number(L2,erl_parse:abstract(Rule)),
				      ValueArg,
				      set_line_number(L2,erl_parse:abstract(Type)),
				      set_line_number(L2,erl_parse:abstract(Opts))]
				    },
				    StoreVar
				   ]
				  }
				 },
				 {tuple,L3,[Ok,{record,L3,ReturnRecVar,atomize(MsgName),Fields}]}
				]},
		     [FClause | Facc]
	     end,
    NewAcc = lists:foldl(Folder, Acc, Extends),
    filter_set_extension(Tail,Clause,NewAcc).

-spec filter_get_extension_atom(list(),clause_tuple(),maybe_improper_list()) -> maybe_improper_list().
filter_get_extension_atom([],_AtomClause,Acc) ->
    Acc;
filter_get_extension_atom([{_,_,disallowed}|Tail],Clause,Acc) ->
    filter_get_extension_atom(Tail,Clause,Acc);
filter_get_extension_atom([{Msg,_,Extends}|Tail],Clause,Acc) ->
    {clause,L,[RecArg,_OldAtom],[RecG],[OldSubcall]} = Clause,
    [{call,L,Guard,[Garg1,_RecName]}] = RecG,
    {call,L1,Subname,[RecVar,_OldInt]} = OldSubcall,
    NewG = [{call,L,Guard,[Garg1,{atom,L,atomize(Msg)}]}],
    NewClauses = [{clause,
		   L,
		   [RecArg,{atom,L,atomize(FName)}],
		   [NewG],
		   [{call,L1,Subname,[RecVar,{integer,L1,FId}]}]
		  } || {FId, _, _, FName, _} <- Extends],
    NewAcc = NewClauses ++ Acc,
    filter_get_extension_atom(Tail,Clause,NewAcc).

-spec filter_get_extension_integer(list(),clause_tuple(),[clause_tuple()]) -> [clause_tuple()].
filter_get_extension_integer([],_,Acc) ->
    Acc;
filter_get_extension_integer([{_,_,disallowed}|Tail],IntClause,Acc) ->
    filter_get_extension_integer(Tail,IntClause,Acc);
filter_get_extension_integer([{Msg,_,_Extends}|Tail],IntClause,Acc) ->
    {clause,L,[{record,L,Pikachu,Fields},IntArg],Gs,Body} = IntClause,
    NewRecName = replace_atom(Pikachu, pikachu, atomize(Msg)),
    NewRecArg = {record,L,NewRecName,Fields},
    NewClause = {clause,L,[NewRecArg,IntArg],Gs,Body},
    NewAcc = [NewClause|Acc],
    filter_get_extension_integer(Tail,IntClause,NewAcc).

-spec filter_has_extension(list(),clause_tuple(),maybe_improper_list()) -> maybe_improper_list().
filter_has_extension([], _, Acc) -> 
    Acc; % non-reverseal is intentional.
filter_has_extension([{_Msg,_,disallowed}|Tail], Clause, Acc) ->
    filter_has_extension(Tail, Clause, Acc);
filter_has_extension([{MsgName,_,Extends}|Tail], Clause, Acc) ->
    {clause,L,[OldRecArg,_],G,[Body]} = Clause,
    {call, L1, {remote,L1,Dict,IsKey},[_Key,DictArg]} = Body,
    RecArg = replace_atom(OldRecArg,pikachu,atomize(MsgName)),
    Folder = fun({ID, _Rules, _Type, Name, _Other}, FoldAcc) ->
    		     AtomClause = {clause,
    				   L,
    				   [RecArg,{atom,L,atomize(Name)}],
    				   G,
    				   [{call,
    				     L,
    				     {remote,L,Dict,IsKey},
    				     [{atom,L,atomize(Name)},DictArg]
    				    }
    				   ]},
    		     IntClause = {clause,
    				  L,
    				  [RecArg,{integer,L,ID}],
    				  G,[{call,
    				      L,
    				      {remote,L,Dict,IsKey},
    				      [{integer,L,ID},DictArg]
    				     }
    				    ]},
    		     [AtomClause,IntClause|FoldAcc]
    	     end,
    NewClauses = lists:foldl(Folder, [], Extends),
    NewAcc = Acc ++ NewClauses,
    filter_has_extension(Tail,Clause,NewAcc).

-spec filter_extension_size(list(),clause_tuple(),[clause_tuple()]) -> [clause_tuple()].
filter_extension_size([], _RecClause, Acc) -> % the non-reversal is intentional.
    Acc;
filter_extension_size([{_MsgName,_,disallowed}|Tail],Clause,Acc) ->
    filter_extension_size(Tail,Clause,Acc);
filter_extension_size([{MsgName,_,_}|Tail],Clause,Acc) ->
    {clause,L,[OldArg],G,Body} = Clause,
    NewClause = {clause,L,[replace_atom(OldArg,pikachu,atomize(MsgName))],G,Body},
    NewAcc = [NewClause | Acc],
    filter_extension_size(Tail,Clause,NewAcc).

-spec filter_encode_clause({string(),_,_},clause_tuple()) -> clause_tuple().
filter_encode_clause({MsgName, _Fields,_Extends}, Clause) ->
    replace_atom(Clause,pikachu,atomize(MsgName)).

-spec expand_iolist_function(list(), integer(), clause_tuple()) -> function_tuple().
expand_iolist_function(Msgs, Line, Clause) ->
    {function,Line,iolist,2,[filter_iolist_clause(Msg, Clause) || Msg <- Msgs]}.

-spec filter_iolist_clause({string(), list(), _}, clause_tuple()) -> clause_tuple().
filter_iolist_clause({MsgName, Fields, _Extends}, {clause,_,Args,Guards,[{cons,_,Call,{nil,L}}]}) ->
    F1 = lists:reverse([case Tag of
			    optional ->
				Field;
			    _ ->
				{FNum,Tag,SType,SName,none}
			end
			|| {FNum,Tag,SType,SName,_} = Field <- Fields ]),

    Cons = lists:foldl(fun({FNum,Tag,SType,SName,Default},Acc) ->
			       R1 = replace_atom(Call,pikachu,atomize(MsgName)),
			       R2 = replace_atom(R1,abc,atomize(SName)),
			       R3 = replace_atom(R2,{integer,L,1},{integer,L,FNum}),
			       R4 = if 
					is_atom(Default) -> replace_atom(R3,none,Default);
					is_list(Default) -> replace_atom(R3,{atom,L,none},{string,L,Default});
					is_float(Default) -> replace_atom(R3,{atom,L,none},{float,L,Default});
					is_integer(Default) -> replace_atom(R3,{atom,L,none},{integer,L,Default})
				    end,
			       R5 = replace_atom(R4,string,atomize(SType)),
			       R6 = replace_atom(R5,required,Tag),
			       {cons,L,R6,Acc}
		       end, {nil,L}, F1),
    {clause,L,replace_atom(Args,pikachu,atomize(MsgName)),Guards,[Cons]}.

-spec expand_decode_function(list(),_,_) -> [clause_tuple()].
expand_decode_function(Msgs, _Line, Clause) ->
    [filter_decode_clause(Msgs, Msg, Clause) || Msg <- Msgs].

-spec filter_decode_clause(_,{string() | char(),[any()],_},clause_tuple()) -> clause_tuple().
filter_decode_clause(Msgs, {MsgName, Fields, Extends}, 
		     {clause,L,Args,Guards,[{match,L1,{var,L1,'Types'},_A},
					    {match,L2,{var,L2,'Defaults'},B},
					    C,D]}) ->
    Types = lists:keysort(1, [{FNum, 
			       list_to_atom(SName), 
			       atomize(SType), 
			       decode_opts(Msgs, Tag, SType), Def} ||
				 {FNum,Tag,SType,SName,Def} <- Fields]),

    Cons = lists:foldl(
	     fun({FNum, FName, Type, Opts, _Def}, Acc) ->
		     {cons,L1,{tuple,L1,[{integer,L1,FNum},{atom,L1,FName},
					 {atom,L1,Type},
					 set_line_number(L1,erl_parse:abstract(Opts))]
			      },
		      Acc
		     }
	     end, {nil,L1}, lists:reverse(Types)),

    ExtendDefault = case Extends of
			disallowed -> {nil,L2};
			_ -> B
		    end,
    Defaults = lists:foldr(
		 fun
		     ({_FNum, _FName, _Type, _Opts, none}, Acc) ->
				  Acc;
		     ({FNum, FName, _Type, _Opts, Def}, Acc) ->
				  {cons,L2,{tuple,L2,[{integer,L2,FNum},
						      {atom,L2,FName},
						      set_line_number(L2,erl_parse:abstract(Def))
						     ]
					   },
				   Acc
				  }
			  end,
		 ExtendDefault,
		 Types),
    A1 = {match,L1,{var,L1,'Types'},Cons},
    B1 = {match,L2,{var,L2,'Defaults'},Defaults},
    D1 = replace_atom(D, pikachu, atomize(MsgName)),
    {clause,L,replace_atom(Args,pikachu,atomize(MsgName)),Guards,[A1,B1,C,D1]}.

-spec filter_decode_extensions_clause(_,list(),_,[clause_tuple()]) -> [clause_tuple()].
filter_decode_extensions_clause(_,[],_,Acc) ->
    lists:reverse(Acc);
filter_decode_extensions_clause(Msgs,[{_,_,disallowed}|Tail],Clause,Acc) ->
    filter_decode_extensions_clause(Msgs,Tail,Clause,Acc);
filter_decode_extensions_clause(Msgs,[{MsgName,_,Extends}|Tail],
				Clause = {clause,L,[Arg],Guards,[_,B,C]},Acc) ->
    Types = lists:keysort(1, [{FNum, list_to_atom(SName), 
			       atomize(SType), 
			       decode_opts(Msgs, Tag, SType), Def} ||
				 {FNum,Tag,SType,SName,Def} <- Extends]),
    Cons = lists:foldl(
	     fun({FNum, FName, Type, Opts, _Def}, Acc) ->
		     {cons,
		      L,
		      {tuple,
		       L,
		       [{integer,L,FNum},
			{atom,L,FName},
			{atom,L,Type},
			set_line_number(L,erl_parse:abstract(Opts))
		       ]
		      },
		      Acc}
	     end, {nil,L}, Types),
    A = {match,L,{var,L,'Types'},Cons},
    NewBody = [A,B,replace_atom(C,pikachu,atomize(MsgName))],
    NewClause = {clause,L,[replace_atom(Arg, pikachu, atomize(MsgName))],Guards,NewBody},
    filter_decode_extensions_clause(Msgs,Tail,Clause,[NewClause|Acc]).

-spec expand_encode_function(list(),integer(),_) -> function_tuple().
expand_encode_function(Msgs, Line, Clause) ->
    {function,Line,encode,2,[filter_encode_clause(Msg, Clause) || Msg <- Msgs]}.

-spec decode_opts(maybe_improper_list(),atom(),string()) -> ['is_record' | 'repeated' | 'repeated_packed'].
decode_opts(Msgs, Tag, Type) ->
    Opts0 = if 
		Tag == repeated -> [repeated]; 
		Tag == repeated_packed -> [repeated_packed]; 
		true -> [] 
	    end,
    case lists:keymember(Type, 1, Msgs) of
        true ->
            [is_record|Opts0];
        false ->
            Opts0
    end.

-spec expand_to_record_function(list(),integer(),list()) -> [clause_tuple()].
expand_to_record_function(Msgs, _Line, [Clause]) ->
    [filter_to_record_clause(Msg, Clause) || Msg <- Msgs].

-spec filter_to_record_clause({string(),_,_},clause_tuple()) -> clause_tuple().
filter_to_record_clause({MsgName, _, Extends}, 
			{clause,L,[Param1,Param2],Guards,[Fold,DecodeExtends]}) ->
    Fold1 = replace_atom(Fold, pikachu, atomize(MsgName)),
    ReturnLine = case Extends of
		     disallowed ->			 {var,L,'Record1'};
		     _ ->
			 DecodeExtends
		 end,
    {clause,L,[replace_atom(Param1,pikachu,atomize(MsgName)),Param2],Guards,[Fold1,ReturnLine]}.

-spec expand_enum_to_int_function([enum_tuple()],integer(),clause_tuple()) -> [clause_tuple()].
expand_enum_to_int_function([], _Line, Clause) ->
    [Clause];
expand_enum_to_int_function(Enums, _Line, Clause) ->
    [filter_enum_to_int_clause(Enum, Clause) || Enum <- Enums].

-spec filter_enum_to_int_clause(enum_tuple(),clause_tuple()) -> clause_tuple().
filter_enum_to_int_clause({enum,EnumTypeName,IntValue,EnumValue}, 
			  {clause,L1,Args,Guards,[{integer,L2,_}]}) ->
    A1 = replace_atom(Args, pikachu, atomize(EnumTypeName)),
    NewArgs = replace_atom(A1, value, EnumValue),
    {clause,L1,NewArgs,Guards,[{integer,L2,IntValue}]}.

-spec expand_int_to_enum_function([enum_tuple()],integer(),clause_tuple()) -> [clause_tuple()].
expand_int_to_enum_function([], _Line, Clause) ->
    [Clause];
expand_int_to_enum_function(Enums, _Line, Clause) ->
    [filter_int_to_enum_clause(Enum, Clause) || Enum <- Enums] ++ [Clause].

-spec filter_int_to_enum_clause(enum_tuple(),clause_tuple()) -> clause_tuple().
filter_int_to_enum_clause({enum,EnumTypeName,IntValue,EnumValue}, {clause,L,_Args,Guards,_}) ->
    {clause,L,
     [{atom,L,atomize(EnumTypeName)},{integer,L,IntValue}],
     Guards,[{atom,L,EnumValue}]
    }.

-spec atomize(string()) -> atom().
atomize(String) ->
    list_to_atom(string:to_lower(String)).

-spec replace_atom(any(),any(),any()) -> any().
replace_atom(Find, Find, Replace) -> 
    Replace;
replace_atom(Tuple, Find, Replace) when is_tuple(Tuple) ->
    list_to_tuple([replace_atom(Term, Find, Replace) || Term <- tuple_to_list(Tuple)]);
replace_atom(List, Find, Replace) when is_list(List) ->
    [replace_atom(Term, Find, Replace) || Term <- List];
replace_atom(Other, _Find, _Replace) ->
    Other.

-spec set_line_number(integer(),any()) -> any().
set_line_number(L,{N,_}) ->
    {N,L};
set_line_number(L,{N,_,R}) when is_list(R) ->
    List = lists:map(fun(Element) -> set_line_number(L,Element) end, R),
    {N,L,List};
set_line_number(L,{N,_,R}) when is_tuple(R) ->
    Tuple = set_line_number(L,R),
    {N,L,Tuple};
set_line_number(L,{N,_,R}) when is_integer(R) ->
    {N,L,R};
set_line_number(L,{N,_,R1,R2}) ->
    Tuple1 = set_line_number(L,R1),
    Tuple2 = set_line_number(L,R2),
    {N,L,Tuple1,Tuple2};
set_line_number(L,{N,_,A}) when is_atom(A) ->
    {N,L,A};
set_line_number(_,V) ->
    V.

-spec collect_full_messages(list()) -> #collected{}.
collect_full_messages(Data) -> collect_full_messages(Data, #collected{}).

-spec collect_full_messages(list(),#collected{}) -> #collected{}.
collect_full_messages([{message, Name, Fields} | Tail], Collected) ->
    ListName = case erlang:is_list (hd(Name)) of
		   true -> Name;
		   false -> [Name]
	       end,

    FieldsOut = lists:foldl(
		  fun ({_,_,_,_,_} = Input, TmpAcc) -> [Input | TmpAcc];
		      (_, TmpAcc) -> TmpAcc
		  end, [], Fields),

    Enums = lists:foldl(
	      fun ({enum,C,D}, TmpAcc) -> [{enum, [C | ListName], D} | TmpAcc];
		  (_, TmpAcc) -> TmpAcc
	      end, [], Fields),

    Extensions = lists:foldl(
		   fun ({extensions, From, To}, TmpAcc) -> [{From,To}|TmpAcc];
		       (_, TmpAcc) -> TmpAcc
		   end, [], Fields),

    SubMessages = lists:foldl(
		    fun ({message, C, D}, TmpAcc) -> [{message, [C | ListName], D} | TmpAcc];
			(_, TmpAcc) -> TmpAcc
		    end, [], Fields),

    ExtendedFields = case Extensions of
			 [] -> disallowed;
			 _ -> []
		     end,

    NewCollected = Collected#collected{
		     msg=[{ListName, FieldsOut, ExtendedFields} | Collected#collected.msg],
		     extensions=[{ListName,Extensions} | Collected#collected.extensions]
		    },
    collect_full_messages(Tail ++ SubMessages ++ Enums, NewCollected);
collect_full_messages([{enum, Name, Fields} | Tail], Collected) ->
    ListName = case erlang:is_list (hd(Name)) of
		   true -> Name;
		   false -> [Name]
	       end,

    FieldsOut = lists:foldl(
		  fun (Field, TmpAcc) ->
			  case Field of
			      {EnumAtom, IntValue} -> [{enum,
							type_path_to_type(ListName), 
							IntValue, 
							EnumAtom} | TmpAcc];
			      _ -> TmpAcc
			  end
		  end, [], Fields),

    NewCollected = Collected#collected{enum=FieldsOut++Collected#collected.enum},
    collect_full_messages(Tail, NewCollected);
collect_full_messages([{package, _PackageName} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{option,_,_} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{import, _Filename} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{extend, Name, ExtendedFields} | Tail], Collected) ->
    ListName = case erlang:is_list (hd(Name)) of
		   true -> Name;
		   false -> [Name]
	       end,

    CollectedMsg = Collected#collected.msg,
    {ListName,FieldsOut,ExtendFields} = lists:keyfind(ListName,1,CollectedMsg),
    {ListName,Extensions} = lists:keyfind(ListName,1,Collected#collected.extensions),

    FunNotInReservedRange = fun(Id) -> not(19000 =< Id andalso Id =< 19999) end,
    FunInRange = fun(Id,From,max) -> From =< Id andalso Id =< 16#1fffffff;
		    (Id,From,To) -> From =< Id andalso Id =< To
		 end,

    ExtendedFieldsOut = lists:append(FieldsOut,
				     lists:foldl(
				       fun ({Id, _, _, FieldName, _} = Input, 
					    TmpAcc) ->
					       case lists:any(
						      fun({From,To}) -> 
							      FunNotInReservedRange(Id) 
								  andalso 
								  FunInRange(Id,From,To)
						      end,
						      Extensions) of 
						   true ->
						       [Input | TmpAcc];
						   _ ->
						       error_logger:error_report(["Extended field not in valid range",
										  {message, Name},
										  {field_id,Id},
										  {field_name,FieldName},
										  {defined_ranges,Extensions},
										  {reserved_range,{19000,19999}},
										  {max,16#1fffffff}]),
						       throw(out_of_range)
					       end;
					   (_, TmpAcc) -> TmpAcc
				       end, [], ExtendedFields)
				    ),
    NewExtends = ExtendFields ++ ExtendedFieldsOut,
    NewCollected = Collected#collected{msg=lists:keyreplace(ListName,1,CollectedMsg,{ListName,FieldsOut,NewExtends})},
    collect_full_messages(Tail, NewCollected);
collect_full_messages([Skip|Tail], Acc) -> %% Skip anything we don't understand
    error_logger:warning_report(["Unkown, skipping",{skip,Skip}]), 
    collect_full_messages(Tail, Acc);
collect_full_messages([], Collected) ->
    Collected.

-spec type_path_to_type(string()) -> string().
type_path_to_type (TypePath) ->
    string:join (lists:reverse (TypePath), "_").

is_enum_type(_Type, [], _Enums) ->
    false;
is_enum_type(Type, [TypePath|Paths], Enums) ->
    case is_enum_type(type_path_to_type(TypePath),
		      Enums) of
	true ->
	    {true,TypePath};
	false ->
	    is_enum_type(Type, Paths, Enums)
    end.
is_enum_type(Type, Enums) ->
    case lists:keysearch(Type,2,Enums) of
	false ->
	    false;
	{value,_} ->
	    true
    end.

resolve_types (Data, Enums) -> 
    resolve_types (Data, Data, Enums, []).

resolve_types ([{TypePath, Fields,Extended} | Tail], AllPaths, Enums, Acc) ->
    ExpandTypes = fun (Input, TmpAcc) ->
			case Input of
			    {Index, Rules, Type, Identifier, Other} ->
				case is_scalar_type (Type) of
				    true -> [Input | TmpAcc];
				    false ->
					PossiblePaths=possible_paths(TypePath,Type),
					RealPath=real_type_path(AllPaths,Enums,Type,PossiblePaths),
					[{Index, Rules,
					  type_path_to_type(RealPath),
					  Identifier, Other} | TmpAcc]
				end;
			    _ -> TmpAcc
			end
		end,
    FieldsOut = lists:foldl(ExpandTypes, [], Fields),
    ExtendedOut = case Extended of
		      disallowed ->
			  disallowed;
		      _ ->
			  MidExtendOut = lists:foldl(ExpandTypes, [], Extended),
			  lists:reverse(MidExtendOut)
		  end,
    resolve_types (Tail, AllPaths, Enums,
        [{type_path_to_type(TypePath),
        lists:reverse (FieldsOut), ExtendedOut } | Acc]);
resolve_types ([], _, _, Acc) ->
    Acc.

real_type_path(AllPaths, Enums, Type, PossiblePaths) ->
    case find_type (PossiblePaths, AllPaths) of
	false ->
	    case is_enum_type(Type, PossiblePaths, Enums) of
		{true,EnumTypePath} ->
		    EnumTypePath;
		false ->
		    throw ({unknown_type, Type})
	    end;
	ResultTypePath ->
	    ResultTypePath
    end.

possible_paths(TypePath, Type) ->
    case string:tokens (Type,".") of
	[Type] ->
	    all_possible_type_paths (Type, TypePath);
	FullPath ->
	    % handle types of the form Foo.Bar which are absolute,
	    % so we just convert to a type path and check it.
	    [lists:reverse (FullPath)]
    end.

is_scalar_type ("double") -> true;
is_scalar_type ("float") -> true;
is_scalar_type ("int32") -> true;
is_scalar_type ("int64") -> true;
is_scalar_type ("uint32") -> true;
is_scalar_type ("uint64") -> true;
is_scalar_type ("sint32") -> true;
is_scalar_type ("sint64") -> true;
is_scalar_type ("fixed32") -> true;
is_scalar_type ("fixed64") -> true;
is_scalar_type ("sfixed32") -> true;
is_scalar_type ("sfixed64") -> true;
is_scalar_type ("bool") -> true;
is_scalar_type ("string") -> true;
is_scalar_type ("bytes") -> true;
is_scalar_type (_) -> false.

sublists(List) when is_list(List) ->
    sublists(List,[]).

sublists([],Acc) ->
    [ [] | Acc ];
sublists(List,Acc) ->
    sublists (tl (List), [ List | Acc ]).

all_possible_type_paths (Type, TypePath) ->
    lists:foldl (fun (TypeSuffix, AccIn) ->
			 [[Type | TypeSuffix] | AccIn]
		 end,
		 [],
		 sublists (TypePath)).

find_type ([], _KnownTypes) ->
    false;
find_type ([Type | TailTypes], KnownTypes) ->
    case lists:keysearch (Type, 1, KnownTypes) of
        false ->
            find_type (TailTypes, KnownTypes);
        {value, {RealType, _, _}} ->
            RealType
    end.

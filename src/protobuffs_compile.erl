%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
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
-module(protobuffs_compile).

-ifdef(TEST).
-compile(export_all).
-else.

-export([scan_file/1, scan_file/2, scan_string/2, scan_string/3, 
	 generate_source/1, generate_source/2]).

-export([parse_file/1]).

-export([parse_imports/2]).
-endif.

-record(collected,{enum=[], msg=[], extensions=[]}).

%%--------------------------------------------------------------------
%% @doc Generats a built .beam file and header file .hrl
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string()) ->
		       ok | {error, _}.
scan_file(ProtoFile) ->
    scan_file(ProtoFile,[]).

-spec scan_string(String :: string(), BaseName :: string()) ->
			 ok | {error, _}.
scan_string(String,BaseName) ->
    scan_string(String,BaseName,[]).

%%--------------------------------------------------------------------
%% @doc Generats a built .beam file and header file .hrl
%%      Considerd option properties: output_include_dir, 
%%                                   output_ebin_dir,
%%                                   imports_dir
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string() | atom(), Options :: list()) ->
		       ok | {error, _}.
scan_file(ProtoFile,Options) when is_list(ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto") ++ "_pb",
    {ok,String} = parse_file(ProtoFile),
    scan_string(String,Basename,Options);
scan_file(ProtoFile,Options) when is_atom(ProtoFile) ->
    Basename = atom_to_list(ProtoFile) ++ "_pb",
    {ok,String} = parse_file(atom_to_list(ProtoFile) ++ ".proto"),
    scan_string(String,Basename,Options).

-spec scan_string(String :: string(), Basename :: string(), Options :: list()) ->
			 ok | {error, _}. 
scan_string(String,Basename,Options) ->
    generate_output(Options, Basename, String, fun output/4).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%--------------------------------------------------------------------
-spec generate_source(ProtoFile :: string()) ->
			     ok | {error, _}.
generate_source(ProtoFile) when is_atom(ProtoFile) ->
    generate_source(atom_to_list(ProtoFile),[]);
generate_source(ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto"),
    generate_source(Basename,[]).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%      Consider option properties: output_include_dir, 
%%                                  output_src_dir,
%%                                  imports_dir
%%--------------------------------------------------------------------
-spec generate_source(ProtoFile :: string(), Options :: list()) ->
			     ok | {error, _}.
generate_source(ProtoFile,Options) when is_list (ProtoFile) ->
    Basename = ProtoFile ++ "_pb",
    {ok,String} = parse_file(ProtoFile),
    generate_output(Options, Basename, String,
                    fun output_source/4).

%% @hidden
output(Basename, Messages, Enums, Options) ->
    create_header_file(Basename, Messages, Options),
    Forms = create_forms(Basename, Messages, Enums, undefined),
    {ok, _, Bytes, _Warnings} = protobuffs_io:compile_forms(Forms, proplists:get_value(compile_flags,Options,[])),
    case proplists:get_value(output_ebin_dir,Options) of
	undefined ->
	    BeamFile = Basename ++ ".beam";
	BeamPath ->
	    BeamFile = filename:join(BeamPath,Basename) ++ ".beam"
    end,
    error_logger:info_msg("Writing beam file to ~p~n",[BeamFile]),
    protobuffs_io:write_file(BeamFile, Bytes).

%% @hidden
output_source (Basename, Messages, Enums, Options) ->
    create_header_file(Basename, Messages, Options),
    Forms1 = create_forms(Basename, Messages, Enums,
                          undefined),
    case proplists:get_value(output_src_dir,Options) of
	undefined ->
	    SrcFile = Basename ++ ".erl";
	SrcPath ->
	    SrcFile = filename:join(SrcPath,Basename) ++ ".erl"
    end,
    error_logger:info_msg("Writing src file to ~p~n",[SrcFile]),
    protobuffs_io:write_file(SrcFile, erl_prettypr:format(erl_syntax:form_list (Forms1))).

%% @hidden
parse_file(FileName) ->
    {ok, InFile} = protobuffs_io:open(FileName, [read]),
    String = parse_file(InFile,[]),
    file:close(InFile),
    {ok,String}.

%% @hidden
parse_file(InFile,Acc) ->
    case protobuffs_io:request(InFile) of
        {ok,Token,_EndLine} ->
            parse_file(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);    
        {eof,_} ->
            Acc
    end.

%% @hidden
resolve_types (Data, Enums) -> resolve_types (Data, Data, Enums, []).
resolve_types ([{TypePath, Fields,Extended} | Tail], AllPaths, Enums, Acc) ->
    FolderFun = fun (Input, TmpAcc) ->
			case Input of
			    {Index, Rules, Type, Identifier, Other} ->
				case is_scalar_type (Type) of
				    true -> [Input | TmpAcc];
				    false ->
					PossiblePaths =
					    case string:tokens (Type,".") of
						[Type] ->
						    all_possible_type_paths (Type, TypePath);
						FullPath ->
						% handle types of the form Foo.Bar which are absolute,
						% so we just convert to a type path and check it.
						    [lists:reverse (FullPath)]
					    end,
					RealPath =
					    case find_type (PossiblePaths, AllPaths) of
						false ->
						    case protobuffs_compile_lib:is_enum_type(Type, PossiblePaths, Enums) of
							{true,EnumType} ->
							    EnumType;
							false ->
							    throw (["Unknown Type ", Type])
						    end;
						ResultType ->
						    ResultType
					    end,
                            [{Index, Rules,
                            protobuffs_compile_lib:type_path_to_type(RealPath),
                            Identifier, Other} | TmpAcc]
				end;
			    _ -> TmpAcc
			end
		end,
    FieldsOut = lists:foldl(FolderFun, [], Fields),
    ExtendedOut = case Extended of
		      disallowed ->
			  disallowed;
		      _ ->
			  MidExtendOut = lists:foldl(FolderFun, [], Extended),
			  lists:reverse(MidExtendOut)
		  end,
    resolve_types (Tail, AllPaths, Enums,
        [{protobuffs_compile_lib:type_path_to_type(TypePath),
        lists:reverse (FieldsOut), ExtendedOut } | Acc]);
resolve_types ([], _, _, Acc) ->
    Acc.

%% @hidden
generate_field_definitions(Fields) ->
    generate_field_definitions(Fields, []).

%% @hidden
generate_field_definitions([], Acc) ->
    lists:reverse(Acc);
generate_field_definitions([{Name, required, _} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = erlang:error({required, ~s})", [Name, Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, _, none} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s", [Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, _, Default} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = ~p", [Name, Default])),
    generate_field_definitions(Tail, [Head | Acc]).

%% @hidden
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

%% @hidden
sublists(List) when is_list(List) ->
    sublists(List,[]).
sublists([],Acc) ->
    [ [] | Acc ];
sublists(List,Acc) ->
    sublists (tl (List), [ List | Acc ]).

%% @hidden
all_possible_type_paths (Type, TypePath) ->
    lists:foldl (fun (TypeSuffix, AccIn) ->
			 [[Type | TypeSuffix] | AccIn]
		 end,
		 [],
		 sublists (TypePath)).

%% @hidden
find_type ([], _KnownTypes) ->
    false;
find_type ([Type | TailTypes], KnownTypes) ->
    case lists:keysearch (Type, 1, KnownTypes) of
        false ->
            find_type (TailTypes, KnownTypes);
        {value, {RealType, _, _}} ->
            RealType
    end.

generate_output(Options, Basename, String, OutputFunction) ->
    {ok, FirstParsed} = protobuffs_compile_lib:parse_string(String),
    ImportPaths = ["./", "src/"
		   | proplists:get_value(imports_dir, Options, [])],
    Parsed = parse_imports(FirstParsed,
                           ImportPaths),
    Collected = protobuffs_compile_lib:collect_full_messages(Parsed),
    Messages = resolve_types(Collected#collected.msg,
                             Collected#collected.enum),
    OutputFunction(Basename, 
		   Messages, 
		   Collected#collected.enum,
                   Options).

create_forms(Basename, Messages, Enums, Options) ->
    PokemonBeamFile = code:where_is_file("pokemon_pb.beam"),
    {ok,
     {_,
      [{abstract_code,
        {_, Forms}}]}} = beam_lib:chunks(PokemonBeamFile,
                                         [abstract_code]),
    protobuffs_compile_lib:filter_forms(Messages, Enums, Forms, Basename, []).


create_header_file(Basename, Messages, Options) ->
    case proplists:get_value(output_include_dir, Options) of
	undefined -> HeaderFile = Basename ++ ".hrl";
	HeaderPath ->
	    HeaderFile = filename:join(HeaderPath, Basename) ++
		".hrl"
    end,
    error_logger:info_msg("Writing header file to ~p~n",
                          [HeaderFile]),
    {ok, FileRef} = protobuffs_io:open(HeaderFile, [write]),
    [begin
	 OutFields = [{string:to_lower(A), Optional, Default}
		      || {_, Optional, _, A, Default}
			     <- lists:keysort(1, Fields)],
	 protobuffs_io:format(FileRef, "-record(~s, {~n    ",
			      [string:to_lower(Name)]),
	 WriteFields0 = generate_field_definitions(OutFields),
	 WriteFields = case Extends of
			   disallowed -> WriteFields0;
			   _ ->
			       ExtenStr = case OutFields of
					      [] -> "'$extensions' = dict:new()";
					      _ -> "'$extensions' = dict:new()"
					  end,
			       WriteFields0 ++ [ExtenStr]
		       end,
	 FormatString = string:join(["~s"
				     || _ <- lists:seq(1, length(WriteFields))],
				    ",~n    "),
	 protobuffs_io:format(FileRef, FormatString,
			      WriteFields),
	 protobuffs_io:format(FileRef, "~n}).~n~n", [])
     end
     || {Name, Fields, Extends} <- Messages],
    ok = protobuffs_io:close(FileRef).


parse_imports(Parsed, Path) ->
    parse_imports(Parsed, Path, []).

parse_imports([], _Path, Acc) ->
    lists:reverse(Acc);
parse_imports([{import, File} = Head | Tail], Path, Acc) ->
    case protobuffs_io:path_open(Path, File, [read]) of
	{ok, F, Fullname} ->
	    file:close(F),
	    {ok,String} = parse_file(Fullname),
	    {ok,FirstParsed} = protobuffs_compile_lib:parse_string(String),
	    Parsed = lists:append(FirstParsed, Tail),
	    parse_imports(Parsed, Path, [Head | Acc]);
	{error, Error} ->
	    error_logger:error_report([
				       "Could not do import",
				       {import, File},
				       {error, Error},
				       {path, Path}
				      ]),
	    parse_imports(Tail, Path, [Head | Acc])
    end;
parse_imports([Head | Tail], Path, Acc) ->
    parse_imports(Tail, Path, [Head | Acc]).

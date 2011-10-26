-module(protobuffs_compile_lib).

-export([parse_imports/2]).

-export([parse_string/1]).

parse_imports(Parsed, Path) ->
    parse_imports(Parsed, Path, []).

parse_imports([], _Path, Acc) ->
    lists:reverse(Acc);
parse_imports([{import, File} = Head | Tail], Path, Acc) ->
    case protobuffs_io:path_open(Path, File, [read]) of
	{ok, F, Fullname} ->
	    file:close(F),
	    {ok,String} = protobuffs_compile:parse_file(Fullname),
	    {ok,FirstParsed} = parse_string(String),
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


parse_string(String) ->
    protobuffs_parser:parse(String).
%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by David AAberg <davabe@hotmail.com>
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

-module(protobuffs_io).

-export([open/2,path_open/3,close/1,format/3,request/1,compile_forms/2,write_file/2]).

open(File, Options) ->
    file:open(File,Options).

path_open(Path, File, Modes) ->
    file:path_open(Path, File, Modes).

close(FileRef) ->
    file:close(FileRef).

format(FileRef, FormatString, WriteFields) ->
    io:format(FileRef, FormatString, WriteFields).

request(InFile) ->
    io:request(InFile,{get_until,prompt,protobuffs_scanner,token,[1]}).

compile_forms(Forms, Options) ->
    compile:forms(Forms, [return] ++ Options).

write_file(File, Bytes) ->
    file:write_file(File,Bytes).

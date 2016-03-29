-module(id3).
-author("fstub42").
-export([read_file/1]).

-include("include/id3.hrl").


read_file(FilePath) when is_list(FilePath) ->
    {ok, FD} = file:open(FilePath, [binary]), %TODO test add modes
    ID3Meta = try id3v2:read_file(FD) of
		  V2Meta=#id3{} ->
		      V2Meta
	      catch
		  header_not_found ->
		      {ok, V1Meta=#id3{}} = id3v1:read_file(FD),
		      V1Meta
	      end,
    ok = file:close(FD),
    {ok, ID3Meta}.

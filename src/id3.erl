-module(id3).
-author("fstub42").
-export([read_file/1]).

-include("include/id3.hrl").


read_file(FilePath) when is_list(FilePath) ->
    {ok, FD} = file:open(FilePath, [binary]), %TODO test add modes
    Return = try id3v2:read_file(FD) of
		  V2Meta=#id3{} ->
		      {ok, V2Meta}
	      catch
		  header_not_found ->
		      try id3v1:read_file(FD) of
			  {ok, V1Meta=#id3{}} ->
			      {ok, V1Meta}
		      catch
			  header_not_found -> {error, invalid_file}
		      end
	      end,
    ok = file:close(FD),
    Return.

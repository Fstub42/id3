-module(id3v1).
-author("fstub42").
-export([read_file/1]).

-include("include/id3.hrl").

read_file(FD) when is_pid(FD) ->
    {ok, Header} = file:pread(FD, {eof, -128}, 128),
    parse(Header).

parse(<<"TAG",
	Title:30/binary,
	Artist:30/binary,
	Album:30/binary,
	Year:4/binary,
	CommentAndTrack:30/binary,
	GenreNum:8/integer>>) ->
    {Comment, Track} = case CommentAndTrack of
			   <<C:28/binary, 0:8, T:8/integer>> when T > 0 ->
			       {C, T};
			   _ ->
			       {CommentAndTrack, 0}
		       end,
    Frames = [{title, extract_string(Title)},
	      {artist, extract_string(Artist)},
	      {album, extract_string(Album)},
	      {year, extract_string(Year)},
	      {comment, extract_string(Comment)},
	      {track, Track}, %% sure?
	      {genre, id3_common:genre(GenreNum)}],
    {ok, #id3{frames=Frames}};
        
parse(_Other) ->
    throw(header_not_found). %% that is no ID3v1 tag

extract_string(Binary) ->
    Bin1 = strip_nulls(Binary),
    Str = unicode:characters_to_list(Bin1),
    string:strip(Str).

strip_nulls(Bin) when is_binary(Bin) ->
    List = erlang:binary_to_list(Bin),
    CleanList = strip_nulls(List),
    erlang:list_to_binary(CleanList);

strip_nulls(List) when is_list(List) ->
    L1 = lists:reverse(strip_nulls2(List)),
    lists:reverse(strip_nulls2(L1)).

strip_nulls2([0|Tail]) ->
    strip_nulls2(Tail);

strip_nulls2(List) when is_list(List) ->
    List.



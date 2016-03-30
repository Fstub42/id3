-module(id3v2).
-author("fstub42").
-export([read_file/1,
	 parse_header/1,
	 read_frames/2]).

-include("include/id3.hrl").


read_file(Path) when is_list(Path) ->
    {ok, FD} = file:open(Path, [binary]),
    read_file(FD);
read_file(FD) when is_pid(FD) ->
    {ok, RawHeader} = file:pread(FD, {bof, 0}, 10),
    Header = parse_header(RawHeader),
    Frames = read_frames(FD, Header),
    #id3{ header=Header, frames=Frames }.

parse_header(<<"ID3",
	       MajorV:8/integer,
	       MinorV:8/integer,
	       FlagA:1/integer,
	       FlagB:1/integer,
	       FlagC:1/integer,
	       _:5,
	       SizeBytes:4/binary>>) ->
    Size = id3_common:decode_size(SizeBytes),
    #id3_header{ version={2, MajorV, MinorV},
		 unsync=id3_common:bool(FlagA),
		 extended=id3_common:bool(FlagB),
		 experiment=id3_common:bool(FlagC),
		 size=Size };

parse_header(_) ->
    throw(header_not_found).


read_frames(FD, Header) ->
    {ok, BinaryFrames} = file:pread(FD, {bof, 10}, Header#id3_header.size),
    case Header#id3_header.version of
	{2, 2, _} -> parse_v2_frames(BinaryFrames, #{});
	{2, 3, _} -> parse_v3_frames(BinaryFrames, #{});
	{2, 4, _} -> parse_v3_frames(BinaryFrames, #{})
    end.

parse_v2_frames(<<FrameID:3/binary, Size:24/integer, Rest/binary>>,
		 Frames) ->
    {Content, Tail} = erlang:split_binary(Rest, Size),
    Frames1 = add_frame(Frames, FrameID, Content, fun parse_v2_frame/2),
    parse_v2_frames(Tail, Frames1);
parse_v2_frames(_, Frames) -> Frames.

parse_v3_frames(<<FrameID:4/binary, Size:32/integer,
		   _FlagBytes:2/binary, Rest/binary>>
		, Frames) when Size > 0 andalso Size =< byte_size(Rest) ->
    {Content, Tail} = erlang:split_binary(Rest, Size),
    Frames1 = add_frame(Frames, FrameID, Content, fun parse_v3_frame/2),
    parse_v3_frames(Tail, Frames1);
parse_v3_frames(_, Frames) -> Frames.


add_frame(Frames, FrameID, Content, ParseFun) ->
    try ParseFun(FrameID, Content) of
	ignore -> Frames;
	Product ->
	    case maps:find(FrameID, Frames) of
		error -> Frames#{ FrameID => [Product] };
		{ok, OtherVals} -> Frames#{ FrameID := [Product|OtherVals] }
	    end
    catch
	LogLvl:Msg ->
	    %% @todo use real logging used provided by the app using this lib
	    %% @todo line numbers would be really useful
	    io:format("~p in ~p:~p~n", [LogLvl, ?MODULE, Msg]),
	    Frames
    end.


parse_v2_frame(<<"TXX">>, RawText) ->
    RawText;
parse_v2_frame(<<"T",_/binary>>=Id, RawText) when size(Id) =:= 3 ->
    id3_common:extract_text(RawText);

parse_v2_frame(<<"UXX">>, RawUrl) ->
    RawUrl;
parse_v2_frame(<<"U",_/binary>>=Id, RawUrl) when size(Id) =:= 3 ->
    id3_common:extract_text(RawUrl);

parse_v2_frame(Id,_) when size(Id) =:= 3 ->
    ignore.


parse_v3_frame(<<"TXXX">>, RawText) ->
    RawText;
parse_v3_frame(<<"T",_/binary>>=Id, RawText) when size(Id) =:= 4 ->
    id3_common:extract_text(RawText);

parse_v3_frame(<<"WXXX">>, RawUrl) ->
    RawUrl;
parse_v3_frame(<<"W",_/binary>>=Id, RawUrl) when size(Id) =:= 4 ->
    id3_common:extract_text(RawUrl);

%% not implemnted frame will be ignored
parse_v3_frame(Id, _Content) when size(Id) =:= 4 -> ignore.


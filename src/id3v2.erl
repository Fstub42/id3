-module(id3v2).
-author("fstub42").
-export([read_file/1]).

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
	{2, 2, _} -> parse_v22_frames(BinaryFrames);
	{2, 3, _} -> parse_v23_frames(BinaryFrames);
	{2, 4, _} -> parse_v24_frames(BinaryFrames)
    end.


parse_v22_frames(Binary) -> parse_v22_frames(Binary, #{}).
parse_v22_frames(<<FrameID:3/binary, Size:24/integer, Rest/binary>>,
		 Frames) ->
    {Content, Tail} = split_binary(Rest, Size),
    Frames1 = add_frame(Frames, FrameID, Content),
    parse_v22_frames(Tail, Frames1);
parse_v22_frames(_, Frames) -> Frames.

parse_v23_frames(Binary) -> parse_v23_frames(Binary, #{}).
parse_v23_frames(<<FrameID:4/binary, Size:32/integer,
		   _FlagBytes:2/binary, Rest/binary>>
		, Frames) when Size > 0 andalso Size =< byte_size(Rest) ->
    {Content, Tail} = split_binary(Rest, Size),
    Frames1 = add_frame(Frames, FrameID, Content),
    parse_v23_frames(Tail, Frames1);
parse_v23_frames(_, Frames) -> Frames.


parse_v24_frames(Binary) -> parse_v24_frames(Binary, #{}). 
parse_v24_frames(<<FrameID:4/binary, Size:4/binary,
		   _FlagBytes:2/binary, Rest/binary>>,
		 Frames) when Size > 0 andalso Size =< byte_size(Rest) ->
    Size = id3_common:decode_size(Size),
    {Content, Tail} = split_binary(Rest, Size),
    Frames1 = add_frame(Frames, FrameID, Content),
    parse_v24_frames(Tail, Frames1);
parse_v24_frames(_, Frames) -> Frames.


add_frame(Frames, FrameId, Content) ->
    try parse_frame(FrameId, Content) of
	Product ->
	    case maps:find(FrameId, Frames) of
		error -> Frames#{ FrameId => [Product] };
		{ok, OtherVals} -> Frames#{ FrameId := [Product|OtherVals] }
	    end
    catch
	LogLvl:Msg ->
	    io:format("~p:parse_frame/2 ~p:~p",
		      [?MODULE, LogLvl, Msg]),
	    Frames
    end.

%%%%%%%%%%%
%%%%%%%%%%% FRAMES
%%%%%%%%%%%
-define(parse_text_frame(ID),
	parse_frame(ID=Id, RawText) ->
	       {Id, id3_common:extract_text(RawText)}).

%% generic text frames
?parse_text_frame(<<"TAL">>);
?parse_text_frame(<<"TALB">>);
?parse_text_frame(<<"TCMP">>);
?parse_text_frame(<<"TCO">>);
?parse_text_frame(<<"TCOM">>);
?parse_text_frame(<<"TCON">>);
?parse_text_frame(<<"TDAT">>);
?parse_text_frame(<<"TDRC">>);
?parse_text_frame(<<"TEN">>);
?parse_text_frame(<<"TENC">>);
?parse_text_frame(<<"TFLT">>);
?parse_text_frame(<<"TIT2">>);
?parse_text_frame(<<"TLAN">>);
?parse_text_frame(<<"TLEN">>);
?parse_text_frame(<<"TOPE">>);
?parse_text_frame(<<"TP1">>);
?parse_text_frame(<<"TPA">>);
?parse_text_frame(<<"TPE2">>);
?parse_text_frame(<<"TPUB">>);
?parse_text_frame(<<"TRCK">>);
?parse_text_frame(<<"TRK">>);
?parse_text_frame(<<"TSOC">>);
?parse_text_frame(<<"TSOT">>);
?parse_text_frame(<<"TSSE">>);
?parse_text_frame(<<"TT2">>);
?parse_text_frame(<<"TYE">>);
?parse_text_frame(<<"TYER">>);

%% not implemnted frame will be ignored
parse_frame(Id, _Content) -> {Id,ignore}.

-module(id3v2).
-author("fstub42").
-export([read_file/1]).

-include("include/id3.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.


read_file(Path) when is_list(Path) ->
    {ok, FD} = file:open(Path, [binary]),
    read_file(FD);
read_file(FD) when is_pid(FD) ->
    {ok, RawHeader} = file:pread(FD, {bof, 0}, 10),
    Header = parse_header(RawHeader),
    Frames = read_frames(FD, Header),
    #id3{header=Header, frames=Frames}.


parse_header(<<"ID3",
	       MajorV:8/integer,
	       MinorV:8/integer,
	       FlagA:1/integer,
	       FlagB:1/integer,
	       FlagC:1/integer,
	       _:5,
	       SizeBytes:4/binary>>) ->
    Size = parse_size(SizeBytes),
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


parse_size(<<_:1, S1:7/integer,
	     _:1, S2:7/integer,
	     _:1, S3:7/integer,
	     _:1, S4:7/integer>>) ->
    <<Size:32/integer>> = <<0:4,
			    S1:7/integer,
			    S2:7/integer,
			    S3:7/integer,
			    S4:7/integer>>,
    Size.


parse_v22_frames(Binary) -> parse_v22_frames(Binary, []).
parse_v22_frames(<<FrameID:3/binary, Size:24/integer, Rest/binary>>,
		 Frames) ->
    {Content, Tail} = split_binary(Rest, Size),
    Frame = try_parse_frame(FrameID, Content),
    parse_v22_frames(Tail, [Frame|Frames]);
parse_v22_frames(_, Frames) ->
    finalized_frames(Frames).


parse_v23_frames(Binary) -> parse_v23_frames(Binary, []).
parse_v23_frames(<<0, _Rest/binary>>, Frames) -> finalized_frames(Frames);
parse_v23_frames(<<FrameID:4/binary,
		   Size:32/integer,
		   _FlagBytes:2/binary,
		   Rest/binary>>
		, Frames) when Size > 0 andalso Size =< byte_size(Rest) ->
    {Content, Tail} = split_binary(Rest, Size),
    Frame = try_parse_frame(FrameID, Content),
    parse_v23_frames(Tail, [Frame|Frames]);
parse_v23_frames(_Rest, Frames) ->
    finalized_frames(Frames).

parse_v24_frames(Binary) -> parse_v24_frames(Binary, []). 
parse_v24_frames(<<0, _Rest/binary>>, Frames) -> finalized_frames(Frames);
 parse_v24_frames(<<FrameID:4/binary,
		   S1:8/integer, S2:8/integer, S3:8/integer, S4:8/integer,
		   _FlagBytes:2/binary, Rest/binary>>,
		 Frames)
  when S1 < 128 andalso S2 < 128 andalso S3 < 128 andalso S4 < 128 ->
    Size = parse_size(<<S1, S2, S3, S4>>),
    case Size of
	0 ->
	    finalized_frames(Frames);
	_ ->
	    {Content, Tail} = split_binary(Rest, Size),
	    Frame = try_parse_frame(FrameID, Content),
	    parse_v24_frames(Tail, [Frame|Frames])
    end;
parse_v24_frames(_, Frames) ->
    finalized_frames(Frames).

finalized_frames(Frames) ->
    [{Id, Data} || {Id, Data} <- Frames, Data =/= ignore].

try_parse_frame(FrameId, Content) ->
    try parse_frame(FrameId, Content) of
	Product -> Product
    catch
	Lvl:Msg ->
	    io:format("~p:~p called as parse_frame(~p)~n",
		      [Lvl, Msg, FrameId]),
	    ignore
    end.

%%%%%%%%%%%
%%%%%%%%%%% FRAMES
%%%%%%%%%%%
-define(parse_text_frame(ID), parse_frame(ID=Id, RawText) -> {Id, extract_text(RawText)}).

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

parse_frame(Id, _Content) -> {Id,ignore}. %% not implemnted frame will be ignored


%%%%%%%%%%%%
%%%%%%%%%%%% COMMON STUFF
%%%%%%%%%%%%

%% TODO Tests!
extract_text(BinString) when is_binary(BinString) ->
    {Result, _} = decode_string(BinString),
    Result.

%% Add tests (one for every encoding)
decode_string(Blop) ->
    {Encoding, Content}  = get_encoding(Blop),
    decode_string(Content, Encoding).

decode_string(Blop, Encoding) ->
    {RawString, Rest} = case case Encoding of
				 {utf16,_} -> binary:split(Blop, [<<0,0>>]);
				 _       -> binary:split(Blop, [<<0>>])
			     end of
			    [Head, Tail] -> {Head, Tail};
			    [Head]       -> {Head, <<>>}
			end,
    String = unicode:characters_to_binary(RawString, Encoding, latin1),
    {String, Rest}.

get_encoding(Blob) when is_binary(Blob) ->
    case Blob of
	<<0, Rest/binary>> ->
	    {latin1, Rest};
	<<1, Rest/binary>> ->
	    {Encoding, Length} = unicode:bom_to_encoding(Rest),
	    <<_Bom:Length/binary, Rest1/binary>> = Rest,
	    {Encoding, Rest1};
 	<<2, Rest/binary>> ->
	    {{utf16, big}, Rest};
	<<3, Rest/binary>> ->
	    {utf8, Rest};
	_ ->
	    {latin1, Blob}
    end.

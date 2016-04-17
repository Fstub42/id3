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
	{2, 2, _} -> parse_v2_frames(BinaryFrames, maps:new());
	{2, 3, _} -> parse_v3_frames(BinaryFrames, maps:new());
	{2, 4, _} -> parse_v3_frames(BinaryFrames, maps:new())
    end.

parse_v2_frames(<<FrameID:3/binary, Size:24/integer, Rest/binary>>,
		 Frames) ->
    {Content, Tail} = erlang:split_binary(Rest, Size),
    Frames1 = handle_frame(Frames, FrameID, Content, fun parse_v2_frame/2),
    parse_v2_frames(Tail, Frames1);
parse_v2_frames(_, Frames) -> Frames.

parse_v3_frames(<<FrameID:4/binary, Size:32/integer,
		  _FlagBytes:2/binary, Rest/binary>>
		, Frames) when Size > 0 andalso Size =< byte_size(Rest) ->
    {Content, Tail} = erlang:split_binary(Rest, Size),
    Frames1 = handle_frame(Frames, FrameID, Content, fun parse_v3_frame/2),
    parse_v3_frames(Tail, Frames1);
parse_v3_frames(_, Frames) -> Frames.


handle_frame(Frames, FrameID, Content, ParseFun) ->
    case parse_frame(FrameID, Content, ParseFun) of
	{_, ignore} -> Frames;
	{ParsedId, ParsedContent} ->
	    case maps:find(ParsedId, Frames) of
		error ->
		    Frames#{ ParsedId => [ParsedContent] };
		{ok, OtherVals} ->
		    Frames#{ ParsedId := [ParsedContent|OtherVals] }
	    end
    end.


parse_frame(RawId, Content, ParseFun) ->
    try {parse_frame_id(RawId), ParseFun(RawId, Content)} of
	{Id, Parsed} -> {Id, Parsed}
    catch
	throw:{unkown_frame_id, UnkownId} ->
	    error_logger:info_msg("Unkown frame id ~p occurred",[UnkownId]);
	LogLvl:Msg ->
	    error_logger:warning_msg(
	      "~p failed to parse a ~p frame with reason:~n~p~n~p",
	      [?MODULE, RawId, {LogLvl, Msg}, erlang:get_stacktrace()]),
	    ignore
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

parse_v3_frame(<<"APIC">>, Raw) ->
    <<RawEnc:1/binary, Raw0/binary>> = Raw,
    {Encoding, _} = id3_common:get_encoding(RawEnc),
    {MimeType0, Raw1} = id3_common:decode_string(Raw0, latin1),
    <<PicType:1/binary, Raw2/binary>> = Raw1,
    {Description, Picture} = id3_common:decode_string(Raw2, Encoding),
    MimeType1 = case binary:match(MimeType0, <<"/">>) of
		    nomatch -> <<"image/", MimeType0/binary>>;
		    _ -> MimeType0
		end,
    {MimeType1, apic_type(PicType), Description, Picture};

parse_v3_frame(<<"TXXX">>, RawText) ->
    RawText;
parse_v3_frame(<<"T",_/binary>>=Id, RawText) when size(Id) =:= 4 ->
    id3_common:extract_text(RawText);

parse_v3_frame(<<"WXXX">>, RawUrl) ->
    RawUrl;
parse_v3_frame(<<"W",_/binary>>=Id, RawUrl) when size(Id) =:= 4 ->
    id3_common:extract_text(RawUrl);

%% not implemented frames will be ignored
parse_v3_frame(Id, _Content) when size(Id) =:= 4 -> ignore.


apic_type(1) -> <<"32x32 pixels 'file icon' (PNG only)">>;
apic_type(2) -> <<"Other file icon">>;
apic_type(3) -> <<"Cover (front)">>;
apic_type(4) -> <<"Cover (back)">>;
apic_type(5) -> <<"Leaflet page">>;
apic_type(6) -> <<"Media (e.g. lable side of CD)">>;
apic_type(7) -> <<"Lead artist/lead performer/soloist">>;
apic_type(8) -> <<"Artist/performer">>;
apic_type(9) -> <<"Conductor">>;
apic_type(10) -> <<"Band/Orchestra">>;
apic_type(11) -> <<"Composer">>;
apic_type(12) -> <<"Lyricist/text writer">>;
apic_type(13) -> <<"Recording Location">>;
apic_type(14) -> <<"During recording">>;
apic_type(15) -> <<"During performance">>;
apic_type(16) -> <<"Movie/video screen capture">>;
apic_type(17) -> <<"A bright coloured fish">>;
apic_type(18) -> <<"Illustration">>;
apic_type(19) -> <<"Band/artist logotype">>;
apic_type(20) -> <<"Publisher/Studio logotype">>;
apic_type(_) -> <<"Other">>.



parse_frame_id(<<"AENC">>) -> aenc;
parse_frame_id(<<"APIC">>) -> apic;
parse_frame_id(<<"ASPI">>) -> aspi;
parse_frame_id(<<"BUF">>) -> buf;
parse_frame_id(<<"CNT">>) -> cnt;
parse_frame_id(<<"COM">>) -> com;
parse_frame_id(<<"COMM">>) -> comm;
parse_frame_id(<<"COMR">>) -> comr;
parse_frame_id(<<"CRA">>) -> cra;
parse_frame_id(<<"CRM">>) -> crm;
parse_frame_id(<<"ENCR">>) -> encr;
parse_frame_id(<<"EQU2">>) -> equ2;
parse_frame_id(<<"EQUA">>) -> equa;
parse_frame_id(<<"EQU">>) -> equ;
parse_frame_id(<<"ETC">>) -> etc;
parse_frame_id(<<"ETCO">>) -> etco;
parse_frame_id(<<"GEOB">>) -> geob;
parse_frame_id(<<"GEO">>) -> geo;
parse_frame_id(<<"GRID">>) -> grid;
parse_frame_id(<<"IPL">>) -> ipl;
parse_frame_id(<<"IPLS">>) -> ipls;
parse_frame_id(<<"LINK">>) -> link;
parse_frame_id(<<"LNK">>) -> lnk;
parse_frame_id(<<"MCDI">>) -> mcdi;
parse_frame_id(<<"MCI">>) -> mci;
parse_frame_id(<<"MLL">>) -> mll;
parse_frame_id(<<"MLLT">>) -> mllt;
parse_frame_id(<<"OWNE">>) -> owne;
parse_frame_id(<<"PCNT">>) -> pcnt;
parse_frame_id(<<"PIC">>) -> pic;
parse_frame_id(<<"POPM">>) -> popm;
parse_frame_id(<<"POP">>) -> pop;
parse_frame_id(<<"POSS">>) -> poss;
parse_frame_id(<<"PRIV">>) -> priv;
parse_frame_id(<<"RBUF">>) -> rbuf;
parse_frame_id(<<"REV">>) -> rev;
parse_frame_id(<<"RVA2">>) -> rva2;
parse_frame_id(<<"RVAD">>) -> rvad;
parse_frame_id(<<"RVA">>) -> rva;
parse_frame_id(<<"RVRB">>) -> rvrb;
parse_frame_id(<<"SEEK">>) -> seek;
parse_frame_id(<<"SIGN">>) -> sign;
parse_frame_id(<<"SLT">>) -> slt;
parse_frame_id(<<"STC">>) -> stc;
parse_frame_id(<<"SYLT">>) -> sylt;
parse_frame_id(<<"SYTC">>) -> sytc;
parse_frame_id(<<"TALB">>) -> talb;
parse_frame_id(<<"TAL">>) -> tal;
parse_frame_id(<<"TBPM">>) -> tbpm;
parse_frame_id(<<"TBP">>) -> tbp;
parse_frame_id(<<"TCM">>) -> tcm;
parse_frame_id(<<"TCOM">>) -> tcom;
parse_frame_id(<<"TCON">>) -> tcon;
parse_frame_id(<<"TCOP">>) -> tcop;
parse_frame_id(<<"TCO">>) -> tco;
parse_frame_id(<<"TCR">>) -> tcr;
parse_frame_id(<<"TDA">>) -> tda;
parse_frame_id(<<"TDAT">>) -> tdat;
parse_frame_id(<<"TDEN">>) -> tden;
parse_frame_id(<<"TDLY">>) -> tdly;
parse_frame_id(<<"TDOR">>) -> tdor;
parse_frame_id(<<"TDRC">>) -> tdrc;
parse_frame_id(<<"TDRL">>) -> tdrl;
parse_frame_id(<<"TDTG">>) -> tdtg;
parse_frame_id(<<"TDY">>) -> tdy;
parse_frame_id(<<"TENC">>) -> tenc;
parse_frame_id(<<"TEN">>) -> ten;
parse_frame_id(<<"TEXT">>) -> text;
parse_frame_id(<<"TFLT">>) -> tflt;
parse_frame_id(<<"TFT">>) -> tft;
parse_frame_id(<<"TIME">>) -> time;
parse_frame_id(<<"TIM">>) -> tim;
parse_frame_id(<<"TIPL">>) -> tipl;
parse_frame_id(<<"TIT1">>) -> tit1;
parse_frame_id(<<"TIT2">>) -> tit2;
parse_frame_id(<<"TIT3">>) -> tit3;
parse_frame_id(<<"TKE">>) -> tke;
parse_frame_id(<<"TKEY">>) -> tkey;
parse_frame_id(<<"TLAN">>) -> tlan;
parse_frame_id(<<"TLA">>) -> tla;
parse_frame_id(<<"TLEN">>) -> tlen;
parse_frame_id(<<"TLE">>) -> tle;
parse_frame_id(<<"TMCL">>) -> tmcl;
parse_frame_id(<<"TMED">>) -> tmed;
parse_frame_id(<<"TMOO">>) -> tmoo;
parse_frame_id(<<"TMT">>) -> tmt;
parse_frame_id(<<"TOAL">>) -> toal;
parse_frame_id(<<"TOA">>) -> toa;
parse_frame_id(<<"TOFN">>) -> tofn;
parse_frame_id(<<"TOF">>) -> tof;
parse_frame_id(<<"TOL">>) -> tol;
parse_frame_id(<<"TOLY">>) -> toly;
parse_frame_id(<<"TOPE">>) -> tope;
parse_frame_id(<<"TOR">>) -> tor;
parse_frame_id(<<"TORY">>) -> tory;
parse_frame_id(<<"TOT">>) -> tot;
parse_frame_id(<<"TOWN">>) -> town;
parse_frame_id(<<"TP1">>) -> tp1;
parse_frame_id(<<"TP2">>) -> tp2;
parse_frame_id(<<"TP3">>) -> tp3;
parse_frame_id(<<"TP4">>) -> tp4;
parse_frame_id(<<"TPA">>) -> tpa;
parse_frame_id(<<"TPB">>) -> tpb;
parse_frame_id(<<"TPE1">>) -> tpe1;
parse_frame_id(<<"TPE2">>) -> tpe2;
parse_frame_id(<<"TPE3">>) -> tpe3;
parse_frame_id(<<"TPE4">>) -> tpe4;
parse_frame_id(<<"TPOS">>) -> tpos;
parse_frame_id(<<"TPRO">>) -> tpro;
parse_frame_id(<<"TPUB">>) -> tpub;
parse_frame_id(<<"TRCK">>) -> trck;
parse_frame_id(<<"TRC">>) -> trc;
parse_frame_id(<<"TRDA">>) -> trda;
parse_frame_id(<<"TRD">>) -> trd;
parse_frame_id(<<"TRK">>) -> trk;
parse_frame_id(<<"TRSN">>) -> trsn;
parse_frame_id(<<"TRSO">>) -> trso;
parse_frame_id(<<"TSI">>) -> tsi;
parse_frame_id(<<"TSIZ">>) -> tsiz;
parse_frame_id(<<"TSOA">>) -> tsoa;
parse_frame_id(<<"TSOP">>) -> tsop;
parse_frame_id(<<"TSOT">>) -> tsot;
parse_frame_id(<<"TSRC">>) -> tsrc;
parse_frame_id(<<"TSSE">>) -> tsse;
parse_frame_id(<<"TSS">>) -> tss;
parse_frame_id(<<"TSST">>) -> tsst;
parse_frame_id(<<"TT1">>) -> tt1;
parse_frame_id(<<"TT2">>) -> tt2;
parse_frame_id(<<"TT3">>) -> tt3;
parse_frame_id(<<"TXT">>) -> txt;
parse_frame_id(<<"TXX">>) -> txx;
parse_frame_id(<<"TXXX">>) -> txxx;
parse_frame_id(<<"TYER">>) -> tyer;
parse_frame_id(<<"TYE">>) -> tye;
parse_frame_id(<<"UFID">>) -> ufid;
parse_frame_id(<<"UFI">>) -> ufi;
parse_frame_id(<<"ULT">>) -> ult;
parse_frame_id(<<"USER">>) -> user;
parse_frame_id(<<"USLT">>) -> uslt;
parse_frame_id(<<"WAF">>) -> waf;
parse_frame_id(<<"WAR">>) -> war;
parse_frame_id(<<"WAS">>) -> was;
parse_frame_id(<<"WCM">>) -> wcm;
parse_frame_id(<<"WCOM">>) -> wcom;
parse_frame_id(<<"WCOP">>) -> wcop;
parse_frame_id(<<"WCP">>) -> wcp;
parse_frame_id(<<"WOAF">>) -> woaf;
parse_frame_id(<<"WOAR">>) -> woar;
parse_frame_id(<<"WOAS">>) -> woas;
parse_frame_id(<<"WORS">>) -> wors;
parse_frame_id(<<"WPAY">>) -> wpay;
parse_frame_id(<<"WPB">>) -> wpb;
parse_frame_id(<<"WPUB">>) -> wpub;
parse_frame_id(<<"WXX">>) -> wxx;
parse_frame_id(<<"WXXX">>) -> wxxx;
parse_frame_id(NotFound) -> throw({unkown_frame_id, NotFound}).

-module(id3_common).
-export([bool/1,
	 genre/1,
	 extract_text/1,
	 decode_size/1,
	 decode_string/1]).

bool(1) -> true;
bool(_) -> false.


decode_size(<<_:1, S1:7/integer,
	     _:1, S2:7/integer,
	     _:1, S3:7/integer,
	     _:1, S4:7/integer>>) ->
    <<Size:32/integer>> = <<0:4,
			    S1:7/integer,
			    S2:7/integer,
			    S3:7/integer,
			    S4:7/integer>>,
    Size.


%% TODO Tests!
extract_text(BinString) when is_binary(BinString) ->
    {Result, _} = decode_string(BinString),
    Result.

%% Add tests (one for every encoding)
decode_string(Blop) ->
    {Encoding, Content}  = get_encoding(Blop),
    decode_string(Content, Encoding).

decode_string(Blop, Encoding) ->
    {RawString, Rest} = case Encoding of
			    {utf16,_} -> binary_split(Blop, <<0,0>>);
			    _         -> binary_split(Blop, <<0>>)
			end,
    String = unicode:characters_to_binary(RawString, Encoding, latin1),
    true = is_binary(String), %%if shit is wrong it should fail #erlang
    {String, Rest}.

binary_split(Blop, Sep) ->
    case binary:split(Blop, [Sep]) of
	[Head, Tail] -> {Head, Tail};
	[Head]       -> {Head, <<>>}
    end.


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
        Rest ->
	    {latin1, Rest}
    end.



genre(1) -> <<"Blues">>;
genre(2) -> <<"Classic Rock">>;
genre(3) -> <<"Country">>;
genre(4) -> <<"Dance">>;
genre(5) -> <<"Disco">>;
genre(6) -> <<"Funk">>;
genre(7) -> <<"Grunge">>;
genre(8) -> <<"Hip-Hop">>;
genre(9) -> <<"Jazz">>;
genre(10) -> <<"Metal">>;
genre(11) -> <<"New Age">>;
genre(12) -> <<"Oldies">>;
genre(13) -> <<"Other">>;
genre(14) -> <<"Pop">>;
genre(15) -> <<"R&B">>;
genre(16) -> <<"Rap">>;
genre(17) -> <<"Reggae">>;
genre(18) -> <<"Rock">>;
genre(19) -> <<"Techno">>;
genre(20) -> <<"Industrial">>;
genre(21) -> <<"Alternative">>;
genre(22) -> <<"Ska">>;
genre(23) -> <<"Death Metal">>;
genre(24) -> <<"Pranks">>;
genre(25) -> <<"Soundtrack">>;
genre(26) -> <<"Euro-Techno">>;
genre(27) -> <<"Ambient">>;
genre(28) -> <<"Trip-Hop">>;
genre(29) -> <<"Vocal">>;
genre(30) -> <<"Jazz+Funk">>;
genre(31) -> <<"Fusion">>;
genre(32) -> <<"Trance">>;
genre(33) -> <<"Classical">>;
genre(34) -> <<"Instrumental">>;
genre(35) -> <<"Acid">>;
genre(36) -> <<"House">>;
genre(37) -> <<"Game">>;
genre(38) -> <<"Sound Clip">>;
genre(39) -> <<"Gospel">>;
genre(40) -> <<"Noise">>;
genre(41) -> <<"AlternRock">>;
genre(42) -> <<"Bass">>;
genre(43) -> <<"Soul">>;
genre(44) -> <<"Punk">>;
genre(45) -> <<"Space">>;
genre(46) -> <<"Meditative">>;
genre(47) -> <<"Instrumental Pop">>;
genre(48) -> <<"Instrumental Rock">>;
genre(49) -> <<"Ethnic">>;
genre(50) -> <<"Gothic">>;
genre(51) -> <<"Darkwave">>;
genre(52) -> <<"Techno-Industrial">>;
genre(53) -> <<"Electronic">>;
genre(54) -> <<"Pop-Folk">>;
genre(55) -> <<"Eurodance">>;
genre(56) -> <<"Dream">>;
genre(57) -> <<"Southern Rock">>;
genre(58) -> <<"Comedy">>;
genre(59) -> <<"Cult">>;
genre(60) -> <<"Gangsta">>;
genre(61) -> <<"Top 40">>;
genre(62) -> <<"Christian Rap">>;
genre(63) -> <<"Pop/Funk">>;
genre(64) -> <<"Jungle">>;
genre(65) -> <<"Native American">>;
genre(66) -> <<"Cabaret">>;
genre(67) -> <<"New Wave">>;
genre(68) -> <<"Psychadelic">>;
genre(69) -> <<"Rave">>;
genre(70) -> <<"Showtunes">>;
genre(71) -> <<"Trailer">>;
genre(72) -> <<"Lo-Fi">>;
genre(73) -> <<"Tribal">>;
genre(74) -> <<"Acid Punk">>;
genre(75) -> <<"Acid Jazz">>;
genre(76) -> <<"Polka">>;
genre(77) -> <<"Retro">>;
genre(78) -> <<"Musical">>;
genre(79) -> <<"Rock & Roll">>;
genre(80) -> <<"Hard Rock">>;
genre(81) -> <<"Folk">>;
genre(82) -> <<"Folk-Rock">>;
genre(83) -> <<"National Folk">>;
genre(84) -> <<"Swing">>;
genre(85) -> <<"Fast Fusion">>;
genre(86) -> <<"Bebob">>;
genre(87) -> <<"Latin">>;
genre(88) -> <<"Revival">>;
genre(89) -> <<"Celtic">>;
genre(90) -> <<"Bluegrass">>;
genre(91) -> <<"Avantgarde">>;
genre(92) -> <<"Gothic Rock">>;
genre(93) -> <<"Progressive Rock">>;
genre(94) -> <<"Psychedelic Rock">>;
genre(95) -> <<"Symphonic Rock">>;
genre(96) -> <<"Slow Rock">>;
genre(97) -> <<"Big Band">>;
genre(98) -> <<"Chorus">>;
genre(99) -> <<"Easy Listening">>;
genre(100) -> <<"Acoustic">>;
genre(101) -> <<"Humour">>;
genre(102) -> <<"Speech">>;
genre(103) -> <<"Chanson">>;
genre(104) -> <<"Opera">>;
genre(105) -> <<"Chamber Music">>;
genre(106) -> <<"Sonata">>;
genre(107) -> <<"Symphony">>;
genre(108) -> <<"Booty Bass">>;
genre(109) -> <<"Primus">>;
genre(110) -> <<"Porn Groove">>;
genre(111) -> <<"Satire">>;
genre(112) -> <<"Slow Jam">>;
genre(113) -> <<"Club">>;
genre(114) -> <<"Tango">>;
genre(115) -> <<"Samba">>;
genre(116) -> <<"Folklore">>;
genre(117) -> <<"Ballad">>;
genre(118) -> <<"Power Ballad">>;
genre(119) -> <<"Rhythmic Soul">>;
genre(120) -> <<"Freestyle">>;
genre(121) -> <<"Duet">>;
genre(122) -> <<"Punk Rock">>;
genre(123) -> <<"Drum Solo">>;
genre(124) -> <<"A capella">>;
genre(125) -> <<"Euro-House">>;
genre(126) -> <<"Dance Hall">>;
genre(_)   -> <<"Unkown">>.

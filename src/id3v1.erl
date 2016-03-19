-module(id3v1).
-author("fstub42").

-export([read_file/1, parse/1]).

-include("include/id3.hrl").


read_file(FD) when is_pid(FD) ->
    {ok, Header} = file:pread(FD, {eof, -128}, 128),
    {ok, parse(Header)}.

%% id3v1.1
parse(<<"TAG",
	Title:30/binary,
	Artist:30/binary,
	Album:30/binary,
	Year:4/binary,
	Comment:28/binary,
	0:8/integer,
	Track:8/integer,
	GenreNum:8/integer>>) ->
    #id3{
       frames=#{
	 title => id3_common:extract_text(Title),
	 artist => id3_common:extract_text(Artist),
	 album => id3_common:extract_text(Album),
	 year => id3_common:extract_text(Year),
	 comment => id3_common:extract_text(Comment),
	 track => Track,
	 genre => id3_common:genre(GenreNum)
	},
       header=#id3_header{ version={1,1,0} } %% ... header should be a map 2
      };

%% id3v1.0
parse(<<"TAG",
	Title:30/binary,
	Artist:30/binary,
	Album:30/binary,
	Year:4/binary,
	Comment:30/binary,
	GenreNum:8/integer>>) ->
    #id3{
       frames=#{
	 title => id3_common:extract_text(Title),
	 artist => id3_common:extract_text(Artist),
	 album => id3_common:extract_text(Album),
	 year => id3_common:extract_text(Year),
	 comment => id3_common:extract_text(Comment),
	 track => 0, %% sure?
	 genre => id3_common:genre(GenreNum)
	},
       header=#id3_header{ version={1,0,0} }
      };

parse(_Other) ->
    throw(header_not_found). %% that is no ID3v1 tag


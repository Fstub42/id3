-module(id3v1_test).
-author("fstub42").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("include/id3.hrl").

parse_1_test() ->
    Input = <<"TAG",
	      "Benzin", 0:192,
	      "Rammstein", 0:168,
	      "Rosenrot", 0:176,
	      "1994",
	      "Penis", 0:192, 5,
	      22>>,
    Result = id3v1:parse(Input),
    io:format("Output: ~p~n",[Result]),
    ?assert(
       {id3, undefined,
	#{album => <<"Rosenrot">>,
	  artist => <<"Rammstein">>,
	  title => <<"Benzin">>,
	  year => <<"1994">>,
	  comment => <<"Penis">>,
	  track => 5,
	  genre => <<"Ska">> }
       } =:= Result
      ).

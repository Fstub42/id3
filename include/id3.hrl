%%TODO: documenation

%% @type id3() = #id3{ header = id3_header(),
%%                     frames = [property()] }
-record(id3, {header, frames=[]}).

%% @type id3_header() = #id3_header{version    = {integer(), integer(), integer()},
%%                                  size       = integer(),
%%                                  unsync     = boolean(),
%%                                  extended   = boolean(),
%%                                  experiment = boolean()}
-record(id3_header, {version :: {integer(), integer(), integer()},
		     size :: integer(),
		     unsync :: integer(),
		     extended :: integer(),
		     experiment :: integer()}).

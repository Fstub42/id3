
%%TODO: docs

-record(id3, {header,
	      frames=#{} :: map()}).


-record(id3_header, {version :: {non_neg_integer(),
				 non_neg_integer(),
				 non_neg_integer()} | undefined,
		     size :: non_neg_integer() | undefined,
		     unsync=false :: boolean(),
		     extended=false :: boolean(),
		     experiment=false :: boolean() }).



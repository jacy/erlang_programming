-module(erl_httpclient).
-compile(export_all).

demo() ->
	inets:start(),
	httpc:request(get,
				  {	"http://www.google.com", 
				   	[
					 {"connection", "close"}
					]
				  },
				  [],
				  []).
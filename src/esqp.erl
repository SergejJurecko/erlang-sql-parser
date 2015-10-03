-module(esqp).
-export([parse/1,loop/2]).
-include("esqp.hrl").

parse(L) when is_list(L) ->
	parse(iolist_to_binary(L));
parse(Bin) ->
	type(esqp_tokenizer:tokenize(Bin)).


type([?TK_SELECT|T]) ->
	esqp_select:parse(T).



loop(N,Sql) ->
	S = os:timestamp(),
	loop1(N,Sql),
	timer:now_diff(os:timestamp(),S).

loop1(0,_) ->
	ok;
loop1(N,Sql) ->
	esqp_tokenize:tokenize(Sql),
	loop1(N-1,Sql).


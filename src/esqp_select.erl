-module(esqp_select).
-export([parse/1]).
-include("esqp.hrl").

parse(L) ->
	mod(#select{},L).


mod(P,[?TK_ALL|L]) ->
	mod(P#select{modifier = all},L);
mod(P,[?TK_DISTINCT|L]) ->
	mod(P#select{modifier = distinct},L);
mod(P,L) ->
	cols(P#select{modifier = all},L).

cols(P,[?TK_STAR|L]) ->
	cols(P#select{cols = ['*'|P#select.cols]},L);
cols(P,[{?TK_ID,Col},Next|L]) when Next /= ?TK_LP ->
	cols(P#select{cols = [Col|P#select.cols]},L);
cols(P,[{?TK_ID,Col},?TK_LP|L]) ->
	colfun(P,L,Col,[]);
cols(P,[?TK_FROM|T]) ->
	from(P,T).

% TODO: subquery...
from(P,[{?TK_ID,Id},?TK_DOT|T]) ->
	from_table(P,T,Id,undefined,undefined);
from(P,[{?TK_ID,Id},?TK_AS,{?TK_ID,Id1}|T]) ->
	from(P#select{from = [#from{table = Id, as = Id1}|P#select.from]},T);
from(P,[{?TK_ID,Id}|T]) ->
	from(P#select{from = [Id|P#select.from]},T);
from(P,[?TK_COMMA|T]) ->
	from(P,T);
from(P,[?TK_WHERE|T]) ->
	where(P#select{from = lists:reverse(P#select.from)},T);
from(P,[?TK_GROUP|T]) ->
	group(P#select{from = lists:reverse(P#select.from)},T).

from_table(P,[{?TK_ID,Table}|T],Db,undefined,undefined) ->
	from_table(P,T,Db,Table,undefined);
from_table(P,[?TK_AS|T],Db,Table,undefined) ->
	{?TK_ID,As} = hd(T),
	from(P#select{from = [#from{db = Db, table = Table, as = As}|P#select.from]},tl(T));
from_table(P,T,Db,Table,As) when Db /= undefined, Table /= undefineds ->
	from(P#select{from = [#from{db = Db, table = Table, as = As}|P#select.from]},T).

colfun(P,[{?TK_ID,Param}|T],Func,FuncParam) ->
	colfun(P, T, Func, [Param|FuncParam]);
colfun(P,[?TK_RP|T],Func,FuncParam) ->
	cols(P#select{cols = [{Func,lists:reverse(FuncParam)}|P#select.cols]}, T).

where(P,T) ->
	{ok,Expr,T1} = expr([],[],T),
	select_fwd(P#select{where = Expr},T1).

group(P,T) ->
	P.

select_fwd(P,[?TK_GROUP|T]) ->
	group(P,T).

% Literals
expr(Lit, Op, [{?TK_INTEGER,Int}|T]) ->
	expr([{?TK_INTEGER,Int}|Lit], Op, T);
expr(Lit, Op, [{?TK_STRING,Str}|T]) ->
	expr([{?TK_STRING,Str}|Lit],Op, T);
expr(Lit, Op, [{?TK_FLOAT,Str}|T]) ->
	expr([{?TK_FLOAT,Str}|Lit],Op, T);
expr(Lit, Op, [?TK_NULL|T]) ->
	expr([?TK_NULL|Lit],Op, T);
expr(Lit, Op, [{?TK_VARIABLE,N}|T]) ->
	expr([{?TK_VARIABLE,N}|Lit],Op, T);
expr(Lit, Op, [{?TK_ID,Db},?TK_DOT,{?TK_ID,Table},?TK_DOT,{?TK_ID,Col}|T]) ->
	expr([{?TK_COLUMN,{Db,Table,Col}}|Lit], Op, T);
expr(Lit, Op, [{?TK_ID,Table},?TK_DOT,{?TK_ID,Col}|T]) ->
	expr([{?TK_COLUMN,{undefined,Table,Col}}|Lit], Op, T);
expr(Lit, Op, [{?TK_ID,Col}|T]) ->
	expr([{?TK_COLUMN,Col}|Lit], Op, T);
% binary expressions
expr(Lit, Op, [?TK_IS,?TK_NOT|T]) ->
	expr(Lit,Op,[?TK_ISNOT|T]);
expr(Lit,[],[Op|T]) ->
	case priority(Op) of
		undefined ->
			ok;
		_ ->
			expr(Lit,[Op],T)
	end;
expr(Lit,[Oph|Op],[H|T]) ->
	OphPr = priority(Oph),
	HPr = priority(H),
	case ok of
		_ when HPr == undefined ->
			ok;
		_ when OphPr < HPr ->
			expr(Lit,[H,Oph|Op],T);
		_ ->
			expr([Oph|Lit],Op,[H|T])
	end.

% Highest to lowest
priority(?TK_CONCAT) ->
	10;
priority(?TK_STAR) ->
	9;
priority(?TK_SLASH) ->
	9;
priority(?TK_REM) ->
	9;
priority(?TK_PLUS) ->
	8;
priority(?TK_MINUS) ->
	8;
priority(?TK_LSHIFT) ->
	7;
priority(?TK_RSHIFT) ->
	7;
priority(?TK_BITAND) ->
	7;
priority(?TK_BITOR) ->
	7;
priority(?TK_LT) ->
	6;
priority(?TK_LE) ->
	6;
priority(?TK_GT) ->
	6;
priority(?TK_GE) ->
	6;
priority(?TK_EQ) ->
	5;
priority(?TK_NE) ->
	5;
priority(?TK_IS) ->
	5;
priority(?TK_ISNOT) ->
	5;
priority(?TK_IN) ->
	5;
priority(?TK_LIKE_KW) ->
	5;
priority(?TK_AND) ->
	4;
priority(?TK_OR) ->
	3;
priority(_) ->
	undefined.


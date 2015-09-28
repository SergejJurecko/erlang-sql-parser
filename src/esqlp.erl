-module(esqlp).
-export([parse/1]).
% -compile(export_all).
-define(A(A),(A == $a orelse A == $A)).
-define(B(B),(B == $b orelse B == $B)).
-define(C(C),(C == $c orelse C == $C)).
-define(D(D),(D == $d orelse D == $D)).
-define(E(E),(E == $e orelse E == $E)).
-define(F(F),(F == $f orelse F == $F)).
-define(G(G),(G == $g orelse G == $G)).
-define(H(H),(H == $h orelse H == $H)).
-define(I(I),(I == $i orelse I == $I)).
-define(K(K),(K == $k orelse K == $K)).
-define(L(L),(L == $l orelse L == $L)).
-define(M(M),(M == $m orelse M == $M)).
-define(N(N),(N == $n orelse N == $N)).
-define(O(O),(O == $o orelse O == $O)).
-define(P(P),(P == $p orelse P == $P)).
-define(R(R),(R == $r orelse R == $R)).
-define(S(S),(S == $s orelse S == $S)).
-define(T(T),(T == $t orelse T == $T)).
-define(U(U),(U == $u orelse U == $U)).
-define(V(V),(V == $v orelse V == $V)).
-define(W(W),(W == $w orelse W == $W)).
-define(X(X),(X == $x orelse X == $X)).
-define(Y(Y),(Y == $y orelse Y == $Y)).
-define(SKIP(X),(X == $\s orelse X == $\n  orelse X == $\r)).
-define(WORD(X),(list_to_binary(lists:reverse(X)))).

% State
-record(dp,{cmd}).

parse(L) when is_list(L) ->
	parse(iolist_to_binary(L));
parse(Bin) ->
	cmd(#dp{},Bin).

cmd(P,<<C,B/binary>>) when ?SKIP(C) ->
	cmd(P,B);
cmd(P,<<"select",C,B/binary>>) when ?SKIP(C) ->
	select(P,B);
cmd(P,<<"SELECT",C,B/binary>>) when ?SKIP(C) ->
	select(P,B);
cmd(P,<<"Select",C,B/binary>>) when ?SKIP(C) ->
	select(P,B);
cmd(P,<<"insert",C,B/binary>>) when ?SKIP(C) ->
	insert(P,B);
cmd(P,<<"INSERT",C,B/binary>>) when ?SKIP(C) ->
	insert(P,B);
cmd(P,<<"Insert",C,B/binary>>) when ?SKIP(C) ->
	insert(P,B);
cmd(P,<<"delete",C,B/binary>>) when ?SKIP(C) ->
	delete(P,B);
cmd(P,<<"DELETE",C,B/binary>>) when ?SKIP(C) ->
	delete(P,B);
cmd(P,<<"Delete",C,B/binary>>) when ?SKIP(C) ->
	delete(P,B);
cmd(P,<<"create",C,B/binary>>) when ?SKIP(C) ->
	create(P,B);
cmd(P,<<"CREATE",C,B/binary>>) when ?SKIP(C) ->
	create(P,B);
cmd(P,<<"Create",C,B/binary>>) when ?SKIP(C) ->
	create(P,B);
cmd(P,<<"update",C,B/binary>>) when ?SKIP(C) ->
	update(P,B);
cmd(P,<<"UPDATE",C,B/binary>>) when ?SKIP(C) ->
	update(P,B);
cmd(P,<<"Update",C,B/binary>>) when ?SKIP(C) ->
	update(P,B);
cmd(P,<<C,R,E,A,T,E,SKIP,Bin/binary>>) when ?SKIP(SKIP) andalso ?C(C) andalso ?R(R) andalso ?E(E) andalso ?A(A) andalso ?T(T) ->
	create(P,Bin);
cmd(P,<<I,N,S,E,R,T,SKIP,Bin/binary>>) when ?SKIP(SKIP) andalso ?I(I) andalso 
	?N(N) andalso ?S(S) andalso ?E(E) andalso ?R(R) andalso ?T(T) ->
	insert(P,Bin);
cmd(P,<<U,P,D,A,T,E,SKIP,Bin/binary>>) when ?SKIP(SKIP) andalso ?U(U) andalso ?P(P) andalso 
	?D(D) andalso ?A(A) andalso ?T(T) andalso ?E(E) ->
	update(P,Bin);
cmd(P,<<D,E,L,E,T,E,SKIP,Bin/binary>>) when ?SKIP(SKIP) andalso ?D(D) andalso ?E(E) andalso ?L(L) andalso ?T(T)  ->
	delete(P,Bin).

% Based on diagrams at: http://sqlite.org/lang_select.html
-record(from,{db, table, as}).
-record(sct,{
% distinct or all
modifier = all, 
% [ColumnNameBin1,ColumnNameBin2,...]
cols = [], 
% #from{}
from = []}).

select(P,<<C,Bin/binary>>) when ?SKIP(C) ->
	select(P#dp{cmd = #sct{}},Bin);
select(P,<<"all",C,Bin/binary>>) when ?SKIP(C) ->
	select(P,Bin);
select(P,<<"ALL",C,Bin/binary>>) when ?SKIP(C) ->
	select(P,Bin);
select(P,<<"All",C,Bin/binary>>) when ?SKIP(C) ->
	select(P,Bin);
select(P,<<"distinct",C,Bin/binary>>) when ?SKIP(C) ->
	select(P,Bin);
select(P,<<"Distinct",C,Bin/binary>>) when ?SKIP(C) ->
	select(P,Bin);
select(P,<<"DISTINCT",C,Bin/binary>>) when ?SKIP(C) ->
	select(P,Bin);
select(P,<<A,L,L1,C,Bin/binary>>) when ?SKIP(C) andalso ?A(A) andalso ?L(L) andalso ?L(L1) ->
	select(P,Bin);
select(P,<<D,I,S,T,I,N,C,T,SK,Bin/binary>>) when ?SKIP(SK) andalso ?D(D) andalso ?I(I) andalso ?S(S) andalso
	?T(T) andalso ?N(N) andalso ?C(C) ->
	select(P#dp{cmd = (P#dp.cmd)#sct{modifier = distinct}},Bin);
select(P,Bin) ->
	select_cols(P,Bin,[]).

% Read select columns
select_cols(P,<<C,B/binary>>,[]) when ?SKIP(C) ->
	select_cols(P,B,[]);
select_cols(P,<<C,B/binary>>,Word) when ?SKIP(C) orelse C == $, ->
	S = P#dp.cmd,
	select_cols(P#dp{cmd = S#sct{cols = [?WORD(Word)|S#sct.cols]}},B,[]);
select_cols(P,<<"from",C,B/binary>>,[]) when ?SKIP(C) ->
	select_from(P,B,{[],[]});
select_cols(P,<<"FROM",C,B/binary>>,[]) when ?SKIP(C) ->
	select_from(P,B,{[],[]});
select_cols(P,<<"From",C,B/binary>>,[]) when ?SKIP(C) ->
	select_from(P,B,{[],[]});
select_cols(P,<<F,R,O,M,C,B/binary>>,[]) when ?SKIP(C) andalso ?F(F) andalso ?R(R) andalso ?O(O) andalso ?M(M) ->
	select_from(P,B,{[],[]});
select_cols(P,<<C,B/binary>>,L) ->
	select_cols(P,B,[C|L]).

select_from(P,<<C,Bin/binary>>,{[],[]}) when ?SKIP(C)  ->
	select_from(P,Bin,{[],[]});
select_from(P,<<C,Bin/binary>>,[]) when ?SKIP(C)  ->
	select_from(P,Bin,[]);
select_from(P,<<C,B/binary>>,{_,_} = Word) when ?SKIP(C) ->
	S = P#dp.cmd,
	case Word of
		{[_|_] = Table,[]} ->
			select_from(P#dp{cmd = S#sct{from = [#from{table = ?WORD(Table)}|S#sct.from]}},B,{[],[]});
		{Db,Table} ->
			select_from(P#dp{cmd = S#sct{from = [#from{db = Db, table = ?WORD(Table)}|S#sct.from]}},B,{[],[]})
	end;
select_from(P,<<C,B/binary>>,[_|_] = Word) when ?SKIP(C) ->
	% Finished AS
	S = P#dp.cmd,
	[H|Tl] = S#sct.from,
	select_from(P#dp{cmd = S#sct{from = [H#from{as = ?WORD(Word)}|Tl]}},B,{[],[]});
select_from(P,<<".",B/binary>>,{[_|_] = Word,[]}) ->
	select_from(P,B,{?WORD(Word),[]});
select_from(P,<<"as",C,B/binary>>,_) when ?SKIP(C) ->
	select_from(P,B,[]);
select_from(P,<<"AS",C,B/binary>>,_) when ?SKIP(C) ->
	select_from(P,B,[]);
select_from(P,<<"As",C,B/binary>>,_) when ?SKIP(C) ->
	select_from(P,B,[]);
select_from(P,<<"aS",C,B/binary>>,_) when ?SKIP(C) ->
	select_from(P,B,[]);
select_from(P,<<"where",C,B/binary>>,_) when ?SKIP(C) ->
	select_where(P,B,[]);
select_from(P,<<"Where",C,B/binary>>,_) when ?SKIP(C) ->
	select_where(P,B,[]);
select_from(P,<<"WHERE",C,B/binary>>,_) when ?SKIP(C) ->
	select_where(P,B,[]);
select_from(P,<<"group",C,B/binary>>,_) when ?SKIP(C) ->
	select_group(P,B,[]);
select_from(P,<<"Group",C,B/binary>>,_) when ?SKIP(C) ->
	select_group(P,B,[]);
select_from(P,<<"GROUP",C,B/binary>>,_) when ?SKIP(C) ->
	select_group(P,B,[]);
select_from(P,<<W,H,E,R,E,C,B/binary>>,_) when ?SKIP(C) andalso ?W(W) andalso ?H(H) andalso ?E(E) andalso ?R(R) ->
	select_where(P,B,[]);
select_from(P,<<G,R,O,U,P,C,B/binary>>,_) when ?SKIP(C) andalso ?G(G) andalso ?R(R) andalso ?O(O) andalso ?U(U) andalso ?P(P) ->
	select_group(P,B,[]);
select_from(P,<<C,B/binary>>,W) when is_list(W) ->
	select_from(P,B,[C|W]);
select_from(P,<<C,B/binary>>,{Db,[]}) when is_list(Db) ->
	select_from(P,B,{[C|Db],[]});
select_from(P,<<C,B/binary>>,{Db,Table}) when is_binary(Db), is_list(Table) ->
	select_from(P,B,{Db,[C|Table]}).

select_where(P,<<C,B/binary>>,[]) when ?SKIP(C) ->
	select_where(P,B,[]).

select_group(P,<<C,B/binary>>,[]) when ?SKIP(C) ->
	select_group(P,B,[]).

-record(ins,{}).
insert(P,<<C,Bin/binary>>) when ?SKIP(C) ->
	insert(P,Bin).

-record(upd,{}).
update(P,<<C,Bin/binary>>) when ?SKIP(C) ->
	update(P,Bin).

-record(del,{}).
delete(P,<<C,Bin/binary>>) when ?SKIP(C) ->
	delete(P,Bin).

-record(crt,{}).
create(P,<<C,Bin/binary>>) when ?SKIP(C) ->
	create(P,Bin).

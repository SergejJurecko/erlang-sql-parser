-module(esqlp).
-export([parse/1, tokenize/1,loop/1]).
-include("esqp.hrl").

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
-define(SKIP(X),(X == $\s orelse X == $\n  orelse X == $\t)).
-define(WORD(X),(list_to_binary(lists:reverse(X)))).


loop(N) ->
	S = os:timestamp(),
	loop1(N),
	timer:now_diff(os:timestamp(),S).

loop1(0) ->
	ok;
loop1(N) ->
	% term_to_binary({self(),{appendentries_response,12084,<<"asdlhf">>,aasdf,1,2,3,5}},[compressed,{minor_version,1}]),
	% tokenize(<<"SELECt FROM WhERE + - NOT NULL DISTiNCT ALL ">>),
	tokenize(<<"SELECT FROM WHERE + - NOT NULL DISTINCT 'AAAAALADJKLAKHDAUDGAIUDHAODHOADH' ALL ">>),
	loop1(N-1).


% Comments not supported atm.
tokenize(B) ->
	tkn(B,[]).
tkn(<<" ",B/binary>>,L) ->
	tkn(B,L);
tkn(<<$\t,B/binary>>,L) ->
	tkn(B,L);
tkn(<<$\r,B/binary>>,L) ->
	tkn(B,L);
tkn(<<$\n,B/binary>>,L) ->
	tkn(B,L);
tkn(<<"-",B/binary>>,L) ->
	tkn(B,[?TK_MINUS|L]);
tkn(<<"(",B/binary>>,L) ->
	tkn(B,[?TK_LP|L]);
tkn(<<")",B/binary>>,L) ->
	tkn(B,[?TK_RP|L]);
tkn(<<";",B/binary>>,L) ->
	tkn(B,[?TK_SEMI|L]);
tkn(<<"+",B/binary>>,L) ->
	tkn(B,[?TK_PLUS|L]);
tkn(<<"*",B/binary>>,L) ->
	tkn(B,[?TK_STAR|L]);
tkn(<<"%",B/binary>>,L) ->
	tkn(B,[?TK_REM|L]);
tkn(<<"=",B/binary>>,L) ->
	tkn(B,[?TK_EQ|L]);
tkn(<<"<=",B/binary>>,L) ->
	tkn(B,[?TK_LE|L]);
tkn(<<"<>",B/binary>>,L) ->
	tkn(B,[?TK_NE|L]);
tkn(<<"<<",B/binary>>,L) ->
	tkn(B,[?TK_LSHIFT|L]);
tkn(<<"<",B/binary>>,L) ->
	tkn(B,[?TK_LT|L]);
tkn(<<">=",B/binary>>,L) ->
	tkn(B,[?TK_GE|L]);
tkn(<<">>",B/binary>>,L) ->
	tkn(B,[?TK_RSHIFT|L]);
tkn(<<">",B/binary>>,L) ->
	tkn(B,[?TK_GT|L]);
tkn(<<"!=",B/binary>>,L) ->
	tkn(B,[?TK_NE|L]);
tkn(<<"||",B/binary>>,L) ->
	tkn(B,[?TK_BITOR|L]);
tkn(<<"|",B/binary>>,L) ->
	tkn(B,[?TK_CONCAT|L]);
tkn(<<",",B/binary>>,L) ->
	tkn(B,[?TK_COMMA|L]);
tkn(<<"&",B/binary>>,L) ->
	tkn(B,[?TK_BITAND|L]);
tkn(<<"~~",B/binary>>,L) ->
	tkn(B,[?TK_BITNOT|L]);
tkn(<<"select",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_SELECT|L]);
tkn(<<"SELECT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_SELECT|L]);
tkn(<<"Select",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_SELECT|L]);
tkn(<<"insert",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_INSERT|L]);
tkn(<<"INSERT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_INSERT|L]);
tkn(<<"Insert",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_INSERT|L]);
tkn(<<"delete",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_DELETE|L]);
tkn(<<"DELETE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_DELETE|L]);
tkn(<<"Delete",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_DELETE|L]);
tkn(<<"create",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_CREATE|L]);
tkn(<<"CREATE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_CREATE|L]);
tkn(<<"Create",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_CREATE|L]);
tkn(<<"update",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_UPDATE|L]);
tkn(<<"UPDATE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_UPDATE|L]);
tkn(<<"Update",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_UPDATE|L]);
tkn(<<"all",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_ALL|L]);
tkn(<<"ALL",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_ALL|L]);
tkn(<<"All",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_ALL|L]);
tkn(<<"as",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_AS|L]);
tkn(<<"As",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_AS|L]);
tkn(<<"aS",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_AS|L]);
tkn(<<"AS",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_AS|L]);
tkn(<<"in",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_IN|L]);
tkn(<<"In",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_IN|L]);
tkn(<<"iN",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_IN|L]);
tkn(<<"IN",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_IN|L]);
tkn(<<"not",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_NOT|L]);
tkn(<<"Not",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_NOT|L]);
tkn(<<"NOT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_NOT|L]);
tkn(<<"from",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_FROM|L]);
tkn(<<"From",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_FROM|L]);
tkn(<<"FROM",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_FROM|L]);
tkn(<<"distinct",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_DISTINCT|L]);
tkn(<<"Distinct",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_DISTINCT|L]);
tkn(<<"DISTINCT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_DISTINCT|L]);
tkn(<<"where",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_WHERE|L]);
tkn(<<"Where",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_WHERE|L]);
tkn(<<"WHERE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_WHERE|L]);
tkn(<<"group",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_GROUP|L]);
tkn(<<"Group",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_GROUP|L]);
tkn(<<"GROUP",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_GROUP|L]);
% tkn(<<"like",C,B/binary>>,L) when ?SKIP(C) ->
% 	tkn(B,[?TK_LIKE|L]);
% tkn(<<"Like",C,B/binary>>,L) when ?SKIP(C) ->
% 	tkn(B,[?TK_LIKE|L]);
% tkn(<<"LIKE",C,B/binary>>,L) when ?SKIP(C) ->
% 	tkn(B,[?TK_LIKE|L]);
tkn(<<"null",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_NULL|L]);
tkn(<<"Null",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_NULL|L]);
tkn(<<"NULL",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(B,[?TK_NULL|L]);
tkn(<<C,R,E,A,T,E,SKIP,B/binary>>,L) when ?SKIP(SKIP) andalso ?C(C) andalso ?R(R) andalso ?E(E) andalso ?A(A) andalso ?T(T) ->
	tkn(B,[?TK_CREATE|L]);
tkn(<<S,E,L,E1,C,T,SKIP,B/binary>>,LL) when ?SKIP(SKIP) andalso ?S(S) andalso ?E(E) andalso ?E(E1) andalso ?L(L) andalso ?C(C) andalso ?T(T) ->
	tkn(B,[?TK_SELECT|LL]);
tkn(<<I,N,S,E,R,T,SKIP,B/binary>>,L) when ?SKIP(SKIP) andalso ?I(I) andalso 
	?N(N) andalso ?S(S) andalso ?E(E) andalso ?R(R) andalso ?T(T) ->
	tkn(B,[?TK_INSERT|L]);
tkn(<<U,P,D,A,T,E,SKIP,B/binary>>,L) when ?SKIP(SKIP) andalso ?U(U) andalso ?P(P) andalso 
	?D(D) andalso ?A(A) andalso ?T(T) andalso ?E(E) ->
	tkn(B,[?TK_UPDATE|L]);
tkn(<<D,E,L,E,T,E,SKIP,B/binary>>,L) when ?SKIP(SKIP) andalso ?D(D) andalso ?E(E) andalso ?L(L) andalso ?T(T)  ->
	tkn(B,[?TK_DELETE|L]);
tkn(<<F,R,O,M,SKIP,B/binary>>,L) when ?SKIP(SKIP) andalso ?F(F) andalso ?R(R) andalso ?O(O) andalso ?M(M)  ->
	tkn(B,[?TK_FROM|L]);
tkn(<<A,L,L1,C,B/binary>>,LL) when ?SKIP(C) andalso ?A(A) andalso ?L(L) andalso ?L(L1) ->
	tkn(B,[?TK_ALL|LL]);
tkn(<<N,O,T,C,B/binary>>,L) when ?SKIP(C) andalso ?N(N) andalso ?O(O) andalso ?T(T) ->
	tkn(B,[?TK_NOT|L]);
% tkn(<<L,I,K,E,C,B/binary>>,L) when ?SKIP(C) andalso ?L(L) andalso ?I(I) andalso ?K(K) andalso ?E(E) ->
% 	tkn(B,[?TK_LIKE|L]);
tkn(<<N,U,L,L,C,B/binary>>,LL) when ?SKIP(C) andalso ?L(L) andalso ?N(N) andalso ?U(U) ->
	tkn(B,[?TK_NULL|LL]);
tkn(<<D,I,S,T,I1,N,C,T,SK,B/binary>>,L) when ?SKIP(SK) andalso ?D(D) andalso ?I(I) andalso ?I(I1) andalso ?S(S) andalso
	?T(T) andalso ?N(N) andalso ?C(C) ->
	tkn(B,[?TK_DISTINCT|L]);
tkn(<<W,H,E,R,E,SK,B/binary>>,L) when ?SKIP(SK) andalso ?W(W) andalso ?H(H) andalso ?E(E) andalso ?R(R) ->
	tkn(B,[?TK_WHERE|L]);
tkn(<<G,R,O,U,P,SK,B/binary>>,L) when ?SKIP(SK) andalso ?G(G) andalso ?R(R) andalso ?O(O) andalso ?U(U) andalso ?P(P) ->
	tkn(B,[?TK_GROUP|L]);
tkn(<<"'",B/binary>>,L) ->
	case binary:split(B,<<"'">>) of
		[String,<<"'",Rem/binary>>] ->
			tknstr([String,<<"''">>],Rem,L);
		[String,Rem] ->
			tkn(Rem,[[String]|L])
	end;
tkn(<<".",B/binary>>,L) ->
	tkn(B,[?TK_DOT|L]);
tkn(<<C,B/binary>>,L) when C >= $0, C =< $9 ->
	tknnum(<<C,B/binary>>,[],L,int);
tkn(<<>>,L) ->
	lists:reverse(L).

tknnum(<<C,B/binary>>,N,L,Type) when C >= $0, C =< $9 ->
	tknnum(<<B/binary>>,[C|N],L,Type);
tknnum(<<".",B/binary>>,N,L,_) ->
	tknnum(B,[$.|N],L,float);
tknnum(B,N,L,int) ->
	tkn(B,[list_to_binary(lists:reverse(N))|L]);
tknnum(B,N,L,float) ->
	tkn(B,[list_to_float(lists:reverse(N))|L]).

tknstr(Str,B,L) ->
	case binary:split(B,<<"'">>) of
		[String,<<"'",Rem/binary>>] ->
			tknstr([Str,String,<<"''">>],Rem,L);
		[String,Rem] ->
			tkn(Rem,[[Str,String]|L])
	end.


parse(L) when is_list(L) ->
	parse(iolist_to_binary(L));
parse(Bin) ->
	cmd(#sql{},Bin).

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



select(P,<<C,Bin/binary>>) when ?SKIP(C) ->
	select(P#sql{cmd = #select{}},Bin);
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
	select(P#sql{cmd = (P#sql.cmd)#select{modifier = distinct}},Bin);
select(P,Bin) ->
	select_cols(P,Bin,[]).

% Read select columns
select_cols(P,<<C,B/binary>>,[]) when ?SKIP(C) ->
	select_cols(P,B,[]);
select_cols(P,<<C,B/binary>>,Word) when ?SKIP(C) orelse C == $, ->
	S = P#sql.cmd,
	select_cols(P#sql{cmd = S#select{cols = [?WORD(Word)|S#select.cols]}},B,[]);
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
	S = P#sql.cmd,
	case Word of
		{[_|_] = Table,[]} ->
			select_token(P#sql{curop = select_from,
				cmd = S#select{from = [#from{table = ?WORD(Table)}|S#select.from]}},B);
		{Db,Table} ->
			select_token(P#sql{curop = select_from,
				cmd = S#select{from = [#from{db = Db, table = ?WORD(Table)}|S#select.from]}},B)
	end;
select_from(P,<<C,B/binary>>,[_|_] = Word) when ?SKIP(C) ->
	% Finished AS
	S = P#sql.cmd,
	[H|Tl] = S#select.from,
	select_token(P#sql{curop = select_from, cmd = S#select{from = [H#from{as = ?WORD(Word)}|Tl]}},B);
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
select_from(P,<<C,B/binary>>,W) when is_list(W) ->
	select_from(P,B,[C|W]);
select_from(P,<<C,B/binary>>,{Db,[]}) when is_list(Db) ->
	select_from(P,B,{[C|Db],[]});
select_from(P,<<C,B/binary>>,{Db,Table}) when is_binary(Db), is_list(Table) ->
	select_from(P,B,{Db,[C|Table]}).

select_token(P,<<"where",C,B/binary>>) when ?SKIP(C) ->
	expr_token(P,B);
select_token(P,<<"Where",C,B/binary>>) when ?SKIP(C) ->
	expr_token(P,B);
select_token(P,<<"WHERE",C,B/binary>>) when ?SKIP(C) ->
	expr_token(P,B);
select_token(P,<<"group",C,B/binary>>) when ?SKIP(C) ->
	select_group(P,B,[]);
select_token(P,<<"Group",C,B/binary>>) when ?SKIP(C) ->
	select_group(P,B,[]);
select_token(P,<<"GROUP",C,B/binary>>) when ?SKIP(C) ->
	select_group(P,B,[]);
select_token(P,<<W,H,E,R,E,C,B/binary>>) when ?SKIP(C) andalso ?W(W) andalso ?H(H) andalso ?E(E) andalso ?R(R) ->
	expr_token(P,B);
select_token(P,<<G,R,O,U,P,C,B/binary>>) when ?SKIP(C) andalso ?G(G) andalso ?R(R) andalso ?O(O) andalso ?U(U) andalso ?P(P) ->
	select_group(P,B,[]);
select_token(#sql{curop = select_from} = P,B) ->
	select_from(P,B,{[],[]});
select_token(#sql{curop = select_where} = P,B) ->
	expr_token(P,B);
select_token(#sql{curop = select_group} = P,B) ->
	select_group(P,B,[]).

% select_where(P,<<C,B/binary>>) when ?SKIP(C) ->
% 	select_where(P,B);
% select_where(P,B) ->
% 	expr_token(P,B).
expr_token(P,<<"in",C,B/binary>>) when ?SKIP(C) ->
	expr_in(P,B);
expr_token(P,<<"In",C,B/binary>>) when ?SKIP(C) ->
	expr_in(P,B);
expr_token(P,<<"iN",C,B/binary>>) when ?SKIP(C) ->
	expr_in(P,B);
expr_token(P,<<"IN",C,B/binary>>) when ?SKIP(C) ->
	expr_in(P,B);
expr_token(P,<<"not",C,B/binary>>) when ?SKIP(C) ->
	expr_not(P,B);
expr_token(P,<<"NOT",C,B/binary>>) when ?SKIP(C) ->
	expr_not(P,B);
expr_token(P,<<"Not",C,B/binary>>) when ?SKIP(C) ->
	expr_not(P,B);
expr_token(P,<<"nOt",C,B/binary>>) when ?SKIP(C) ->
	expr_not(P,B);
expr_token(P,<<"noT",C,B/binary>>) when ?SKIP(C) ->
	expr_not(P,B);
expr_token(P,<<"like",C,B/binary>>) when ?SKIP(C) ->
	expr_like(P,B);
expr_token(P,<<"Like",C,B/binary>>) when ?SKIP(C) ->
	expr_like(P,B);
expr_token(P,<<"LIKE",C,B/binary>>) when ?SKIP(C) ->
	expr_like(P,B);
expr_token(P,<<L,I,K,E,C,B/binary>>) when ?SKIP(C) andalso ?L(L) andalso ?I(I) andalso ?K(K) andalso ?E(E) ->
	expr_like(P,B);
expr_token(P,B) ->
	expr_literal(P,B).

expr_in(P,<<C,B/binary>>) when ?SKIP(C) ->
	expr_in(P,B).

expr_like(P,<<C,B/binary>>) when ?SKIP(C) ->
	expr_like(P,B).

expr_literal(P,<<C,B/binary>>) when ?SKIP(C) ->
	expr_literal(P,B).

expr_not(P,<<C,B/binary>>) when ?SKIP(C) ->
	expr_not(P,B);
expr_not(P,<<"null",C,B/binary>>) when ?SKIP(C) ->
	expr_not_null(P,B);
expr_not(P,<<"Null",C,B/binary>>) when ?SKIP(C) ->
	expr_not_null(P,B);
expr_not(P,<<"NULL",C,B/binary>>) when ?SKIP(C) ->
	expr_not_null(P,B);
expr_not(P,<<"like",C,B/binary>>) when ?SKIP(C) ->
	expr_like(P,B);
expr_not(P,<<"Like",C,B/binary>>) when ?SKIP(C) ->
	expr_not_like(P,B);
expr_not(P,<<"LIKE",C,B/binary>>) when ?SKIP(C) ->
	expr_not_like(P,B);
expr_not(P,<<"in",C,B/binary>>) when ?SKIP(C) ->
	expr_not_in(P,B);
expr_not(P,<<"In",C,B/binary>>) when ?SKIP(C) ->
	expr_not_in(P,B);
expr_not(P,<<"iN",C,B/binary>>) when ?SKIP(C) ->
	expr_not_in(P,B);
expr_not(P,<<"IN",C,B/binary>>) when ?SKIP(C) ->
	expr_not_in(P,B);
expr_not(P,<<N,U,L,L,C,B/binary>>) when ?SKIP(C) andalso ?N(N) andalso ?U(U) andalso ?L(L) ->
	expr_not_null(P,B);
expr_not(P,<<L,I,K,E,C,B/binary>>) when ?SKIP(C) andalso ?L(L) andalso ?I(I) andalso ?K(K) andalso ?E(E) ->
	expr_not_like(P,B).

expr_list_literal(P,<<C,B/binary>>) when ?SKIP(C) ->
	expr_list_literal(P,B);
expr_list_literal(P,<<"(",B/binary>>) ->
	expr_list_literal1(P,B,[],[]).

expr_list_literal1(P,<<")",B/binary>>,_,_) ->
	ok.

expr_not_in(P,B) ->
	expr_list_literal(P#sql{operator = [notin|P#sql.operator]},B).

expr_not_like(P,B) ->
	expr_literal(P#sql{operator = [notlike|P#sql.operator]},B).

expr_not_null(P,B) ->
	expr_token(P#sql{operator = [{notnull,hd(P#sql.literal)}, literal = tl(P#sql.literal)]},B).

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

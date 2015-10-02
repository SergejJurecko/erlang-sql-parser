-module(esqp).
-export([parse/1, tokenize/1,loop/2]).
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
% -define(WORD(X),(list_to_binary(lists:reverse(X)))).
-define(WORD(X),X).


loop(N,Sql) ->
	S = os:timestamp(),
	loop1(N,Sql),
	timer:now_diff(os:timestamp(),S).

loop1(0,_) ->
	ok;
loop1(N,Sql) ->
	tokenize(Sql),
	loop1(N-1,Sql).


% Comments not supported atm.
tokenize(B) ->
	tkn(undef,B,[]).
tkn(undef,<<" ",B/binary>>,L) ->
	tkn(undef,B,L);
tkn(undef,<<$\t,B/binary>>,L) ->
	tkn(undef,B,L);
tkn(undef,<<$\r,B/binary>>,L) ->
	tkn(undef,B,L);
tkn(undef,<<$\n,B/binary>>,L) ->
	tkn(undef,B,L);
tkn(undef,<<"-",B/binary>>,L) ->
	tkn(undef,B,[?TK_MINUS|L]);
tkn(undef,<<"(",B/binary>>,L) ->
	tkn(undef,B,[?TK_LP|L]);
tkn(undef,<<")",B/binary>>,L) ->
	tkn(undef,B,[?TK_RP|L]);
tkn(undef,<<";",B/binary>>,L) ->
	tkn(undef,B,[?TK_SEMI|L]);
tkn(undef,<<"+",B/binary>>,L) ->
	tkn(undef,B,[?TK_PLUS|L]);
tkn(undef,<<"*",B/binary>>,L) ->
	tkn(undef,B,[?TK_STAR|L]);
tkn(undef,<<"%",B/binary>>,L) ->
	tkn(undef,B,[?TK_REM|L]);
tkn(undef,<<"=",B/binary>>,L) ->
	tkn(undef,B,[?TK_EQ|L]);
tkn(undef,<<"<=",B/binary>>,L) ->
	tkn(undef,B,[?TK_LE|L]);
tkn(undef,<<"<>",B/binary>>,L) ->
	tkn(undef,B,[?TK_NE|L]);
tkn(undef,<<"<<",B/binary>>,L) ->
	tkn(undef,B,[?TK_LSHIFT|L]);
tkn(undef,<<"<",B/binary>>,L) ->
	tkn(undef,B,[?TK_LT|L]);
tkn(undef,<<">=",B/binary>>,L) ->
	tkn(undef,B,[?TK_GE|L]);
tkn(undef,<<">>",B/binary>>,L) ->
	tkn(undef,B,[?TK_RSHIFT|L]);
tkn(undef,<<">",B/binary>>,L) ->
	tkn(undef,B,[?TK_GT|L]);
tkn(undef,<<"!=",B/binary>>,L) ->
	tkn(undef,B,[?TK_NE|L]);
tkn(undef,<<"||",B/binary>>,L) ->
	tkn(undef,B,[?TK_BITOR|L]);
tkn(undef,<<"|",B/binary>>,L) ->
	tkn(undef,B,[?TK_CONCAT|L]);
tkn(undef,<<",",B/binary>>,L) ->
	tkn(undef,B,[?TK_COMMA|L]);
tkn(undef,<<"&",B/binary>>,L) ->
	tkn(undef,B,[?TK_BITAND|L]);
tkn(undef,<<"~~",B/binary>>,L) ->
	tkn(undef,B,[?TK_BITNOT|L]);
tkn(undef,<<"select",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_SELECT|L]);
tkn(undef,<<"SELECT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_SELECT|L]);
tkn(undef,<<"Select",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_SELECT|L]);
tkn(undef,<<"insert",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_INSERT|L]);
tkn(undef,<<"INSERT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_INSERT|L]);
tkn(undef,<<"Insert",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_INSERT|L]);
tkn(undef,<<"delete",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_DELETE|L]);
tkn(undef,<<"DELETE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_DELETE|L]);
tkn(undef,<<"Delete",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_DELETE|L]);
tkn(undef,<<"create",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_CREATE|L]);
tkn(undef,<<"CREATE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_CREATE|L]);
tkn(undef,<<"Create",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_CREATE|L]);
tkn(undef,<<"update",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_UPDATE|L]);
tkn(undef,<<"UPDATE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_UPDATE|L]);
tkn(undef,<<"Update",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_UPDATE|L]);
tkn(undef,<<"all",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_ALL|L]);
tkn(undef,<<"ALL",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_ALL|L]);
tkn(undef,<<"All",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_ALL|L]);
tkn(undef,<<"as",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_AS|L]);
tkn(undef,<<"As",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_AS|L]);
tkn(undef,<<"aS",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_AS|L]);
tkn(undef,<<"AS",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_AS|L]);
tkn(undef,<<"in",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IN|L]);
tkn(undef,<<"In",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IN|L]);
tkn(undef,<<"iN",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IN|L]);
tkn(undef,<<"IN",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IN|L]);
tkn(undef,<<"not",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_NOT|L]);
tkn(undef,<<"Not",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_NOT|L]);
tkn(undef,<<"NOT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_NOT|L]);
tkn(undef,<<"from",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_FROM|L]);
tkn(undef,<<"From",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_FROM|L]);
tkn(undef,<<"FROM",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_FROM|L]);
tkn(undef,<<"distinct",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_DISTINCT|L]);
tkn(undef,<<"Distinct",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_DISTINCT|L]);
tkn(undef,<<"DISTINCT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_DISTINCT|L]);
tkn(undef,<<"where",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_WHERE|L]);
tkn(undef,<<"Where",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_WHERE|L]);
tkn(undef,<<"WHERE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_WHERE|L]);
tkn(undef,<<"group",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_GROUP|L]);
tkn(undef,<<"Group",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_GROUP|L]);
tkn(undef,<<"GROUP",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_GROUP|L]);
tkn(undef,<<"limit",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_LIMIT|L]);
tkn(undef,<<"Limit",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_LIMIT|L]);
tkn(undef,<<"LIMIT",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_LIMIT|L]);
tkn(undef,<<"offset",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_OFFSET|L]);
tkn(undef,<<"Offset",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_OFFSET|L]);
tkn(undef,<<"OFFSET",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_OFFSET|L]);
tkn(undef,<<"null",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_NULL|L]);
tkn(undef,<<"Null",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_NULL|L]);
tkn(undef,<<"NULL",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_NULL|L]);
tkn(undef,<<"or",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_OR|L]);
tkn(undef,<<"Or",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_OR|L]);
tkn(undef,<<"oR",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_OR|L]);
tkn(undef,<<"OR",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_OR|L]);
tkn(undef,<<"and",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_AND|L]);
tkn(undef,<<"And",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_AND|L]);
tkn(undef,<<"AND",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_AND|L]);
tkn(undef,<<"'",B/binary>>,L) ->
	case binary:split(B,<<"'">>) of
		[String,<<"'",Rem/binary>>] ->
			tkn([str,[String,<<"''">>]],Rem,L);
		[String,Rem] ->
			tkn(undef,Rem,[{?TK_STRING,String}|L])
	end;
tkn(undef,<<".",B/binary>>,L) ->
	tkn(undef,B,[?TK_DOT|L]);
tkn(undef,<<C,B/binary>>,L) when C >= $0, C =< $9 ->
	tkn([int,[C]],B,L);
tkn(undef,<<C,B/binary>>,L) when C >= $a, C =< $z ->
	tkn([C],B,L);
tkn(undef,<<C,B/binary>>,L) when C >= $A, C =< $Z ->
	tkn([C+32],B,L);
tkn([int,N],<<C,B/binary>>,L) when C >= $0, C =< $9 ->
	tkn([int,[C|N]],B,L);
tkn([float,N],<<C,B/binary>>,L) when C >= $0, C =< $9 ->
	tkn([float,[C|N]],B,L);
tkn([int,N],<<".",B/binary>>,L) ->
	tkn([float,[$.|N]],B,L);
tkn([int,N],B,L) ->
	tkn(undef,B,[{?TK_INTEGER,list_to_integer(lists:reverse(N))}|L]);
tkn([float,N],B,L) ->
	tkn(undef,B,[{?TK_FLOAT,list_to_float(lists:reverse(N))}|L]);
tkn([str,S],Bin,L) ->
	case binary:split(Bin,<<"'">>) of
		[String,<<"'",Rem/binary>>] ->
			tkn([str,[S,String,<<"''">>]],Rem,L);
		[String,Rem] ->
			tkn(undef,Rem,[{?TK_STRING,[S,String]}|L])
	end;
tkn([_|_]=W,<<C,B/binary>>,L) when C >= $a, C =< $z ->
	tkn([C|W],B,L);
tkn([_|_]=W,<<C,B/binary>>,L) when C >= $A, C =< $Z ->
	tkn([(C+32)|W],B,L);
tkn([_|_]=W,B,L) ->
	case W of
		"sa" ->
			tkn(undef,B,[?TK_AS|L]);
		"tceles" ->
			tkn(undef,B,[?TK_SELECT|L]);
		"tresni" ->
			tkn(undef,B,[?TK_INSERT|L]);
		"puorg" ->
			tkn(undef,B,[?TK_GROUP|L]);
		"tesffo" ->
			tkn(undef,B,[?TK_OFFSET|L]);
		"timil" ->
			tkn(undef,B,[?TK_LIMIT|L]);
		"erehw" ->
			tkn(undef,B,[?TK_WHERE|L]);
		"lla" ->
			tkn(undef,B,[?TK_ALL|L]);
		"tcnitsid" ->
			tkn(undef,B,[?TK_DISTINCT|L]);
		"llun" ->
			tkn(undef,B,[?TK_NULL|L]);
		"morf" ->
			tkn(undef,B,[?TK_FROM|L]);
		"ton" ->
			tkn(undef,B,[?TK_NOT|L]);
		"ni" ->
			tkn(undef,B,[?TK_IN|L]);
		"eteled" ->
			tkn(undef,B,[?TK_DELETE|L]);
		"etadpu" ->
			tkn(undef,B,[?TK_UPDATE|L]);
		"dna" ->
			tkn(undef,B,[?TK_AND|L]);
		"ro" ->
			tkn(undef,B,[?TK_OR|L]);
		_ ->
			tkn(undef,B,[{?TK_ID,?WORD(lists:reverse(W))}|L])
	end;
tkn(undef,<<>>,L) ->
	lists:reverse(L).

tknstr(Str,B,L) ->
	case binary:split(B,<<"'">>) of
		[String,<<"'",Rem/binary>>] ->
			tknstr([Str,String,<<"''">>],Rem,L);
		[String,Rem] ->
			tkn(undef,Rem,[[Str,String]|L])
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

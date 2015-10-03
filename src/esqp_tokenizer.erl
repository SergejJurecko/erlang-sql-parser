-module(esqp_tokenizer).
-export([tokenize/1]).
-include("esqp.hrl").
-define(SKIP(X),X == $\s; X == $\n; X == $\t).
% -define(WORD(X),(list_to_binary(lists:reverse(X)))).
-define(WORD(X),X).


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

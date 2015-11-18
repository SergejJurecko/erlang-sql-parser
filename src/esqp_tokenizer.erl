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
tkn(undef,<<"/",B/binary>>,L) ->
	tkn(undef,B,[?TK_SLASH|L]);
tkn(undef,<<"%",B/binary>>,L) ->
	tkn(undef,B,[?TK_REM|L]);
tkn(undef,<<"==",B/binary>>,L) ->
	tkn(undef,B,[?TK_EQ|L]);
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
	tkn(undef,B,[?TK_CONCAT|L]);
tkn(undef,<<"|",B/binary>>,L) ->
	tkn(undef,B,[?TK_BITOR|L]);
tkn(undef,<<",",B/binary>>,L) ->
	tkn(undef,B,[?TK_COMMA|L]);
tkn(undef,<<"&",B/binary>>,L) ->
	tkn(undef,B,[?TK_BITAND|L]);
tkn(undef,<<"~~",B/binary>>,L) ->
	tkn(undef,B,[?TK_BITNOT|L]);
tkn(undef,<<"?",B/binary>>,L) ->
	tkn([var,[]],B,L);
tkn(undef,<<"is",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IS|L]);
tkn(undef,<<"Is",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IS|L]);
tkn(undef,<<"iS",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IS|L]);
tkn(undef,<<"IS",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_IS|L]);
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
tkn(undef,<<"like",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_LIKE_KW|L]);
tkn(undef,<<"Like",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_LIKE_KW|L]);
tkn(undef,<<"LIKE",C,B/binary>>,L) when ?SKIP(C) ->
	tkn(undef,B,[?TK_LIKE_KW|L]);
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
tkn([var,N],<<C,B/binary>>,L) when C >= $0, C =< $9 ->
	tkn([var,[C|N]],B,L);
tkn([float,N],<<C,B/binary>>,L) when C >= $0, C =< $9 ->
	tkn([float,[C|N]],B,L);
tkn([int,N],<<".",B/binary>>,L) ->
	tkn([float,[$.|N]],B,L);
tkn([int,N],B,L) ->
	tkn(undef,B,[{?TK_INTEGER,list_to_integer(lists:reverse(N))}|L]);
tkn([float,N],B,L) ->
	tkn(undef,B,[{?TK_FLOAT,list_to_float(lists:reverse(N))}|L]);
tkn([var,N],B,L) ->
	tkn(undef,B,[{?TK_VARIABLE,list_to_integer(lists:reverse(N))}|L]);
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
tkn([_|_]=W,<<C,B/binary>>,L) when C >= $0, C =< $9 ->
	tkn([C|W],B,L);
tkn([_|_]=W,<<"_",B/binary>>,L) ->
	tkn([$_|W],B,L);
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
		"ekil" ->
			tkn(undef,B,[?TK_LIKE_KW|L]);
		_ ->
			tkn(undef,B,[{?TK_ID,?WORD(lists:reverse(W))}|L])
	end;
tkn(undef,<<>>,L) ->
	lists:reverse(L).


% Actually a faster version, except for one ugly case that messes everything up. 
% Last case of define: tk(undef,B,tk(undef,W,[])++L)
% This gets executed for: column list without spaces, where clause without spaces or just plain column names.
% Parsing that completely destroys performance (most likely results in a copy of entire sql).

% -define(CHK(W), case W of
% 	<<"as">> ->
% 		tk(undef,B,[?TK_AS|L]);
% 	<<"As">> ->
% 		tk(undef,B,[?TK_AS|L]);
% 	<<"aS">> ->
% 		tk(undef,B,[?TK_AS|L]);
% 	<<"AS">> ->
% 		tk(undef,B,[?TK_AS|L]);
% 	<<"select">> ->
% 		tk(undef,B,[?TK_SELECT|L]);
% 	<<"Select">> ->
% 		tk(undef,B,[?TK_SELECT|L]);
% 	<<"SELECT">> ->
% 		tk(undef,B,[?TK_SELECT|L]);
% 	<<"insert">> ->
% 		tk(undef,B,[?TK_INSERT|L]);
% 	<<"Insert">> ->
% 		tk(undef,B,[?TK_INSERT|L]);
% 	<<"INSERT">> ->
% 		tk(undef,B,[?TK_INSERT|L]);
% 	<<"group">> ->
% 		tk(undef,B,[?TK_GROUP|L]);
% 	<<"Group">> ->
% 		tk(undef,B,[?TK_GROUP|L]);
% 	<<"GROUP">> ->
% 		tk(undef,B,[?TK_GROUP|L]);
% 	<<"offset">> ->
% 		tk(undef,B,[?TK_OFFSET|L]);
% 	<<"Offset">> ->
% 		tk(undef,B,[?TK_OFFSET|L]);
% 	<<"OFFSET">> ->
% 		tk(undef,B,[?TK_OFFSET|L]);
% 	<<"limit">> ->
% 		tk(undef,B,[?TK_LIMIT|L]);
% 	<<"Limit">> ->
% 		tk(undef,B,[?TK_LIMIT|L]);
% 	<<"LIMIT">> ->
% 		tk(undef,B,[?TK_LIMIT|L]);
% 	<<"where">> ->
% 		tk(undef,B,[?TK_WHERE|L]);
% 	<<"Where">> ->
% 		tk(undef,B,[?TK_WHERE|L]);
% 	<<"WHERE">> ->
% 		tk(undef,B,[?TK_WHERE|L]);
% 	<<"all">> ->
% 		tk(undef,B,[?TK_ALL|L]);
% 	<<"All">> ->
% 		tk(undef,B,[?TK_ALL|L]);
% 	<<"ALL">> ->
% 		tk(undef,B,[?TK_ALL|L]);
% 	<<"distinct">> ->
% 		tk(undef,B,[?TK_DISTINCT|L]);
% 	<<"Distinct">> ->
% 		tk(undef,B,[?TK_DISTINCT|L]);
% 	<<"DISTINCT">> ->
% 		tk(undef,B,[?TK_DISTINCT|L]);
% 	<<"null">> ->
% 		tk(undef,B,[?TK_NULL|L]);
% 	<<"Null">> ->
% 		tk(undef,B,[?TK_NULL|L]);
% 	<<"NULL">> ->
% 		tk(undef,B,[?TK_NULL|L]);
% 	<<"from">> ->
% 		tk(undef,B,[?TK_FROM|L]);
% 	<<"From">> ->
% 		tk(undef,B,[?TK_FROM|L]);
% 	<<"FROM">> ->
% 		tk(undef,B,[?TK_FROM|L]);
% 	<<"not">> ->
% 		tk(undef,B,[?TK_NOT|L]);
% 	<<"Not">> ->
% 		tk(undef,B,[?TK_NOT|L]);
% 	<<"NOT">> ->
% 		tk(undef,B,[?TK_NOT|L]);
% 	<<"in">> ->
% 		tk(undef,B,[?TK_IN|L]);
% 	<<"In">> ->
% 		tk(undef,B,[?TK_IN|L]);
% 	<<"iN">> ->
% 		tk(undef,B,[?TK_IN|L]);
% 	<<"IN">> ->
% 		tk(undef,B,[?TK_IN|L]);
% 	<<"delete">> ->
% 		tk(undef,B,[?TK_DELETE|L]);
% 	<<"Delete">> ->
% 		tk(undef,B,[?TK_DELETE|L]);
% 	<<"DELETE">> ->
% 		tk(undef,B,[?TK_DELETE|L]);
% 	<<"update">> ->
% 		tk(undef,B,[?TK_UPDATE|L]);
% 	<<"Update">> ->
% 		tk(undef,B,[?TK_UPDATE|L]);
% 	<<"UPDATE">> ->
% 		tk(undef,B,[?TK_UPDATE|L]);
% 	<<"and">> ->
% 		tk(undef,B,[?TK_AND|L]);
% 	<<"And">> ->
% 		tk(undef,B,[?TK_AND|L]);
% 	<<"AND">> ->
% 		tk(undef,B,[?TK_AND|L]);
% 	<<"or">> ->
% 		tk(undef,B,[?TK_OR|L]);
% 	<<"Or">> ->
% 		tk(undef,B,[?TK_OR|L]);
% 	<<"oR">> ->
% 		tk(undef,B,[?TK_OR|L]);
% 	<<"OR">> ->
% 		tk(undef,B,[?TK_OR|L]);
% 	<<"like">> ->
% 		tk(undef,B,[?TK_LIKE_KW|L]);
% 	<<"Like">> ->
% 		tk(undef,B,[?TK_LIKE_KW|L]);
% 	<<"LIKE">> ->
% 		tk(undef,B,[?TK_LIKE_KW|L]);
% 	% <<C,_/binary>> when C >= $0, C =< $9 ->
% 	% 	tk([int,C],B,L);
% 	W ->
% 		tk(undef,B,tk(undef,W,[])++L)
% end).


% tk(undef,<<>>,L) ->
% 	L;
% tk(undef,<<" ",B/binary>>,L) ->
% 	tk(undef,B,L);
% tk(undef,<<"\t",B/binary>>,L) ->
% 	tk(undef,B,L);
% tk(undef,<<"\n",B/binary>>,L) ->
% 	tk(undef,B,L);
% tk(undef,<<"'",B/binary>>,L) ->
% 	case binary:split(B,<<"'">>) of
% 		[String,<<"'",Rem/binary>>] ->
% 			tk([str,[String,<<"''">>]],Rem,L);
% 		[String,Rem] ->
% 			tk(undef,Rem,[{?TK_STRING,String}|L])
% 	end;
% tk(undef,<<".",B/binary>>,L) ->
% 	tk(undef,B,[?TK_DOT|L]);
% tk(undef,<<"-",B/binary>>,L) ->
% 	tk(undef,B,[?TK_MINUS|L]);
% tk(undef,<<"(",B/binary>>,L) ->
% 	tk(undef,B,[?TK_LP|L]);
% tk(undef,<<")",B/binary>>,L) ->
% 	tk(undef,B,[?TK_RP|L]);
% tk(undef,<<";",B/binary>>,L) ->
% 	tk(undef,B,[?TK_SEMI|L]);
% tk(undef,<<"+",B/binary>>,L) ->
% 	tk(undef,B,[?TK_PLUS|L]);
% tk(undef,<<"*",B/binary>>,L) ->
% 	tk(undef,B,[?TK_STAR|L]);
% tk(undef,<<"/",B/binary>>,L) ->
% 	tk(undef,B,[?TK_SLASH|L]);
% tk(undef,<<"%",B/binary>>,L) ->
% 	tk(undef,B,[?TK_REM|L]);
% tk(undef,<<"==",B/binary>>,L) ->
% 	tk(undef,B,[?TK_EQ|L]);
% tk(undef,<<"=",B/binary>>,L) ->
% 	tk(undef,B,[?TK_EQ|L]);
% tk(undef,<<"<=",B/binary>>,L) ->
% 	tk(undef,B,[?TK_LE|L]);
% tk(undef,<<"<>",B/binary>>,L) ->
% 	tk(undef,B,[?TK_NE|L]);
% tk(undef,<<"<<",B/binary>>,L) ->
% 	tk(undef,B,[?TK_LSHIFT|L]);
% tk(undef,<<"<",B/binary>>,L) ->
% 	tk(undef,B,[?TK_LT|L]);
% tk(undef,<<">=",B/binary>>,L) ->
% 	tk(undef,B,[?TK_GE|L]);
% tk(undef,<<">>",B/binary>>,L) ->
% 	tk(undef,B,[?TK_RSHIFT|L]);
% tk(undef,<<">",B/binary>>,L) ->
% 	tk(undef,B,[?TK_GT|L]);
% tk(undef,<<"!=",B/binary>>,L) ->
% 	tk(undef,B,[?TK_NE|L]);
% tk(undef,<<"||",B/binary>>,L) ->
% 	tk(undef,B,[?TK_CONCAT|L]);
% tk(undef,<<"|",B/binary>>,L) ->
% 	tk(undef,B,[?TK_BITOR|L]);
% tk(undef,<<",",B/binary>>,L) ->
% 	tk(undef,B,[?TK_COMMA|L]);
% tk(undef,<<"&",B/binary>>,L) ->
% 	tk(undef,B,[?TK_BITAND|L]);
% tk(undef,<<"~~",B/binary>>,L) ->
% 	tk(undef,B,[?TK_BITNOT|L]);
% tk(undef,<<"?",B/binary>>,L) ->
% 	tk([var,[]],B,L);
% tk(undef,<<A:1/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:2/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:3/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:4/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:5/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:6/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:7/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:8/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:9/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:10/binary," ",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:1/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:2/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:3/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:4/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:5/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:6/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:7/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:8/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:9/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:10/binary,"\n",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:1/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:2/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:3/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:4/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:5/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:6/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:7/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:8/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:9/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk(undef,<<A:10/binary,"\t",B/binary>>,L) ->
% 	?CHK(A);
% tk([str,S],Bin,L) ->
% 	case binary:split(Bin,<<"'">>) of
% 		[String,<<"'",Rem/binary>>] ->
% 			tk([str,[S,String,<<"''">>]],Rem,L);
% 		[String,Rem] ->
% 			tk(undef,Rem,[{?TK_STRING,[S,String]}|L])
% 	end;
% tk([int,N],<<C,B/binary>>,L) when C >= $0, C =< $9 ->
% 	tk([int,[C|N]],B,L);
% tk([var,N],<<C,B/binary>>,L) when C >= $0, C =< $9 ->
% 	tk([var,[C|N]],B,L);
% tk([float,N],<<C,B/binary>>,L) when C >= $0, C =< $9 ->
% 	tk([float,[C|N]],B,L);
% tk([int,N],<<".",B/binary>>,L) ->
% 	tk([float,[$.|N]],B,L);
% tk([int,N],B,L) ->
% 	tk(undef,B,[{?TK_INTEGER,list_to_integer(lists:reverse(N))}|L]);
% tk([float,N],B,L) ->
% 	tk(undef,B,[{?TK_FLOAT,list_to_float(lists:reverse(N))}|L]);
% tk([var,N],B,L) ->
% 	tk(undef,B,[{?TK_VARIABLE,list_to_integer(lists:reverse(N))}|L]);
% tk(undef,<<C,B/binary>>,L) when C >= $0, C =< $9 ->
% 	tk([int, [C]],B,L);
% tk(undef,<<C,B/binary>>,L) when C >= $a, C =< $z ->
% 	tk([C],B,L);
% tk(undef,<<C,B/binary>>,L) when C >= $A, C =< $Z ->
% 	tk([C+32],B,L);
% tk(undef,<<"_",B/binary>>,L) ->
% 	tk([$_],B,L);
% tk([_|_]=W,<<C,B/binary>>,L) when C >= $a, C =< $z ->
% 	tk([C|W],B,L);
% tk([_|_]=W,<<C,B/binary>>,L) when C >= $A, C =< $Z ->
% 	tk([(C+32)|W],B,L);
% tk([_|_]=W,<<C,B/binary>>,L) when C >= $0, C =< $9 ->
% 	tk([C|W],B,L);
% tk([_|_]=W,<<"_",B/binary>>,L) ->
% 	tk([$_|W],B,L);
% tk([_|_]=W,B,L) ->
% 	case W of
% 		"sa" ->
% 			tk(undef,B,[?TK_AS|L]);
% 		"tceles" ->
% 			tk(undef,B,[?TK_SELECT|L]);
% 		"tresni" ->
% 			tk(undef,B,[?TK_INSERT|L]);
% 		"puorg" ->
% 			tk(undef,B,[?TK_GROUP|L]);
% 		"tesffo" ->
% 			tk(undef,B,[?TK_OFFSET|L]);
% 		"timil" ->
% 			tk(undef,B,[?TK_LIMIT|L]);
% 		"erehw" ->
% 			tk(undef,B,[?TK_WHERE|L]);
% 		"lla" ->
% 			tk(undef,B,[?TK_ALL|L]);
% 		"tcnitsid" ->
% 			tk(undef,B,[?TK_DISTINCT|L]);
% 		"llun" ->
% 			tk(undef,B,[?TK_NULL|L]);
% 		"morf" ->
% 			tk(undef,B,[?TK_FROM|L]);
% 		"ton" ->
% 			tk(undef,B,[?TK_NOT|L]);
% 		"ni" ->
% 			tk(undef,B,[?TK_IN|L]);
% 		"eteled" ->
% 			tk(undef,B,[?TK_DELETE|L]);
% 		"etadpu" ->
% 			tk(undef,B,[?TK_UPDATE|L]);
% 		"dna" ->
% 			tk(undef,B,[?TK_AND|L]);
% 		"ro" ->
% 			tk(undef,B,[?TK_OR|L]);
% 		"ekil" ->
% 			tk(undef,B,[?TK_LIKE_KW|L]);
% 		_ ->
% 			tk(undef,B,[{?TK_ID,?WORD(lists:reverse(W))}|L])
% 	end.

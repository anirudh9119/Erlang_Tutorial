-module(fact).
-export([calc/1]).
calc (N) when N==0 -> 1;
calc(N) when N >0 -> N*calc(N-1).


%% Tail recursion to compute factorial.

%tail_fac(N)-> tail_fac(N,ans).

%tail_fac(0,ans) -> ans;
%tail_fac(N,ans) -> tail_fac(N-1, N*ans).


%% Tail function to compute tail length .

tail_len(L) -> tail_len(L,0).

 
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,Acc+1).

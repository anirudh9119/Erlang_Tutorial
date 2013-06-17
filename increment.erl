-module(increment).
-export([increment/1,decrement/1,base/1,a/0,b/1,base/0]).

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].
 
decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

base(A) ->
 B = A + 1,
 G=fun() -> A * B end,
 G().

base() ->
A = 1,
  (fun(A) -> A = 7 end)(7).
 
a() ->
Secret = "pony",
       fun() -> Secret end.
       
b(G) ->
       "a/0's password is "++G().

%based(A) ->
% B = A + 1,
% F = fun() -> C = A * B end,
% F(),
% C.

-module(f019_continuations).
-compile(export_all).

%Simple recursive factorial function
factorial1(0) -> 1;
factorial1(N) -> N * factorial1(N-1).


%The problem with above function is that recursive
%call has context around it and context grows as one
%goes deeper and deepr into recursion.

%There is also lack of control as we cannot jump out of
%the recursive structure and must wait for all recursive
%calls to finish

%The same can be written in Continuation Passing Style (CPS) as
factorial2_k(0, Cont1) -> Cont1(1);
factorial2_k(N, Cont1) -> factorial2_k(N-1, fun(X) -> Cont1(X * N) end).

factorial2(N) -> factorial2_k(N, fun(X) -> X end).


%Steps to convert any recursive function into tail recursive function
%in Continuation Passing Style(CPS):
% 1. Add one more variable Cont1, K etc. for continuation
% 2. Ensure that function never returns. We should pass return value when calculation is
%    complete to current continuation with which function was called.
% 3. For any continuation function like factorial2_k(A, B) where B is
%    continuation, at end B will get called with factorial2(A) as argument. Now
%    it is upto B what it wants to do with that value. It can simply return it,
%    double it or even discard it.


%Again a recursive function with context around recursive call
product1([]) -> 1;
product1([H1 | T1]) -> H1 * product1(T1).


%Product in CPS style
product2_k([], Cont1) -> Cont1(1);
product2_k([H1 | T1], Cont1) -> product2_k(T1, fun(X) -> Cont1(X * H1) end).

product2(L1) -> product2_k(L1, fun(X) -> X end).
%Function 'fun(X) -> X end' is also referred as top continuation


%Advantage of CPS style is that we can optimize and return quickly and directly
%without completing recursion. Hence the control on when, how and what to return
%is completely with program.



product3_k([], Cont1) -> Cont1(1);
product3_k([H1 | T1], Cont1) -> 
	if
		H1 =:= 0 -> 0;
		true -> product2_k(T1, fun(X) -> Cont1(X * H1) end)
	end.
product3(L1) -> product3_k(L1, fun(X) -> X end).

%Now because of CPS product3 returns immediately to caller
%with 0 if any element in list is 0. It does not processes
%rest of the list, ie, [6, 3, 2] also it does not returns
%0 to itself recursively all the way to main caller. 
%It directly returns 0 to main caller (sample_run() in our
%case) which is very efficient.

sample_run() -> 
	L1=[1, 3, 7, 9],
	io:format("factorial1 of 5 is ~p~n", [factorial1(5)]),
	io:format("factorial2 of 5 is ~p~n", [factorial2(5)]),
	io:format("L1 is ~p~n", [L1]),
	io:format("product1 of L1 is ~p~n", [product1(L1)]),
	io:format("product2 of L1 is ~p~n", [product2(L1)]),
	io:format("product3 of L1 is ~p~n", [product3(L1)]),
	L2=[1, 4, 5, 0, 6, 3, 2],
	io:format("L2 is ~p~n", [L2]),
	io:format("product2 of L2 is ~p~n", [product2(L2)]),
	io:format("product3 of L2 is ~p~n", [product3(L2)]),
	ok.


%Converting recursive function to CPS based on fun(X) (lambda)
%is easy but we do not have option of returning to few levels
%or skipping last few recursive calls but not all etc. If 
%continuation is represented in data-structure format then
%we get to return to any point in call structure which is
%really powerful. 


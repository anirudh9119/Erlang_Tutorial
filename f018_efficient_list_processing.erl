-module(f018_efficient_list_processing).
-compile(export_all).

% double1 takes list as parameter and doubles each element in given list
double1([]) -> [];
% Below recursive call creates context, but list operations of prepending head
% or splitting head, tail are very efficient.
double1([H1 | T1]) -> [H1 * 2 | double1(T1)].


double2([]) -> [];
%First ++ is very inefficient so should be avoided unless lists are very small.
%Secondly double2 produces list in reverse order. We would have to reverse it
%   with lists:reverse to get return list also in same order.
%double2 also creates context around recursive call. So this is a very bad
%way of doubling list
double2([H1 | T1]) -> double2(T1) ++ [H1 * 2].


%double3 uses second parameter to accumulate doubled list
%This way we do not accumulate context around recursive call.
%Second parameter can be called accumulator or even continuation.
double3([], L1) -> L1;
%Note that this will also result in return list in reverse order.
double3([H1 | T1], L1) -> double3(T1, [H1 *2 | L1]). 


%Now list would be in proper order, but ++ operation is very
%expensive. It is better to call double3 and use lists:reverse
%then using double4
double4([], L1) -> L1;
double4([H1 | T1], L1) -> double4(T1, L1 ++ [2 * H1]). 


%Hence overall the best way is to use double3 and then use lists:reverse
%which is internally optimized. 

sample_run() -> 
	Test1=lists:seq(1,10),
	io:format("double1 of ~p is ~p~n", [Test1, double1(Test1)]),
	io:format("double2 of ~p is ~p~n", [Test1, double2(Test1)]),
	io:format("double3 of ~p is ~p~n", [Test1, double3(Test1, [])]),
	io:format("double4 of ~p is ~p~n", [Test1, double4(Test1, [])]),
	ok.

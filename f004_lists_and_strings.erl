-module(f004_lists_and_strings).
-export([sample_run/0]).

sample_run() -> 
		%We can declare list by enclosing various elements in []
	X1=[1,2,3,4],
	io:format("1. Value of X1 is ~p~n", [X1]),

		%We can extract head (first element) and tail (rest elements) of
		%list using | and [] together in pattern matching.
		%Note: that Head is an element and Tail is a list
		%    Head can also be a list in case of nested list with first element
		%    also as list.
	[Head | Tail] = X1,
	io:format("2. Head and Tail of X1 are ~p and ~p respectively~n", [Head, Tail]),


		%We can also pattern match list with [,,] if we know exact number of elements
	[A1, A2, A3, A4]=X1,
	io:format("3. Value of A1, A2, A3 and A4 are ~p, ~p, ~p and ~p respectively~n", [A1, A2, A3, A4]),

		%We can match first few elements and store / ignore rest of them
	[B1, B2 | B3 ] =X1,
	io:format("4. Value of B1, B2 and B3 are ~p, ~p and ~p respectively~n", [B1, B2, B3]),

		%Strings are enclosed in double quotes
	X5="Saurabh Barjatiya",
	io:format("5. Value of X5 is ~p~n", [X5]),

		%Strings are nothing but list of ASCII values of printable characters
	X6=[$h, $e, $l, $l, $o],
	io:format("6. Value of X6 is ~p~n", [X6]),

		%If any non-printable character is present in list then values are
		%printed as integers
	X7=[1|X5],
	io:format("7. Value of X7 is ~p~n", [X7]),

		%We can use ++ to append lists and -- to subtract lists. Both of these
		%operations are expensive and should be avoided, especially on long lists.
	X8=[1, 2, 3] ++ [4, 5, 6],
	io:format("8. Value of X8 is ~p~n", [X8]),

		%L1 -- L2,  subtracts only K occurences of X from L1 if X occurs only K
		%times in L2. These subtractions are done from start of list.
	X9=[1, 2, 3] -- [a, b, c],
	X10=[1, 2, a, 3, 4, a, 5, 6, a] -- [a, p],
	X11=[1, 2, a, 3, 4, a, 5, 6, a] -- [a, a],
	X12=[1, 2, a, 3, 4, a, 5, 6, a] -- [a, a, a],
	X13=[1, 2, a, 3, 4, a, 5, 6, a] -- [a, a, a, a],
	io:format("9. Values of X9 to X13 are ~p,~n \t~p, ~p, ~p and ~p~n", [X9, X10, X11, X12, X13]),
	ok.

%Note we can use f(). when using erl shell to make shell forget all bindings.
%Similarly q(). or init:stop(). can be used to quit erlang shell.


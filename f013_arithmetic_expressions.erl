-module(f013_arithmetic_expressions).
-compile(export_all).

sample_run() ->
	%All arithmetic expressions are used below once for demonstration
	io:format("+1 = ~p~n", [+1]),
	io:format("-1 = ~p~n", [-1]),
	io:format("2*3 = ~p~n", [2*3]),
	%Floating point divison
	io:format("2/3 = ~p~n", [2/3]),
	%Bitwise not
	io:format("bnot 1 = ~p~n", [bnot 1]), 
	%Integer divison
	io:format("5 div 3 = ~p~n", [5 div 3]),
	%Integer divison remainder
	io:format("5 rem 3 = ~p~n", [5 rem 3]),
	%Bitwise and
	io:format("5 band 3 = ~p~n", [5 band 3]),
	io:format("5 + 3 = ~p~n", [5 + 3]),
	io:format("5 - 3 = ~p~n", [5 - 3]),
	%Bitwise or
	io:format("5 bor 3 = ~p~n", [5 bor 3]),
	%Bitwise xor
	io:format("5 bxor 3 = ~p~n", [5 bxor 3]),
    %Arithmetic bit-shift left 
	io:format("1 bsl 3 = ~p~n", [1 bsl 3]),
	%Arithmetic bit-shift right
	io:format("16 bsr 3 = ~p~n", [16 bsr 3]),
	ok.

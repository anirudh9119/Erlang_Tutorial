-module(f001_basic_math).
-export([basic_math/0]).

%Note: 
%1. The name of module must match the name of file
	%2. We need to export all the functions that we intend to call from outside
%   functions are listed in name/arity format (arity is number of arguments that function expects)
	%3. If we are practicing then we can export all functions using
	%      -compile(export_all).



	%This function performs basic arithmetic and prints results
basic_math() ->

	%io:format is used to print values. ~p gets replaced with value from list supplied
	%as second argument. ~n is for newline. ~p is for pretty print. One can use
	%     erl -man io
	%to get man page of io module and lookup help for format module to understand 
	%options other than ~p and ~n
	io:format("1. Simple addition - ~p~n", [2+3]),

	%Integer arithmetic in erlang is exact so one never has to worry about overflow
	io:format("2. Multiplication of very big integers - ~p~n", [2234234234 * 234234234234234]),

	%Integers can be written in any base from 2 to 36. The format of writing integers
	%with base other than 10 is 'base#number'
	io:format("3. Integers in other base are - ~p and ~p~n", [16#cafe, 32#sugar]),

	%We can get ASCII value of characters as integers by preceeding character
	%with $ sign
	io:format("4. ASCII value of a and A are ~p and ~p respectively~n", [$a, $A]),

	%Integer division
	io:format("5. Integer division 5/3=~p~n", [5/3]),

	%Float division
	io:format("6. Float division 5.0/3.0=~p~n", [5.0/3.0]),

	%Remainder of divison
	io:format("7. Remainder of divison 5/3=~p~n", [5 rem 3]),

	%Quotient of divison
	io:format("8. Quotient of divison 5/3=~p~n", [5 div 3]),

	%Value of last expression is returned as value of function call. Last statement
	%of function ends with . and not with ,   dot(.) helps in indicating end of function
	%definition.
	ok.



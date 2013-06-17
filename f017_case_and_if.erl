-module(f017_case_and_if).
-compile(export_all).

%We can use case statement to match return value of expression to
%set of patterns. We can use if to check for set of conditions. At 
%least one case or one if statement must match else error is raised.
%Hence very often a catch all condition is kept at end of case / if
%statements. For case catch all can be simple unbound variable name
%which will match will all values. For if catch call can be atom true.

%Simply returns atom yellow.
return_yellow() -> yellow.

sample_run() ->
	case return_yellow() of
		blue -> 
			io:format("Blue was returned~n", []),
			%Multiple statements separated by , can be part
			%of same pattern clause.
			ok;
		%Note that separate pattern clauses are separated by 
		%semicolon.
		yellow ->
			io:format("Yellow was returned. ", []),
			io:format("As expected~n", []),
			ok;
		%This is catch all clause which will always match as
		%Color1 is unbound. In this clause Color1 will get bound
		%to value of Expression.
		Color1 ->
			io:format("Color ~p was returned.", [Color1]),
			error
	end,

	%Is used by function Is_prime to check for primes
	%Argument Func1 helps in achieveing recursion in 
	%anonymous function. Many guard conditions are
	%specified to indicate that we can have very complex
	%Guard conditions
	Is_prime1 = fun(Func1, Num1, Start, End) 
				when 
					is_integer(Num1), 
					is_integer(Start), 
					is_integer(End), 
					Start =< End,
					Start < Num1, 
					End<Num1,
					Start>=2 
				-> 	if 
						%If number got divided then it is not prime
						(Num1 rem Start) =:= 0 -> false;
						%If we have checked all numbers then number is prime
						Start =:= End -> true;
						%If number did not get divided and more numbers are left
						%Then continue checking from next number
						true -> Func1(Func1, Num1, Start+1, End)
					end
			 	end,

	%This functions uses Is_prime1 by calling it with appropriate values.
	Is_prime = fun(Num1) -> Is_prime1(Is_prime1, Num1, 2, Num1-1) end,

	io:format("Checking if 11 is prime - ~p~n", [Is_prime(11)]),
	io:format("Checking if 97 is prime - ~p~n", [Is_prime(97)]),
	io:format("Checking if 98 is prime - ~p~n", [Is_prime(98)]),
	ok.	
	

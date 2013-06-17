-module(f014_guards).
-compile(export_all).

%Following function clause body will be used only
%when guard A>B is satisfied. We can use guards in
%this manner to have if condition for function.
maximum(A, B) when A>B -> A;
maximum(_A, B) -> B.

sample_run() ->
	%Testing maximum function written above to verify
	%whether guard A>B is working properly or not
	io:format("1. Maximum among 3 and 7 is ~p~n", [maximum(3,7)]),

	% When using guards comma (,) can be used for AND logic
	% and (;) can be used for OR logic. 
	% Set of conditions seperated by commas(,) is called guard or
	% guard expression. Several guard expressions can be ORed by
	% seperating them with semi-colon(;)

	%Rules and possible values for guard evaluation are:
	% 1. Atom true -> Evaluates to true
	% 2. All other atoms -> Evaluate to false
	% 3. Arithmetic expressions
	% 4. Comparisons
	% 5. Boolean expressions 
	% 6. Guard predicates / BIFs that start with is_ can be called in guard
	%  Guard predicates that start with is_ are listed in online documentation
	%  at http://www.erlang.org/doc/reference_manual/expressions.html#id79005
	%  The same page also lists BIFs like abs(), node(), length() etc. which
	%  are allowed in guard sequences.


	% Function that takes an argument and tells whether given
	% Argument is prime or not. Here we have used guards to
	% Ensure that function body is evaluated only when argument
	% is integer and >= 2
	Is_prime = fun(Num1) when is_integer(Num1), Num1>=2 -> 
		%We OR all the booleans obtained after divison. So that
		%Even if one divison ends with remainder 0 then overall
		%value of this fold would be true.
		%  Since successful divison gives true, we negate it with
		%  not to indicate not prime. Also insuccessful divison 
		%  would return false which also get negated to indicate
		%  that number is prime.
		not lists:foldl(
			fun(A, B) -> A or B end, 
			false, 
				%Below call to map will generate list of booleans
				%where each boolean will represent whether Num1 was
				%divisible by number between 2 and Num1-1 or not.
			lists:map
			(
				fun(Divisor) -> Num1 rem Divisor =:= 0 end,
				lists:seq(2, Num1-1)
			)
		);
		(_) -> false end,
	
	io:format("2. Checking whether 11 is prime ~p~n", [Is_prime(11)]),		
	io:format("3. Checking whether 12 is prime ~p~n", [Is_prime(12)]),		
	io:format("4. Checking whether 13 is prime ~p~n", [Is_prime(13)]),		
	io:format("5. Checking whether 97 is prime ~p~n", [Is_prime(97)]),		
	io:format("6. Checking whether 98 is prime ~p~n", [Is_prime(98)]),		

	%Verifying that guards is_integer(Num1) and Num1>=2 are working
	io:format("7. Checking whether -1 is prime ~p~n", [Is_prime(-1)]),		
	io:format("8. Checking whether 3.4 is prime ~p~n", [Is_prime(3.2)]),		
	io:format("9. Checking whether atom a is prime ~p~n", [Is_prime(a)]),		



	ok.


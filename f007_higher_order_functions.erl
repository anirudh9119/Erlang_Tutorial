-module(f007_higher_order_functions).
-compile(export_all).

double(X) -> 2*X.

sample_run() ->
	L1=lists:seq(1, 5),

	%When we pass named functions we need to use fun
	%keyword and also provide arity of the function
	L2=lists:map(fun double/1, L1),

	Triple=fun(X) -> 3*X end,

	%We can pass anonymous function stored in variable
	%Triple to lists:map as argument as it is without
	%requiring fun keyword or specifying arity
	L3=lists:map(Triple, L1),

	io:format("L1, L2 and L3 are ~p, ~p and ~p~n", [L1, L2, L3]),
	
	% :=: is the equality operator which returns 'true' when
	% both operands are equal and 'false' otherwise. Note that
	% 'true' and 'false' atoms have special meaning as boolean
	% true or false
	Even=fun(Num1) -> ((Num1 rem 2) =:= 0) end,

	% We can use lists:filter and lists:map together to achieve
	% complex results
	[L1_even, L2_even, L3_even] = lists:map(fun (List1) -> lists:filter(Even, List1) end, [L1, L2, L3]),
	io:format("L1_even, L2_even and L3_even are ~p, ~p and ~p~n", [L1_even, L2_even, L3_even]), 

	% We can return functions as return value from functions
	Times = fun(Num1) -> fun (Num2) -> Num1 * Num2 end end,

	% Now we can call Times which will return functions
	Double2 = Times(2),
	Triple2 = Times(3),

	%Now we can use the functions returned by Times in expressions
	L1_double = lists:map(Double2, L1),
	L1_triple = lists:map(Triple2, L1),
	io:format("L1_double and L1_triple are ~p and ~p~n", [L1_double, L1_triple]),
	
	ok.

	

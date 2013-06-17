-module(f006_anonymous_functions).
-compile(export_all).

cost(mouse) -> 300;
cost(laptop) -> 25000;
cost(keyboard) -> 600;
cost(harddisk) -> 4000;
cost(ram) -> 4000;
cost(cpu) -> 20000;
cost([]) -> 0;
cost([H | T]) -> cost(H) + cost(T);
cost({Item, Count}) -> cost(Item) * Count.

sample_run() ->
	io:format("Cost of mouse is ~p~n", [cost(mouse)]),
	io:format("Cost of 10 mouse {mouse, 10} is ~p~n", [cost({mouse, 10})]),
	io:format("Cost of one mouse and one keyboard [mouse, keyboard] is ~p~n", 
			[cost([mouse, keyboard])]),
	io:format("Cost of 10 mouse, 10 keyboard [{mouse, 10}, {keyboard, 10}] is ~p~n",
			[cost([{mouse, 10}, {keyboard, 10}])]),

	%We can declare anonymouse functions and assign them to variables
	%Then we can call anonymous function using variable just like we call
	%normal function
	Cost1=fun(Item) -> cost(Item) end,
	io:format("Cost of keyboard is ~p~n", [Cost1(keyboard)]),


	%Anonymous functions also support pattern matching. Only recursion
	%of anonymous functions is tricky
	Cost2=fun(mouse) -> 300;
			(keyboard) -> 600;
			([]) -> 0 end,
	io:format("Cost of mouse is ~p~n", [Cost2(mouse)]), 

	%For anonymous recursive function we need to keep one
	%extra parameter where we would supply the function
	%itself as parameter. Thus we create a parent function
	%which accepts one extra parameter, the function itself
	%and we use that parameter to make recursive calls.
	Fact_parent=fun(_, 0) -> 1; 
				   (Func, N) -> N * Func(Func, N-1)
				end,
	
	%Then we define the actual function by passing all 
	%arguments to parent function as it is after 
	%prepending the parent function itself as first argument.
	Fact=fun(N) -> Fact_parent(Fact_parent, N) end,
	io:format("Factorial 5 is ~p~n", [Fact(5)]),
	ok.



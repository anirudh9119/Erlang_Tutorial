-module(f009_list_comprehensions).
-compile(export_all).

sample_run() ->
	%Getting list of numbers from 1 to 5, ie, [1, 2, 3, 4, 5]
	L1 = lists:seq(1, 5),

	%Doubling every element of L1 to create L2
	L2 = lists:map(fun(X) -> 2*X end, L1),

	%Doubling every element of L1 to create L3 using list comprehension
	%We can read below expressios as - "For every X in L1 return list
	%comprising of 2 * X"
	L3 = [ 2 * X || X <- L1],
	io:format("1. L1, L2 and L3 are ~p, ~p and ~p~n", [L1, L2, L3]),



	%Creating list of fruits
	Fruits1 = [{apple, 3}, {orange, 4}, {banana, 1}, {grapes, 23}],
	
	%Doubling number of each fruit using list comprehension
	%Note that now we are extracting tuples from list and generating
	%List with tuples
	Fruits2 = [{Fruit, Quantity*2} || {Fruit, Quantity} <- Fruits1],
	io:format("2. Fruits1 and Fruits2 are ~p~n    and ~p~n", [Fruits1, Fruits2]),

	
	%We can add filter statements to list compresions
	Fruits3 = [{Fruit, Quantity} || {Fruit, Quantity} <- Fruits1, Quantity < 5],
	io:format("3. Fruits with less than 5 quantity are ~p~n", [Fruits3]),

	%Here,
	%  {Fruit, Quantity} is called constructor
	%  {Fruit, Quantity} <- Fruits1 is called generator
	%  Quantity < 5 is called filter.
	%It is important to understand that generators can also act as filters
    %as shown below

	First_a = [{a, B} || {a, B} <- [{a, 1}, {aa, 1}, {a, 3}, {b, 5}, "hello", {a, b, c}]],
	io:format("4. First_a list is ~p~n", [First_a]),
	
	ok.



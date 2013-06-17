-module(f003_atoms_and_tuples).
-export([atoms_and_tuples/0]).

atoms_and_tuples() ->
		%Any string starting with lower case letter is treated as atom
	X1=hello,
	io:format("1. Value of X1 is ~p~n", [X1]),

		%We can enclose atom in single quotes('') if we want to start it
		%with capital letters
	X2='Hello',
	io:format("2. Value of X2 is ~p~n", [X2]),

		%Atoms can contains numbers, @ and _ characters
	X3=saurabh_barjatiya@iiit_ac_in,
	io:format("3. Value of X3 is ~p~n", [X3]),

		%Tuples are created with {} and can contain multiple values 
		%Separated by comma
	X4={a, b, 1, 2},
	io:format("4. Value of X4 is ~p~n", [X4]),

		%To extract values from tuples we can use pattern matching
	{A1, A2, A3, A4}=X4,
	io:format("5. Values of A1, A2, A3 and A4 are ~p, ~p, ~p and ~p~n", [A1, A2, A3, A4]),

		%If we are not interested in particular value then we can
		%ignore it by matching it with wildcard character _
	{_, B1, _, B2}=X4,
	io:format("6. Values of B1 and B2 are ~p and ~p~n", [B1, B2]),

		%Recommended use of tuples is to value tuple type and values tagged as
		%  {point, {x, 2}, {y, 4}}
		%Here, point is type of tuple and value of x coordinate of point is 2 and
		%value of y coordinate of point is 4. Above representation should be preferred
		%over 
		%   {2, 4}
		%   {point, 2, 4}
		%whenever possible
	First_name={first_name, 'Saurabh'},
	Last_name={last_name, 'Barjatiya'},
	Full_name={full_name, First_name, Last_name},
	io:format("7. Value of Full_name is ~p~n", [Full_name]),
	ok.



-module(f005_basic_functions).
-export([area/1]).


%% We can have multiple definitions of function, each with
%% different argument types / number of argument. For example
%% we can specify that function expects tuple as argument
%% where first value in tuple must be atom circle. This way
%% the function can be called only with given type of tuples
%% for it to work, and in all other cases it will generate
%% error mentioning no function clause matched


area({circle, Radius}) -> math:pi() * Radius * Radius;  %Different clauses are separated by
														%Semi-colon
area({square, Side}) -> Side * Side;

%%The order of clauses matters when clauses are not mutually exclusive.
%%For example below clause if put at top would match both circle and square

area({A, B}) ->
	A=rectangle,  %Ensure that we got called with rectangle
				  %This line will generate error if A is not
				  %atom rectangle and may terminate the program
				  %if error handling is not used
	{Width, Height}=B,  %Extract width and height  
						%This function should be called as 
						%  f05_basic_functions:area({rectangle, {1, 2}}
	Width * Height.
					




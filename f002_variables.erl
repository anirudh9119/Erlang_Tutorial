-module(f002_variables).
-export([variables/0]).

variables() ->
%1. We assign values to variables using pattern matching operator =
%   Note that = is not assignment operator, it is used for pattern matching
%2. Once value is assigned (bound) to variable then it cannot be changed later. 
%3. We do not declare type of variable. Variables type is type of value bound to it.
%4. All variable names MUST start with captial letter

%Simple variable assignment (Pattern matching)
	X1=1,
	io:format("1. Value of X1 is ~p~n", [X1]),

	%Unbound variables must be on left hand side of pattern matching expression
	%If we write X1=X2 below then we would get error
	X2=X1,
	io:format("2. Value of X2 is ~p~n", [X2]),

	%We can have bound variables on left side of pattern matching expression
	%but if patterns do not match then it would give error
	X2=X1,	%Not a problem as patterns X2 and X1 match
	X1=X2,  %Same as before
	%X1=3	%Would give error as X1 is already bound to 1 and we cannot reassign values.

	X3=X1/4,
	io:format("3. Value of X3 is ~p~n",[X3]),

	%If we are not going to use variable anywhere then erlang will generate warning during
	%compilation. To avoid such warnings we can precede name of variable with _. _ is wildcard
	%and hence value will be lost. This is useful to indicate intention / type of value that
	%can be obtained from a expression, if required, without actually storing it in a variable.
	_X4=X3,
	ok.


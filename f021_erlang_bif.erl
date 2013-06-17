-module(f021_erlang_bif).
-compile(export_all).

%BIF or built-in-functions are functions provided by erlang which otherwise
%would be impossible to program. Example for BIFs are functions like
%   tuple_to_list/1 -> Converts tuple to list
%    time/0 -> Returns current time (Only time not date)
%To get full list of BIFs use 'erl -man erlang' or visit erlang HTML
%documentation on BIFs.
%
% BIFS which are allowed in Guard tests are marked (Guard)
% to indicate so

%Few interesting BIFs provided by erlang are used here
sample_run() ->
	% abs -> Absolute value (Guard)
	io:format("abs(-4) is ~p~n", [abs(-4)]),
	io:format("abs(-4.4) is ~p~n", [abs(-4.4)]),

	% erlang:append_element() -> Appends term to tuple
	io:format("erlang:append_element({one, two}, three) is ~p~n", 
		[erlang:append_element({one, two}, three)]),

	% atom_to_binary() -> Convert atom to binary
	io:format("atom_to_binary(hello_world, latin1) is ~p~n",
		[atom_to_binary(hello_world, latin1)]),

	% atom_to_list() -> Convert atom to list
	io:format("atom_to_list(hello_world) is ~p~n", 
		[atom_to_list(hello_world)]),

	% binary_part() -> Extract part of binary (Guard)
	io:format("binary_part(<<saurabh>>, 1, 3) is ~p~n",
		[binary_part(<<"saurabh">>, 1, 3)]),

	% binary_to_list() -> List of integers corresponding to bytes in binary
	io:format("binary_to_list(<<abcdABCD>>) is ~p~n",
		[binary_to_list(<<"abcdABCD">>)]),
	
	%Similarly one can study about more BIFs and use them. The list of
	%BIFs is very large and it is impractical to try out / list even
	%interesting ones as example here.

	ok.

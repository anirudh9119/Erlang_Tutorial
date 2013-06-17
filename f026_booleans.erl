-module(f026_booleans).
-compile(export_all).
-author("Saurabh Barjatiya").
-vsn(1.0).
-description("This is sample program to demonstrate use of booleans "
		"in erlang").

truth_table(Fun1) ->
	io:format("\t false   false -> ~p~n", [Fun1(false, false)]),
	io:format("\t false   true  -> ~p~n", [Fun1(false, true)]),
	io:format("\t true    false -> ~p~n", [Fun1(true, false)]),
	io:format("\t true    true  -> ~p~n", [Fun1(true, true)]),
	ok.

call_and(A, B) -> A and B.

call_or(A, B) -> A or B.

call_xor(A, B) -> A xor B.

sample_run() ->
	io:format("1. Truth table of and is ~n", []),
	truth_table(fun call_and/2),
	io:format("2. Truth table of or is ~n", []),
	truth_table(fun call_or/2),
	io:format("3. Truth table of xor is ~n", []),
	truth_table(fun call_xor/2),
	io:format("4. not true is ~p~n", [not true]),

	%In short circuit boolean expressions rest of the terms do not
	%get evaluated if result of entire expression can be determined by
	%value of current terms

	%We can use begin-end whereever we want multiple statements in place
	%of one statement. Value of entire begin-end block is same as value
	%of last statement
	false orelse 
		begin
			io:format("5. This will never get printed~n", []),
			true
		end,
	false andalso 
		begin
			io:format("6. This wont get printed either~n", []),	
			true
		end,
	true or 
		begin
			io:format("7. This will get printed~n", []),
			false
		end,
	false and 
		begin
			io:format("8. So will this~n", []),	
			true
		end,
	ok.

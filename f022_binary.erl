-module(f022_binary).
-compile(export_all).

sample_run() ->
	Bin1 = <<1, 2, 3>>,
	Bin2 = <<4, 5>>,
	Bin3 = <<6>>,
	Bin4=list_to_binary([0, Bin1, 3, Bin2, Bin3, 7]),
	io:format("1. Bin4 is ~p~n", [Bin4]),
	io:format("2. split_binary(Bin4, 4) is ~p~n", [split_binary(Bin4, 4)]),

	%Binaries are extremely useful in storing any kind of value
	%in files or transmitting them over network. We can use 
	%term_to_binary to get binary value of any term. Later on
	%we can use binary_to_term to get old term back.
	Bin5=term_to_binary({a, [1, 2], 'Hello', [{3}]}),
	io:format("3. Bin5 = term_to_binary({a, [1, 2], 'Hello', [{3}]}) is ~p~n",
		[Bin5]),
	io:format("4. binary_to_term(Bin5) is ~p~n", [binary_to_term(Bin5)]),

	io:format("size(Bin5) is ~p~n", [size(Bin5)]),

	ok.

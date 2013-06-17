-module(dolphins).
-compile(export_all).

%learn about (pid ! msg ) pid is the PID of the process to which msg is sent .

dolphin1() ->
receive
	do_a_flip ->
		io:format("How about no?~n");
	fish ->
		io:format("So long and thanks for all the fish!~n");
	_ ->
		io:format("Heh, we're smarter than you humans.~n")
end.

dolphin2() ->
receive
	{From, do_a_flip} ->
		From ! "How about no?";
	{From, fish} ->
		From ! "So long and thanks for all the fish!";
	_ ->
		io:format("Heh, we're smarter than you humans.~n")
end.

dolphin3() ->
receive
	{From, do_a_flip} ->
		From ! "How about no?",
     		dolphin3();
	{From, fish} ->
		From ! "So long and thanks for all the fish!";
	_ ->
		io:format("Heh, we're smarter than you humans.~n"),
		dolphin3()
end.

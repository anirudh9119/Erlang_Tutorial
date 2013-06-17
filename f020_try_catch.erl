-module(f020_try_catch).
-compile(export_all).

%Following function generates different types of exceptions
%based on supplied value of N
%  N=1 -> Returns normally atom n1
%  N=2 -> Returns normally tuple {'EXIT', n2}
%  N=3 -> Exits using exit(n3)
%  N=4 -> Throws error using throw(n4).
%  N=5 -> Generates serious exception using erlang:error(n5)
%
% 1. Note that exit() is used to kill current process and send
%    {'EXIT', Pid} message to all linked processes.
% 2. Throw is used to indicate error that calling function can
%    handle
% 3. erlang:error() is used to indicate internal error which 
%    caller is not supposed to handle.
%
% We can distinguish between the above three types of errors in
% try catch and choose to catch errors of only particular type.

generate_exception(N) ->
	case N of
		1 -> n1;
		2 -> {'EXIT', n2};
		3 -> exit(n3);
		4 -> throw(n4);
		5 -> erlang:error(n5)
	end.


% Below function calls generate_exception function using supplied
% value of N. It tries to catch errors thrown using try catch.
% It also returns return_value if generate_exception() returns
% normally
call_generate_exception(N) ->
	try generate_exception(N) of
		Return_value -> {N, normal, Return_value}
	catch
		throw : ThrowReason -> {N, throw_error, ThrowReason};
		exit : ExitReason -> {N, exit_error, ExitReason};
		error : ErrorReason -> {N, erlang_error, ErrorReason}
	end.



%In this we call call_generate_exception/1 with different
%values of N in range 1, 5 and print returned value in each
%case.
sample_run() -> 
	List_of_N = lists:seq(1, 5),
	Return_values = lists:map(fun call_generate_exception/1, List_of_N),
	io:format("Return values are ~p~n", [Return_values]),
	ok.
	
	

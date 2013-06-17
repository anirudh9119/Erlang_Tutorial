-module(f039_keep_alive).
-compile(export_all).
-vsn(1.0).
-author("Saurabh Barjatiya").
-description("This module is used to define a keep_alive function that can ensure that an atom is "
			" always registered with spawned Pid and in case spawned Pid dies then another process is "
			" created and registered with same name. Thus ensuring that given service is always "
			" available at particular name.").

%This function executes Function Fun with reason Why when process with given
%Pid dies
on_exit(Pid, Fun) ->
	spawn(
		fun() ->
			process_flag(trap_exit, true),
			link(Pid),
			receive
				{'EXIT', Pid, Why} -> Fun (Why)
			end
		end
	).



%This function can be called with any atom as Name and function with zero arguments as
%Fun so that one spawned copy of Fun is always registered with atom Name.
keep_alive(Name, Fun) ->
	register(Name, Pid = spawn(Fun)),
	on_exit(Pid, fun(Why) -> 
					io:format("~p with pid ~p died due to ~p. Respawning it ~n", [Name, Pid, Why]),
					keep_alive(Name, Fun) 
				 end).


%This function will be used to test keep_alive. We can spawn below
%function with atom divide and try to divide various numbers even
%sometimes with denominator 0, but still things should work.	
test_keep_alive() ->
	receive
		{Sender, {divide, Numerator, Denominator}} ->
%			io:format("Received request to divide ~p by ~p from ~p~n",
%					[Numerator, Denominator, Sender]),
			Sender ! (Numerator / Denominator),
%			io:format("Sent message to ~p successfully~n", [Sender]),
			test_keep_alive()
	end.


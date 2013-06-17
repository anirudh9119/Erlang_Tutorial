-module(linkmon).
-compile(export_all).
myproc() ->
	timer:sleep(5000),
		exit(reason).



start_critic() ->
	spawn(?MODULE, critic, []).
 
judge(Pid, Band, Album) ->
	Pid ! {self(), {Band, Album}},
    	receive
		{Pid, Criticism} -> Criticism
	after 2000 ->
		timeout
	end.
 
%% To keep the critic alive, we'll write a basic 'supervisor' process whose only role is to restart it when it goes down:

start_critic2() ->
	spawn(?MODULE, restarter, []).


%% Here the change is made to crtitic2.	
restarter() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, critic2, []),
	register(critic, Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
					ok;
		{'EXIT', Pid, shutdown} -> % manual termination, not a crash
					ok;
		{'EXIT', Pid, _} ->
				restarter()
	end.
%% Here, the restarter will be its own process. It will in turn start the critic's process and if it ever dies of abnormal cause, restarter/0 will loop and create a new critic.

judge2(Band, Album) ->
	critic ! {self(), {Band, Album}},
       	Pid = whereis(critic),
        receive
		{Pid, Criticism} -> Criticism
	after 2000 ->
		timeout
end.

%%  the line Pid = whereis(critic) is used to find the critic's process identifier in order to pattern match against it in the receive expression.
%% in the above judge2 and restarter function critic variable is shared variable so race consition can happen here so we will be re writin these functions again.

%% Here judge3 N critic2 there would be no race funciton.
judge3(Band, Album) ->
	Ref = make_ref(),
        critic ! {self(), Ref, {Band, Album}},
        receive
               {Ref, Criticism} -> Criticism
        after 2000 ->
            	timeout
	end.

	
critic2() ->
	receive
		{From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
			From ! {Ref, "They are great!"};
		{From, Ref, {"System of a Downtime", "Memoize"}} ->
			From ! {Ref, "They're not Johnny Crash but they're good."};
		{From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
			From ! {Ref, "Simply incredible."};
		{From, Ref, {_Band, _Album}} ->
			From ! {Ref, "They are terrible!"}
	end,
	critic2().


%critic() ->
%	receive
%		{From {"Rage Against the Turing Machine", "Unit Testify"}} ->
%			From ! {self(), "They are great!"};
%		{From, {"System of a Downtime", "Memoize"}} ->
%			From ! {self(), "They're not Johnny Crash but they're good."};
%		{From, {"Johnny Crash", "The Token Ring of Fire"}} ->
%			From ! {self(), "Simply incredible."};
%		{From, {_Band, _Album}} ->
%			From ! {self(), "They are terrible!"}
%	end,
%	critic().

chain(0) ->
	receive
		_ -> ok
	after 2000 ->
		exit("chain dies here")
	end;

chain(N) ->
	Pid = spawn(fun() -> chain(N-1) end),
    	link(Pid),
    	receive
    		_ -> ok
    	end.

-module(kitchen).
-compile(export_all).
 
fridge1() ->
receive
 	{From, {store, _Food}} ->
		From ! {self(), ok},
     			fridge1();
	{From, {take, _Food}} ->
		%% uh....
	From ! {self(), not_found},
     		fridge1();
	terminate ->
		ok
end.

fridge2(FoodList) ->
receive
	{From, {store, Food}} ->
		From ! {self(), ok},
     			fridge2([Food|FoodList]);
	{From, {take, Food}} ->
		case lists:member(Food, FoodList) of
			true ->
				From ! {self(), {ok, Food}},
     				fridge2(lists:delete(Food, FoodList));
			false ->
				From ! {self(), not_found},
     				fridge2(FoodList)
		end;
	terminate ->
	ok
end.


%% ?MODULE return the name of the current module.
start(FoodList) ->
	spawn(?MODULE, fridge2, [FoodList]).

store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
    	receive
		{Pid, Msg} -> Msg
	end.
 
take(Pid, Food) ->
	Pid ! {self(), {take, Food}},
    	receive
		{Pid, Msg} -> Msg
	end.

 %% after is used for timout. Suppose the function doesnot recieve any msg for that particular amount of time then it executes the part written in after.

store2(Pid, Food) ->
	Pid ! {self(), {store, Food}},
    	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.
 
take2(Pid, Food) ->
	Pid ! {self(), {take, Food}},
    	receive
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.

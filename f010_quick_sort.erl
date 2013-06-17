-module(f010_quick_sort).
-compile(export_all).

%Quick sort function that takes list as argument
%and returns sorted list

quick_sort([]) -> [];
quick_sort([Pivot | Tail]) ->
	%Let L1 be sorted list with elements less than Pivot
	L1=quick_sort([X || X <- Tail, X<Pivot]),

	%Let L2 be sorted list with elements greater than or equal to Pivot
	%Also prepent pivot to the the sorted list
	L2=[Pivot | quick_sort([X || X<-Tail, X>=Pivot])],

	%Combine L1 and L2 to get sorted list
	lists:append(L1, L2).


%Function that takes a list as argument and tries to sort it with
%fun quicksort/1. It prints both original list and sorted list
test_quicksort(Test_list) ->
	Result=quick_sort(Test_list),
	io:format("Result of sorting ~p is ~p~n", [Test_list, Result]),
	ok.


%Calls fun test_quicksort/1 with few test cases to check whether
%function is working properly or not.
test()->
	test_quicksort([]),
	test_quicksort([1, 2, 3, 4, 5]),
	test_quicksort([5, 4, 3, 2, 1]),
	test_quicksort([1, 1, 1, 2, 2]),
	test_quicksort([-1, -10, -1, 20, 30, 40, -200]),
	ok.

-module(f011_pythagorean_triplets).
-compile(export_all).


%This function returns list of sides such that there sum is less than
%or equal to N and they form a right angled triangle. Function demonstrates
%ease of use and compactness of code when list comprehensions are used
pythagorean(N) -> 
	%Using list comprehension get list of sides S1, S2, S3 such that
	[{S1, S2, S3} ||
		%S1 is between 1 and N
		S1 <- lists:seq(1, N),

		%S2 is between 1 and N
		S2 <- lists:seq(1, N),

		%S3 is between 1 and N
		S3 <- lists:seq(1, N),

		%Sum of all three sides is less than or equal to N
		(S1 + S2 + S3) =< N,

		%S1 is equal or smaller than S2 (To remove duplicates)
		S1 =< S2,

		%S1, S2, S3 are possible sides of a right angled triangle.
		S1 * S1 + S2 * S2 =:= S3 * S3].


test() ->
	%Test function with N=20
	io:format("Pythagorean triplets where sum of sides is less than or equal to 20 are ~n~p~n", [pythagorean(20)]),

	%Test function with N=30
	io:format("Pythagorean triplets where sum of sides is less than or equal to 30 are ~n~p~n", [pythagorean(30)]),
	ok.

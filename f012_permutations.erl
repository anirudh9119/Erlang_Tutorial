-module(f012_permutations).
-compile(export_all).

%Permuations must return list of lists
%In case L1 is empty then only possible permuatation is 
%Empty list. So we return list containing empty list 
permutations([]) ->[[]];

permutations(L1) ->
	%Divide L1 into two parts head and tail
	%    [Head1 | Tail1 ] = L1,
	%All permutations of L1 that start with Head1 can be obtained using
	%    lists:map(fun (X) -> [Head1|X] end, permuations(Tail1))
	%
	%Now if we can get different elements of list everytime instead of Head1
	%then we can get all possible permutations that start with that element
	%Combined list of all such permutations will give all possible permutations
	%of L1

	%Defining Permutations to be a function that takes Start and Rest
	%And returns all permutations of [Start | Rest] that start with
	%Start. Thus if we can rotate Start to be different element each time
	%Then we would get permutations that start with that particular element.
	Permutations = 
		%Permutations takes two parameters 
		%   Start -> Character or atom or element that will start each permutation
		%		     generated by this function
		%   Rest  -> List for which all possible permutations would be generated and
		%            then all of these combinations would be prefixed by Start
		%
		fun(Start, Rest) -> 
			%Get all possible permutations of Rest
			Perm_Rest = permutations(Rest),
			
			%Prefix all possible permustions of Rest with Start
			lists:map(fun(X) -> [Start | X] end, Perm_Rest)
		end,

	%For every X1 belonging to L1
	%Calcuate permutations that start with X1 and have 
	%permutations of L1--[X1] after X1
	%and then get list of all such permutations (X2)
	[ X2  || X1 <- L1, X2 <- Permutations(X1, L1--[X1])].
			


%We can also get permutations recursively with above logic explained in this manner:
% 1. Permutations of empty list[] should be [[]] list containing empty list
%	 as permutations are lists of lists. For example permutations of "12" would
%    be ["12", "21"] which is list of lists.
%
% 2. To find all possible permutation of string (say "abcd") we can divide problem
%    into smaller parts. First we will find all possible permuations of "abcd" that
%    start with "a", then we will find all possible permutations of "abcd" that
%    start with "b" and so on. Then we will add these four lists to get all
%    possible permutation of "abcd". 
%
%    Note that when we follow this approach
%    we will not get duplicate entries as no element can come in two lists
%    lists that start with "a" or lists that start with "b", etc. as lists
%    are dis-joint.
%       Also note that we will not miss any possible permutation of "abcd" as
%    any possible permutation of "abcd" will either start with "a" or will
%    start with "b" or "c" or "d" and we have captured all such possibilties.
%
% 3. To get all possible permutations of "abcd" that start with "a" we can
%    find all possible permutations of "bcd" and prepend "a" to all of them.
%    This way we will get all possible permutations of "abcd" that start with "a".
%
%    Similarly to get all possible permutations that start with "b" we will get
%    all possible permutations of "acd" and prepend "b" to all of them.
%   
% 4. Thus
%        permutations("abcd") = prepend("a", permutations("bcd")) +
%        						prepend("b", permutations("acd")) +
%        						prepend("c", permutations("abd")) +
%        						prepend("d", permutations("abc"))
%        permutations("abcd") = 
%			lists:flatten(lists:map(fun(Element) -> prepend(Element, "abcd" -- [Element]) end, "abcd"))
% The same is written in simpler way below
permutations2([]) -> [[]];
permutations2(L1) -> [[H | T] || H <- L1, T <- permutations2(L1--[H])].




combinations([]) -> [[]];
%All combinations of list with Head H and Tail T 
%would be all possible combinations of tail T and
%all possible combinations of tail T with H added to each combination.
combinations([H | T]) -> 
	lists:append(
		combinations(T),
		[[H|X] || X <-combinations(T)] ).


%All possible permutation and combinations of List are
%List of all permutations for each possible combination of L1
perm_and_comb(L1) ->
	lists:append([permutations(X) || X<-combinations(L1)]).


%Testing all above functions by calling then with test string like "1234"
test() -> 
	io:format("All permutations of '1234' are ~p~n", [permutations("1234")]),
	io:format("All permutations of '1234' are ~p~n", [permutations2("1234")]),
	io:format("All combinations of '1234' are ~p~n", [combinations("1234")]),
	io:format("All permutations and combinations of '1234' are ~p~n", [perm_and_comb("1234")]),
	ok.

-module(f016_use_records).
-compile(export_all).

%This is done so that we can use person and person2 records
%Defined in hrl file in current module. We can include same
%hrl file in multiple modules to ensure that all modules
%use same record definition. This is similar to having
%struct definition in header files in C.
-include("f015_records.hrl").

sample_run() ->
	%We can create records by suppling all parameters
	Person1 = #person{name="Saurabh Barjatiya", email="barjatiya.saurabh@gmail.com", mobile="93939 14337"},
	io:format("1. Person1 is ~p~n", [Person1]),

	%If we leave value unspecified then it takes atom undefined as value
	Person2 = #person{mobile="93993 14337"},
	io:format("2. Person2 is ~p~n", [Person2]),

	%If we have specified a default value in .hrl file while defining record
	%then that default value is used and not atom undefined
	Person3 = #person2{name="IIIT Hyderabad"},
	io:format("3. Person3 is ~p~n", [Person3]),

	%We can create new records by changing desired values of existing records
	Person4=Person1#person{mobile="86865 99552"},
	io:format("4. Person4 is ~p~n", [Person4]),

	%We can use is_record() which is also allowed in guards to ensure that
	%given variable is record and of given type
	io:format("5. Person1 is of record of type person - ~p~n", [is_record(Person1, person)]),
	io:format("6. Person3 is of record of type person - ~p~n", [is_record(Person3, person)]),

	%We can extract values from records by match operator =
	#person{name=Name1, email=Email1, mobile=Mobile1}=Person1,
	io:format("7. Name, Email and Mobile of Person1 are ~p, ~p and ~p~n", [Name1, Email1, Mobile1]),

	%We can choose to extract only interesting records
	#person{name=Name2}=Person2,
	io:format("8. Name of Person2 is ~p~n", [Name2]),

	%Finally we can use #<record>.<field> to extract <field> value from given person record
	Mobile3=Person3#person2.mobile,
	io:format("9. Mobile number of person3 is ~p~n", [Mobile3]),

	io:format("10, Going to print_record(Person1)~n    ", []),
	print_record(Person1),
	io:format("11, Going to print_record(Person2)~n    ", []),
	print_record(Person2),
	io:format("12, Going to print_record(Person3)~n    ", []),
	print_record(Person3),
	io:format("13, Going to print_record(some_atom)~n    ", []),
	print_record(some_atom),
	ok.


%print_record function defined below demonstrates how we can use
%records to match arguments to function. A given function clause
%matches only when record type or value as specified in function definition
%matches. 

% The following clause will match when argument is of record of type
% person and name is exactly same as "Saurabh Barjatiya"
print_record(#person{name="Saurabh Barjatiya"} = Person1) ->
 	#person{mobile=Mobile1, email=Email1} = Person1,
	io:format("Mobile and Email of Saurabh Barjatiya (person) are ~p and ~p~n", [Mobile1, Email1]),
	ok;

% The following clause will match for any argument of person record type
print_record(#person{} = Person1) ->
	#person{name=Name1, mobile=Mobile1, email=Email1} = Person1,
	io:format("Name, Mobile and Email of given record (person) are ~p, ~p and ~p~n", [Name1, Mobile1, Email1]),
	ok;

% The following clause will match for any argument of person2 record type
print_record(Person2) when is_record(Person2, person2) ->
	#person2{name=Name2, mobile=Mobile2, email=Email2}=Person2,
	io:format("Name, Mobile and Email of given record (person2) are ~p, ~p and ~p~n", [Name2, Mobile2, Email2]),
	ok;

% The following clause is default / catch all and will execute on any
% argument. But we will reach here only when all above clauses fail.
print_record(_) ->
	io:format("Error. print_record called with argument which is not of type person or person2~n", []),
	error.
	



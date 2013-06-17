-record(person, {name, email, mobile}).

% Supplying default values for email, mobile so that if email, mobile
% are not defined then they take these values instead of atom
% undefined

-record(person2, {name, email="help@iiit.ac.in", mobile="040 6653 1000"}).

%1. To load above record in shell use
%      rr("f015_records.hrl").

%2. To use above record in other files use
%      -include("f015_records.hrl").
%   near head of .erl file

%3. Use 
%        #person{name="Saurabh"}.
%   in shell to create record of type person with name="Saurabh". All
%   unsupplied values would be 'undefined' by default


%4.  We can use
%       rf(person).
%    to make shell forget about record person.


%5. Records are not a different data types. Records are internally stored as tuples.



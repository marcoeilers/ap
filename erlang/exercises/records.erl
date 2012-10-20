-module(records).
-compile(export_all).

%% In the shell, one needs to load a record definition by
%% 'rr(module_name).' in this case:
%%
%%   rr(records).
%%
%% It can take a wildcard, like rr("*"), and there's also a version
%% taking two arguments, where the second is a list of records to load.
%%
%% * rd(Name, Definition). Lets you define a record.
%% * rf(). Unload all record definitions. Also has version rf(Name)
%%   and rf([Names]).
%% * rl(). Print all record definitions. Also, rl(Name) and rl([Names]).
%% * rp(Term). Convert a tuple into a record (given the definition exists).

%% Record declaration:
-record(robot, { name,
		 type=industrial,
		 hobbies,
		 details=[]}).

%% Record instantiation (in the shell, remember to load the definition
%% as described above).
first_robot() ->
    #robot{name="Mechatron",
	   type=handmade,
	   details=["Moved by a small man inside"]}.

car_factory(CorpName) ->
    #robot{name=CorpName, hobbies="building cars"}.


%% Accessing record values. Try:
%%
%% > Crusher = records:crusher().
%% > Crusher#robot.hobbies.
%%
crusher() ->
    #robot{name="Crusher", hobbies=["Crushing people", "petting cats"]}.


%% Another example
-record(user, {id, name, group, age}).

admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed!".

adult_section(U = #user{}) when U#user.age >= 18 ->
    allowed;
adult_section(_) ->
    forbidden.



%% Record updating
repairman(Rob = #robot{details=Details}) ->
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.


%% Header file inclusion.
%%
%% Erlang header files are very similar to C's header files, as in,
%% they're a snippet of code that gets added to the module as if it
%% were written there in the first place
-include("records.hrl").

included() ->
    #included{some_field="Some value"}.

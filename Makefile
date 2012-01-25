all:
	./rebar get-deps && ./rebar compile && ./rebar generate

clear:
	./rebar clean

clean:
	./rebar clean
rebild: 
	./rebar clean
	./rebar compile && ./rebar generate
	

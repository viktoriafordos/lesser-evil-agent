all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

eunit:
	rebar3 eunit

proper:
	rebar3 proper -n 100

ct:
	rebar3 ct -v

xref:
	rebar3 xref

dialyzer:
	rebar3 dialyzer

upgrade:
	rebar3 upgrade

unlock:
	rebar3 unlock

lock:
	rebar3 lock

start:
	rebar3 shell --sname lesser_evil_agent

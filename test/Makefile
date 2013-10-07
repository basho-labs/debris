## Note: In order to run this, you must have the commercial version
## of QuickCheck http://www.quviq.com/index.html

test_utils:
	erlc -DTEST -DEQC eqc_test_utils.erl
	erlc -DTEST -DEQC eqc_tests.erl
	erl -noshell -pa ebin -eval "eqc_test_utils:test_all()" -s init stop
	erl -noshell -pa ebin -eval "eqc_tests:test_all()" -s init stop

all: gen_stubs


clean:
	rm *.beam
	rm *.hrl


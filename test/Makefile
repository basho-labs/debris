## Note: In order to run this, you must have the commercial version
## of QuickCheck http://www.quviq.com/index.html
eqc:
	erlc -DTEST -DEQC eqc_test_utils.erl
	erl -noshell -pa ebin -eval "eqc_test_utils:test_all()" -s init stop
	erl -noshell -pa ebin -eval "eqc_test_utils:generate_stubs()" -s init stop

clean:
	rm *.beam
	rm *.hrl


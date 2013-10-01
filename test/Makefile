## Note: In order to run this, you must have the commercial version
## of QuickCheck http://www.quviq.com/index.html
eqc:
	erlc -DTEST -DEQC eqc_tests.erl
	erl -noshell -pa ebin -eval "eunit:test(eqc_tests, [verbose])" -s init stop


SOURCES=$(wildcard *.erl)
HEADERS=$(wildcard *.hrl)
OBJECTS=$(SOURCES:%.erl=%.beam)
TESTS=tests

all : test

objects : $(OBJECTS)



%.beam : %.erl $(HEADERS)
	erlc +debug_info $<

protocol_tests: objects
	@mkdir -p $(TESTS)
	@cp -f *_tests.beam $(TESTS)
	@erl -noshell -pathz $(TESTS) -eval 'protocol_tests:test()' -s init stop	

telephone_tests: objects
	@mkdir -p $(TESTS)
	@cp -f *_tests.beam $(TESTS)
	@erl -noshell -pathz $(TESTS) -eval 'telephone_tests:test()' -s init stop	


test : objects
	@mkdir -p $(TESTS)
	@cp -f *_tests.beam $(TESTS)
	@erl -noshell -eval 'eunit:test({dir, "$(TESTS)"})' -s init stop

clean:
	$(RM) $(OBJECTS)
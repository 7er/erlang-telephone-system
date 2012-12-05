
SOURCES=$(wildcard *.erl)
HEADERS=$(wildcard *.hrl)
OBJECTS=$(SOURCES:%.erl=%.beam)
TESTS=tests

all : test

objects : $(OBJECTS)



%.beam : %.erl $(HEADERS)
	erlc +debug_info $<

test : objects
	@mkdir -p $(TESTS)
	@cp -f *_tests.beam $(TESTS)
	@erl -noshell -eval 'eunit:test({dir, "$(TESTS)"})' -s init stop

clean:
	$(RM) $(OBJECTS)
.PHONY: all compile deps test clean dist_clean

CC = ./rebar

all: compile

compile: deps
	@${CC} co

deps:
	@${CC} g-d

test: compile
	@${CC} eu

dialyze:
	

clean:
	@rm -rf ebin
	
dist_clean:
	@rm -rf ebin
	@rm -rf deps


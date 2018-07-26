all:
	stack build && gpu ./run.sh

run:
	gpu ./run.sh

build:
	stack build


all:
	@mkdir -p build
	@swipl --goal=main --stand_alone=true -o build/subject-schedule-solver -c src/run.pl

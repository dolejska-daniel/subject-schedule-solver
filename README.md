# Subject Schedule Solver
> v0.1.0

## Introduction
_TBD_

## Build
```shell
make
```

What is done by make:
```shell
swipl --goal=main --stand_alone=true -o build/subject-schedule-solver -c src/run.pl
```

## Downloading
_TBD_

## Usage
By default reads the input from STDIN and writes the output to STDOUT.
The program also accepts CLI arguments `--load-from` and `--save-to` specifying the name of the input and output files.
Arguments must be in `--argname=argvalue` format:

```shell
build/subject-schedule-solver --load-from=examples/subjects.json --save-to=output.json
```

_TBD_

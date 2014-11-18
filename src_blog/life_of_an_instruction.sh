#!/bin/bash
clang -O3 -S -emit-llvm life_of_an_instruction.c -o life_of_an_instruction.ll

echo "1. Display the DAG after being built, before the first optimization pass:"
llc -view-dag-combine1-dags life_of_an_instruction.ll

echo "2. Display the DAG before legalization:"
llc -view-legalize-dags life_of_an_instruction.ll

echo "3. Display the DAG before the second optimization pass:"
llc -view-dag-combine2-dags life_of_an_instruction.ll

echo "4. Display the DAG before the Select phase:"
llc -view-isel-dags life_of_an_instruction.ll

echo "5. Display the DAG before scheduling:"
llc -view-sched-dags life_of_an_instruction.ll

echo "6. Display the scheduler's dependency graph:"
llc -view-sunit-dags life_of_an_instruction.ll


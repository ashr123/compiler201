#!/bin/bash
CWD="$(pwd)"
test_="TEST_"
test_dir="tests"
input="$CWD/$test_dir/input"
output="$CWD/$test_dir/output"
make_files="$CWD/$test_dir/make_files"
comp="compiler.s"
passed=0
no_result="(equal? '() '())"
true="#t"
RED='\033[1;31m'
RED1='\033[0;31m'
GREEN='\033[1;32m'
PURPLE='\033[0;35m'
BLUE='\033[1;34m'
NC='\033[0m' # No Color

echo -e "${NC}removing old outputs..${RED1}"; rm -v $output/*
echo -e "${NC}removing old make files..${RED1}"; rm -v $make_files/*
echo -e "${NC}moving failed tests to input folder..${RED1}"; mv $input/failed/*.scm $input
echo -e "${NC}moving passed tests to input folder..${RED1}"; mv $input/passed/*.scm $input
echo -e "${NC}making a copy of $comp..${RED1}"; cp $CWD/$comp $make_files/$comp
echo -e "${PURPLE}======== TESTS THAT PASS WILL BE REMOVED! ========"
echo -e "${BLUE}running $num_of_tests tests...${NC}"

for file in $input/*.scm
do
    test_=$(echo "${file##*/}"| cut  -d'.' -f 1)
    echo -e "${NC}========================================================"
    echo -e "${BLUE}Test name: $test_.scm${NC}"
    echo "running compiler.ml on $test_.scm"
    cd $CWD && ocaml compiler.ml $input/$test_.scm > $make_files/$test_.s 
    echo "running makefile on $test_.s"
    cd $make_files && nasm -f elf64 -o $make_files/$test_.o $make_files/$test_.s && gcc -static -m64 -o $make_files/$test_ $make_files/$test_.o
    o1=$(scheme -q < $input/$test_.scm)
    o2=$($make_files/$test_);
    echo "running $test_.scm & $test_.s to compare results..."
    echo "(equal? '($o1) '($o2))" > $output/$test_.scm;
    result_check="$(scheme -q < $output/$test_.scm)"
    result=$(<$output/$test_.scm)
    if [ "$result" == "$no_result" ]
    then
        echo -e "${BLUE}$test_: ${PURPLE}no result on both scheme and assembly!${NC}"
        mv $input/$test_.scm $input/passed/$test_.scm
        rm -v $make_files/$test_
        rm -v $make_files/$test_.s
        rm -v $make_files/$test_.o
        rm -v $output/$test_.scm

    elif [ "$result_check" == "$true" ]  
    then
        echo -e "${BLUE}$test_: ${GREEN}PASSED!${RED1}"
        mv $input/$test_.scm $input/passed/$test_.scm
        rm -v $make_files/$test_
        rm -v $make_files/$test_.s
        rm -v $make_files/$test_.o
        rm -v $output/$test_.scm
    else
        echo -e "${BLUE}$test_: ${RED}FAILED :(${NC}"
        mv $input/$test_.scm $input/failed/$test_.scm
    fi
done

echo "=========================="
echo -e "${NC}removing .o files..${RED1}"; cd $make_files && rm -v *.o
echo -e "${NC}removing $comp copy${RED1}"; cd $make_files && rm -v $make_files/$comp

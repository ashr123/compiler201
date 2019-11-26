#!/bin/bash

BASE_DIR=$(pwd)
PATCH=$1
AUTHENTICITY=readme.txt
CODE=reader.ml
STATEMENT="realize that should our code be found to contain code from other sources, that a formal case shall be opened against"
PROBLEM=0
GIT_REPO="https://www.cs.bgu.ac.il/~comp201/compiler"
LOCAL_REPO=compiler
TMP_FOLDER=$(mktemp -d)
OCAML_TMP_FILE=struct_test.ml
OCAML_TYPE_TEST="#use \"reader.ml\";;
(Bool true : sexpr);;
(Nil : sexpr);;
(Number (Int 1) : sexpr);;
(Number (Float 1.) : sexpr);;
(Char 'a' : sexpr);;
(String \"Asdas\" : sexpr);;
(Symbol \"lsdh\" : sexpr);;
(Pair (Nil, Nil) : sexpr);;
(TaggedSexpr (\"lsiksalkd\",Nil) : sexpr);;
(TagRef \"nasd\" : sexpr);;"
OCAML_SIG_TEST="#use \"reader.ml\";;
try
  ((Reader.read_sexpr \"\") : sexpr)
with 
| X_not_yet_implemented -> print_string \"Warning: Your submission is missing an implementation for Reader.read_sexpr!\n\"; Nil
| PC.X_no_match -> Nil;;

try
  ((Reader.read_sexprs \"\") : sexpr list)
with X_not_yet_implemented -> print_string \"Warning: Your submission is missing an implementation for Reader.read_sexprs!\n\";  [Nil];;"

cleanup () {
    #echo "cleaning up temporary files and exiting."
    rm -rf $TMP_FOLDER
}

if [ $# -lt 1 ]; then
    PATCH="compiler/compiler.patch"
fi

if ! [ -f $PATCH ]; then
    echo "ERROR: The patch file '$PATCH' cannot be found. Please provide the relative path filename to your patch file."
    exit 2
fi

cd $TMP_FOLDER
git clone -q $GIT_REPO
if [ "$?" -ne 0 ]; then
    echo "ERROR: There was a problem creating a temporary clone of the project repository. There might be a problem with your network connection. The structure test cannot be completed."
    cleanup
    exit 2
fi

cd $LOCAL_REPO
git apply --ignore-whitespace --whitespace=nowarn $BASE_DIR/$PATCH
if [ "$?" -ne 0 ]; then
    echo "ERROR: The contents of your patch file are invalid and git cannot apply it. The structure test cannot be completed."
    cleanup
    exit 2
fi


if ! [ -f $AUTHENTICITY ]; then
    echo "ERROR: Your submission is missing the authenticity statement file ($AUTHENTICITY)."
    PROBLEM=1
else 
    ID=$(egrep -e '[0-9]{7,10}' $AUTHENTICITY)
    STMNT=$(cat $AUTHENTICITY | tr -d [:space:] | grep -i "$(echo "$STATEMENT" | tr -d [:space:])")

    if [ -z "$ID" ] || [ -z "$STMNT" ] ; then
	echo "ERROR: Your authenticity statement (in $AUTHENTICITY) is incomplete."
	PROBLEM=1
    fi
fi

if ! [ -f $CODE ]; then
    echo "ERROR: Your submission is missing the required code file: $CODE."
    PROBLEM=1
fi

echo $OCAML_TYPE_TEST > $OCAML_TMP_FILE && ocaml $OCAML_TMP_FILE 2> /dev/null
if [ $? -ne 0 ]; then
    echo "ERROR: Your OCaml code contains invalid changes to the sexpr and/or number types."
    PROBLEM=1
fi

echo $OCAML_SIG_TEST > $OCAML_TMP_FILE && ocaml $OCAML_TMP_FILE 2> /dev/null
if [ $? -ne 0 ]; then
    echo "ERROR: Your OCaml code contains invalid changes to the signatures of the Reader module."
    PROBLEM=1
fi

if [ $PROBLEM -ne 0 ]; then
    echo "!!! Your submission is invalid. Please correct the problems and try again. !!!"
else 
    echo "Your submission passed the structure test.
This does not mean that your assignment is correct, only that we can test it properly."
fi

cleanup
exit $PROBLEM

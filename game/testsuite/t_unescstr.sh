#!/bin/sh

checkword() {
	if [ $# -ne 3 ]
	then
		return 1
	fi

	./t_unescstr "$1" "$2"
	ret=$?

	echo -n "$1 -> $2: "
	if [ $ret -eq 0 ]
	then
		echo -n "ok: "
	else
		echo -n "nok: "
	fi

	if [ $ret -eq $3 ]
	then
		echo "success"
	else
		echo "failure"
	fi
}

checkword user user 0
checkword "a	b" "a	b" 0
checkword "a\\	b" "a	b" 0
checkword "\\" "\\" 1
checkword "\\\\" "\\" 0

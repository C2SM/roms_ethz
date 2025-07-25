#!/bin/sh
DIR=$1
shift 1

case "$DIR" in
"" | ".")
	cpp -MM -MG "$@" | sed -e 's@^\(.*\)\.o:@\1.d \1.o:@'
      ;;
*)
      cpp -MM -MG "$@" | sed -e "s@?\(.*\)\.o:@$DIR/\1.d  $DIR/\1.o:@"
      ;;
esac

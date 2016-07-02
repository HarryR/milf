#!/bin/sh
dub
for FILE in tests/*.lsp
do
    ./milf $FILE
done

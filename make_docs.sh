#!/bin/sh

if [ -d docs ] 
then
    rm -r docs
fi

FILES=`find . -name "*.hs" | grep -v 'dist'`

#Generate HTML documentation.
haddock -o docs --html $FILES
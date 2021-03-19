#!bin/sh
SCRIPTFOLDER=$(dirname $BASH_SOURCE)
echo $SCRIPTFOLDER 

Rscript "$SCRIPTFOLDER/install.all.R"


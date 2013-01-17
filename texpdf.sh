#!/bin/bash

DVI_PATH=`echo $1 | sed -e "s/^\(.*\)\.tex$/\1.dvi/"`
PDF_PATH=`echo $1 | sed -e "s/^\(.*\)\.tex$/\1.pdf/"`

nkf -e --overwrite $1
platex $1
dvipdfmx "${DVI_PATH}"
evince "${PDF_PATH}"

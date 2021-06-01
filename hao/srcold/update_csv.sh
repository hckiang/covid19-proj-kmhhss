#!/bin/sh

NOW=$(date +"%Y%m%dT%H%M")
EUROSTAT_FILENAME="../dat/eurostat-${NOW}.csv"
POLISH_FILENAME="../dat/polish-${NOW}.csv"

python3 ./eurostat.py -o "${EUROSTAT_FILENAME}"
python3 ./polish.py "${POLISH_FILENAME}"

#!/bin/sh

if [ $(ps aux | grep Extract_NLDAS.py | wc -l) -eq 1 ]
  then
    /home/ubuntu/telegram.sh "Resetting NLDAS"
    python3 /home/ubuntu/temp-sentiment/extract/Extract_NLDAS.py
fi

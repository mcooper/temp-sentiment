#!/bin/sh

if [ $(ps aux | grep python3.6 | wc -l) -eq 1 ]
  then
    /home/ubuntu/telegram.sh "Need to reset NLCS"
fi

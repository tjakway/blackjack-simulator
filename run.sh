#!/usr/bin/env bash

#10 million
NUM_GAMES=1000000

echo "fr start time: " > fr_time.txt
date >> fr_time.txt

~/git/blackjack-simulator/.cabal-sandbox/bin/blackjack-simulator --with-dealer=BasicDealer --num-BasicPlayer=1 --num-games="$NUM_GAMES" --tablename-suffix=fr 2> fr_stderr.txt 1> fr_stdout.txt

echo "fr end time: " >> fr_time
date >> fr_time.txt

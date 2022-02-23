#!/bin/sh

pName=$1
pTime=$2
pFileName="$pName"".iti"

for i in $(seq 1 6); do 

    echo "[" "$i" "] IN PROGRESS" 

    mkdir -p "$pName"/Run"$i" &&

    stack run "$pFileName" genetic $pTime > GAOutput.txt

    mv GAOutput.txt "$pName"/Run"$i"
    mv GADebug.csv "$pName"/Run"$i"

    echo "[" "$i" "] DONE" 
done

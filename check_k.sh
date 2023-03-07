#!/bin/bash

## script to check model fit for entropy outputs with different values of k, outputs results to text file
## Amanda Meuser -- March 2023

## USAGE: ./check_k.sh nametag

out=$1

for file in ./*.hdf5 
do 
    /project/rrg-emandevi/bin/estpost.entropy -s 3 -p deviance $file >> $out"_DIC.txt"
done

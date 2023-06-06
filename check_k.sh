#!/bin/bash

## script to check model fit for entropy outputs with different values of k, outputs results to text file
## Amanda Meuser -- March 2023

## USAGE: ./check_k.sh nametag /path/to/entropy/files (no / at the end of the path)

out=$1
path=$2

for file in $path/*.hdf5 
do 
    /project/rrg-emandevi/bin/estpost.entropy -s 3 -p deviance $file >> $out"_DIC_raw.txt"
done

# remove file extension, retain k and rep values -- HARDCODED
sed -e 's/file = \/project\/rrg-emandevi\/hybrid_ameuser\/AMP22\/entropy\/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_//g' -e 's/_150k_/\t/g' -e 's/_qk[0-9]*inds\.hdf5//g' $out"_DIC_raw.txt" > temp.txt

#remove unecessary repeated line
sed -i '/parameter dimensions for/d' temp.txt

# remove new lines
tr -d '\n' < temp.txt > temp2.txt

# add new lines
sed -i 's/k/\nk/g' temp2.txt

# get number  on one line per file and remove words before numbers
sed -i -e 's/Model deviance: /\t/g' -e 's/Effective number of parameters: /\t/g' -e 's/Model DIC: /\t/g' temp2.txt

# remove blank second line
sed -i '/^$/d' temp2.txt

# insert header
sed '1i k\trep\tModel_deviance\tEffective_number_of_parameters\tModel_DIC' temp2.txt > $out"_DIC_trimmed.txt"

rm temp.txt temp2.txt postout.txt



## Liz's idea for new code
##    /project/rrg-emandevi/bin/estpost.entropy -s 3 -p deviance $file | grep 'DIC' 
##    dic=`my fancy grep statement`
##    echo $file $dic >> $out"_DIC.txt"

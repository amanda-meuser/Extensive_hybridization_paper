#!/bin/bash

## script to check model fit for entropy outputs with different values of k, outputs results to text file
## Amanda Meuser -- March 2023

## USAGE: check_k.sh nametag /path/to/entropy/files (no / at the end of the path)
## TIP: for rerunning the script, delete the previous output files bc the script appends, so it'll add in duplicate lines of DIC values

out=$1
path=$2

# you can add text in here to differentiate b/w entropy runs
for file in $path/*100k*.hdf5 
do 
    /project/rrg-emandevi/bin/estpost.entropy -s 3 -p deviance $file >> $out"_DIC_raw.txt"
done

# remove file extension, retain k and rep values -- HARDCODED
sed -e 's/file = \/project\/rrg-emandevi\/hybrid_ameuser\/AMP22_pub\/geographical_groupings\/algonquin\/entropy\/AMP22_Pimephales_algonquin_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_//g' -e 's/_100k_/\t/g' -e 's/_qk[0-9]*inds\.hdf5//g' $out"_DIC_raw.txt" > temp.txt

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

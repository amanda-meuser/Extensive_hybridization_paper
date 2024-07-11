#!/bin/bash

## script to loop my q_barplot script for all values of k
## Amanda Meuser -- July 2024

## Usage: loop_plotting.sh max-k-value nametag names_file.txt

module load r

nametag=$2
names_list=$3

echo "Here's the nametag: $nametag"
echo "Here's the names list: $names_list"

echo "Starting to plot..."

for k in $(seq 2 $1) # start at 2 and go up by 1 till you get to $1
do
    Rscript /project/rrg-emandevi/hybrid_ameuser/q_barplot.R $k $nametag $names_list /project/rrg-emandevi/hybrid_ameuser/AMP22_pub/entropy/AMP22_Pimephales_target_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k"$k"_100k_rep1_qk"$k"inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22_pub/entropy/AMP22_Pimephales_target_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k"$k"_100k_rep2_qk"$k"inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22_pub/entropy/AMP22_Pimephales_target_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k"$k"_100k_rep3_qk"$k"inds.hdf5
    if [ "$#" -ne 3 ]; then 
	    echo "Must supply K-value, nametag, and names list on the command line!!"
        break
    fi 
done

echo "Done!!"

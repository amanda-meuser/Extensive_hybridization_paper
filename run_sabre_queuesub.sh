#!/bin/sh

## This script uses sabre to demultiplex .fastq files - note that it can use .fastq.gz
## Usage: sbatch run_sabre_queuesub.sh

#SBATCH --account=def-emandevi
#SBATCH --time=0-03:00:00
#SBATCH --nodes=1
#SBATCH --mem=4000
#SBATCH --ntasks-per-node=1
#SBATCH --mail-user=ameuser@uoguelph.ca
#SBATCH --mail-type=END

## Load modules
module load sabre

echo "starting demultiplexing with sabre"

sabre se -f /project/def-emandevi/genomic_data/agstream_2020/CCCSLib_combined.fastq.gz -b sabre_CCCSlib_barcode_key_amanda17jun20.txt -u unknown_barcode.fastq -m 2

## Print to log file (slurm-XXXXXX.out)
echo "All done now"



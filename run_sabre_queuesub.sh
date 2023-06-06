#!/bin/sh

## This script uses sabre to demultiplex .fastq files - note that it can use .fastq.gz
## Usage: sbatch run_sabre_queuesub.sh

#SBATCH --account=rrg-emandevi
#SBATCH --time=0-06:00:00
#SBATCH --nodes=1
#SBATCH --mem=4000
#SBATCH --ntasks-per-node=1
#SBATCH --mail-user=ameuser@uoguelph.ca
#SBATCH --mail-type=END

## Load modules
module load sabre

echo "starting demultiplexing with sabre"

sabre se -f /project/rrg-emandevi/genomic_data/ontario_minnows_GBS2023/4YM706K/MAN23769.20230328/230327_A02072_0030_BH3YG2DRX3/AMP22_PCR2_S2_L002_R1_001.fastq.gz -b ../AMP22_library2_sabre_barcodes_21apr23.txt -u unknown_barcode_lib2.fastq -m 2

## Print to log file (slurm-XXXXXX.out)
echo "All done now"



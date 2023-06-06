#!/bin/sh

## Usage: sbatch count_assembled_7jul22.sh output_file_name.txt

#SBATCH --account=rrg-emandevi
#SBATCH --time=0-00:10:00 ## days-hours:minutes:seconds
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16 # number of threads
#SBATCH --mem=24000 # requested memory (in MB)
#SBATCH --mail-user=ameuser@uoguelph.ca
#SBATCH --mail-type=END

echo "Current working directory: $PWD"
echo "starting run at: $(date)"

echo "Reading command line input..."

output_file=$1

echo "Loading modules..."

module load StdEnv/2020 
module load samtools/1.12

echo "Modules loaded, starting assembly"

# for running just one file, comment out the 'for', 'do', and 'done' lines, then remove the comments in front of the following line
#file=../bwa/EGM19_0166.sorted.bam 

echo "ind raw assembled" > $output_file 
for file in ../bwa/*.sorted.bam 
do
indname=$(basename $file .sorted.bam)
raw=`samtools stats $file | grep "raw total sequences:" | sed 's/SN\t.*:\t//g'`
assembled=`samtools stats $file | grep "reads mapped:" | sed 's/SN\t.*:\t//g'`
echo "$indname $raw $assembled" >> $output_file 
done

echo "Yay! All done!"

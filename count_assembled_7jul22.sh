#!/bin/sh

## Usage: sbatch count_assembled_7jul22.sh output_file_name.txt
## REMEMBER TO CHANGE INPUT DIRECTORY IN LINES 27 AND 29

#SBATCH --account=rrg-emandevi
#SBATCH --time=0-04:00:00 ## days-hours:minutes:seconds
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16 # number of threads
#SBATCH --mem=24000 # requested memory (in MB)
#SBATCH --mail-user=ameuser@uoguelph.ca
#SBATCH --mail-type=END

echo "Reading command line input..."

output_file=$1

echo "Loading modules..."

module load StdEnv/2020 
module load samtools/1.12

echo "Modules loaded, starting assembly"


echo "ind raw assembled" > $output_file ##to try on one file first, sub: _test
for file in bwa/*.sorted.bam ##sub: test_assem/*.sorted.bam
do
indname=`echo $file | sed 's/bwa\///g' | sed 's/\.sorted\.bam//g'`
raw=`samtools stats $file | grep "raw total sequences:" | sed 's/SN\t.*:\t//g'`
assembled=`samtools stats $file | grep "reads mapped:" | sed 's/SN\t.*:\t//g'`
echo "$indname $raw $assembled" >> $output_file ##sub: _test
done

echo "Yay! All done!"

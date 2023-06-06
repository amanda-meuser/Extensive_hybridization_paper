#!/bin/sh

## usage (for one file):
## sbatch ../run_bwa_queuesub_22jun22.sh /project/rrg-emandevi/hybrid_ameuser/EGM19_ceos/split_fastqs/EGM19_0005.fq /project/rrg-emandevi/communal_genomes/creekchub_hifiasm_nov2022/creekchub_assembly_hifiasm_nov2022.bp.p_ctg.fa

## usage (looping):
##  for file in /project/rrg-emandevi/hybrid_ameuser/AMP22/split_fastqs/*.fq; do sbatch ../run_bwa_queuesub_22jun22.sh $file /project/rrg-emandevi/communal_genomes/creekchub_hifiasm_nov2022/creekchub_assembly_hifiasm_nov2022.bp.p_ctg.fa; done

#SBATCH --account=rrg-emandevi
#SBATCH --time=00-00:60:00 ## days-hours:minutes:seconds
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16 ## number of threads
#SBATCH --mem=96000 ## requested memory (in MB)
##SBATCH --mail-user=ameuser@uoguelph.ca
##SBATCH --mail-type=END

module load StdEnv/2020
module load bwa/0.7.17
module load samtools/1.12


fastq=$1
basename=$(basename $fastq .fq)
echo "This is the basename: $basename"
ref=$2
echo "This is the path to the reference genome: $ref"


echo "Starting alignment of $fastq to reference genome"
bwa mem -t 16 $ref $fastq > bwa/$basename.sam

echo "Converting sam to bam for $basename"
samtools view -b -S -o bwa/$basename.bam bwa/$basename.sam

echo "Sorting and indexing bam files for $basename"
samtools sort bwa/$basename.bam -o bwa/$basename.sorted.bam
samtools index bwa/$basename.sorted.bam

echo "Cleaning up the mess... just a minute!"
if [[ -s bwa/$basename.bam ]]
   then
       rm bwa/$basename.sam
       echo "removed $basename.sam"

else
    echo "$basename.sam is empty! Something's fishy..."
fi



if [[ -s bwa/$basename.sorted.bam ]]
   then
       rm bwa/$basename.bam
       echo "removed $basename.bam"
	echo "good job! alignment done, detritus removed."

else
    echo "$basename.bam is empty! Something's fishy..."
fi



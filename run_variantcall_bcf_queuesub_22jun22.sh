#!/bin/sh

## usage:
## sbatch run_variantcall_bcf.sh /path/to/reference/ /path/to/output/folder/ /path/to/bam/files/ 

#SBATCH --account=rrg-emandevi
#SBATCH --time=2-00:00:00 ## days-hours:minutes:seconds
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16 # number of threads
#SBATCH --mem=128000 # requested memory (in MB)
#SBATCH --mail-user=ameuser@uoguelph.ca
#SBATCH --mail-type=END

module load samtools/1.12
module load bcftools/1.11


##is the path to the directory with reference genome
reference_path=$1 

##is the path to the directory where the .bcf output file will be placed
output_folder_path=$2

##is the path to the directory with all sorted bam files 
path_to_bam=$3

if [ "$#" -ne 3]; then 
	echo "Must supply 3 paths on the command line"
fi 

echo "Starting variant calling on sorted bam files"

##creating bcf file
bcftools mpileup -a DP,AD --skip-indels -P ILLUMINA -f $reference_path $path_to_bam*.sorted.bam -o $output_folder_path/out.bcf #add *.sorted.bam after path_to_bam for more than 1 file

##creating vcf file 
bcftools call -m --variants-only --format-fields GQ --skip-variants indels $output_folder_path/out.bcf  | bcftools filter --set-GTs . --include 'QUAL > 19 && FMT/GQ >9' | bcftools view --min-alleles 2 --max-alleles 2 --types snps --apply-filter "PASS" --output-type v --output-file $output_folder_path/out.vcf

echo "That's a wrap! Great work on set today guys!"
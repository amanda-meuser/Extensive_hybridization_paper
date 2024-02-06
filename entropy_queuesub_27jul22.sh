#!/bin/bash

## Bash script for submitting a job to the SHARCNet Cedar queue (modified back to use on Graham)
## Usage: sbatch entropy_queuesub_27jul22.sh mpgl_file k_number rep_number ldak_file (OR USE THE LOOPING SCRIPT)
## sbatch ../entropy_queuesub_27jul22.sh /project/def-emandevi/hybrid_ameuser_cedar_testing/EGM19_CCxBND/EGM19_target_27jul22_miss0.4_mac3_Q30_DP3_ind95_maf001_CCxBND.recode.mpgl 2 1 /project/def-emandevi/hybrid_ameuser_cedar_testing/EGM19_CCxBND/qk2inds.txt

### ---------- Job configuration --------------------------------------------

# Run dependent and permanent parameters
# will be run on complete nodes NOT partial

#SBATCH --nodes=1                       # number of nodes to use
#SBATCH --time=25-00:00:00              # time (DD-HH:MM:SS)
##SBATCH --time=00-12:00:00 		        # short time for testing
#SBATCH --account=rrg-emandevi          # account name
#SBATCH --job-name="entropy_all"            # name to display in queue
#SBATCH --ntasks-per-node=1             # taks per node (one core per node)
#SBATCH --mem=8000M                     # memory per node
##SBATCH --output=res_long_6-%j.log     # log file
##SBATCH --error=res_long_6-%j.err      # error file
#SBATCH --mail-user=ameuser@uoguelph.ca # who to email
#SBATCH --mail-type=ALL                 # when to email

# Load required modules - hdf5/1.8.18 gsl/2.5

module load nixpkgs/16.09  intel/2017.1 gsl/2.3 hdf5/1.8.18

### ---------- Useful job infoformation -------------------------------------

echo "Current working directory: $PWD"
echo "starting run at: $(date)"
echo "------------------------------------------------"
echo "job is running on node: $HOSTNAME"
echo "------------------------------------------------"
echo "Job identifier is $SLURM_JOB_ID"
echo "Job name is $SLURM_JOB_NAME"

### ---------- Filtering options --------------------------------------------

# USE: function Dead kills the process if files have not been created that are
# required for further filtering
dead () {

  # Prints warning message that process is about to be terminated
  echo "********** WARNING: EARLY TERMINATION **********"
  echo "File not found, terminating process."

  # Quits running program
  exit 1
}

### ---------- Main ---------------------------------------------------------



# set working directory
WORKDIR=$PWD


# Path to entropy program
ENTROPY="/project/rrg-emandevi/bin/entropy"

# Path to mpgl file
IN_FILE=$1
echo "Path to MPGL file: $IN_FILE"
PREFIX=$(basename $IN_FILE .recode.mpgl)
echo "MPGL file without path: $PREFIX"

# Path to ldak file
LDAK_FILE=$4
echo "Path to LDAK file: $LDAK_FILE"
SUFFIX=$(basename $LDAK_FILE .txt)
echo "LDAK file without path: $SUFFIX"


echo "starting entropy"

# Entropy call
# k and rep supplied by command line arguments, potentially fed in by loop_queuesub.sh
k=$2
rep=$3

# MAKE SURE TO UPDATE BOTH 83 AND 85 IF CHANGING PARAMETERS
entropyrun="$ENTROPY -i $IN_FILE -l 150000 -b 125000 -t 25 -k $k -o $PREFIX\_k$k\_150k_rep$rep\_$SUFFIX.hdf5 -n 2 -m 1 -w 0 -q $LDAK_FILE -Q 0 -r $RANDOM" # this line is printing our parameters to the log file
echo $entropyrun
$ENTROPY -i $IN_FILE -l 150000 -b 125000 -t 25 -k $k -o $PREFIX\_k$k\_150k_rep$rep\_$SUFFIX.hdf5 -n 2 -m 1 -w 0 -q $LDAK_FILE -Q 0 -r $RANDOM # this is telling the computer what to do 

echo "entropy run done. Results in '$PREFIX'_150k_rep'$rep'_$SUFFIX.hdf5"




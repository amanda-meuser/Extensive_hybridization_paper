#!/bin/bash

##USAGE: sbatch startingvals_loop_queuesub.sh k rep /path/to/mpgl_and_ldak
##example: sbatch ../../startingvals_loop_queuesub.sh 8 3 /project/rrg-emandevi/hybrid_ameuser/AMP22/starting_values_entropy
    ## Tips:
    ## modify k and starting values script/path on command line
    ## do NOT put a / at the end of the path on command line

### ---------- Job configuration --------------------------------------------

# Run dependent and permanent parameters
# will be run on complete nodes NOT partial

#SBATCH --nodes=1                       # number of nodes to use            
#SBATCH --time=00-0:15:00 		        # time (DD-HH:MM:SS)
#SBATCH --account=rrg-emandevi          # account name
#SBATCH --job-name="loop_script"            # name to display in queue
#SBATCH --ntasks-per-node=1             # taks per node (one core per node)
#SBATCH --mem=4000M                     # memory per node
#SBATCH --mail-user=ameuser@uoguelph.ca # who to email
#SBATCH --mail-type=ALL                 # when to email



for k in $(seq 1 $1) ##start at 1, sequentially go up by 1 till it gets to value of $1
do
    for rep in $(seq 1 $2)
    do 
        ##echo $k $rep #was for testing!
        sbatch /project/rrg-emandevi/hybrid_ameuser/entropy_queuesub_27jul22.sh $3/*.mpgl $k $rep $3/qk"$k"inds.txt  
    done
done


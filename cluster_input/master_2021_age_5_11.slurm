#!/bin/bash
#SBATCH --job-name=master_submit
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/tester/master.%J.err
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/tester/master.%J.out
#SBATCH --time=00:10:00
#SBATCH --ntasks=1
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


echo "Submitting Step 1..."
JOBID1=$(sbatch --parsable tester2.slurm)
echo "Step 1 submitted with ID $JOBID1"

echo "Submitting Step 2 with dependency on Step 1 (afterok:$JOBID1)..."
JOBID2=$(sbatch --parsable --dependency=afterok:$JOBID1 tester.slurm)
echo "Step 2 submitted with ID $JOBID2"

echo "All jobs submitted."


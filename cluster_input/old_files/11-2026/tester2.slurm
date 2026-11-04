#!/bin/bash
#SBATCH --job-name=1min_test
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/tester/job.%J.err
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/tester/job.%J.out
#SBATCH --time=00:10:00   # Set the time limit to 10 minutes (HH:MM:SS)
#SBATCH --nodes=1
#SBATCH --ntasks=1

echo "Job started at: $(date)"

# The job can complete naturally before the time limit
echo "Sleeping for 60 seconds..."
sleep 60

echo "Job finished at: $(date)"

# When the script ends, the job is automatically terminated by Slurm


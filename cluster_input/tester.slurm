#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mem-per-cpu=5G
#SBATCH --time=20:00:00
#SBATCH --partition=day-long-cpu
#SBATCH --job-name=APSVE_dataset_workflow
#SBATCH --array=1-2
#SBATCH --cores-per-socket=1
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/tester/job.%J.err
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/tester/job.%J.out
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


module load R/4.2.2

Rscript /home/amoor53/APSVE_cluster/code/Tester.R $SLURM_ARRAY_TASK_ID --no-restore --no-save -- quiet


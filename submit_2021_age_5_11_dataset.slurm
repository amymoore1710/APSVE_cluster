#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mem-per-cpu=1G
#SBATCH --time=06:00:00
#SBATCH --partition=short-cpu
#SBATCH --job-name=APSVE_dataset_workflow
#SBATCH --error=/cluster_output/job.%J.err
#SBATCH --output=/cluster_output/job.%J.out
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


module load R/4.2.2

Rscript /home/amoor53/APSVE_cluster/code/2021_age_5_11_01_dataset_workflow.R --no-restore --no-save -- quiet

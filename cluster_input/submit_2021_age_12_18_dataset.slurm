#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=5G
#SBATCH --time=20:00:00
#SBATCH --partition=day-long-cpu
#SBATCH --job-name=APSVE_dataset_workflow_2021_ages_12_18
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/'2025-12-01 - 2021_ages_12_18'/job.%J.err
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/'2025-12-01 - 2021_ages_12_18'/job.%J.out
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


module load R/4.2.2

Rscript /home/amoor53/APSVE_cluster/code/2021_age_12_18_01_dataset_workflow.R --no-restore --no-save -- quiet




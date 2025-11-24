#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=6
#SBATCH --mem-per-cpu=5G
#SBATCH --time=20:00:00
#SBATCH --partition=day-long-cpu
#SBATCH --job-name=APSVE_TND_models_2021_ages_12_18
#SBATCH --array=1-6
#SBATCH --cores-per-socket=1
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/'2025-11-24 - 2021_ages_12_18'/job.%J.err
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/'2025-11-24 - 2021_ages_12_18'/job.%J.out
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


module load R/4.2.2

Rscript /home/amoor53/APSVE_cluster/code/2021_age_12_18_03_TND_model_fitting_parallel.R $SLURM_ARRAY_TASK_ID --no-restore --no-save -- quiet




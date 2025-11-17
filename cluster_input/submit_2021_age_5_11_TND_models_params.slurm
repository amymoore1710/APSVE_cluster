#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=5G
#SBATCH --time=1:00:00
#SBATCH --partition=day-long-cpu
#SBATCH --job-name=APSVE_TND_params
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/'2025-11-17 - 2021_ages_5_11'/job.%J.err
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/'2025-11-17 - 2021_ages_5_11'/job.%J.out
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


module load R/4.2.2

Rscript /home/amoor53/APSVE_cluster/code/2021_age_5_11_02_TND_model_params.R --no-restore --no-save -- quiet


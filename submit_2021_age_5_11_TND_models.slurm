#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mem-per-cpu=1G
#SBATCH --time=20:00:00
#SBATCH --partition=day-long-cpu 
#SBATCH --job-name=APSVE_dataset_workflow
#SBATCH --error=job.%J.err
#SBATCH --output=job.%J.out
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


module load R/4.2.2

Rscript /home/amoor53/APSVE_cluster/code/2021_age_5_11_02_TND_model_fitting.R --no-restore --no-save -- quiet

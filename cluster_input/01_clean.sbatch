#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=5G
#SBATCH --time=20:00:00
#SBATCH --partition=day-long-cpu
#SBATCH --job-name=APSVE_01_cleaning
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/'2026-11-03'/01_clean.%J.out
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/'2026-11-03'/01_clean.%J.err
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL

module load R/4.2.2                 # adjust to your cluster (or activate your conda/renv setup)
Rscript /home/amoor53/APSVE_cluster/code/01_cleaning.R 
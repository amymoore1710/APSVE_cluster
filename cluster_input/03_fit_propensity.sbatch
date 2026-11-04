#!/bin/bash
#SBATCH --array=1-4
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=20:00:00
#SBATCH --partition=day-long-cpu
#SBATCH --job-name=APSVE_03_propensity
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/'2026-11-03'/03_ps_fit_%A_%a.out
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/'2026-11-03'/03_ps_fit_%A_%a.err
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


module load R/4.2.2
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK   # avoids threads oversubscribing the cores you requested
Rscript /home/amoor53/APSVE_cluster/code/03_fit_propensity.R
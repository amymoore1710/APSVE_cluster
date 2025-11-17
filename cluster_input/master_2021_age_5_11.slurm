#!/bin/bash
#SBATCH --job-name=APSVE_master
#SBATCH --error=/home/amoor53/APSVE_cluster/cluster_output/'2025-11-17 - 2021_ages_5_11'/master.%J.err
#SBATCH --output=/home/amoor53/APSVE_cluster/cluster_output/'2025-11-17 - 2021_ages_5_11'/master.%J.out
#SBATCH --time=00:10:00
#SBATCH --ntasks=1
#SBATCH --mail-user=amoor53@emory.edu
#SBATCH --mail-type=ALL


echo "Submitting Step 1..."
JOBID1=$(sbatch --parsable submit_2021_age_5_11_dataset.slurm)
echo "Step 1 submitted with ID $JOBID1"

echo "Submitting Step 2 with dependency on Step 1 (afterok:$JOBID1)..."
JOBID2=$(sbatch --parsable --dependency=afterok:$JOBID1 submit_2021_age_5_11_TND_models_params.slurm)
echo "Step 2 submitted with ID $JOBID2"

echo "Submitting Step 3 with dependency on Step 2 (afterok:$JOBID2)..."
JOBID3=$(sbatch --parsable --dependency=afterok:$JOBID2 submit_2021_age_5_11_TND_models_parallel.slurm)
echo "Step 3 submitted with ID $JOBID3"

echo "Submitting Step 4 with dependency on Step 1 (afterok:$JOBID1)..."
JOBID4=$(sbatch --parsable --dependency=afterok:$JOBID1 submit_2021_age_5_11_TTE_dataset.slurm)
echo "Step 4 submitted with ID $JOBID4"

echo "Submitting Step 5 with dependency on Step 4 (afterok:$JOBID4)..."
JOBID5=$(sbatch --parsable --dependency=afterok:$JOBID4 submit_2021_age_5_11_TTE_models_params.slurm)
echo "Step 5 submitted with ID $JOBID5"

echo "Submitting Step 6 with dependency on Step 5 (afterok:$JOBID5)..."
JOBID6=$(sbatch --parsable --dependency=afterok:$JOBID5 submit_2021_age_5_11_TTE_models_parallel.slurm)
echo "Step 6 submitted with ID $JOBID6"


echo "All jobs submitted."


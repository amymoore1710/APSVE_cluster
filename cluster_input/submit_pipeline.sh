#!/bin/bash
# slurm/submit_pipeline.sh  -- run with: bash cluster_input/submit_pipeline.sh (from the project root)
set -euo pipefail

j1=$(sbatch --parsable cluster_input/01_clean.sbatch)
j2=$(sbatch --parsable --dependency=afterok:$j1 --kill-on-invalid-dep=yes cluster_input/02_build_weekly.sbatch)
j3=$(sbatch --parsable --dependency=afterok:$j2 --kill-on-invalid-dep=yes cluster_input/03_fit_propensity.sbatch)
j4=$(sbatch --parsable --dependency=afterok:$j3 --kill-on-invalid-dep=yes cluster_input/04_match.sbatch)
j5=$(sbatch --parsable --dependency=afterok:$j4 --kill-on-invalid-dep=yes cluster_input/05_fit_ve_models.sbatch)

echo "Submitted: $j1 -> $j2 -> $j3 -> $j4 -> $j5"
#!/bin/bash

#### common SBATCH options
#SBATCH --job-name=esp
#SBATCH --partition=lin_bat
#SBATCH --output=ldl_full_cohort.slog
#SBATCH --error=ldl_full_cohort.error
srun R312 CMD BATCH --no-save --no-restore ldl_full_cohort.R


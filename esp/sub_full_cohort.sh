#!/bin/bash

#### common SBATCH options
#SBATCH --job-name=esp
#SBATCH --partition=lin_bat
#SBATCH --output=full_cohort.slog
#SBATCH --error=full_cohort.error
srun R312 CMD BATCH --no-save --no-restore full_cohort.R


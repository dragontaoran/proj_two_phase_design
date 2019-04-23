#!/bin/bash

#### common SBATCH options
#SBATCH --job-name=esp
#SBATCH --partition=lin_bat
#SBATCH --output=ldl_analysis.slog
#SBATCH --error=ldl_analysis.error
srun R312 CMD BATCH --no-save --no-restore ldl_analysis.R


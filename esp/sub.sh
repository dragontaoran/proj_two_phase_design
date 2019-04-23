#!/bin/bash

#### common SBATCH options
#SBATCH --job-name=esp
#SBATCH --partition=lin_bat
#SBATCH --output=analysis.slog
#SBATCH --error=analysis.error
srun R312 CMD BATCH --no-save --no-restore analysis.R


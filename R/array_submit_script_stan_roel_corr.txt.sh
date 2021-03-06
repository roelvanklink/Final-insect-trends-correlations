#!/bin/bash

#SBATCH --time 0-06:00:00

#SBATCH --cpus-per-task=1

#SBATCH --mem-per-cpu=8G

#SBATCH --job-name=Roel_taxa_cors_sd1

#SBATCH -o /work/%u/%x-%A-%a.txt

#SBATCH --array=1-71

#SBATCH --mail-type=ALL

echo "processing chunk $SLURM_ARRAY_TASK_ID ..."

module load foss/2020b R/4.0.4-2

mkdir -p /work/$USER/$SLURM_JOB_NAME/$SLURM_ARRAY_JOB_ID
cd /work/$USER/$SLURM_JOB_NAME/$SLURM_ARRAY_JOB_ID

Rscript /data/idiv_chase/vanKlink/simple_correlations_brms.R

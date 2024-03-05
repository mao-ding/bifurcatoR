#!/bin/bash
#SBATCH --job-name=module2_norm_weib_Distribution
#SBATCH --partition=long
#SBATCH --export=ALL
#SBATCH --nodes 1
#SBATCH --cpus-per-task=1
#SBATCH --ntasks 32
#SBATCH --mem 5G
#SBATCH --time 120:00:00
##SBATCH --mail-type=ALL  #usually suppress this option for array jobs
##SBATCH --mail-user=ma.ding@northeastern.edu
#SBATCH --array=1-5


# load modules
# there isn't a R module for version 4.2.1 but we can run R from the same container that you've been using for the OOD app

module load singularity/3.10.3

# run your rscript from the container. We are also binding the scratch and work directories to the container so we can access them when we run the R script.

singularity run -B "/scratch:/scratch,/work:/work" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript Module2_Norm_Weib_Distribution.R $SLURM_ARRAY_TASK_ID
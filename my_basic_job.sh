#!/bin/bash

#SBATCH -c 5  
#SBATCH -p shared 
#SBATCH --mem=64000 
#SBATCH -t 0-20:00:00 
#SBATCH -o myoutput_binmodother_6.11_%j.out 
#SBATCH -e myerrors_binmodother_6.11_%j.err  
#SBATCH --mail-type=BEGIN,END,FAIL  # Mail notifications
#SBATCH --mail-user=crodriguezcabrera@g.harvard.edu   # Account to email


#Load R and the environment
module load R/4.3.1-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.3.1:$R_LIBS_USER

# Change the directory to where your scripts are
cd $HOME/ecbayesbinmix/

#Run the R script
Rscript run_binmodels_other.R

#!/bin/bash

#SBATCH -c 8  
#SBATCH -p shared 
#SBATCH --mem=64000 
#SBATCH -t 0-35:00:00 
#SBATCH -o myoutput_updatedmods14v_4.16_%j.out 
#SBATCH -e myerrors_updatedmods14v_4.16_%j.err  
#SBATCH --mail-type=BEGIN,END,FAIL  # Mail notifications
#SBATCH --mail-user=crodriguezcabrera@g.harvard.edu   # Account to email


#Load R and the environment
module load R/4.3.1-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.3.1:$R_LIBS_USER

# Change the directory to where your scripts are
cd $HOME/ecbayesbinmix/Model_run_updates/

#Run the R script
Rscript UpdatedModels_04162025.R

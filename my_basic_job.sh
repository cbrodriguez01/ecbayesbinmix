#!/bin/bash

#SBATCH -c 5  
#SBATCH -p shared 
#SBATCH --mem=64000 
#SBATCH -t 0-5:00:00 
#SBATCH -o myoutput_%j.out 
#SBATCH -e myerrors_%j.err  
#SBATCH --mail-type=END  
#SBATCH --mail-user=crodriguezcabrera@g.harvard.edu  

#Load R and the environment
$HOME/load_R.sh

# Change the directory to where your scripts are
cd $HOME/ecbayesbinmix/

#Run the R script
Rscript run_binmodels_save_output.R

#!/bin/bash
#SBATCH -J ACS_dataset_        # Job name for the array
#SBATCH -o ACS_dataset_%A.out  # Shared standard output with job ID
#SBATCH -e ACS_dataset_%A.err  # Shared standard error with job ID
#SBATCH -p shared      # Partition to submit to
#SBATCH -n 1	       # Number of cores
#SBATCH -t 0-12:00:00  # Runtime (D-HH:MM:SS)
#SBATCH --mem=5000     # Memory request
#SBATCH --mail-type=BEGIN,END,FAIL  # Mail notifications
#SBATCH --mail-user=crodriguezcabrera@g.harvard.edu   # Account to email

module load R/4.3.1-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.3.1:$R_LIBS_USER
heats=111211
Rscript  TroubleshootingHeats.R ${dataset}${SLURM_ARRAY_TASK_ID}
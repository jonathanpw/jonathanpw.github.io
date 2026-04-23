#!/bin/tcsh
#BSUB -J my_job_name[1-3]  #job name AND job array
#BSUB -n 8                  #number of cores
#BSUB -R span[hosts=1]       #distribute across 1 node
#BSUB -W 00:01               #walltime limit: hh:mm
#BSUB -o /home/jwilli27/Output_%J_%I.out 
#BSUB -e /home/jwilli27/Error_%J_%I.err  #error - %J is the job-id %I is the job-array index 

module load R

Rscript script_example.r $LSB_JOBINDEX
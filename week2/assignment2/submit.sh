#!/bin/bash
#BSUB -J poisson
#BSUB -o poisson.out
#BSUB -q hpcintro
#BSUB -n 24
#BSUB -W 59

module add studio/12u6

for N in {1..24};do OMP_WAIT_POLICY=active OMP_NUM_THREADS=$N ./runme; done

#PBS -r n
#PBS -N hello-test
#PBS -l nodes=1:ppn=2
#PBS -j oe
#PBS -l walltime=00:10:30
#NPROCS=`wc -l < $PBS_NODEFILE`
#echo "NPROCS = " $NPROCS
#module add mpi/studio
#pwd
#cd /xbar/nas2/home3/mek/jhwa/example/
#mpirun -np $NPROCS myexefile


for i in {1..18}
do
mpirun -np 2 runme $((2**$i))

done

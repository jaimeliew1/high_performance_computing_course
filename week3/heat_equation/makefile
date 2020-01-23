
#----------------------------------------------------------------------
#  Makefile : heat equation
#  Version  : 1.0
#  Author   : Jaime Liew (jyli@dtu.dk)
#  Created  : 10/1/2020
#----------------------------------------------------------------------
# macro defining the target ie executable
target = runme
FC = mpifort
FFLAGS = -ffree-form -O3
#----------------------------------------------------------------------
#  Default target
#----------------------------------------------------------------------
all: $(target)

#----------------------------------------------------------------------
#  Object files:
#  NOTE: you HAVE to sort the objects files such that no file will
#  depend on files below it ! in this example, the diffuse2.f and .o
#  depends on all he module files (i named them m_*.f), and the m_init
#  depends (USE) the m_diffuse; thus m_diffuse HAS to be compiled
#  before m_init and before diffuse2
#----------------------------------------------------------------------
objs = m_global.o\
			 m_exchange_boundary.o\
			 m_save_output.o\
			 m_update_memory.o\
			 m_step.o\
			 m_alloc.o\
			 m_init.o\
			 m_diagnostic.o\
			 m_read_input.o\
			  main.o\

#----------------------------------------------------------------------
#  Dependencies:
#----------------------------------------------------------------------
%.o: %.f90 makefile
	$(FC) $(FFLAGS) -c $<


#----------------------------------------------------------------------
#  link
# if objects are newer than the arget they are re-linked
#----------------------------------------------------------------------

$(target):$(objs) makefile
	$(FC) $(FFLAGS) $(objs) -o $(target)

clean:
	rm -fr $(objs)
	rm -fr *.o
	rm -fr *.mod

cleandata:
	rm -fr diff*.dat
	rm -fr *.dat


run:
	mpirun -np 16 $(target)
run1:
	mpirun -np 1 $(target)
run2:
	mpirun -np 2 $(target)
run4:
	mpirun -np 4 $(target)
run5:
	mpirun -np 5 $(target)


animate:
	python animate.py


plot:
	python plot.py


#----------------------------------------------------------------------
#  Makefile : montecarlo_pi
#  Version  : 1.0
#  Author   : Jaime Liew (jyli@dtu.dk)
#  Created  : 22/1/2020
#----------------------------------------------------------------------
# macro defining the target ie executable
target = runme
FC = gfortran
FFLAGS = -ffree-form#-free -O3
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
objs =   main.o\

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

run:
	./$(target)
#mpirun -np 16 $(target)


#----------------------------------------------------------------------
#  Makefile : OpenMP Assignment
#  Version  : 1.0
#  Author   : Jaime Liew (jyli@dtu.dk)
#  Created  : 15/1/2020
#----------------------------------------------------------------------
# macro defining the target ie executable
target = runme
FC = gfortran
FFLAGS = -ffree-form #fopenmp
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
			m_alloc.o\
			m_init.o\
			m_read_input.o\
			m_diagnostic.o\
			m_jacobi.o\
			m_gauss_seidel.o\
			m_save_output.o\
			main.o\


#----------------------------------------------------------------------
#  Dependencies:
#----------------------------------------------------------------------
%.o: %.f90
	$(FC) $(FFLAGS) -c $<


#----------------------------------------------------------------------
#  link
# if objects are newer than the arget they are re-linked
#----------------------------------------------------------------------

$(target):$(objs)
	$(FC) $(FFLAGS) $(objs) -o $(target)

clean:
	rm -fr $(objs)
	rm -fr diff*.dat
	rm -fr *.dat
	rm -fr *.o
	rm -fr *.mod

run:
	./$(target)


plot:
	python single_plot.py

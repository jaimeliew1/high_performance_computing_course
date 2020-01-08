
#macro defining the target ie executable
target = runme
FC = gfortran
FFLAGS = -ffree-form
# macro defininf the list of object files
# objects are recompiled in the order in which they appear in the list
# NOTE: TABS not required while defining macros
objs = main.o\
			 m_global.o\
			 s_init.o\
			 s_save_output.o\
			 s_update_memory.o\
			 f_step.o\
			 s_alloc.o\


# linking: the target depends on the objects
#if objects are newer than the arget they are re-linked
$(target):$(objs)
	$(FC) $(FFLAGS) $(objs) -o $(target)

#dependencies:
main.o: main.f90 m_global.mod
	$(FC) $(FFLAGS) -c main.f90

m_global.mod: m_global.o

# The following is shorthand for the commented out section below.
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# s_init.o: s_init.f90
# 	$(FC) $(FFLAGS) -c s_init.f90
# s_save_output.o: s_save_output.f90
# 	$(FC) $(FFLAGS) -c s_save_output.f90
# s_update_memory.o: s_update_memory.f90
# 	$(FC) $(FFLAGS) -c s_update_memory.f90
# f_step.o: f_step.f90
# 	$(FC) $(FFLAGS) -c f_step.f90
# m_global.o: m_global.f90
# 	$(FC) $(FFLAGS) -c m_global.f90
# s_alloc.o: s_alloc.f90
# 	$(FC) $(FFLAGS) -c s_alloc.f90



clean:
	rm -fr $(objs)
	rm -fr *.dat
	rm -fr *.o
	rm -fr *.mod

# Example of running other targets from a target using the inbuilt $(MAKE) macro
run:
	./$(target)
	$(MAKE) animate
	$(MAKE) clean

animate:
	python animate.py

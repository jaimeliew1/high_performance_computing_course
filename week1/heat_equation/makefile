
#macro defining the target ie executable
target = runme
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
	gfortran -ffree-form $(objs) -o $(target)

#dependencies:
main.o: main.f90 m_global.mod
	gfortran -ffree-form -c main.f90

m_global.mod: m_global.o

s_init.o: s_init.f90
	gfortran -ffree-form -c s_init.f90
s_save_output.o: s_save_output.f90
	gfortran -ffree-form -c s_save_output.f90
s_update_memory.o: s_update_memory.f90
	gfortran -ffree-form -c s_update_memory.f90
f_step.o: f_step.f90
	gfortran -ffree-form -c f_step.f90
m_global.o: m_global.f90
	gfortran -ffree-form -c m_global.f90
s_alloc.o: s_alloc.f90
	gfortran -ffree-form -c s_alloc.f90



clean:
	rm -fr $(objs)
	rm -fr *.dat
	rm -fr *.o
	rm -fr *.mod


run:
	./runme

animate: 
	python animate.py

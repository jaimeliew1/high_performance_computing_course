
#macro defining the target ie executable
target = runme
# macro defininf the list of object files
# objects are recompiled in the order in which they appear in the list
# NOTE: TABS not required while defining macros
objs = module_file.o\
       main.o\


# linking: the target depends on the objects
#if objects are newer than the arget they are re-linked
$(target):$(objs)
	gfortran -ffree-form $(objs) -o $(target)

#dependencies:
main.o: main.f90 m_global.mod m_routines.mod
	gfortran -ffree-form -c main.f90

m_routines.mod: module_file.f90
	gfortran -ffree-form -c module_file.f90
m_global.mod: module_file.f90
	gfortran -ffree-form -c module_file.f90


clean:
	rm -fr $(objs)
	rm -fr *.dat

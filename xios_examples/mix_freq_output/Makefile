# Make file for the multiple time step demonstartion XIOS programme
# Targets provided our detailed below...
#
# all: (default) Build the multiple_timestep programme
# clean: Delete all final products and working files
# run: run the programme
#
# Environment Variables expected by this MakeFile:
#
# NETCDF_LIBDIR: the directories for the netCDF lib files
#                encoded as a -L string, e.g.
#                "-L/dir1 -L/dir2"
# NETCDF_INCDIR: the directories for the netCDF include files
#                encoded as a -I string, e.g.
#                "-I/dir3 -I/dir4"
#     (note, this is for consistency with the XIOS build process
#      required for the CI build machine.
#      this is not required for other directories)
#
# XIOS_INCDIR: The directory for XIOS include files
# XIOS_LIBDIR: The directory for XIOS lib files
# XIOS_BINDIR: The directory for XIOS binary files

FCFLAGS = -g

FC = mpif90       # compiler driver for MPI programs

# compiler flag, includes
FCFLAGS += -I$(XIOS_INCDIR) $(NETCDF_INCDIR)

# loader flags
LDFLAGS = \
	-L$(XIOS_LIBDIR) \
	$(NETCDF_LIBDIR) \
	-lxios \
	-lnetcdf \
	-lnetcdff \
	-lstdc++ 

.PHONY: all, clean, run

all: multiple_timestep

# fortran compilation
%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

# fortran linking
multiple_timestep: multiple_timestep.o
	$(FC) -o multiple_timestep.exe multiple_timestep.o $(LDFLAGS) \
		&& ln -fs $(XIOS_BINDIR)/xios_server.exe .

run:
	mpiexec -n 1 ./multiple_timestep.exe : -n 1 ./xios_server.exe

# cleanup
clean:
	rm -f *.exe *.o *.mod *.MOD *.out *.err *.nc

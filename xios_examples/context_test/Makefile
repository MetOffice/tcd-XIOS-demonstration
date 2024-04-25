# Make file for the resample demonstartion XIOS programme
# Targets provided our detailed below...
#
# all: (default) Build the context programme
# clean: Delete all final products and working files
# run: run the programme
#
# Environment Variables expected by this MakeFile:
#
# NETCDF_LIB_DIR: the directories for the netCDF lib files
#                 encoded as a -L string, e.g.
#                 "-L/dir1 -L/dir2"
# NETCDF_INC_DIR: the directories for the netCDF include files
#                 encoded as a -I string, e.g.
#                 "-I/dir3 -I/dir4"
#     (note, this is for consistency with the XIOS build process
#      required for the CI build machine.
#      this is not required for other directories)
#
# XIOS_INCDIR: The directory for XIOS include files
# XIOS_LIBDIR: The directory for XIOS lib files
# XIOS_BINDIR: The directory for XIOS binary files

FCFLAGS = -g -ffree-line-length-none

FC = mpif90 # compiler driver for MPI programs

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

OBJ = custom_type_mod.o context_def_mod.o

all: context_def_test

custom_type_mod.o: custom_type_mod.F90
	$(FC) $(FCFLAGS) -c $<

context_def_mod.o: context_def_mod.F90 custom_type_mod.o
	$(FC) $(FCFLAGS) -c $<

context_def_test.o: context_def_test.F90 custom_type_mod.o context_def_mod.o
	$(FC) $(FCFLAGS) -c $<

# fortran linking
context_def_test: context_def_test.o $(OBJ)
	$(FC) -o context_def_test.exe context_def_test.o $(OBJ) $(LDFLAGS) \
		&& ln -fs $(XIOS_BINDIR)/xios_server.exe .

run:
	mpiexec -n 1 ./context_def_test.exe : -n 1 ./xios_server.exe

clean:
	rm -f *.exe *.o *.mod *.MOD *.out *.err *.nc

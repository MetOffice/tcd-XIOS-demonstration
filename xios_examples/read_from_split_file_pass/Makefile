# Make file for the multiple time step demonstartion XIOS programme
# Targets provided our detailed below...
#
# all: (default) Build the split file reading programme
# clean: Delete all final products and working files
# run: run the programme
#
# Environment Variables expected by this MakeFile:
#
# FC: mpif90
# FCFLAGS: -g & include files for netcdf & xios
# LD_FLAGS: for xios, netcdf, netcdff, stfc++
# LD_LIBRARY_PATH: for netCDF & XIOS libs
# XIOS_BINDIR: The directory for XIOS binary files


.PHONY: all, clean, run

all: split_file_test

# fortran compilation
%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

# fortran linking
split_file_test: split_file_test.o
	$(FC) -o split_file_test.exe split_file_test.o $(LDFLAGS) \
		&& ln -fs $(XIOS_BINDIR)/xios_server.exe .

run:
	mpiexec -n 1 ./split_file_test.exe : -n 1 ./xios_server.exe

# cleanup
clean:
	rm -f *.exe *.o *.mod *.MOD *.err *.out *.nc

# Make file for the resample demonstartion XIOS programme
# Targets provided our detailed below...
#
# all: (default) Build the resample programme
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

all: client

# fortran compilation
%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

# fortran linking
client: client.o
	$(FC) -o client.exe client.o $(LDFLAGS) \
		&& ln -fs $(XIOS_BINDIR)/xios_server.exe .

run:
	mpiexec -n 1 ./client.exe : -n 1 ./xios_server.exe

# cleanup
clean:
	rm -f *.exe *.o *.mod *.MOD *.out *.err
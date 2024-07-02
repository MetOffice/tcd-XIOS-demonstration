# Makefile intital

CC = cc

all: quantize

quantize: quantize.c quantize.h quantize_params.c
	$(CC) quantize.c -lnetcdf -o quantize.exe

quantize_github: quantize.c quantize.h quantize_params.c
	$(CC) quantize.c -lnetcdf -lm -o quantize.exe



clean:
	rm -rf *exe core *nc

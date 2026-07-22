Read axis values
------

This example demonstrates a bug when reading axis values from an input file.

The configuration file `main.xml` defines two axis `x` and `y`, but does not
set their dimensions. Instead the client program (`client.F90`) sets this using
the fortran interface (lines 115 and 116). Both axis are set to have values:
`[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]`.

However, the configuration includes an input file `input_data.nc`. When closing
the context definition, XIOS reads the axis values from this file which should
be: `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`. However, the output file `output_data.nc`
ends up containing values `[0, 0, 0, 0, 0, 0, 0, 0, 0, 0]`.

This bug only occurs when `par_access="independent"` is set on the input file.
If `par_access="collective"` is used instead, the test will pass.
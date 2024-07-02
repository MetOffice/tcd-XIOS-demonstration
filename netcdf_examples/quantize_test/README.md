# NetCDF quantize test

The following test demonstrates the functionality of the NetCDF quantize feature, along with the deflate calls. This test requires NetCDF 4.9 or later. The choice of C rather than Fortran was made, as the C interface will be used for any applications within XIOS.

The experiments are defined in the file `quantize_params.c`. There is a definition at the top for variable `NUM_PACKING_PARAMS` which defines the number of individual test files to be written. This sets the size of the array of `struct` `PackingParams`, which is also set in this file. The structure allows for a choice of different quantize methods, and significant digits

```c
struct PackingParams {
  int compress; // perform compression, 1 for compression, 0 for not
  int do_quantize; // perform quantization, 1 for quantization, 0 for not
  int netcdf_quantize_mode; /* Chose netcdf quantization mode, choices:
			       1) NC_QUANTIZE_BITGROOM
			       2) NC_QUANTIZE_GRANULARBR
			       3) NC_QUANTIZE_BITROUND */
  int netcdf_nsd; /* Number of significant digits to preserve. For
		     NC_QUANTIZE_BITGROOM and NC_QUANTIZE_GRANULARBR these are
		     decimal significant figures, for NC_QUANTIZE_BITROUND
		     these are binary signficant figures.
		     Note: 1 decimal sf requires ~3.32 bits. */
  char filename[200]; // Name of file to write to
  char fieldname[200]; // Name of field to write to file
};
```

The actual test code is in the file `quantize.c`. This test uses an analytic function for data, in a 2d array of size `dim1_size` x `dim2_size` set in the `main()` function. The analytic function is $z\left(x,y\right) = x \sin\left( \frac{2 \pi y}{y_\text{max}} \right)$.

We loop over all the items in the `packing_params` array, writing the same data in each file defined by this array, setting up the quantization (if required) using the call to `nc_def_var_quantize()`, and the compression (if required) using the call to `nc_def_var_deflate()`. We gave hard wired the deflate to have no shuffle, and a deflate level of one.

In addition to the writing of data, there are two timer calls, around the functions `nc_put_var_float()`, and `nc_close()`, giving individual and combined times for these two functions to return.

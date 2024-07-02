//How many experiments are we running?
#define NUM_PACKING_PARAMS 5

/* Define our struct to hold the packing parameters. This are populated
   in the define_params function below, allowing a neat way of iterating
   over experiments */
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

struct PackingParams *define_params() {

  //Allocate our array of parameters
  struct PackingParams* param_array;
  param_array = (struct PackingParams*) \
    malloc(NUM_PACKING_PARAMS*sizeof(struct PackingParams));

  // define our parameters for this experiment
  // Reference
  param_array[0].compress = 0;
  param_array[0].do_quantize = 0;
  strcpy(param_array[0].filename, "reference.nc");
  strcpy(param_array[0].fieldname, "field");

  // Reference compress
  param_array[1].compress = 1;
  param_array[1].do_quantize = 0;
  strcpy(param_array[1].filename, "reference_comp.nc");
  strcpy(param_array[1].fieldname, "field");

  // Bitgroom pack and compress, nsd = 3
  param_array[2].compress = 1;
  param_array[2].do_quantize = 1;
  param_array[2].netcdf_quantize_mode = NC_QUANTIZE_BITGROOM;
  param_array[2].netcdf_nsd = 3;
  strcpy(param_array[2].filename, "quant_bg_3_comp.nc");
  strcpy(param_array[2].fieldname, "field");

  // Granular pack and compress, nsd = 3
  param_array[3].compress = 1;
  param_array[3].do_quantize = 1;
  param_array[3].netcdf_quantize_mode = NC_QUANTIZE_GRANULARBR;
  param_array[3].netcdf_nsd = 3;
  strcpy(param_array[3].filename, "quant_gran_3_comp.nc");
  strcpy(param_array[3].fieldname, "field");

  // Bitround pack and compress, nsd = 10bit (3dec)
  param_array[4].compress = 1;
  param_array[4].do_quantize = 1;
  param_array[4].netcdf_quantize_mode = NC_QUANTIZE_BITROUND;
  param_array[4].netcdf_nsd = 10;
  strcpy(param_array[4].filename, "quant_br_10b_comp.nc");
  strcpy(param_array[4].fieldname, "field");


  return param_array;
}

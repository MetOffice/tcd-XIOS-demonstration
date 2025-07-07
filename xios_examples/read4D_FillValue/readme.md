read4D
------

This example reads in a sample NetCDF file of 4D structured data (data payload is `missing`).
XIOS simply reads data from that file into memory.

This is designed to be extensible, enabling much larger files to be read,
across multiple client and server ranks, and performance analyses undertaken

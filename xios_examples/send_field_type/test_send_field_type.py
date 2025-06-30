import copy
import glob
import netCDF4
import numpy as np
import os
import subprocess
import unittest

import xios_examples.shared_testing as xshared

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)

class TestWriteField(xshared._TestCase):
    test_dir = this_dir
    transient_outputs = ['typed_output.nc']
    executable = './write.exe'

    def test_write(self):
        outputfile = self.transient_outputs[0]
        self.run_mpi_xios()

        # load the result netCDF file
        runfile = '{}/{}'.format(self.test_dir, outputfile)
        assert(os.path.exists(runfile))
        rootgrp = netCDF4.Dataset(runfile, 'r')

        # read data from the packed, expected & diff variables
        dblout = rootgrp['double_data'][:]
        realout = rootgrp['float_data'][:]
        self.assertTrue(dblout.dtype == np.dtype('float64') and
                        realout.dtype == np.dtype('float32'))

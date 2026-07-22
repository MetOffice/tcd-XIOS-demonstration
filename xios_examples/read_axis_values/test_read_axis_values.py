from typing import List
from netCDF4 import Dataset
import numpy as np
import numpy.testing as npt
import os

import xios_examples.shared_testing as xshared

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)

class TestReadAxisValues(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = ['input_data.nc']
    transient_outputs = ["output_data.nc"]
    executable = "./client.exe"
    axis_size = 10

    def test_axis_values(self):
        self.make_netcdf("input_data.cdl", self.transient_inputs[0])
        self.run_mpi_xios()
        self.check_axis("x")
        self.check_axis("y")       

    def check_axis(self, axis_name):
        outputfile = '{}/{}'.format(self.test_dir, self.transient_outputs[0])
        rootgrp = Dataset(outputfile, "r")
        axis = rootgrp.variables[axis_name]
        axis_values = axis[:]

        # Calculate expected value and diff
        expected = np.arange(1, self.axis_size + 1, dtype=np.float32)
        diff = axis_values - expected

        # prepare message for failure
        msg = (
            f"{outputfile}[{axis.name}]: the expected result\n {expected}\n"
            f" differs from the actual result\n {axis_values} \n"
            f" with diff \n {diff}\n"
        )

        npt.assert_allclose(axis_values, expected, rtol=self.rtol, err_msg=msg)

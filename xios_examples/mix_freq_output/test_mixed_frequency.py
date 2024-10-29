import copy
import glob
import netCDF4
import numpy as np
import os
import subprocess
import glob
import unittest

import xios_examples.shared_testing as xshared

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)


class MixedFrequency(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = []
    transient_outputs = ["mixed_frequency.nc"]
    rtol = 5e-03

    @unittest.expectedFailure
    def test_mixed_frequency_output(self):
        """
        Check/test the frequency of outputted fields are correct.
        """
        with open("{}/xios.xml".format(self.test_dir)) as cxml:
            print(cxml.read(), flush=True)
        result = subprocess.run(
            [
                "mpiexec",
                "-n",
                "1",
                "./multiple_timestep.exe",
                ":",
                "-n",
                "1",
                "./xios_server.exe",
            ],
            cwd=self.test_dir,
            check=True,
        )

        output_file = "mixed_frequency.nc"
        reference_file = "mixed_frequency_ref.nc"
        cdl_file = "mixed_frequency_ref.cdl"

        subprocess.run(
            ["ncgen", "-k", "nc4", "-o", reference_file, cdl_file],
            cwd=self.test_dir,
            check=True,
        )

        run_file = "{}/{}".format(self.test_dir, output_file)
        comp_file = "{}/{}".format(self.test_dir, reference_file)

        test_results_t_instants = netCDF4.Dataset(run_file, "r")["time_instant"][:]
        expected_t_instants = netCDF4.Dataset(comp_file, "r")["time_instant"][:]
        test_results_p = netCDF4.Dataset(run_file, "r")["pressure"][:]
        expected_p = netCDF4.Dataset(comp_file, "r")["pressure"][:]
        test_results_t = netCDF4.Dataset(run_file, "r")["temperature"][:]
        expected_t = netCDF4.Dataset(comp_file, "r")["temperature"][:]

        msg = (
            "The produced time series data in file {} "
            "differs from that in the reference cdl file {}\n".format(
                output_file, cdl_file
            )
        )

        if (
            not np.array_equal(test_results_t_instants, expected_t_instants)
            or not np.allclose(test_results_p, expected_p, rtol=self.rtol)
            or not np.allclose(test_results_t, expected_t, rtol=self.rtol)
        ):
            # print message for fail case,
            # as expected failures do not report msg.
            print(msg)

        self.assertTrue(
            np.array_equal(test_results_t_instants, expected_t_instants)
            and np.allclose(test_results_p, expected_p, rtol=self.rtol)
            and np.allclose(test_results_t, expected_t, rtol=self.rtol),
            msg=msg,
        )

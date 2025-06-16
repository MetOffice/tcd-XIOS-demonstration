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


class SplitFile(xshared._TestCase):
    test_dir = this_dir
    executable = "./split_file_test.exe"
    rtol = 5e-03

    def test_split_file_output(self):
        """
        Check/test the split across files of outputted fields are correct.
        """
        self.run_mpi_xios()

        with open("{}/xios.xml".format(self.test_dir)) as cxml:
            print(cxml.read(), flush=True)

        cdl_files = [
            os.path.basename(f) for f in glob.glob(self.test_dir + "/split_file_*.cdl")
        ]
        output_files = [f.replace("cdl", "nc") for f in cdl_files]

        for cdl_file, output_file in zip(cdl_files, output_files):

            # Check output file with correct name was produced
            run_file = "{}/{}".format(self.test_dir, output_file)
            self.assertTrue(os.path.exists(run_file))

            # Check time axis values and field values are what is expected
            comp_file = "comp_{}".format(output_file)
            subprocess.run(
                ["ncgen", "-k", "nc4", "-o", comp_file, cdl_file],
                cwd=self.test_dir,
                check=True,
            )
            comp_file = "{}/{}".format(self.test_dir, comp_file)

            test_results_t_instants = netCDF4.Dataset(run_file, "r")["time_instant"][:]
            expected_t_instants = netCDF4.Dataset(comp_file, "r")["time_instant"][:]
            test_results_t = netCDF4.Dataset(run_file, "r")["temperature"][:]
            expected_t = netCDF4.Dataset(comp_file, "r")["temperature"][:]

            msg = (
                "The produced time series data in file {} "
                "differs from that in the reference cdl file {}\n".format(
                    output_file, cdl_file
                )
            )

            self.assertTrue(
                np.array_equal(test_results_t_instants, expected_t_instants)
                and np.allclose(test_results_t, expected_t, rtol=self.rtol),
                msg=msg,
            )

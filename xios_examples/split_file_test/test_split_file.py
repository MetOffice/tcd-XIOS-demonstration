import copy
import glob
from netCDF4 import Dataset
import numpy as np
import os
import subprocess
import unittest

import xios_examples.shared_testing as xshared

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)


class SplitFile(xshared._TestCase):
    test_dir = this_dir
    kgo_cdl_dir = this_dir
    executable = "./split_file_test.exe"
    rtol = 5e-03

    def test_split_file_output(self):
        """
        Check/test the split across files of outputted fields are correct.
        """
        self.run_mpi_xios()

        with open("{}/xios.xml".format(self.test_dir)) as cxml:
            print(cxml.read(), flush=True)

        kgo_cdl_files = [
            "diag_file_2022121313-2022121316.cdl",
            "diag_file_2022121317-2022121320.cdl",
            "diag_file_2022121321-2022121400.cdl",
            "diag_file_2022121401-2022121404.cdl",
            "diag_file_2022121405-2022121408.cdl",
            "diag_file_2022121409-2022121412.cdl",
            "prog_file_2022121313-2022121316.cdl",
            "prog_file_2022121317-2022121320.cdl",
            "prog_file_2022121321-2022121400.cdl",
            "prog_file_2022121401-2022121404.cdl",
            "prog_file_2022121405-2022121408.cdl",
            "prog_file_2022121409-2022121412.cdl",
        ]

        for cdl_file in kgo_cdl_files:

            kgo_file = cdl_file.replace("cdl", "nc")
            kgo_file_path = f"{self.test_dir}/kgo_{kgo_file}"

            # Check output file with same name as kgo file was produced
            output_file = kgo_file
            output_file_path = f"{self.test_dir}/{output_file}"
            self.assertTrue(os.path.exists(output_file_path))

            # Create netcdf kgo file
            subprocess.run(
                [
                    "ncgen",
                    "-k",
                    "nc4",
                    "-o",
                    kgo_file_path,
                    cdl_file,
                 ],
                cwd=self.kgo_cdl_dir,
                check=True,
            )

            # Check time axis values and field values are what is expected
            print(f"Comparing output file {output_file_path} to reference kgo file {kgo_file_path}", flush=True)

            if "prog_file_" in output_file:
               test_results_t_instants = Dataset(output_file_path, "r")["time_instant"][:]
               expected_t_instants = Dataset(kgo_file_path, "r")["time_instant"][:]
               test_results_t = Dataset(output_file_path, "r")["temperature"][:]
               expected_t = Dataset(kgo_file_path, "r")["temperature"][:]

            if "diag_file_" in output_file:
               test_results_t_instants = Dataset(output_file_path, "r")["time_centered"][:]
               expected_t_instants = Dataset(kgo_file_path, "r")["time_centered"][:]
               test_results_t = Dataset(output_file_path, "r")["average_temperature"][:]
               expected_t = Dataset(kgo_file_path, "r")["average_temperature"][:]

            msg = (
                f"Time series data in output file {output_file_path} differs from data in kgo file {kgo_file_path}"
            )

            self.assertTrue(
                np.array_equal(test_results_t_instants, expected_t_instants)
                and np.allclose(test_results_t, expected_t, rtol=self.rtol),
                msg=msg,
            )

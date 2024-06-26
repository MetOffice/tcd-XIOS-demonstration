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


class SplitFile(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = []
    transient_outputs = ["mixed_frequency.nc"]

    def test_split_file_output(self):
        """
        Check/test the split across files of outputted fields are correct.
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
        #result = subprocess.run(
        #    'for file in $(ls -1 split_file_*.nc); do cp $file $file"_saved"; done',
        #    shell=True,
        #    cwd=self.test_dir,
        #    check=True,
        #)
        self.assertTrue(result.returncode == 0)

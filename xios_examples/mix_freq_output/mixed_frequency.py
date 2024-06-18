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

    def test_mixed_frequency_output(self):
        """
        Check/test the frequency of outputted fields are correct.
        """
        self.assertTrue(False)


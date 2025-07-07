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

class TestRead4D(xshared._TestCase):
    test_dir = this_dir
    transient_inputs = ['sample_smaller.nc']
    transient_outputs = []
    executable = './read.exe'

    # There is a bug in the dimensionality reading of structured domains
    # in XIOS2r2252 which is fixed in the newer version of XIOS2.
    # this manifests as:
    # ```In file "nc4_data_input.cpp", function "virtual void 
    # xios::CNc4DataInput::readFieldAttributes_(xios::CField*, bool)",
    # line 154 -> Field 'specific_humidity' has incorrect dimension 
    # Verify dimension of grid defined by 'grid_ref' or 'domain_ref'/'axis_ref' 
    # and dimension of grid in read file.```
    # The dimensionality is correct in the test, but XIOS2r2252 gets this wrong.
    # hence, skip test for this test matrix element.
    @unittest.skipIf(os.environ.get('MVER', '') == 'XIOS/trunk@2252',
                     "skipping for ")
    def test_read_4d(self):
        inputfile = self.transient_inputs[0]
        infile = inputfile.replace('.nc', '.cdl')
        
        subprocess.run(['ncgen', '-k', 'nc4', '-o', inputfile,
                        infile], cwd=self.test_dir, check=True)
        self.run_mpi_xios()

    @classmethod
    def setUpClass(cls):
        if os.environ.get('MVER', '').startswith('XIOS3/trunk'):
            with open(os.path.join(cls.test_dir, 'read.F90'), 'r') as ioin:
                iodef_in = ioin.read()
            # patch in XIOS3 domain access source code syntax
            in2 = 'xios_get_domain_attr("original_domain"'
            in3 = ('xios_get_domain_attr("specific_humidity::"')
            iodef_out = iodef_in.replace(in2, in3)
            with open(os.path.join(cls.test_dir, 'read.F90'), 'w') as ioout:
                ioout.write(iodef_out)
        super().setUpClass()

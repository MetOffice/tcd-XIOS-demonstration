import copy
import glob
import netCDF4
import numpy
import os
import sys
import subprocess
import unittest

this_path = os.path.realpath(__file__)
this_dir = os.path.dirname(this_path)

class TestQuantize(unittest.TestCase):

    test_dir = this_dir
    tolerance = 1.0e-04
    expected_netcdf_files = ['quant_bg_3_comp.nc',
                             'quant_br_10b_comp.nc',
                             'quant_gran_3_comp.nc',
                             'reference.nc',
                             'reference_comp.nc']

    def setUp(self):
        '''
        In the setup, we build the quantization test, and run it
        '''
        subprocess.run(['make', 'clean'], cwd=self.test_dir)
        subprocess.run(['make', 'quantize_github'], cwd=self.test_dir)
        subprocess.run('./quantize.exe', cwd=self.test_dir)

    def test_cdl_files(self):
        '''
        Test we have the expected .cdl files
        '''
        expected_cdl_files = ['quant_bg_3_comp.cdl',
                              'quant_br_10b_comp.cdl',
                              'quant_gran_3_comp.cdl',
                              'reference.cdl',
                              'reference_comp.cdl']
        cdl_files = [f for f in os.listdir(self.test_dir) if f[-3:] == 'cdl']
        self.assertCountEqual(cdl_files, expected_cdl_files)

    def test_netcdf_files(self):
        '''
        Check the expected netcdf files are generated
        '''
        nc_files = [f for f in os.listdir(self.test_dir) if f[-2:] == 'nc']
        self.assertCountEqual(nc_files, self.expected_netcdf_files)

    def test_check_output(self):
        '''
        Generate our reference netcdf files from the cdl files, and ensure
        they are the same (within self.tolerance) as those produced from
        the test
        '''
        for netcdf_file in self.expected_netcdf_files:
            netcdf_fileroot = ''.join(netcdf_file.split('.')[:-1])
            # create our reference netcdf file
            reference_ncfile = '{}_ref.nc'.format(netcdf_fileroot)
            subprocess.run(['ncgen', '-k', 'nc4', '-o', reference_ncfile,
                            '{}.cdl'.format(netcdf_fileroot)],
                           cwd=self.test_dir)
            test_results = netCDF4.Dataset(
                os.path.join(self.test_dir, netcdf_file))['field'][:]
            expected = netCDF4.Dataset(
                os.path.join(self.test_dir, reference_ncfile))['field'][:]
            diff = test_results - expected
            result = numpy.allclose(test_results, expected, rtol=self.tolerance)
            if not result:
                sys.stdout.write('The produced data array in file {0}.nc '
                                 'differes from \nthat in'
                                 ' the reference cdl file {0}.cdl\n'.
                                 format(netcdf_file))
            self.assertTrue(result)

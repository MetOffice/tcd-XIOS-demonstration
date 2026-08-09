import sys
import os
import argparse
import netCDF4 as nc
import numpy as np
from .dataFunc import dataFunc

# Global defaults here as needed for command line arguments defaults and argument defaults to run function
defaults = {
            'func_str': 'sinusiod',
            'file_out': None,
            'ugrid_file_out': None,
            'mesh_file': None,
            'mesh_varname': None,
            'nlat': [101],
            'nlon': [100],
            'nlev': 0,
            'dim_prefix': '',
            'dim_suffix': '',
            'data_name': 'data',
            'ugrid_data_name': 'data',
           }

def create_ncfile(ncfile, nlat, nlon, nlev, func, dim_prefix='', dim_suffix='', data_name=''):
    """
    Create netCDF file variables for data on a regular latitude/longitude grid.

        Parameters:
            ncfile: An open, writable netCDF4 dataset
            nlat: Number of latitude points
            nlon: Number of longitude points
            nlev: Number of vertical levels
            func: Function to generate data values
            dim_prefix: prefix for the latitude/longitude/level dimension name
            dim_suffix: suffix for the latitude/longitude/level dimension name
            data_name: data variable name
    """

    latname = f'{dim_prefix}latitude{dim_suffix}'
    lonname = f'{dim_prefix}longitude{dim_suffix}'

    ncfile.createDimension(latname, nlat)
    ncfile.createDimension(lonname, nlon)
    if 'nbounds' not in ncfile.dimensions:
        ncfile.createDimension('nbounds', 2)

    lat = ncfile.createVariable(latname, np.float32, (latname,))
    lat.units = 'degrees_north'
    lat.standard_name = 'latitude'
    lat.bounds = f'{latname}_bounds'

    step_lat = 180.0/(nlat-1)
    first_lat = -90.0
    lat[:] = first_lat + step_lat*np.arange(nlat)
    lat2d = np.repeat(lat,nlon).reshape(nlat,nlon)

    lat_bnds = ncfile.createVariable(lat.bounds, np.float32, (latname,'nbounds'))
    lat_bnds[:,0] = lat[:] - step_lat/2.0
    lat_bnds[:,1] = lat[:] + step_lat/2.0

    lon = ncfile.createVariable(lonname, np.float32, (lonname,))
    lon.units = 'degrees_east'
    lon.standard_name = 'longitude'
    lon.bounds = f'{lonname}_bounds'

    step_lon = 360.0/nlon
    first_lon = 0.0
    lon[:] = first_lon + step_lon*np.arange(nlon)
    lon2d = np.tile(lon,(nlat,1))

    lon_bnds = ncfile.createVariable(lon.bounds, np.float32, (lonname,'nbounds'))
    lon_bnds[:,0] = lon[:] - step_lon/2.0
    lon_bnds[:,1] = lon[:] + step_lon/2.0

    if nlev > 0:
        levname = f'{dim_prefix}level{dim_suffix}'
        ncfile.createDimension(levname, nlev)

        lev = ncfile.createVariable(levname, np.float32, (levname,))
        lev.units = '1'
        lev.standard_name = 'model_levels'
        lev[:] = np.arange(nlev) + 1

        data = ncfile.createVariable(data_name, np.float64, (levname,latname,lonname))
        data.long_name = "input data values"
        data[:] = np.tile(func(lat2d, lon2d), (nlev,1,1))
    else:
        data = ncfile.createVariable(data_name, np.float64, (latname,lonname))
        data.long_name = "input data values"
        data[:] = func(lat2d, lon2d)

def create_ncfile_unstructured(ncmeshout, meshin_file, meshin_varname, nlev, func, 
                               add_bounds=True, dim_prefix='', dim_suffix='', data_name=''):
    """
    Create netCDF file variables for data on a unstructured latitude/longitude grid,
    uses unstructured mesh from UGRID netCDF file.

        Parameters:
            ncmeshout: An open, writable netCDF4 dataset
            meshin_file: UGRID netCDF file, used to extract mesh topology
            meshin_varname: Variable name of mesh topology data in meshin_file
            func: Function to generate data values
            add_bounds: Add latitude/longitude bounds information
            dim_prefix: prefix for the level dimension name
            dim_suffix: suffix for the level dimension name
            data_name: data variable name
    """

    ncmeshin = nc.Dataset(meshin_file, 'r', format='NETCDF4')

    if meshin_varname is None:
        for name,var in ncmeshin.variables.items():
            if 'cf_role' in var.ncattrs():
                if var.cf_role == 'mesh_topology':
                    # Will use the first instance of cf_role == 'mesh_topology' found.
                    # If multiple instances in file consider specifying --meshvar meshin_varname on command line
                    meshin_varname = name
                    break

    try:
        meshin_var = ncmeshin.variables[meshin_varname]
    except KeyError:
        print (f'Mesh topology variable {meshin_varname} does not exist')
        raise

    nface = ncmeshin.dimensions[f'n{meshin_varname}_face'].size
    nnode = ncmeshin.dimensions[f'n{meshin_varname}_node'].size
    nedge = ncmeshin.dimensions[f'n{meshin_varname}_edge'].size
    
    face_node_connectivity = ncmeshin.variables[meshin_var.face_node_connectivity]
    edge_node_connectivity = ncmeshin.variables[meshin_var.edge_node_connectivity]
    face_edge_connectivity = ncmeshin.variables[meshin_var.face_edge_connectivity]
    if 'edge_face_connectivity' in meshin_var.ncattrs():
        edge_face_connectivity = ncmeshin.variables[meshin_var.edge_face_connectivity]
    face_face_connectivity = ncmeshin.variables[meshin_var.face_face_connectivity]

    for face_coord in meshin_var.face_coordinates.split(" "):
        face_coordvar = ncmeshin.variables[face_coord]
        if face_coordvar.standard_name in ['longitude', 'projection_x_coordinate']:
            face_lon = face_coordvar[:]
        elif face_coordvar.standard_name in ['latitude', 'projection_y_coordinate']:
            face_lat = face_coordvar[:]

    for node_coord in meshin_var.node_coordinates.split(" "):
        node_coordvar = ncmeshin.variables[node_coord]
        if node_coordvar.standard_name in ['longitude', 'projection_x_coordinate']:
            node_lon = node_coordvar[:]
        elif node_coordvar.standard_name in ['latitude', 'projection_y_coordinate']:
            node_lat = node_coordvar[:]

    if 'edge_coordinates' in meshin_var.ncattrs():
        for edge_coord in meshin_var.edge_coordinates.split(" "):
            edge_coordvar = ncmeshin.variables[edge_coord]
            if edge_coordvar.standard_name in ['longitude', 'projection_x_coordinate']:
                edge_lon = edge_coordvar[:]
            elif edge_coordvar.standard_name in ['latitude', 'projection_y_coordinate']:
                edge_lat = edge_coordvar[:]

    meshout_varname = 'Mesh2d'
    ncmeshout.Conventions = "UGRID-1.0"
    start_index = 0

    face_dim = ncmeshout.createDimension(f'n{meshout_varname}_face', nface)
    node_dim = ncmeshout.createDimension(f'n{meshout_varname}_node', nnode)
    edge_dim = ncmeshout.createDimension(f'n{meshout_varname}_edge', nedge)
    vertex_dim = ncmeshout.createDimension(f'n{meshout_varname}_vertex', 4)
    two_dim = ncmeshout.createDimension('Two', 2)

    meshout_var = ncmeshout.createVariable(meshout_varname, np.int32)
    meshout_var.cf_role = "mesh_topology"
    meshout_var.long_name = "Topology data of 2D unstructured mesh"
    meshout_var.topology_dimension = np.int32(2)
    meshout_var.face_coordinates = f"{meshout_varname}_face_x {meshout_varname}_face_y"
    meshout_var.node_coordinates = f"{meshout_varname}_node_x {meshout_varname}_node_y"
    meshout_var.edge_coordinates = f"{meshout_varname}_edge_x {meshout_varname}_edge_y"
    meshout_var.face_node_connectivity = f"{meshout_varname}_face_nodes"
    meshout_var.edge_node_connectivity = f"{meshout_varname}_edge_nodes"
    meshout_var.face_edge_connectivity = f"{meshout_varname}_face_edges"
    if 'edge_face_connectivity' in meshin_var.ncattrs():
        meshout_var.edge_face_connectivity = f"{meshout_varname}_edge_face_links"
    meshout_var.face_face_connectivity = f"{meshout_varname}_face_links"

    face_x = ncmeshout.createVariable(f"{meshout_varname}_face_x", np.float32, (face_dim.name,))
    face_x.standard_name = "longitude"
    face_x.long_name = "Characteristic longitude of mesh faces."
    face_x.units = "degrees_east"
    face_x[:] = face_lon

    face_y = ncmeshout.createVariable(f"{meshout_varname}_face_y", np.float32, (face_dim.name,))
    face_y.standard_name = "latitude"
    face_y.long_name = "Characteristic latitude of mesh faces."
    face_y.units = "degrees_north"
    face_y[:] = face_lat

    node_x = ncmeshout.createVariable(f"{meshout_varname}_node_x", np.float32, (node_dim.name,))
    node_x.standard_name = "longitude"
    node_x.long_name = "Longitude of mesh nodes."
    node_x.units = "degrees_east"
    node_x[:] = node_lon

    node_y = ncmeshout.createVariable(f"{meshout_varname}_node_y", np.float32, (node_dim.name,))
    node_y.standard_name = "latitude"
    node_y.long_name = "Latitude of mesh nodes."
    node_y.units = "degrees_north"
    node_y[:] = node_lat

    edge_x = ncmeshout.createVariable(f"{meshout_varname}_edge_x", np.float32, (edge_dim.name,))
    edge_x.standard_name = "longitude"
    edge_x.long_name = "Characteristic longitude of mesh edges."
    edge_x.units = "degrees_east"
    if 'edge_coordinates' in meshin_var.ncattrs():
        edge_x[:] = edge_lon

    edge_y = ncmeshout.createVariable(f"{meshout_varname}_edge_y", np.float32, (edge_dim.name,))
    edge_y.standard_name = "latitude"
    edge_y.long_name = "Characteristic latitude of mesh edges."
    edge_y.units = "degrees_north"
    if 'edge_coordinates' in meshin_var.ncattrs():
        edge_y[:] = edge_lat

    face_node = ncmeshout.createVariable(f"{meshout_varname}_face_nodes", np.int32, (face_dim.name,vertex_dim.name))
    face_node.cf_role = "face_node_connectivity"
    face_node.long_name = "Maps every face to its corner nodes."
    face_node.start_index = np.int32(start_index)
    face_node[:] = face_node_connectivity[:] - face_node_connectivity.start_index + start_index

    edge_node = ncmeshout.createVariable(f"{meshout_varname}_edge_nodes", np.int32, (edge_dim.name,two_dim.name))
    edge_node.cf_role = "edge_node_connectivity"
    edge_node.long_name = "Maps every edge/link to two nodes that it connects."
    edge_node.start_index = np.int32(start_index)
    edge_node[:] = edge_node_connectivity[:] - edge_node_connectivity.start_index + start_index

    face_edge = ncmeshout.createVariable(f"{meshout_varname}_face_edges", np.int32, (face_dim.name,vertex_dim.name), fill_value=999999)
    face_edge.cf_role = "face_edge_connectivity"
    face_edge.long_name = "Maps every face to its edges."
    face_edge.start_index = np.int32(start_index)
    face_edge[:] = face_edge_connectivity[:] - face_edge_connectivity.start_index + start_index

    if 'edge_face_connectivity' in meshin_var.ncattrs():
        edge_face = ncmeshout.createVariable(f"{meshout_varname}_edge_face_links", np.int32, (edge_dim.name,two_dim.name), fill_value=-999)
        edge_face.cf_role = "edge_face_connectivity"
        edge_face.long_name = "neighbor faces for edges"
        edge_face.start_index = np.int32(start_index)
        edge_face.comment = "missing neighbor faces are indicated using _FillValue"
        edge_face[:] = edge_face_connectivity[:] - edge_face_connectivity.start_index + start_index

    face_face = ncmeshout.createVariable(f"{meshout_varname}_face_links", np.int32, (face_dim.name,vertex_dim.name), fill_value=999999)
    face_face.cf_role = "face_face_connectivity"
    face_face.long_name = "Indicates which other faces neighbor each face"
    face_face.start_index = np.int32(start_index)
    face_face.flag_values = np.int32(-1) ;
    face_face.flag_meanings = "out_of_mesh" ;
    face_face[:] = face_face_connectivity[:] - face_face_connectivity.start_index + start_index

    if add_bounds:
        face_x.bounds = f"{face_x.name}_bounds"
        face_x_bnds = ncmeshout.createVariable(face_x.bounds, face_x.dtype, face_node.dimensions)
        face_x_bnds[:] = node_x[face_node[:].flatten()].reshape(face_x_bnds.shape)
        
        face_y.bounds = f"{face_y.name}_bounds"
        face_y_bnds = ncmeshout.createVariable(face_y.bounds, face_y.dtype, face_node.dimensions)
        face_y_bnds[:] = node_y[face_node[:].flatten()].reshape(face_y_bnds.shape)

    if nlev > 0:
        levname = f'{dim_prefix}level{dim_suffix}'
        ncmeshout.createDimension(levname, nlev)

        lev = ncmeshout.createVariable(levname, np.float32, (levname,))
        lev.units = '1'
        lev.standard_name = 'model_levels'
        lev[:] = np.arange(nlev) + 1

        data = ncmeshout.createVariable(data_name, np.float64, (levname,face_dim.name))
        data.long_name = "input data values"
        data.mesh = meshout_varname
        data.location = "face"
        data.coordinates = f"{face_y.name} {face_x.name}"

        data[:] = np.tile(func(face_lat, face_lon), (nlev,1,1))
    else:
        data = ncmeshout.createVariable(data_name, np.float64, (face_dim.name,))
        data.long_name = "input data values"
        data.mesh = meshout_varname
        data.location = "face"
        data.coordinates = f"{face_y.name} {face_x.name}"

        data[:] = func(face_lat, face_lon)

    ncmeshin.close()

def getargs(argv=None):

    df = dataFunc()
    funclist = df.get_funclist()
    del df

    parser = argparse.ArgumentParser(description="Generate netCDF files using analytical functions on regular lat/lon grids and in UGRID data format")

    parser.add_argument("-o","--output", help="Name of regular grid output netCDF file", dest='file_out')
    parser.add_argument("-u","--ugrid_output", help="Name of UGRID output netCDF file", dest='ugrid_file_out')
    parser.add_argument("-m","--meshfile", help="Name of netCDF file containing UGRID mesh topology data, needed for UGRID data", dest='mesh_file')
    parser.add_argument("--meshvar", help="Variable name of mesh topology data in netCDF file, optional for UGRID data", dest='mesh_varname')
    parser.add_argument("--func", help="Analytic function for data variable (default: %(default)s)", choices=funclist, dest='func_str')
    parser.add_argument("--nlat", help=f"Number of latitude points for regular grid, not needed for UGRID data (default: {defaults['nlat'][0]})", type = int, nargs = '+')
    parser.add_argument("--nlon", help=f"Number of longitude points for regular grid, not needed for UGRID data (default: {defaults['nlon'][0]})", type = int, nargs = '+')
    parser.add_argument("--nlev", help=f"Number of vertical levels for regular grid and UGRID data (default: {defaults['nlev']})", type = int)
    parser.add_argument("--dim_prefix", help=f"Prefix applied to dimension variable names in output file (default: {defaults['dim_prefix']})",  nargs = '+')
    parser.add_argument("--dim_suffix", help=f"Suffix applied to dimension variable names in output file (default: {defaults['dim_suffix']})",  nargs = '+')
    parser.add_argument("--data_name", help=f"Data variable names in output file (default: {defaults['data_name']})",  nargs = '+')
    parser.add_argument("--ugrid_data_name", help=f"Data variable name in UGRID output file (default: {defaults['ugrid_data_name']})")

    parser.set_defaults(**defaults)
    args = parser.parse_args(argv)
    if vars(args)['file_out'] is None and vars(args)['ugrid_file_out'] is None:
        parser.error('No output file specified one of --output or --ugrid_output option needed')
    if vars(args)['ugrid_file_out'] is not None and vars(args)['mesh_file'] is None:
        parser.error('--meshfile option needed when --ugrid_output option used')
    nnlat = len(vars(args)['nlat'])
    nnlon = len(vars(args)['nlon'])
    if nnlat != nnlon:
        parser.error('Number of latitude and longitude points specified must be equal')

    return args

def get_strval(name, index):

    if isinstance(name, str):
        name_i = name
    else:
        name_i = name[index]

    return name_i

def run(file_out=defaults['file_out'], ugrid_file_out=defaults['ugrid_file_out'], 
        func_str=defaults['func_str'], mesh_file=defaults['mesh_file'], 
        mesh_varname=defaults['mesh_varname'], 
        nlat=defaults['nlat'], nlon=defaults['nlon'], nlev=defaults['nlev'],
        dim_prefix=defaults['dim_prefix'], dim_suffix=defaults['dim_suffix'],
        data_name=defaults['data_name'], ugrid_data_name=defaults['ugrid_data_name']):
    """
    Generate netCDF files with data on domains suitable for regridding

        Parameters:
            file_out: Name of regular lat/lon output netCDF file
            ugrid_file_out: Name of UGRID output netCDF file
            func_str: Name of analytic function for data variable
            mesh_file: Name of netCDF file containing UGRID mesh topology data, needed for UGRID data
            mesh_varname: Variable name of mesh topology data in netCDF file, optional for UGRID data
            nlat: Number of latitude points for lat/lon data, not needed for UGRID data
            nlon: Number of longitude points for lat/lon data, not needed for UGRID data
            nlev: Number of vertical levels for lat/lon and UGRID data
            dim_prefix: prefix for the latitude/longitude/level dimension name
            dim_suffix: suffix for the latitude/longitude/level dimension name
            data_name: data variable names for regular lat/lon file
            ugrid_data_name: data variable name for UGRID file
    """

    df = dataFunc()
    func = df.get_func(func_str)

    # Create regular lat/lon grid netCDF file
    if file_out is not None:
        ncfile = nc.Dataset(file_out, 'w', format='NETCDF4')
        for index,(nlat0,nlon0) in enumerate(zip(nlat,nlon)):
            dim_p = get_strval(dim_prefix, index)
            dim_s = get_strval(dim_suffix, index)
            data_n = get_strval(data_name, index)
            create_ncfile(ncfile, nlat0, nlon0, nlev, func,
                          dim_prefix=dim_p, dim_suffix=dim_s, data_name=data_n)
        ncfile.close()

    # Create UGRID netCDF file
    if ugrid_file_out is not None:
        ncfile = nc.Dataset(ugrid_file_out, 'w', format='NETCDF4')
        dim_p = get_strval(dim_prefix, 0)
        dim_s = get_strval(dim_suffix, 0)
        create_ncfile_unstructured(ncfile, mesh_file, mesh_varname, nlev, func,
                                   add_bounds=True, dim_prefix=dim_p, dim_suffix=dim_s, 
                                   data_name=ugrid_data_name)
        ncfile.close()

def main(argv=None):
    args = getargs(argv)
    run(**vars(args))

if __name__ == "__main__":
    main()

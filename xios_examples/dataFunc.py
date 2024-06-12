import numpy as np

class dataFunc:
    """
    Class containing functions which generate analytical data values for an input of
    latitude and longitude arrays.

    Functions sinusiod,harmonic,vortex,gulfstream are taken from the paper
    `Benchmarking Regridding Libraries Used in Earth System Modelling`, see
    https://www.mdpi.com/2297-8747/27/2/31
    """

    def func_sinusiod(self, latarr, lonarr):
    
        length = 1.2*np.pi
        conv = np.pi/180.0
        coef = 2.0
        coefmult = 1.0
    
        data = np.array(coefmult*(coef - np.cos( np.pi*(np.arccos( np.cos(lonarr*conv)*np.cos(latarr*conv) )/length))), dtype=np.float64)
    
        return data
    
    def func_harmonic(self, latarr, lonarr):
    
        conv = np.pi/180.0
    
        data = np.array(2.0 + (np.sin(2.0*latarr*conv)**16)*np.cos(16.0*lonarr*conv), dtype=np.float64)
    
        return data
    
    def func_vortex(self, latarr, lonarr):
    
        lon0 = 5.5
        lat0 = 0.2
        r0 = 3.0
        d = 5.0
        t = 6.0
        conv = np.pi/180.0
        sinc = np.sin(lat0)
        cosc = np.cos(lat0)
    
        # Find the rotated Longitude and Latitude of a point on a sphere
        # with pole at (lon0, lat0)
        cost = np.cos(latarr*conv)
        sint = np.sin(latarr*conv)
    
        trm = cost * np.cos(lonarr*conv-lon0)
        x = sinc * trm - cosc*sint
        y = cost * np.sin(lonarr*conv-lon0)
        z = sinc * sint + cosc*trm
    
        lon = np.arctan2(y, x)
        lon = np.where(lon < 0.0, lon+2.0*np.pi, lon)
        lat = np.arcsin(z)
    
        rho = r0 * np.cos(lat)
        vt = 3.0 * np.sqrt(3.0)/2.0/np.cosh(rho)/np.cosh(rho)*np.tanh(rho)
        omega = np.where(rho == 0.0, 0.0, vt/rho)
    
        data = np.array(2.0*(1.0+np.tanh(rho/d * np.sin(lon-omega*t))), dtype=np.float64)
    
        return data
    
    def func_gulfstream(self, latarr, lonarr):

        # Parameters for analytical function
        coef = 2.0
        length = 1.2*np.pi
        conv = np.pi/180.0
        gf_coef    = 1.0   # Coefficient for Gulf Stream term (0.0 = no Gulf Stream)
        gf_ori_lon = -80.0 # Origin of the Gulf Stream (longitude in deg)
        gf_ori_lat = 25.0  # Origin of the Gulf Stream (latitude in deg)
        gf_end_lon = -1.8  # End point of the Gulf Stream (longitude in deg)
        gf_end_lat = 50.0  # End point of the Gulf Stream (latitude in deg)
        gf_dmp_lon = -25.5 # Point of the Gulf Stream decrease (longitude in deg)
        gf_dmp_lat = 55.5  # Point of the Gulf Stream decrease (latitude in deg)

        dr0 = np.sqrt(((gf_end_lon - gf_ori_lon)*conv)**2 + 
                      ((gf_end_lat - gf_ori_lat)*conv)**2)

        dr1 = np.sqrt(((gf_dmp_lon - gf_ori_lon)*conv)**2 +
                      ((gf_dmp_lat - gf_ori_lat)*conv)**2)

        # Original OASIS fcos analytical test function
        fnc_ana = (coef-np.cos(np.pi*(np.arccos(np.cos(latarr*conv)*np.cos(lonarr*conv))/length)))
        gf_per_lon = lonarr
        gf_per_lon = np.where(gf_per_lon > 180.0, gf_per_lon-360.0, gf_per_lon)
        gf_per_lon = np.where(gf_per_lon < -180.0, gf_per_lon+360.0, gf_per_lon)
        dx = (gf_per_lon - gf_ori_lon)*conv
        dy = (latarr - gf_ori_lat)*conv
        dr = np.sqrt(dx*dx + dy*dy)
        dth = np.arctan2(dy, dx)
        dc = 1.3*gf_coef
        dc = np.where(dr > dr0, 0.0, dc)
        dc = np.where(dr > dr1, dc * np.cos(np.pi*0.5*(dr-dr1)/(dr0-dr1)), dc)
        data = np.array(fnc_ana + (np.maximum(1000.0*np.sin(0.4*(0.5*dr+dth)+0.007*np.cos(50.0*dth) +
                                                            0.37*np.pi),999.0)-999.0)*dc, dtype=np.float64)

        return data
    
    def func_cossin(self, latarr, lonarr):
    
        length = 1.0*np.pi
        conv = np.pi/180.
        coef = 21.0
        coefmult = 3.846 * 20.0
    
        data = np.array(coefmult*(coef - np.cos( np.pi*(np.arccos( np.cos(lonarr*conv)*np.cos(latarr*conv) )/length)) *
                                         np.sin( np.pi*(np.arcsin( np.sin(lonarr*conv)*np.sin(latarr*conv) )/length))), dtype=np.float64)
    
        return data

    def get_funclist(self):

        funclist = [func.removeprefix('func_') for func in dir(self) if callable(getattr(self, func)) and func.startswith('func_')]

        return funclist

    def get_func(self, name: str):

        do = f"func_{name}"
        if hasattr(self, do) and callable(func := getattr(self, do)):
            return func

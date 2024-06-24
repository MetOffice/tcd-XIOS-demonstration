!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Resam on a 1D Pressure vertical axis using the supplied
!> Pressure and temperature on model levels

program multiple_timestep
  use xios
  use mpi

  implicit none

  integer :: comm = -1
  integer :: rank = -1
  integer :: npar = 0

  call initialise()
  call simulate()
  call finalise()
contains

  subroutine initialise()

    type(xios_date) :: origin
    type(xios_date) :: start
    type(xios_duration) :: tstep
    integer :: mpi_error
    double precision, dimension (1) :: lons = [-4.5]
    double precision, dimension (1) :: lats = [51.5]
    double precision, dimension (2,1) :: blons
    double precision, dimension (2,1) :: blats

    blons = reshape((/-6.0, -3.0/), shape(blons))
    blats = reshape((/50.0, 53.0/), shape(blats))

    ! Arbitrary datetime setup, required for XIOS but unused
    ! in this example
    origin = xios_date(2022, 2, 2, 12, 0, 0)
    start = xios_date(2022, 12, 13, 12, 0, 0)
    tstep = xios_hour

    ! Initialise MPI and XIOS
    call MPI_INIT(mpi_error)

    call xios_initialize('client', return_comm=comm)

    call MPI_Comm_rank(comm, rank, mpi_error)
    call MPI_Comm_size(comm, npar, mpi_error)

    call xios_context_initialize('main', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    call xios_set_domain_attr('latlon1_domain', ni=1, nj=1, ibegin=0, jbegin=0)
    call xios_set_domain_attr('latlon1_domain', lonvalue_1d=lons, latvalue_1d=lats, bounds_lon_1d=blons, bounds_lat_1d=blats)

    call xios_close_context_definition()

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    ! Finalise XIOS and MPI
    call xios_context_finalize()

    call xios_finalize()
    call MPI_Finalize(mpi_error)

  end subroutine finalise

  subroutine simulate()

    type(xios_date) :: current
    integer :: ts
    integer :: lenz
    integer :: lenrz
    integer :: nlevs = 39

    integer, parameter :: sim_period = 20

    double precision, dimension (:), allocatable :: inpdata
    double precision, dimension (:), allocatable :: intdata

    allocate ( inpdata(nlevs) )
    allocate ( intdata(nlevs) )

    ! create data
    ! pressure on model levels
    inpdata = [&
    99966.51733734, 99940.00334122, 99879.75646023, 99777.78684877,&
    99635.30154044, 99451.57613718, 99227.03147757, 98960.87975741,&
    98652.27529804, 98301.45813138, 97908.79783106, 97473.21205427,&
    96994.06194264, 96470.87150873, 95901.78892901, 95284.84539213,&
    94617.56355344, 93898.14972652, 93123.02534389, 92287.80902342,&
    91391.57971605, 90430.9865711,  89403.66167799, 88321.39379902,&
    87213.38301499, 86106.06396005, 84999.27014238, 83877.20722173,&
    82723.67003746, 81516.40664914, 80240.47719038, 78881.30453228,&
    77425.96195086, 75842.56242889, 74128.77364138, 72079.17018205,&
    69945.02914929, 65667.62435439, 63427.28244824]

    ! temperature on model levels
    intdata = [&
    273.39701404, 273.14989444, 272.87913071, 272.50214011, 271.77289302,&
    270.70622676, 269.60430231, 268.39273869, 266.90100832, 265.29298229,&
    263.39088121, 261.48536033, 258.99438088, 256.21760745, 252.83385896,&
    248.6965695,  243.8680557,  238.57359575, 232.80450043, 226.62957162,&
    220.02910932, 213.47976457, 206.95727661, 205.63877024, 210.23499515,&
    216.93309253, 221.44028771, 223.98930063, 224.66841468, 224.15347685,&
    223.6943803,  223.15001293, 222.66638538, 222.80326535, 223.20642547,&
    223.01865617, 227.3452969,  219.20421226, 266.77144776]

    do ts=1, sim_period
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      ! Send the pressure data to the output file.
      call xios_send_field('pressure', inpdata + ts*1000000)
      ! Send the temperature data to the output file.
      call xios_send_field('temperature', intdata + ts*1000000)
    enddo

    deallocate (inpdata)
    deallocate (intdata)

  end subroutine simulate

end program multiple_timestep

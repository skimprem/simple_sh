program sh_makegrid
  use SHTOOLS
  use subroutines
  implicit none
  real(kind=8), dimension(:,:), allocatable :: grid
  real(kind=8), dimension(:,:,:), allocatable :: cilm
  integer(kind=4) :: lmax, n_lat, n_lon, norm, cs_phase_mode, dealloc, exit_status,&
  stdout, i, j, sampling_mode, lmax_calc
  real(kind=8) :: d_lat, flattening, major_semiaxis, north, south, east, west
  character(len=1000) :: arg
  character(len=:), allocatable :: grid_file, sh_file, sh_method
  arg = ''
  norm = 1
  cs_phase_mode = 1
  sampling_mode = 2
  stdout = 6
  i = 0
  lmax_calc = 0
  do while(i < command_argument_count())
    i = i + 1
    call get_command_argument(i, arg)
    select case(arg)
    case('-in', '--input')
      i = i + 1
      call get_command_argument(i, arg)
      sh_file = trim(adjustl(arg))
      write(stdout, *) 'input: ', sh_file
    case('-out', '--output')
      i = i + 1
      call get_command_argument(i, arg)
      grid_file = trim(adjustl(arg))
      write(stdout, *) 'output: ', grid_file
    case('-deg', '--degree')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) lmax_calc
      write(stdout, *) 'degree: ', integer_to_string(lmax_calc)
    case('-dlat', '--latitude_interval')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) d_lat 
      write(stdout, *) 'latitude interval: ', double_to_string(d_lat)
    case('--norm')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) norm
      write(stdout, *) 'norm: ', integer_to_string(norm)
    case('--csphase')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) cs_phase_mode
      write(stdout, *) 'csphase: ', integer_to_string(cs_phase_mode)
    case('-mode', '--shmode')
      i = i + 1
      call get_command_argument(i, arg)
      sh_method = trim(adjustl(arg))
      write(stdout, *) 'method: ', sh_method
    end select
  end do
  write(stdout, '(1x, a)', advance = 'no') 'reading coeff ...'
  call sh_reader(sh_file, cilm, lmax)
  write(stdout, *) ' done!'
  select case(sh_method)
  case('int')
    flattening = 298.257223563_8
    major_semiaxis = 6378137._8
    north = 90._8 - d_lat / 2._8
    south = -90._8 + d_lat / 2._8
    east = 360._8 - d_lon / 2._8
    west = 0._8 + d_lon / 2._8
    dealloc = 0
    allocate(grid(nint(180._8 / d_lat, kind = 4) + 1, nint(360._8 / d_lon, kind = 4) + 1))
    grid = 0._8
    write(stdout, '(1x, a)', advance = 'no') 'makegrid2d() ... '
    call makegrid2d(&
      grid,& ! output, real(kind = 8), dimension(180 / d_lat + 1, 360 / d_lat + 1)
      cilm = cilm,& ! input, real(kind = 8), dimension(2, lmax + 1, lmax + 1)
      lmax = lmax_calc,& ! input, integer
      interval = d_lat,& ! input, real(kind = 8)
      nlat = n_lat,& ! output, integer
      nlong = n_lon,& ! output, integer
      norm = norm,& ! input, optional, integer, default = 1
      csphase = cs_phase_mode,& ! input, optional, integer, default = 1
      !f = flattening,& ! input, optional, real(kind = 8)
      !a = major_semiaxis,& ! input, optional, real(kind = 8)
      north = north,& ! input, real(kind = 8), optional, default = 90.0
      south = south,& ! input, real(kind = 8), optional, default = -90.0
      east = east,& ! input, real(kind = 8), optional, default = 360.0
      west = west,& ! input, real(kind = 8), optional, default = 0.0
      dealloc = dealloc,& ! input, optional, integer, default = 0
      exitstatus = exit_status) ! output, optional, integer
    if(exit_status == 0) then
      write(stdout, *) 'done!'
    else
      write(stdout, *) 'failed!'
      stop
    end if
    write(stdout, '(1x, a)', advance = 'no') 'writing grid ... '
    call grid_writer(grid_file, grid, d_lat, n_lat, n_lon, north, west)
    write(stdout, *) 'done!'
  case('dh')
    allocate(grid(2 * lmax + 2, 4 * lmax + 4))
    grid = 0._8
    call makegriddh(&
      griddh = grid,&
      n = n_lat,&
      cilm = cilm,&
      lmax = lmax,&
      norm = norm,&
      sampling = sampling_mode,&
      csphase = csphase,&
      lmax_calc = lmax_calc,&
      exitstatus = exit_status)
  end select

end program sh_makegrid

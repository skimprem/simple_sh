program sh_makegrid
  use SHTOOLS
  use subroutines
  implicit none
  real(kind=8), dimension(:,:), allocatable :: grid
  real(kind=8), dimension(:,:,:), allocatable :: cilm
  integer(kind=4) :: lmax, n_lat, n_lon, norm, csphase, dealloc, exit_status,&
  stdout, i, j, sampling, lmax_calc
  real(kind=8) :: interval, f, a, north, south, east, west
  character(len=1000) :: arg
  character(len=:), allocatable :: grid_file, sh_file, sh_method
  arg = ''
  norm = 1
  csphase = 1
  stdout = 6
  i = 0
  lmax_calc = 0
  do while(i < command_argument_count())
    i = i + 1
    call get_command_argument(i, arg)
    select case(arg)
    case('-in')
      i = i + 1
      call get_command_argument(i, arg)
      sh_file = trim(adjustl(arg))
      write(stdout, *) 'input: ', sh_file
    case('-out')
      i = i + 1
      call get_command_argument(i, arg)
      grid_file = trim(adjustl(arg))
      write(stdout, *) 'output: ', grid_file
    case('-deg')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) lmax_calc
      write(stdout, *) 'degree: ', integer_to_string(lmax_calc)
    case('-step')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) interval
      write(stdout, *) 'step: ', double_to_string(interval)
    case('-mode')
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
    f = 298.257223563_8
    a = 6378137._8
    !north = 90._8
    north = 89.75_8
    !south = -90._8
    south = -89.75_8
    !east = 360._8
    east = 359.75_8
    !west = 0._8
    west = 0.25_8
    dealloc = 0
    allocate(grid(int(180._8 / interval + 1, kind = 4), int(360._8 / interval + 1, kind = 4)))
    grid = 0._8
    write(stdout, '(1x, a)', advance = 'no') 'makegrid2d() ... '
    call makegrid2d(&
      grid,&
      cilm = cilm,&
      lmax = lmax_calc,&
      interval = interval,&
      nlat = n_lat,&
      nlong = n_lon,&
      norm = norm,&
      csphase = csphase,&
      !f = f,&
      !a = a,&
      north = north,&
      south = south,&
      east = east,&
      west = west,&
      dealloc = dealloc,&
      exitstatus = exit_status)
    if(exit_status == 0) then
      write(stdout, *) 'done!'
    else
      write(stdout, *) 'failed!'
    end if
    write(stdout, '(1x, a)', advance = 'no') 'writing grid ... '
    call grid_writer(grid_file, grid, interval, n_lat, n_lon, north, west)
    write(stdout, *) 'done!'
  case('dh')
    sampling = 2
    allocate(grid(2 * lmax + 2, 4 * lmax + 4))
    grid = 0._8
    call makegriddh(&
      griddh = grid,&
      n = n_lat,&
      cilm = cilm,&
      lmax = lmax,&
      norm = norm,&
      sampling = sampling,&
      csphase = csphase,&
      lmax_calc = lmax_calc,&
      exitstatus = exit_status)
  end select

end program sh_makegrid

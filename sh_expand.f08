program sh_expand
  use SHTOOLS
  use subroutines
  use sh_expand_ls
  implicit none
  real(kind=8), dimension(:,:), allocatable :: griddh
  real(kind=8), dimension(:,:,:), allocatable :: cilm
  real(kind=8), dimension(:), allocatable :: grid, coeff, lat, lon
  real(kind=8) :: lat_step
  real(kind=8), dimension(:) :: grid_passport(6)
  integer(kind=4) :: i, j, k, n, lmax, lmax_calc, exit_status, stdout
  integer(kind=16) :: mem_size, ii, jj
  character(len=:), allocatable :: grid_file, sh_file, sh_method
  character(len=500) :: arg
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
      grid_file = trim(adjustl(arg))
      write(stdout, *) 'input: ', grid_file
    case('-out')
      i = i + 1
      call get_command_argument(i, arg)
      sh_file = trim(adjustl(arg))
      write(stdout, *) 'output: ', sh_file
    case('-deg')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) lmax_calc
      write(stdout, *) 'degree: ', integer_to_string(lmax_calc)
    case('-mode')
      i = i + 1
      call get_command_argument(i, arg)
      sh_method = trim(adjustl(arg))
      write(stdout, *) 'method: ', sh_method
    end select
  end do
  select case(sh_method)
  case('dh')
    write(stdout, '(1x, a)', advance = 'no') 'reading grid ...'
    call grid_reader(&
      file_name = grid_file,&
      mode = 'reg_2d',&
      grid_reg_2d = griddh,&
      n_lat = n)
    write(stdout, *) ' done!'
    write(stdout, *) 'samples: ', integer_to_string(n), ' x ',  integer_to_string(n * 2)
    lmax = n / 2 - 1
    if(lmax_calc == 0) then
      lmax_calc = lmax
    else if(lmax_calc > lmax) then
      lmax_calc = lmax
    end if 
    allocate(cilm(2, lmax_calc + 1, lmax_calc + 1))
    write(stdout, '(1x, a)', advance = 'no') 'shexpanddh() ...'
    call shexpanddh(&
      griddh,&
      n = n,&
      cilm = cilm,&
      lmax = lmax,&
      norm = 1,&
      sampling = 2,&
      csphase = 1,&
      lmax_calc = lmax_calc,&
      exitstatus = exit_status)
    if(exit_status == 0) then
      write(stdout, '(a)') ' 100% done!'
    else
      write(stdout, *) 'error with status: ', integer_to_string(exit_status)
      stop
    end if
  case('int')
    write(stdout, '(1x, a)', advance = 'no') 'reading grid ...'
    call grid_reader(&
      file_name = grid_file,&
      mode = 'reg_1d',&
      grid_reg_1d = grid,&
      n_lat = n)
    write(stdout, *) ' done!'
    write(stdout, *) 'samples: ', integer_to_string(n), ' x ',  integer_to_string(n * 2)
    write(stdout, '(1x, a)', advance = 'no') 'shetim() ... '
    !call expandint(griddh, n, cilm, lmax_calc)
    allocate(coeff((lmax_calc + 1) * (lmax_calc + 2) - lmax_calc - 1))
    allocate(cilm(2, lmax_calc + 1, lmax_calc + 1))
    lat_step = 180._8 / real(n, kind=8)
    !k = 0
    !do i = 1, n
      !do j = 1, n * 2
        !k = k + 1
        !grid(k) = griddh(i, j)
      !end do
    !end do
    call shetim(lmax_calc, grid, lat_step, coeff)
    k = 1
    cilm = 0._8
    cilm(1, 1, 1) = coeff(k)
    do i = 1, lmax_calc 
      k = k + 1
      cilm(1, i + 1, 1) = coeff(k)
      cilm(1, 1, i + 1) = coeff(k)
      do j = 1, i
        k = k + 1
        cilm(1, i + 1, j + 1) = coeff(k)
        cilm(1, j + 1, i + 1) = coeff(k)
        k = k + 1
        cilm(2, i + 1, j + 1) = coeff(k)
        cilm(2, j + 1, i + 1) = coeff(k)
      end do
    end do
    write(stdout, '(a)') ' done!'
  case('ls')
    write(stdout, '(1x, a)', advance = 'no') 'reading grid ...'
    call grid_reader(&
      file_name = grid_file,&
      mode = 'reg_1d',&
      grid_reg_1d = grid,&
      n_lat = n)
    write(stdout, *) ' done!'
    write(stdout, *) 'samples: ', integer_to_string(n), ' x ',  integer_to_string(n * 2)
    write(stdout, '(1x, a)', advance = 'no') 'sf_ab() ... '
    allocate(coeff((lmax_calc + 1) * (lmax_calc + 2) - lmax_calc - 1))
    allocate(cilm(2, lmax_calc + 1, lmax_calc + 1))
    grid_passport(1) = 90._8
    grid_passport(2) = -90._8
    grid_passport(3) = 180._8 / real(n, kind=8)
    grid_passport(4) = 0._8
    grid_passport(5) = 360._8
    grid_passport(6) = grid_passport(3)
    !k = 0
    !do i = 1, n
      !do j = 1, n * 2
        !k = k + 1
        !grid(k) = griddh(i, j)
      !end do
    !end do
    call sf_ab(grid, grid_passport, lmax_calc, coeff)
    k = 1
    cilm = 0._8
    cilm(1, 1, 1) = coeff(k)
    do i = 1, lmax_calc 
      k = k + 1
      cilm(1, i + 1, 1) = coeff(k)
      cilm(1, 1, i + 1) = coeff(k)
      do j = 1, i
        k = k + 1
        cilm(1, i + 1, j + 1) = coeff(k)
        cilm(1, j + 1, i + 1) = coeff(k)
        k = k + 1
        cilm(2, i + 1, j + 1) = coeff(k)
        cilm(2, j + 1, i + 1) = coeff(k)
      end do
    end do
    write(stdout, '(a)') ' done!'
  case('lsq')
    write(stdout, '(1x, a)', advance = 'no') 'reading grid ...'
    call grid_reader(&
      file_name = grid_file,&
      mode = 'irr_1d',&
      grid_irr_1d = grid,&
      n_points = n,&
      lat = lat,&
      lon = lon)
    write(stdout, *) ' done!'
    write(stdout, *) 'points: ', integer_to_string(n)
    allocate(cilm(2, lmax_calc + 1, lmax_calc + 1))
    write(stdout, '(1x, a)', advance = 'no') 'shexpandlsq() ...'
    ii = int(n, kind = 16)
    jj = int((lmax_calc + 1) ** 2, kind = 16)
    print *, 'n = ', ii, '(lmax + 1) ^ 2 = ', jj
    print *, 'n * (lmax + 1) ^ 2 = ', ii * jj
    mem_size = ii * jj 
    !print *, mem_info_int(mem_size, 'B')
    deallocate(grid)
    allocate(grid(mem_size))
    allocate(griddh(int(n, kind = 8), (int(lmax_calc, kind = 8) + 1) ** 2), stat = i)
    print *, i
    stop
    call shexpandlsq(&
      cilm,&
      d = grid,&
      lat = lat,&
      lon = lon,&
      nmax = n,&
      lmax = lmax_calc,&
      exitstatus = exit_status)
    print *, exit_status
  end select
  call sh_writer(sh_file, cilm, lmax_calc)
end program sh_expand

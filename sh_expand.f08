program sh_expand
  use SHTOOLS
  use subroutines
  implicit none
  real(kind=8), dimension(:,:), allocatable :: griddh
  real(kind=8), dimension(:,:,:), allocatable :: cilm
  real(kind=8), dimension(:), allocatable :: grid, coeff
  real(kind=8) :: lat_step
  integer(kind=4) :: i, j, k, n, lmax, lmax_calc, exit_status, stdout
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
  write(stdout, '(1x, a)', advance = 'no') 'reading grid ...'
  call grid_reader(grid_file, griddh, n)
  write(stdout, *) ' done!'
  write(stdout, *) 'latitude blocks: ', integer_to_string(n)
  write(stdout, *) 'longitude blocks: ', integer_to_string(n * 2)
  select case(sh_method)
  case('dh')
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
    write(stdout, '(1x, a)', advance = 'no') 'expandint() ... '
    !call expandint(griddh, n, cilm, lmax_calc)
    allocate(grid(n * n * 2))
    allocate(coeff((lmax_calc + 1) * (lmax_calc + 2) - lmax_calc - 1))
    allocate(cilm(2, lmax_calc + 1, lmax_calc + 1))
    lat_step = 180._8 / real(n, kind=8)
    k = 0
    do i = 1, n
      do j = 1, n * 2
        k = k + 1
        grid(k) = griddh(i, j)
      end do
    end do
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
  end select
  call sh_writer(sh_file, cilm, lmax_calc)
end program sh_expand

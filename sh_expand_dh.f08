program sh_expand_dh
  use SHTOOLS
  implicit none
  real(kind=8), dimension(:,:), allocatable :: griddh
  real(kind=8), dimension(:,:,:), allocatable :: cilm
  integer(kind=4) :: i, j, n, lmax, lmax_calc, exit_status, stdout
  character(len=:), allocatable :: grid_file, sh_file
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
      write(stdout, *) 'calc degree: ', integer_to_string(lmax_calc)
    end select
  end do
  write(stdout, '(1x, a)', advance = 'no') 'reading grid...'
  call grid_reader(grid_file, griddh, n)
  write(stdout, *) ' done!'
  write(stdout, *) 'latitude blocks: ', integer_to_string(n)
  write(stdout, *) 'longitude blocks: ', integer_to_string(n * 2)
  lmax = n / 2 - 1
  if(lmax_calc == 0) then
    lmax_calc = lmax
  else if(lmax_calc > lmax) then
    lmax_calc = lmax
  end if 
  allocate(cilm(2, lmax_calc + 1, lmax_calc + 1))
  write(stdout, '(1x, a)', advance = 'no') 'shexpanddh()...'
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
    write(stdout, '(1x, a)') ' done!'
  else
    write(stdout, *) 'error with status: ', integer_to_string(exit_status)
    stop
  end if
  call sh_writer(sh_file, cilm, lmax_calc)
  contains
  subroutine grid_reader(file_name, grid, n)
    implicit none
    character(*), intent(in) :: file_name
    real(kind=8), dimension(:,:), allocatable, intent(out) :: grid 
    integer(kind=4), intent(out) :: n
    real(kind=4) :: lat, lon
    integer(kind=4) :: i, j, file_unit, io_status, blocks, n2 
    open(newunit = file_unit, file = file_name, action = 'read', status = 'old')
    io_status = 0
    blocks = 0
    do
      read(file_unit, *, iostat = io_status)
      if(io_status == 0) then
        blocks = blocks + 1
        cycle
      else
        exit
      end if
    end do
    rewind(file_unit)
    n = int(dsqrt(real(blocks, kind = 8) / 2), kind = 4)
    n2 = n * 2
    allocate(grid(n, n2))
    do i = 1, n
      do j = 1, n2
        read(file_unit, *) lat, lon, grid(i, j)
      end do
    end do
    close(file_unit)
    return
  end subroutine grid_reader
  subroutine sh_writer(file_name, coeff, degree)
    implicit none
    character(*), intent(in) :: file_name
    real(kind=8), dimension(:,:,:) :: coeff
    integer(kind=4) :: file_unit, i, j, degree
    open(newunit = file_unit, file = file_name, action = 'write')
    do i = 0, degree
      do j = 0, i
        write(file_unit, *) i, j, coeff(1, i + 1, j + 1), coeff(2, i + 1, j + 1)
      end do
    end do
    close(file_unit)
    return
  end subroutine sh_writer
  function double_to_string(value, frmt) result(output_string)
    implicit none
    real(kind=8), intent(in) :: value
    character(*), intent(in), optional :: frmt
    character(len=:), allocatable :: output_string
    character(10000) :: max_string
    if(present(frmt) .eqv. .true.) then
      write(max_string, frmt) value
    else
      write(max_string, *) value
    end if
    output_string = trim(adjustl(max_string))
    return
  end function double_to_string
  function integer_to_string(value, frmt) result(output_string)
    implicit none
    integer(kind=4), intent(in) :: value
    character(*), intent(in), optional :: frmt
    character(len=:), allocatable :: output_string
    character(10000) :: max_string
    if(present(frmt) .eqv. .true.) then
      write(max_string, frmt) value
    else
      write(max_string, *) value
    end if
    output_string = trim(adjustl(max_string))
    return
  end function integer_to_string 
end program sh_expand_dh

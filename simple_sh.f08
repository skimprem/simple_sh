program simple_sh
  use SHTOOLS
  implicit none
  real(kind=8), dimension(:,:), allocatable :: grid_value
  real(kind=8), dimension(:,:,:), allocatable :: spherical_harmonic
  integer(kind=4) :: i, lon_count, lat_count, max_degree, exit_status
  real(kind=8) lat_north, lat_south, lon_east, lon_west, block_size
  character(len=:), allocatable :: grid_file_name
  character(len=500) :: arg
  i = 0
  do while(i < command_argument_count())
    i = i + 1
    call get_command_argument(i, arg)
    grid_file_name = trim(adjustl(arg))
  end do
  call grid_reader(grid_file_name, grid_value, lat_count)
  allocate(spherical_harmonic(2, lat_count / 2, lat_count / 2))
  call shexpanddh(&
    grid_value,&
    n = lat_count,&
    cilm = spherical_harmonic,&
    lmax = max_degree,&
    norm = 1,&
    sampling = 2,&
    csphase = 1,&
    exitstatus = exit_status)
  print *, 'exit_status =', exit_status
  contains
  subroutine grid_reader(file_name, grid_value, lat_count)
    implicit none
    character(*), intent(in) :: file_name
    real(kind=8), dimension(:,:), allocatable :: grid_value 
    real(kind=4) :: lat, lon
    integer(kind=4) :: i, j, file_unit, io_status, blocks_count, lat_count, lon_count
    open(newunit = file_unit, file = file_name, action = 'read', status = 'old')
    io_status = 0
    blocks_count = 0
    do
      read(file_unit, *, iostat = io_status)
      if(io_status == 0) then
        blocks_count = blocks_count + 1
        cycle
      else
        exit
      end if
    end do
    rewind(file_unit)
    lat_count = int(dsqrt(real(blocks_count, kind = 8) / 2), kind = 4)
    lon_count = lat_count * 2
    allocate(grid_value(lat_count, lon_count))
    do i = 1, lat_count
      do j = 1, lon_count
        read(file_unit, *) lat, lon, grid_value(i, j)
      end do
    end do
    return
  end subroutine grid_reader
end program simple_sh

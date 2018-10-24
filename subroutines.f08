module subroutines
contains
subroutine grid_reader(file_name, mode, grid_reg_2d, grid_reg_1d, grid_irr_1d, grid_glq_2d, n_lat, n_points, lat, lon, lmax)
  implicit none
  character(*), intent(in) :: file_name, mode
  real(kind=8), dimension(:,:), allocatable, intent(out), optional :: grid_reg_2d
  real(kind=8), dimension(:), allocatable, intent(out), optional :: grid_reg_1d, grid_irr_1d,lat,lon
  real(kind=8) :: lat_i, lon_i
  integer(kind=4), intent(out), optional :: n_lat, n_points, lmax
  integer(kind=4) :: stdout, i, j, k, file_unit, io_status, enter_status, n_lines, n_lon 
  character(len=:), allocatable :: variable_name
  io_status = 0
  enter_status = 0
  stdout = 6
  open(newunit = file_unit, file = file_name, action = 'read', status = 'old')
  select case(mode)
  case('glq_2d')
    if(present(grid_reg_1d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_reg_1d'
    if(present(grid_reg_2d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_reg_2d'
    else if(present(grid_irr_1d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_irr_1d'
    else if(present(n_lat) .eqv. .true.) then
      enter_status = 1
      variable_name = 'n_lat'
    else if(present(n_points) .eqv. .true.) then
      enter_status = 1
      variable_name = 'n_points'
    else if(present(grid_glq_2d) .eqv. .false.) then
      enter_status = 2
      variable_name = 'grid_glq_2d'
    else if(present(grid_reg_1d) .eqv. .false.) then
      enter_status = 2
      variable_name = 'grid_reg_1d'
    else if(present(n_lat) .eqv. .false.) then
      enter_status = 2
      variable_name = 'n_lat'
    end if
    if(enter_status == 0) then
      n_lines = 0
      do
        read(file_unit, *, iostat = io_status)
        if(io_status == 0) then
          n_lines = n_lines + 1
          cycle
        else
          exit
        end if
      end do
      rewind(file_unit)
      lmax = 
    end if
  case('reg_1d')
    if(present(grid_reg_2d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_reg_2d'
    else if(present(grid_irr_1d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_irr_1d'
    else if(present(n_points) .eqv. .true.) then
      enter_status = 1
      variable_name = 'n_points'
    else if(present(grid_reg_1d) .eqv. .false.) then
      enter_status = 2
      variable_name = 'grid_reg_1d'
    else if(present(n_lat) .eqv. .false.) then
      enter_status = 2
      variable_name = 'n_lat'
    end if
    if(enter_status == 0) then
      n_lines = 0
      do
        read(file_unit, *, iostat = io_status)
        if(io_status == 0) then
          n_lines = n_lines + 1
          cycle
        else
          exit
        end if
      end do
      rewind(file_unit)
      n_lat = int(dsqrt(real(n_lines, kind = 8) / 2), kind = 4)
      allocate(grid_reg_1d(n_lines))
      if((present(lat) .eqv. .true.) .and. (present(lon) .eqv. .true.)) then
        do i = 1, n_lines
          read(file_unit, *) lat_i, lon_i, grid_reg_1d(i)
        end do
      else
        n_lon = n_lat * 2
        allocate(lat(n_lat), lon(n_lon))
        k = 0
        do i = 1, n_lat
          do j = 1, n_lon
            k = k + 1
            read(file_unit, *) lat(i), lon(j), grid_reg_1d(k)
          end do
        end do
      close(file_unit)
      end if
    end if
  case('reg_2d')
    if(present(grid_reg_1d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_reg_1d'
    else if(present(grid_irr_1d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_irr_1d'
    else if(present(n_points) .eqv. .true.) then
      enter_status = 1
      variable_name = 'n_points'
    else if(present(grid_reg_2d) .eqv. .false.) then
      enter_status = 2
      variable_name = 'grid_reg_2d'
    else if(present(n_lat) .eqv. .false.) then
      enter_status = 2
      variable_name = 'n_lat'
    end if
    if(enter_status == 0) then
      n_lines = 0
      do
        read(file_unit, *, iostat = io_status)
        if(io_status == 0) then
          n_lines = n_lines + 1
          cycle
        else
          exit
        end if
      end do
      rewind(file_unit)
      n_lat = int(dsqrt(real(n_lines, kind = 8) / 2), kind = 4)
      n_lon = n_lat * 2
      allocate(grid_reg_2d(n_lat, n_lon))
      if((present(lat) .eqv. .true.) .and. (present(lon) .eqv. .true.)) then
        allocate(lat(n_lat), lon(n_lon))
        do i = 1, n_lat
          do j = 1, n_lon
            read(file_unit, *) lat(i), lon(j), grid_reg_2d(i, j)
          end do
        end do
      else
        do i = 1, n_lat
          do j = 1, n_lon
            read(file_unit, *) lat_i, lon_i, grid_reg_2d(i, j)
          end do
        end do
      end if
      close(file_unit)
    end if
  case('irr_1d')
    if(present(grid_reg_1d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_reg_1d'
    else if(present(grid_reg_2d) .eqv. .true.) then
      enter_status = 1
      variable_name = 'grid_reg_2d'
    else if(present(n_lat) .eqv. .true.) then
      enter_status = 1
      variable_name = 'n_lat'
    else if(present(grid_irr_1d) .eqv. .false.) then
      enter_status = 2
      variable_name = 'grid_irr_1d'
    else if(present(n_points) .eqv. .false.) then
      enter_status = 2
      variable_name = 'n_points'
    else if((present(lat) .eqv. .false.) .or. (present(lon) .eqv. .false.)) then
      enter_status = 2
      variable_name = 'lat or lon'
    end if
    if(enter_status == 0) then
      n_lines = 0
      do
        read(file_unit, *, iostat = io_status)
        if(io_status == 0) then
          n_lines = n_lines + 1
          cycle
        else
          exit
        end if
      end do
      rewind(file_unit)
      n_points = n_lines
      allocate(lat(n_points), lon(n_points), grid_irr_1d(n_points))
      do i = 1, n_points
        read(file_unit, *) lat(i), lon(i), grid_irr_1d(i)
      end do
      return
    end if
  end select
  select case(enter_status)
  case(0)
    return
  case(1)
    write(stdout, *)
    write(stdout, *) 'error: output variable "', variable_name,&
    '" do not match mode ', '"', mode,'"'
    stop
  case(2)
    write(stdout, *)
    write(stdout, *) 'error: there are no necessary output variable',&
    '"', variable_name, '"', 'in mode ', '"', mode, '"'
    stop
  end select
end subroutine grid_reader
subroutine grid_writer(file_name, grid, interval, nlat, nlon, north, west)
  implicit none
  character(*), intent(in) :: file_name
  real(kind=8), dimension(:,:), intent(in) :: grid
  real(kind=8), intent(in) :: interval, north, west
  integer(kind=4), intent(in) :: nlat, nlon
  integer(kind=4) :: i, j, file_unit
  real(kind=8) :: lat, lon
  open(newunit = file_unit, file = file_name, action = 'write', status = 'replace')
  lat = north
  do i = 1, nlat
    lon = west
    do j = 1, nlon
      write(file_unit, *) lat, lon, grid(i, j)
      lon = lon + interval
    end do
    lat = lat - interval
  end do

end subroutine grid_writer
subroutine sh_writer(file_name, cilm, lmax)
  implicit none
  character(*), intent(in) :: file_name
  real(kind=8), dimension(:,:,:) :: cilm
  integer(kind=4) :: file_unit, i, j, lmax
  open(newunit = file_unit, file = file_name, action = 'write')
  do i = 0, lmax
    do j = 0, i
      write(file_unit, *) i, j, cilm(1, i + 1, j + 1), cilm(2, i + 1, j + 1)
    end do
  end do
  close(file_unit)
  return
end subroutine sh_writer
subroutine sh_reader(file_name, cilm, lmax)
  implicit none
  character(*), intent(in) :: file_name
  real(kind=8), dimension(:,:,:), intent(out) :: cilm
  integer(kind=4), intent(in) :: lmax
  integer(kind=4) :: i, j, file_unit, degree, order
  open(newunit = file_unit, file = file_name, action = 'read')
  do i = 0, lmax
    do j = 0, i
      read(file_unit, *) degree, order, cilm(1, i + 1, j + 1), cilm(2, i + 1, j + 1)
      cilm(1, j + 1, i + 1) = cilm(1, i + 1, j + 1)
      cilm(2, j + 1, i + 1) = cilm(2, i + 1, j + 1)
    end do
  end do
  return
end subroutine sh_reader
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
!subroutine expandint(grid, n, cilm, lmax)
  !implicit none
  !real(kind=8), intent(in), dimension(:,:) :: grid
  !integer(kind=4), intent(in) :: n, lmax
  !real(kind=8), intent(in), dimension(:,:,:) :: cilm
  !integer(kind=4) :: n_lat, n_lon, n_leg, lat, lon, degree, order
  !real(kind=8) :: lat_step, lon_step, pi, rad_lon_step, square, north_lat,&
  !cos_lat, center_lat, west_lon, east_lon, cos_east_lon, cos_west_lon,&
  !sin_east_lon, sin_west_lon, current_value
  !real(kind=8), dimension(:), allocatable :: leg
  !real(kind=8), dimension(:,:,:), allocatable :: current_cilm
  !write(*, *)
  !n_lat = n
  !n_lon = n * 2
  !n_leg = (lmax + 1) * (lmax + 2) / 2
  !allocate(leg(n_leg))
  !lat_step = 180._8 / real(n_lat, kind = 8)
  !lon_step = lat_step
  !pi = 4._8 * datan(1._8)
  !rad_lon_step = lon_step * pi / 180._8
  !square = 4._8 * 32400._8 / (pi * 4._8 * datan(1._8) * lat_step * lon_step)
  !north_lat = 90._8
  !do lat = 1, n_lat
    !leg = 0._8
    !call int_ass_leg(north_lat, lat_step, lmax, n_leg, leg, 5)
    !cos_lat = dcosd(north_lat - lat_step / 2._8) / square
    !west_lon = 0._8
    !do lon = 1, n_lon
      !east_lon = west_lon + lon_step
      !cos_east_lon = dcosd(east_lon)
      !cos_west_lon = dcosd(west_lon)
      !sin_east_lon = dsind(east_lon)
      !sin_west_lon = dsind(west_lon)
      !current_value = grid(lat, lon) * cos_lat
      !current_cilm
      !do degree = 0, lmax
        !do order = 0, degree
          
        !end do
      !end do
    !end do
    !!current_lat = current_lat + lat_step
  !end do
!end subroutine expandint
!subroutine int_ass_leg(lat, lat_size, lmax, n_leg, leg, n_samples)
!implicit none
!! i_max - число полиномов
!! lat - северная широта трапеции
!! lat_size - размер трапеции
!! leg() - интегрированные полиномы Лежандра
!! lat_step - шаг интегрирования (по широте)
!! current_lat - текущая широта "шага"
!! ass_leg() - присоединенные функции Лежандра (вычисляются в l_pn_pr)
!! weight - вес i-ый / сумму весов
!! weights() - веса
!integer(kind=4), intent(in) :: lmax, n_leg
!real(kind=8), intent(in) :: lat, lat_size
!real(kind=8), dimension(:) :: leg
!integer(kind=4) :: i, j, n_samples
!real(kind=8) :: lat_step, current_lat, weight
!real(kind=8), dimension(:) :: ass_leg(n_leg)
!real(kind=8), dimension(:), allocatable :: weights
!allocate(weights(n_samples))
!j = (n_samples + 1) / 2
!do i = 1, j
  !weights(i) = i
  !weights(n_samples - i + 1) = i
!end do
!ass_leg = 0._8
!lat_step = lat_size / 4._8
!current_lat = lat
!do i = 1, 5
  !!call l_pn_pr(Pn_a, n, dcosd(Bt), dsind(Bt))
  !call plmbar(ass_leg, lmax, dsind(current_lat))
  !weight = weights(i) / 9._8
  !do j = 1, n_leg
    !leg(j) = leg(j) + ass_leg(j) * weight
  !end do
  !current_lat = current_lat - lat_step
!end do
!end subroutine int_ass_leg
subroutine l_pn_pr( Pol,Nst,cB,sB )
!  Pol   - полиномы Лежандра
!  Nst   - степень разложения (НЕ количество степеней)
!  cB,sB - косинус и синус широты
!  порядок записи- как верхняя треугольная матрица 
!                  номер строки = номер порядка + 1
!                  номер столбца = номер степени + 1 
!  внешние параметры
   integer(kind=4) :: Nst
   real(kind=8) :: Pol(*),cB,sB
!  внутренние параметры
   integer(kind=4) :: l,iPr,iSt
   real(kind=8) :: C1,C2
!  используемые функции
!   integer*4    myindx
 do iPr = 0, Nst
  l = myindx( iPr+1,iPr+1 )
  if( iPr.eq.0 ) then
   Pol(l) = 1.d0
  elseif( iPr.eq.1 ) then
   Pol(l) = dsqrt( 3.d0 )*cB
  elseif( iPr.ge.2 ) then
   Pol(l) = dsqrt( (2*iPr+1)/(2.d0*iPr) ) * Pol(myindx(iPr,iPr)) * cB
  endif
  do iSt = iPr+1, Nst
   l = myindx( iPr+1,iSt+1 )
   C1 = sB*dsqrt( ( 4.d0*iSt**2-1.d0 )/( iSt**2 - iPr**2 + 0.d0 ) )
    if( iSt.gt.1 ) then
     C2 = (2*iSt+1.d0)/(2.d0*iSt-3.d0)*((iSt-1.)**2-iPr**2)/(iSt**2-iPr**2+0.d0)
    if( C2.ge.0 ) then
     C2 = dsqrt( C2 )
    else
     C2 = 0.d0
    endif
    Pol(l) = C1*Pol( myindx(iPr+1,iSt) ) - C2*Pol( myindx(iPr+1,iSt-1) )
   else
    Pol(l) = C1*Pol( myindx(iPr+1,iSt) )
   endif
  enddo
 enddo
 return
end subroutine l_pn_pr
integer(4) function myindx(i,j)
    implicit none
    integer(4), intent(in) :: i, j
!    i, j - должны начинаться с 1
    if((i-j) < 0) then
        myindx=j*(j-1)/2+i
        return
    else
        myindx=i*(i-1)/2+j
        return
    end if
end function myindx
!1. ofelia_2
subroutine ofelia_2(B, dB, n, nn, P)
implicit none
! i_max - число полиномов
! B - северная широта трапеции
! dB - размер трапеции
! P() - интегрированные полиномы Лежандра
! dBi - шаг интегрирования (по широте)
! Bt - текущая широта "шага"
! sB - sin(Bt)
! cB - cos(Bt)
! Pn() - присоединенные полиномы Лежандра (вычисляются в l_pn_pr)
! w - вес i-ый / сумму весов
! ves() - веса
integer(4), intent(in) :: n, nn
real(8), intent(in) :: B, dB
real(8), intent(out) :: P(*)
integer(4) i, j
real(8) dBi, Bt, sB, cB, Pn(nn), w, ves(5)
ves(1) = 1
ves(2) = 2
ves(3) = 3
ves(4) = 2
ves(5) = 1
Pn = 0
dBi = dB / 4
Bt = B
do i = 1, 5
    cB = dcosd(Bt)
    sB = dsind(Bt)
    call l_pn_pr(Pn, n, cB, sB)
    w = ves(i) / 9.d0
    do j = 1, nn
        P(j) = P(j) + Pn(j) * w
    end do
    Bt = Bt - dBi
end do
end subroutine ofelia_2
!5. shetim
!****************************************************************************
! SUBROUTINE SHETIM (Spherical Harmonic Expansion Tabular Integration Method)
! PURPOSE:  spherical harmonic expansion by tabular integration method
!           (Zhongolovich method)
!****************************************************************************
subroutine shetim(lmax, f, lat_step, ab)
!use variables
implicit none
! Variables
! Input
! n - максимальная степень разложения
! nY - количество трапеций
! nAB - количество коэффициентов
! Y() - средние значения фукции по трапециям (массив)
! Bn - начальная широта
! dB/dL - шаг трапеции по широте/долготе
!type(egrid), intent(in) :: grd
real(kind=8), intent(in) :: lat_step 
!type(degval), intent(in) :: n
integer(kind=4), intent(in) :: lmax
!real(kind=8) :: f(grd%nt)
real(kind=8), intent(in) :: f(*)
! Output
! AB() - коэффициенты (массив)
real(8), intent(out), dimension(:) :: AB
! Internal
integer(4) :: i, j, ii, jj, l, k, t, n_leg, n_coeff, n_lat, n_lon, n_blocks
real(8) :: PB(:), Pnm(:), Bi, dLr, ft, dw, Lw, Le, pi,&
            cBi, siLe, siLw, ciLe, ciLw, cLe, cLw, sLe, sLw, ctLe, ctLw,&
            status_i, t_begin, t_end,&
            north_lat, south_lat, west_lon, east_lon, lon_step
character(len=:), allocatable :: progress_bar_string
allocatable PB, Pnm
call cpu_time(t_begin)
pi = 3.1415926535897932384626433832795_8
north_lat = 90._8
south_lat = -90._8
west_lon = 0._8
east_lon = 360._8
lon_step = lat_step
n_leg = (lmax + 1) * (lmax + 2) / 2
n_coeff = (lmax + 1) * (lmax + 2) - lmax - 1
n_lat = int(180._8 / lat_step, kind = 4)
n_lon = n_lat * 2
allocate( PB(n_leg), Pnm(n_coeff) )
! data display
status_i = 100.0 / n_lat
! progress bar
progress_bar_string = ' '
write(6, '(a)', advance = 'no') progress_bar_string
write(6, '(f5.1)', advance = 'no') 0.0_8
t = 1
Bi = north_lat 
AB = 0._8
dLr = lon_step * pi / 180._8
dw = 4 * 32400 / (pi * lat_step * lon_step)
do i = 1, n_lat
  PB = 0._8
  call ofelia_2(Bi, lat_step, lmax, n_leg, PB)
  cBi = dcosd(Bi - lat_step / 2._8)/ dw
  Lw = west_lon
  do j = 1, n_lon
    Le = Lw + lon_step
    cLe = dcosd(Le)
    cLw = dcosd(Lw)
    sLe = dsind(Le)
    sLw = dsind(Lw)
    ft = f(t) * cBi
    Pnm(1) = PB(1) * ft
    k = 1
    l = 1
    do ii = 1, lmax
      k = k + 1
      l = l + 1
      Pnm(k) = PB(l) * ft
      ciLe = 1.d0
      siLe = 0.d0
      ciLw = 1.d0
      siLw = 0.d0
      do jj = 1, ii
        ctLe = ciLe
        ciLe = ciLe * cLe - siLe * sLe
        siLe = siLe * cLe + sLe * ctLe
        ctLw = ciLw
        ciLw = ciLw * cLw - siLw * sLw
        siLw = siLw * cLw + sLw * ctLw
        l = l + 1
        k = k + 1
        Pnm(k) = PB(l) * ((siLe - siLw) / jj) / dLr * ft
        k = k + 1
        Pnm(k) = PB(l) * ((ciLw - ciLe) / jj) / dLr * ft
      end do
    end do
    AB = AB + Pnm
    t = t + 1
    Lw = Le
  end do
  Bi = Bi - lat_step
! progress bar
  do ii = 1, len(progress_bar_string) + 6
    write(6, '(a)', advance = 'no') '\b'
  end do
  write(6, '(a)', advance = 'no') progress_bar_string
  write(6, '(f5.1, a)', advance = 'no') real(i, 4) * status_i, '%'
end do
write(6,'(a)', advance = 'no') ''
call cpu_time(t_end)
!write(6, *) 'expand time: ', number_to_string((t_end - t_begin) / 60.0)
end subroutine shetim
!3. indx_2  по индексам двухмерного массива определят
!           соответствющий индекс одномерного массива
!           массив вида: 1  2  5  10  17 ...
!                           3  6  11  18 ...
!                           4  7  12  19 ...
!                              8  13  20 ...
!                              9  14  21 ...
!                                 15  22 ...
!                                 16  23 ...
!                                     24 ...
!                                     25 ...
integer(4) function indx_2(i,j)
    implicit none
    integer(4), intent(in) :: i, j
    if((i-j)<0)then
        indx_2=j*(j-1)+2*i-j
    else
        indx_2=i*(i-1)+2*j-i
    end if
end function indx_2
function mem_info_int(value, units_mode)
  implicit none
  integer(kind=8), intent(in) :: value
  character(len=:), allocatable :: mem_info_int
  character(*), intent(in) :: units_mode
  character(len=:), allocatable :: units
  integer(kind=4) :: i
  real(kind=8) :: value_double 
  i = 1
  select case(units_mode)
  case('b')
    value_double = real(value, 8)
    units = 'b'
  case('B')
    value_double = real(value, 8) / 8._8
    units = 'B'
  end select
  do
    if(value_double < 1000) then
      select case(i)
      case(2)
        units = ' k'//units
      case(3)
        units = ' M'//units
      case(4)
        units = ' G'//units
      case(5)
        units = ' T'//units
      case(6)
        units = ' P'//units
      case(7)
        units = ' E'//units
      case(8)
        units = ' Z'//units
      case(9)
        units = ' Y'//units
      end select
      mem_info_int = double_to_string(value_double, '(f100.1)')//units
      return
    end if
    value_double = value_double / 1000._8
    i = i + 1
  end do
end function mem_info_int

end module subroutines

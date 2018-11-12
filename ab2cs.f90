program ab2cs
  use subroutines
  implicit none
  real(kind = 8), dimension(:), allocatable :: coeff_cs, coeff_ab
  integer(kind = 4) :: lmax, stdout, i
  character(len = 1000) :: arg
  stdout = 6
  i = 0
  do while(i < command_argument_count())
    i = i + 1
    call get_command_argument(i, arg)
    select case(arg)
    case('-deg')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) lmax
      write(stdout, *) 'degree: ', integer_to_string(lmax)
    case('-in')
      i = i + 1
      call get_command_argument(i, arg)
      input_file_name = trim(adjustl(arg))
      write(stdout, *) 'input: ', input_file_name
    case('-out')
      i = i + 1
      call get_command_argument(i, arg)
      output_file_name = trim(adjustl(arg))
      write(stdout, *) 'output: ', output_file_name
    case('-gm')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) gm
      write(stdout, *) 'gm: ', double_to_string(gm)
    case('-ae')
      i = i + 1
      call get_command_argument(i, arg)
      read(arg, *) ae
      write(stdout, *) 'ae: ', double_to_string(ae) 
    end select
  end do
  call cstoab(.false., lmax, )
end program ab2cs

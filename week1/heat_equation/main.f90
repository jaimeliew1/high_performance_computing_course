! Exercise 1b: FORTRAN program to solve the 2D heat equation
! author: Jaime Liew (jyli@dtu.dk)

PROGRAM main
  USE m_global, ONLY: Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter
  !USE m_routines
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE read_input(filename, Nx, Ny, D, T_max)
       CHARACTER(len=*), INTENT(in) :: filename
       INTEGER, INTENT(out) :: Nx, Ny
       REAL, INTENT(out) :: D, T_max
     END SUBROUTINE read_input

     SUBROUTINE init(Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter)
       INTEGER, INTENT(in) :: Nx, Ny  ! Number of grid points on x and y direction
       REAL, INTENT(in) :: D
       REAL, INTENT(in) :: T_max
       REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
       REAL, INTENT(out) :: dx, dy, dt
       INTEGER, INTENT(out) :: N_iter
     END SUBROUTINE init

     SUBROUTINE save_output(fileroot, T, Nx, Ny, dx, dy, step_no)
       CHARACTER(LEN=*), INTENT(in) :: fileroot
       REAL, DIMENSION(:,:) :: T
       INTEGER :: Nx, Ny
       REAL :: dx, dy
       INTEGER, OPTIONAL :: step_no
     END SUBROUTINE save_output

     SUBROUTINE update_memory(A, A_old)
       REAL, DIMENSION(:, :), INTENT(inout) :: A, A_old
     END SUBROUTINE update_memory

     FUNCTION step(T_old, D, Nx, Ny, dx, dy, dt)
       REAL, DIMENSION(:, :) :: T_old
       INTEGER :: Nx, Ny
       REAL :: dx, dy, dt, D
       REAL, DIMENSION(:, :), ALLOCATABLE :: step
     END FUNCTION step

     SUBROUTINE diagnostic(step_no, T, close_file)
       INTEGER, INTENT(in) :: step_no
       REAL, DIMENSION(:, :), INTENT(in) :: T
       LOGICAL, OPTIONAL :: close_file
     END SUBROUTINE diagnostic
  END INTERFACE

  INTEGER :: i, j
  INTEGER:: t_start, t_stop, count_rate ! system clock variables
  REAL :: cpu_t_start, cpu_t_stop ! CPU clock variables

  CHARACTER(len=8) :: date
  CHARACTER(len=10) :: time

  CALL DATE_AND_TIME(DATE=date, TIME=time)
  PRINT*, 'Date: ', date(7:8), '/',date(5:6), '/',date(1:4)
  PRINT*, 'Time: ', time(1:2), ':',time(3:4), ':',time(5:)

  CALL read_input('params.in', Nx, Ny, D, T_max)

  CALL init(Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter)

  ! Solve the heat equation iteratively
  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)
  CALL CPU_TIME(TIME=cpu_t_start)

!!!!! MAIN LOOP !!!!!
  DO i = 1, N_iter
     T = step(T_old, D, Nx, Ny, dx, dy, dt)
     CALL update_memory(T, T_old)
     CALL save_output('diff', T, Nx, Ny, dx, dy, i)
     IF (MOD(i, 10) == 0) THEN
        CALL diagnostic(i, T)
     ENDIF
  ENDDO

  CALL diagnostic(i, T, close_file=.TRUE.)
  CALL SYSTEM_CLOCK(COUNT=t_stop)
  CALL CPU_TIME(TIME=cpu_t_stop)

  CALL save_output('final', T, Nx, Ny, dx, dy)

  PRINT*, 'System time elapsed [s]: ', (t_stop-t_start)/REAL(count_rate)
  PRINT*, 'CPU time elapsed [s]: ', cpu_t_stop - cpu_t_start
END PROGRAM main

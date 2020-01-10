! FORTRAN program to solve the 2D heat equation
!  Version  : 1.0
!  Author   : Jaime Liew (jyli@dtu.dk)
!  Created  : 10/1/2020

PROGRAM main
  USE m_global, ONLY: Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter
  USE m_read_input, ONLY: read_input
  USE m_init, ONLY: init
  USE m_update_memory, ONLY: update_memory
  USE m_step, ONLY: step
  USE m_save_output, ONLY: save_output
  USE m_diagnostic, ONLY: diagnostic

  IMPLICIT NONE

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

  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)
  CALL CPU_TIME(TIME=cpu_t_start)

  !!!!! MAIN LOOP !!!!! Solve the heat equation iteratively
  DO i = 1, N_iter
     T = step(T_old, D, Nx, Ny, dx, dy, dt)
     CALL update_memory(T, T_old)
     CALL save_output('diff', T, Nx, Ny, dx, dy, i)

     IF (MOD(i, 10) == 0) THEN
        CALL diagnostic(i, T)
     ENDIF
  ENDDO

  CALL SYSTEM_CLOCK(COUNT=t_stop)
  CALL CPU_TIME(TIME=cpu_t_stop)
  PRINT*, 'System time elapsed [s]: ', (t_stop-t_start)/REAL(count_rate)
  PRINT*, 'CPU time elapsed [s]: ', cpu_t_stop - cpu_t_start

  CALL diagnostic(i, T, close_file=.TRUE.)
  CALL save_output('final', T, Nx, Ny, dx, dy)

END PROGRAM main

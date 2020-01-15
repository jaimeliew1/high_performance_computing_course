! FORTRAN program to solve the OpenMp Assignment - Poisson problem
!  Version  : 1.0
!  Authors   : Jaime Liew (jyli@dtu.dk)
!  Created  : 15/1/2020

PROGRAM main
  USE m_global, ONLY: N, U, U_old, f, dx, N_iter, thres, coords
  USE m_read_input, ONLY: read_input
  USE m_init, ONLY: init
  USE m_jacobi, ONLY: jacobi
  USE m_save_output, ONLY: save_output
  

  IMPLICIT NONE

  INTEGER :: i, j
  INTEGER:: t_start, t_stop, count_rate ! system clock variables
  REAL :: cpu_t_start, cpu_t_stop ! CPU clock variables
  CHARACTER(len=8) :: date
  CHARACTER(len=10) :: time

  CALL DATE_AND_TIME(DATE=date, TIME=time)
  PRINT*, 'Date: ', date(7:8), '/',date(5:6), '/',date(1:4)
  PRINT*, 'Time: ', time(1:2), ':',time(3:4), ':',time(5:)

  CALL read_input('params.in', N, N_iter, thres)

  CALL init(N, U, U_old, f, dx, coords)

  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)
  CALL CPU_TIME(TIME=cpu_t_start)

!!!!! MAIN LOOP !!!!! Solve the heat equation iteratively
  call jacobi(U, U_old, N, f, dx, N_iter, thres)

  CALL SYSTEM_CLOCK(COUNT=t_stop)
  CALL CPU_TIME(TIME=cpu_t_stop)
  PRINT*, 'System time elapsed [s]: ', (t_stop-t_start)/REAL(count_rate)
  PRINT*, 'CPU time elapsed [s]: ', cpu_t_stop - cpu_t_start

  !CALL diagnostic(i, T, close_file=.TRUE.)
  CALL save_output('final', N, U, coords)

  DEALLOCATE(U)
  DEALLOCATE(U_old)
  DEALLOCATE(f)
  DEALLOCATE(coords)

END PROGRAM main

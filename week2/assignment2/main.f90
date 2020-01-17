! FORTRAN program to solve the OpenMp Assignment - Poisson problem
!  Version  : 1.0
!  Authors   : Jaime Liew (jyli@dtu.dk)
!  Created  : 15/1/2020

PROGRAM main
  USE m_global, ONLY: N, U, U_old, f, dx, N_iter, thres, coords
  USE m_read_input, ONLY: read_input
  USE m_init, ONLY: init
  USE m_jacobi_naive, ONLY: jacobi_naive
  USE m_jacobi_optimised, ONLY: jacobi_optimised
  USE m_gauss_seidel, ONLY: gauss_seidel
  USE m_save_output, ONLY: save_output
  USE omp_lib, ONLY: omp_get_max_threads
  IMPLICIT NONE

  INTEGER :: i, j
  INTEGER :: strategy, actual_iters
  INTEGER:: t_start, t_stop, count_rate ! system clock variables

  CALL read_input('params.in', N, N_iter, thres, strategy)

  CALL init(N, U, U_old, f, dx, coords)

  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)

!!!!! MAIN LOOP !!!!! Solve the heat equation iteratively
  IF (strategy == 1) THEN
     CALL jacobi_naive(U, U_old, N, f, dx, N_iter, thres, actual_iters)
  ELSEIF (strategy == 4) THEN
     CALL jacobi_optimised(U, U_old, N, f, dx, N_iter, thres, actual_iters)
  ELSEIF (strategy == 0) THEN
     CALL gauss_seidel(U, U_old, N, f, dx, N_iter, thres)
  ENDIF

  CALL SYSTEM_CLOCK(COUNT=t_stop)
  PRINT*,omp_get_max_threads(),  actual_iters, (t_stop-t_start)/REAL(count_rate)

  CALL save_output('final', N, U, coords)

  DEALLOCATE(U)
  DEALLOCATE(U_old)
  DEALLOCATE(f)
  DEALLOCATE(coords)

END PROGRAM main

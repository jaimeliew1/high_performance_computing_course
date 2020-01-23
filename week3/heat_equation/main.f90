! FORTRAN program to solve the 2D heat equation
! Parallelised using MPI
!  Version  : 1.0
!  Author   : Jaime Liew (jyli@dtu.dk)
!  Created  : 23/1/2020

PROGRAM main
  USE m_global, ONLY: Nx, Ny, Nx_local, D, T_max, T, T_old, dx, dy, dt, N_iter
  USE m_read_input, ONLY: read_input
  USE m_init, ONLY: init
  USE m_exchange_boundary, ONLY: exchange_boundary
  USE m_update_memory, ONLY: update_memory
  USE m_step, ONLY: step
  USE m_save_output, ONLY: save_output
  USE m_diagnostic, ONLY: diagnostic

  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER :: rank, n_proc, ierror
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER :: i, j

  CALL MPI_init(ierror)
  CALL MPI_COMM_rank(MPI_COMM_WORLD, rank, ierror)
  CALL MPI_COMM_size(MPI_COMM_WORLD, n_proc, ierror)

  CALL read_input('params.in', Nx, Ny, D, T_max, rank, n_proc)

  CALL init(Nx, Ny, Nx_local, D, T_max, T, T_old, dx, dy, dt, N_iter, rank, n_proc)

  PRINT*, 'I am process ', rank, '. My strip is ', Nx_local, ' points wide.'

!!!!! MAIN LOOP !!!!! Solve the heat equation iteratively
  DO i = 1, N_iter

    ! Perform MPI data exchange (if there are multiple processes)
     IF (n_proc > 1) THEN
        CALL exchange_boundary(T_old, Nx_local, Ny, rank, n_proc)
     ENDIF

     CALL step(T, T_old, D, Nx_local+2, Ny+2, dx, dy, dt)

     CALL update_memory(T, T_old)

     IF (MOD(i, 10) == 0) THEN
     ENDIF
  ENDDO

  CALL save_output('final', T_old(2:Nx_local+1, 2:Ny), Nx_local, Ny, dx, dy, step_no=rank)
  CALL MPI_finalize(ierror)
END PROGRAM main

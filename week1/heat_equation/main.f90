! Exercise 1b: FORTRAN program to solve the 2D heat equation
! author: Jaime Liew (jyli@dtu.dk)

PROGRAM main
  USE m_global, only: Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter
  !USE m_routines
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE init(Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter)
       IMPLICIT NONE
       INTEGER, INTENT(in) :: Nx, Ny  ! Number of grid points on x and y direction
       REAL, INTENT(in) :: D
       REAL, INTENT(in) :: T_max
       REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
       REAL, INTENT(out) :: dx, dy, dt
       INTEGER, INTENT(out) :: N_iter
     END SUBROUTINE init

     SUBROUTINE save_output(fileroot, T, Nx, Ny, dx, dy, step_no)
       IMPLICIT NONE
       CHARACTER(LEN=*), INTENT(in) :: fileroot
       REAL, DIMENSION(:,:) :: T
       INTEGER :: Nx, Ny
       REAL :: dx, dy
       INTEGER, OPTIONAL :: step_no
     END SUBROUTINE save_output

     SUBROUTINE update_memory(A, A_old)
       REAL, DIMENSION(:, :), INTENT(inout) :: A, A_old
     END SUBROUTINE update_memory

     FUNCTION step(T_old, D, Nx, Ny, dx, dy, dt) ! TO BE FINISHED!
       REAL, DIMENSION(:, :) :: T_old
       INTEGER :: Nx, Ny
       REAL :: dx, dy, dt, D
       REAL, DIMENSION(:, :), ALLOCATABLE :: step
     END FUNCTION step
  END INTERFACE

  INTEGER :: i, j

  CALL init(Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter)

  ! Solve the heat equation iteratively
  DO i = 1, N_iter
     T = step(T_old, D, Nx, Ny, dx, dy, dt)
     CALL update_memory(T, T_old)
     CALL save_output('diff', T, Nx, Ny, dx, dy, i)
  ENDDO

  CALL save_output('final', T, Nx, Ny, dx, dy)

END PROGRAM main

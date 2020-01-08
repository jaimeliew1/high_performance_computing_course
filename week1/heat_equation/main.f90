! Exercise 1b: FORTRAN program to solve the 2D heat equation
! author: Jaime Liew (jyli@dtu.dk)

PROGRAM main
  USE m_global
  !USE m_routines
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE init(N, D, T_max, T, T_old, dx, dt, N_iter)
       IMPLICIT NONE
       INTEGER, INTENT(in) :: N  ! Number of grid points on x and y direction
       REAL, INTENT(in) :: D
       REAL, INTENT(in) :: T_max
       REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
       REAL, INTENT(out) :: dx, dt
       INTEGER, INTENT(out) :: N_iter
     END SUBROUTINE init

     SUBROUTINE save_output(fileroot, T, N, dx, step_no)
       IMPLICIT NONE
       CHARACTER(LEN=*), INTENT(in) :: fileroot
       REAL, DIMENSION(:,:) :: T
       INTEGER :: N
       REAL :: dx
       INTEGER, OPTIONAL :: step_no
     END SUBROUTINE save_output

     SUBROUTINE update_memory(A, A_old)
       REAL, DIMENSION(:, :), INTENT(inout) :: A, A_old
     END SUBROUTINE update_memory

     FUNCTION step(T_old, D, N, dx, dt) ! TO BE FINISHED!
       REAL, DIMENSION(:, :) :: T_old
       INTEGER :: N
       REAL :: dx, dt, D
       REAL, DIMENSION(:, :), ALLOCATABLE :: step
     END FUNCTION step
  END INTERFACE

  INTEGER :: i, j

  CALL init(N, D, T_max, T, T_old, dx, dt, N_iter)

  ! Solve the heat equation iteratively
  DO i = 1,N_iter
     T = step(T_old, D, N, dx, dt)
     CALL update_memory(T, T_old)
     CALL save_output('diff', T, N, dx, i)
  ENDDO

  CALL save_output('final', T, N, dx)

END PROGRAM main

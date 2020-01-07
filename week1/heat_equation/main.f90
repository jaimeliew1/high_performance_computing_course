! Exercise 1b: FORTRAN program to solve the 2D heat equation
! author: Jaime Liew (jyli@dtu.dk)

PROGRAM main
  USE m_global
  USE m_routines
  IMPLICIT NONE
  INTEGER :: i, j


  call init()

  ! Solve the heat equation iteratively
  DO i = 1,N_iter
     T(2:N-1, 2:N-1) =  T_old(2:N-1, 2:N-1)
     T(2:N-1, 2:N-1) =  T(2:N-1, 2:N-1) + D*dt/dx**2*(T_old(2:N-1, 1:N-2) - 2*T_old(2:N-1, 2:N-1) + T_old(2:N-1, 3:N))
     T(2:N-1, 2:N-1) =  T(2:N-1, 2:N-1) + D*dt/dx**2*(T_old(1:N-2, 2:N-1) - 2*T_old(2:N-1, 2:N-1) + T_old(3:N, 2:N-1))
     call update_memory()
     call save_output('diff', i)
  ENDDO

   call save_output('final')

END PROGRAM main

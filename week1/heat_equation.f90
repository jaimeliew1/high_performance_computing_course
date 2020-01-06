! Exercise 1b: FORTRAN program to solve the 2D heat equation
! author: Jaime Liew (jyli@dtu.dk)

PROGRAM heat_equation
  INTEGER, PARAMETER :: N = 21 ! Number of grid points on x and y direction
  REAL, PARAMETER :: D = 1
  REAL, PARAMETER :: T_max = 0.125
  INTEGER :: i, j
  REAL, DIMENSION(:,:), ALLOCATABLE :: T_old
  REAL, DIMENSION(:,:), ALLOCATABLE :: T
  REAL :: dx
  REAL:: dt
  INTEGER :: N_iter
  REAL:: fourier_limit

  ALLOCATE(T(N, N))
  ALLOCATE(T_old(N, N))

  dx = 1.0/(REAL(N) - 1)
  fourier_limit = dx**2/(4*D)
  dt = fourier_limit
  N_iter = T_max/dt + 1

  ! Set initial condition
  T_old = 0

  ! Set boundary conditions
  T_old(1, :) = 1
  T_old(N, :) = 1
  T_old(:, 1) = 1
  T_old(:, N) = 1

  T(1, :) = 1
  T(N, :) = 1
  T(:, 1) = 1
  T(:, N) = 1


  PRINT*, 'N: ', N
  PRINT*, 'dx: ', dx
  PRINT*, 'dt: ', dt
  PRINT*, 'fourier_limit: ', fourier_limit
  PRINT*, 'N_iter: ', N_iter

  ! Solve the heat equation iteratively
  DO i = 1,N_iter
     T(2:N-1, 2:N-1) =  T_old(2:N-1, 2:N-1)
     T(2:N-1, 2:N-1) =  T(2:N-1, 2:N-1) + D*dt/dx**2*(T_old(2:N-1, 1:N-2) - 2*T_old(2:N-1, 2:N-1) + T_old(2:N-1, 3:N))
     T(2:N-1, 2:N-1) =  T(2:N-1, 2:N-1) + D*dt/dx**2*(T_old(1:N-2, 2:N-1) - 2*T_old(2:N-1, 2:N-1) + T_old(3:N, 2:N-1))
     T_old = T
  ENDDO


  ! save result to file
  OPEN(10,FILE='diff.dat')
  DO j=1,N
     DO i=1,N
        WRITE(10,'(3E12.4)') REAL(i-1)*dx,REAL(j-1)*dx,T(i,j)
     ENDDO
     WRITE(10,'(A)') ! Will produce a new empty line – and tell gnuplot to lift the pen
  ENDDO
  CLOSE(10)
END PROGRAM heat_equation

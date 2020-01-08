SUBROUTINE init(N, D, T_max, T, T_old, dx, dt, N_iter)
  IMPLICIT NONE
  INTEGER, INTENT(in) :: N  ! Number of grid points on x and y direction
  REAL, INTENT(in) :: D
  REAL, INTENT(in) :: T_max
  REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
  REAL, INTENT(out) :: dx, dt
  INTEGER, INTENT(out) :: N_iter

  INTEGER :: i, j
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
END SUBROUTINE init

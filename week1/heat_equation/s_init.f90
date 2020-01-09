SUBROUTINE init(Nx, Ny, D, T_max, T, T_old, dx, dy, dt, N_iter)
  ! Initialises variables for the heat equation solver.
  IMPLICIT NONE
  INTEGER, INTENT(in) :: Nx, Ny  ! Number of grid points on x and y direction
  REAL, INTENT(in) :: D
  REAL, INTENT(in) :: T_max
  REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
  REAL, INTENT(out) :: dx, dy, dt
  INTEGER, INTENT(out) :: N_iter

  INTEGER :: i, j, info
  REAL:: fourier_limit


  INTERFACE alloc
     SUBROUTINE salloc(field, Nx, Ny, info)
       INTEGER, PARAMETER :: MK = KIND(1.0E0)
       REAL(MK), DIMENSION(:, :), ALLOCATABLE, INTENT(out):: field
       INTEGER, INTENT(in) :: Nx, Ny
       INTEGER, INTENT(out) :: info

     END SUBROUTINE salloc

     SUBROUTINE dalloc(field, Nx, Ny, info)
       INTEGER, PARAMETER :: MK = KIND(1.0D0)
       REAL(MK), DIMENSION(:, :), ALLOCATABLE, INTENT(out):: field
       INTEGER, INTENT(in) :: Nx, Ny
       INTEGER, INTENT(out) :: info

     END SUBROUTINE dalloc
  END INTERFACE alloc

  call alloc(T, Nx, Ny, info)
  call alloc(T_old, Nx, Ny, info)


  dx = 1.0/(REAL(Nx) - 1)
  dy = 1.0/(REAL(Ny) - 1)
  fourier_limit = max(dx, dy)**2/(4*D)
  dt = fourier_limit
  N_iter = T_max/dt + 1

  ! Set initial condition
  T_old = 0

  ! Set boundary conditions
  T_old(1, :) = 1
  T_old(Nx, :) = 1
  T_old(:, 1) = 1
  T_old(:, Ny) = 1

  T(1, :) = 1
  T(Nx, :) = 1
  T(:, 1) = 1
  T(:, Ny) = 1


  PRINT*, 'Nx: ', Nx
  PRINT*, 'Ny: ', Ny
  PRINT*, 'dx: ', dx
  PRINT*, 'dy: ', dy
  PRINT*, 'dt: ', dt
  PRINT*, 'fourier_limit: ', fourier_limit
  PRINT*, 'N_iter: ', N_iter
END SUBROUTINE init

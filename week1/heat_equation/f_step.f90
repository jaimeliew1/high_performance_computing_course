FUNCTION step(T_old, D, Nx, Ny, dx, dy, dt)
  ! Performs a single step of the heat equation solver
  REAL, DIMENSION(:, :) :: T_old
  INTEGER :: Nx, Ny
  REAL :: dx, dy, dt, D
  REAL, DIMENSION(:, :), ALLOCATABLE :: step
  ALLOCATE(step(Nx, Ny))
  step =  T_old
  step(2:Nx-1, 2:Ny-1) =  step(2:Nx-1, 2:Ny-1) + D*dt/dy**2*(T_old(2:Nx-1, 1:Ny-2) - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(2:Nx-1, 3:Ny))
  step(2:Nx-1, 2:Ny-1) =  step(2:Nx-1, 2:Ny-1) + D*dt/dx**2*(T_old(1:Nx-2, 2:Ny-1) - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(3:Nx, 2:Ny-1))
END FUNCTION step

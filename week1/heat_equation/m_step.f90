MODULE m_step
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
CONTAINS

  FUNCTION step(T_old, D, Nx, Ny, dx, dy, dt)
    ! Performs a single step of the heat equation solver
    REAL(MK), DIMENSION(:, :) :: T_old
    INTEGER :: Nx, Ny
    REAL(MK) :: dx, dy, dt, D
    REAL(MK), DIMENSION(:, :), ALLOCATABLE :: step

    ALLOCATE(step(Nx, Ny))

    step(1:Nx, 1:Ny) =  T_old(1:Nx, 1:Ny)

    step(2:Nx-1, 2:Ny-1) =  step(2:Nx-1, 2:Ny-1) + D*dt/dy**2*(T_old(2:Nx-1, 1:Ny-2) &
     - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(2:Nx-1, 3:Ny))

    step(2:Nx-1, 2:Ny-1) =  step(2:Nx-1, 2:Ny-1) + D*dt/dx**2*(T_old(1:Nx-2, 2:Ny-1) &
    - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(3:Nx, 2:Ny-1))
  END FUNCTION step

END MODULE m_step

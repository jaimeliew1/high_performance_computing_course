MODULE m_step
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
CONTAINS

  SUBROUTINE step(T, T_old, D, Nx, Ny, dx, dy, dt)
    ! Performs a single step of the heat equation solver
    REAL(MK), DIMENSION(:, :) :: T_old
    REAL(MK), DIMENSION(:, :) :: T
    INTEGER :: Nx, Ny
    REAL(MK) :: dx, dy, dt, D
    INTEGER :: i, j
    REAL(MK) :: C
    C = D*dt/dy**2

    T(2:Nx-1, 2:Ny-1) =  T_old(2:Nx-1, 2:Ny-1) + C*(T_old(2:Nx-1, 1:Ny-2) &
         - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(2:Nx-1, 3:Ny)) &
         + C*(T_old(1:Nx-2, 2:Ny-1) &
         - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(3:Nx, 2:Ny-1))

  END SUBROUTINE step

END MODULE m_step

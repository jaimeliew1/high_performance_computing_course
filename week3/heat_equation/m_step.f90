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
    ! DO i=2, Nx-1
    !    DO j=2, Ny-1
    !       T(i, j) = T_old(i, j) + D*dt/dy**2*(T_old(i, j-1) &
    !            - 2*T_old(i, j) + T_old(i, j+1)) &
    !            + D*dt/dx**2*(T_old(i-1, j) &
    !            - 2*T_old(i, j) + T_old(i+1, j))
    !    ENDDO
    ! ENDDO

    !T(1:Nx, 1:Ny) =  T_old(1:Nx, 1:Ny)

    T(2:Nx-1, 2:Ny-1) =  T_old(2:Nx-1, 2:Ny-1) + C*(T_old(2:Nx-1, 1:Ny-2) &
         - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(2:Nx-1, 3:Ny)) &
         + C*(T_old(1:Nx-2, 2:Ny-1) &
         - 2*T_old(2:Nx-1, 2:Ny-1) + T_old(3:Nx, 2:Ny-1))

  END SUBROUTINE step

END MODULE m_step

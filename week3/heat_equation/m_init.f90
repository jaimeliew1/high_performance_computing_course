MODULE m_init
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
CONTAINS
  SUBROUTINE init(Nx, Ny, Nx_local, D, T_max, T, T_old, dx, dy, dt, N_iter, rank, n_proc)
    ! Initialises variables for the heat equation solver.
    USE m_alloc, ONLY: alloc
    IMPLICIT NONE

    INTEGER, INTENT(in) :: Nx, Ny
    INTEGER, INTENT(out) :: Nx_local
    REAL(MK), INTENT(in) :: D
    REAL(MK), INTENT(in) :: T_max
    REAL(MK), DIMENSION(:,:), ALLOCATABLE :: T, T_old
    REAL(MK), INTENT(out) :: dx, dy, dt
    INTEGER, INTENT(out) :: N_iter
    INTEGER, INTENT(in) :: rank, n_proc

    INTEGER :: i, j, info
    REAL:: fourier_limit
    IF (rank < n_proc-1) THEN
       Nx_local = Nx/n_proc
    ELSE
       Nx_local = Nx - (n_proc-1)*(Nx/n_proc)
    ENDIF


    ! Allocate interior points plus boundary points
    CALL alloc(T, Nx_local+2, Ny+2, info)
    CALL alloc(T_old, Nx_local+2, Ny+2, info)

    dx = 1.0/(REAL(Nx) - 1)
    dy = 1.0/(REAL(Ny) - 1)
    fourier_limit = MAX(dx, dy)**2/(4*D)
    dt = fourier_limit
    N_iter = T_max/dt + 1


    ! Set initial condition
    T_old = 0
    T = 0

    ! Set boundary conditions at top and bottom
    T_old(:, 1) = 1
    T_old(:, Ny+2) = 1
    T(:, 1) = 1
    T(:, Ny+2) = 1

    ! Set boundary conditions at edge strips
    IF (rank == 0) THEN
       T(1, :) = 1
       T_old(1, :) = 1
    ENDIF
    IF (rank == n_proc-1) THEN
       T(Nx_local+2, :) = 1
       T_old(Nx_local+2, :) = 1
    ENDIF

  END SUBROUTINE init

END MODULE m_init

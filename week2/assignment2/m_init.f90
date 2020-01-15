MODULE m_init
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
CONTAINS
  SUBROUTINE init(N, U, U_old, f, dx, coords)
    ! Initialises variables for the heat equation solver.
    USE m_alloc, ONLY: alloc
    IMPLICIT NONE

    INTEGER, INTENT(in) :: N
    REAL(MK), DIMENSION(:,:), ALLOCATABLE :: U, U_old, f
    REAL(MK), INTENT(out) :: dx
    REAL(MK), DIMENSION(:), allocatable :: coords

    INTEGER :: i, j, info

    CALL alloc(U, N+2, N+2, info)
    CALL alloc(U_old, N+2, N+2, info)
    CALL alloc(f, N+2, N+2, info)
    ALLOCATE(coords(N+2))

    dx = 2/REAL(N+1)

    DO i=1,N+2
       coords(i) = -1 + dx*(i-1)
       !PRINT*,i, coords(i)
    ENDDO


    ! Set external heat source
    f = 0
    DO i=1, N+2
       DO j=1, N+2
          IF ((0 .LE. coords(i)) .AND.( coords(i) .LE. 1/3.0)) THEN
             IF ((-2/3.0 .LE. coords(j)) .AND. (coords(j) .LE. -1/3.0)) THEN
                f(i, j) = 200
             ENDIF
          ENDIF
       ENDDO
    ENDDO

    ! Set boundary conditions
    U_old(1, :) = 20
    U_old(N+2, :) = 20
    U_old(:, 1) = 0
    U_old(:, N+2) = 20

    U(1, :) = 20
    U(N+2, :) = 20
    U(:, 1) = 0
    U(:, N+2) = 20

  END SUBROUTINE init

END MODULE m_init

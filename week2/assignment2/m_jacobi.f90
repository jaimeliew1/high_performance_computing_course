MODULE m_jacobi
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
CONTAINS

  SUBROUTINE jacobi(U, U_old, N, f, dx, N_iter, thres)
    USE m_diagnostic, ONLY: diagnostic
    ! Performs a single step of the heat equation solver
    REAL(MK), DIMENSION(:, :), INTENT(INOUT) :: U, U_old
    REAL(MK), DIMENSION(:, :), INTENT(IN) :: f
    INTEGER, INTENT(IN) :: N, N_iter
    REAL(MK), INTENT(IN) :: dx, thres

    REAL(MK) :: a_quarter = 1/4.0
    REAL(MK) :: norm
    LOGICAL :: converged
    INTEGER :: i, j, k

    converged = .FALSE.

    DO k=1,N_iter
       norm = 0
       DO i=2,N+1
          DO j=2,N+1
             U(i, j) = a_quarter*(U_old(i, j-1) + U_old(i,j+1) &
                  + U_old(i-1, j) + U_old(i+1, j)&
                  + dx**2*f(i, j))
             norm = norm + (U(i, j) - U_old(i, j))**2
          ENDDO
       ENDDO

       norm = SQRT(norm)

       IF (MOD(k, 100) == 0) THEN
          CALL diagnostic(k, norm)
       ENDIF

       IF (norm < thres) THEN
          converged = .TRUE.
          EXIT
       ENDIF

       U_old = U

    ENDDO

    IF (converged) THEN
       PRINT*, 'iterations: ', k
    ELSE
       PRINT*, 'NO CONVERGENCE'
    ENDIF

    CALL diagnostic(k, norm, .TRUE.)
  END SUBROUTINE jacobi

END MODULE m_jacobi

PROGRAM matrix_times_vector
  IMPLICIT NONE
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
  INTEGER, PARAMETER :: N = 500
  INTEGER :: i
  INTEGER:: t_start, t_end, count_rate
  REAL(MK), DIMENSION(N, N) :: A
  REAL(MK), DIMENSION(N) :: b1, b2, x

  CALL RANDOM_NUMBER(A)
  CALL RANDOM_NUMBER(x)




  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)
  DO i=1, 1000
     b1 = MATMUL(A, x)
  ENDDO
  CALL SYSTEM_CLOCK(COUNT=t_end)
  PRINT*, 'INBUILT time ellapsed [s]: ', (t_end - t_start)/REAL(count_rate)
  PRINT*, ''


  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)
  DO i=1, 1000
     b2 = matmult_serial(A, x)
  ENDDO
  CALL SYSTEM_CLOCK(COUNT=t_end)
  PRINT*, 'SERIAL time ellapsed [s]: ', (t_end - t_start)/REAL(count_rate)
  PRINT*, 'Max Error: ', MAXVAL(ABS(b2-b1))
  PRINT*, ''


  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)
  DO i=1, 1000
     b2 = matmult_parallel(A, x)
  ENDDO
  CALL SYSTEM_CLOCK(COUNT=t_end)
  PRINT*, 'PARALLEL time ellapsed [s]: ', (t_end - t_start)/REAL(count_rate)
  PRINT*, 'Max Error: ', MAXVAL(ABS(b2-b1))



CONTAINS

  FUNCTION matmult_serial(A, x)
    ! performs matrix multiplication Ax = b for square matrix, A of size NxN.
    IMPLICIT NONE
    INTEGER, PARAMETER :: MK = KIND(1.0E0)

    REAL(MK), DIMENSION(:, :) :: A
    REAL(MK), DIMENSION(:) :: x
    REAL(MK), ALLOCATABLE, DIMENSION(:) :: matmult_serial
    REAL(MK) :: sum
    INTEGER :: N, i, j

    N = SIZE(x)
    ALLOCATE(matmult_serial(N))

    DO i=1, N
       sum = 0.0
       DO j=1, N
          sum = sum + A(i, j)*x(j)
       ENDDO
       matmult_serial(i) = sum
    ENDDO
  END FUNCTION matmult_serial


  FUNCTION matmult_parallel(A, x)
    ! performs matrix multiplication Ax = b for square matrix, A of size NxN.
    IMPLICIT NONE
    INTEGER, PARAMETER :: MK = KIND(1.0E0)

    REAL(MK), DIMENSION(:, :) :: A
    REAL(MK), DIMENSION(:) :: x
    REAL(MK), ALLOCATABLE, DIMENSION(:) :: matmult_parallel
    REAL(MK) :: sum
    INTEGER :: N, i, j

    N = SIZE(x)
    ALLOCATE(matmult_parallel(N))

    !$OMP PARALLEL IF (N > 120) private(sum, i, j)
    !$OMP DO
    DO i=1, N
       sum = 0.0
       DO j=1, N
          sum = sum + A(i, j)*x(j)
       ENDDO
       !$OMP CRITICAL
       matmult_parallel(i) = sum
       !$OMP END CRITICAL
    ENDDO
    !$OMP END DO

    !$OMP END PARALLEL
  END FUNCTION matmult_parallel




END PROGRAM matrix_times_vector

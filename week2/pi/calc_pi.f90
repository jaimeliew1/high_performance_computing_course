PROGRAM calc_pi
  IMPLICIT NONE
  INTEGER, PARAMETER :: MK = KIND(1.0D0)
  INTEGER, PARAMETER :: N = 100000000
  REAL(mk) :: pi, temp
  INTEGER:: t_start, t_end, count_rate

  PRINT*, 'N: ', N

  CALL SYSTEM_CLOCK(COUNT=t_start, COUNT_RATE=count_rate)
  pi = pi_serial(N)
  CALL SYSTEM_CLOCK(COUNT=t_end)
  PRINT*, 'pi (serial): ', pi
  PRINT*, 'time ellapsed [s]: ', (t_end - t_start)/REAL(count_rate)


  CALL SYSTEM_CLOCK(COUNT=t_start)
  pi = pi_parallel(N)
  CALL SYSTEM_CLOCK(COUNT=t_end)
  PRINT*, 'pi (parallel): ', pi
  PRINT*, 'time ellapsed [s]: ', (t_end - t_start)/REAL(count_rate)

  CALL SYSTEM_CLOCK(COUNT=t_start)
  pi = pi_reduction(N)
  CALL SYSTEM_CLOCK(COUNT=t_end)
  PRINT*, 'pi (reduction): ', pi
  PRINT*, 'time ellapsed [s]: ', (t_end - t_start)/REAL(count_rate)


CONTAINS

  FUNCTION pi_serial(N)
    IMPLICIT NONE
    INTEGER, PARAMETER :: MK = KIND(1.0D0)
    INTEGER :: N, i
    REAL(mk) :: pi_serial, temp

    pi_serial = 0
    DO i=1, N
       temp = 4/REAL(N)/(1 + ((i - 0.5)/N)**2)
       pi_serial = pi_serial + temp
    ENDDO
  END FUNCTION pi_serial


  FUNCTION pi_parallel(N)
    INTEGER, PARAMETER :: MK = KIND(1.0D0)
    INTEGER :: N, i
    REAL(mk) :: pi_parallel, loc_sum

    pi_parallel = 0
    loc_sum = 0

    !$OMP PARALLEL private(i, loc_sum)

    !$OMP DO
    DO i=1, N
       loc_sum = loc_sum + 4/REAL(N)/(1 + ((i - 0.5)/N)**2)
    ENDDO
    !$OMP END DO

    !$OMP CRITICAL
    pi_parallel = pi_parallel + loc_sum
    !$OMP END CRITICAL

    !$OMP END PARALLEL
  END FUNCTION pi_parallel


  FUNCTION pi_reduction(N)
    INTEGER, PARAMETER :: MK = KIND(1.0D0)
    INTEGER :: N, i
    REAL(mk) :: pi_reduction

    pi_reduction = 0
    !$OMP PARALLEL private(i)

    !$OMP DO REDUCTION(+:pi_reduction)
    DO i=1, N
       pi_reduction = pi_reduction + 4/REAL(N)/(1 + ((i - 0.5)/N)**2)
    ENDDO
    !$OMP END DO

    !$OMP END PARALLEL
  END FUNCTION pi_reduction

END PROGRAM calc_pi

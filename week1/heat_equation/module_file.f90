MODULE m_global
  INTEGER, PARAMETER :: N = 21 ! Number of grid points on x and y direction
  REAL, PARAMETER :: D = 1
  REAL, PARAMETER :: T_max = 0.125
  REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
  REAL :: dx, dt
  INTEGER :: N_iter

END MODULE m_global


MODULE m_routines
CONTAINS
  SUBROUTINE init()
    USE m_global
    IMPLICIT NONE
    INTEGER :: i, j
    REAL:: fourier_limit

    ALLOCATE(T(N, N))
    ALLOCATE(T_old(N, N))

    dx = 1.0/(REAL(N) - 1)
    fourier_limit = dx**2/(4*D)
    dt = fourier_limit
    N_iter = T_max/dt + 1

    ! Set initial condition
    T_old = 0

    ! Set boundary conditions
    T_old(1, :) = 1
    T_old(N, :) = 1
    T_old(:, 1) = 1
    T_old(:, N) = 1

    T(1, :) = 1
    T(N, :) = 1
    T(:, 1) = 1
    T(:, N) = 1


    PRINT*, 'N: ', N
    PRINT*, 'dx: ', dx
    PRINT*, 'dt: ', dt
    PRINT*, 'fourier_limit: ', fourier_limit
    PRINT*, 'N_iter: ', N_iter
  END SUBROUTINE init


  SUBROUTINE save_output(fileroot, step_no)
    ! Saves the temperature field to file for a given fileroot. If a step number
    ! is provided, the step number is appended to the fileroot.
    ! The output filename receives the extension .dat.
    USE m_global
    IMPLICIT NONE
    INTEGER, OPTIONAL :: step_no
    INTEGER i, j
    CHARACTER(LEN=*), INTENT(in) :: fileroot
    CHARACTER(LEN=20) :: filename

    IF (PRESENT(step_no)) THEN
      WRITE(filename,'(A,I6.6,A)') fileroot, step_no,'.dat'
    ELSE
      filename = fileroot // '.dat'
    ENDIF

    OPEN(10, FILE=filename)
    DO j=1,N
       DO i=1,N
          WRITE(10,'(3E12.4)') REAL(i-1)*dx, REAL(j-1)*dx, T(i,j)
       ENDDO
       WRITE(10,'(A)') ! Will produce a new empty line – and tell gnuplot to lift the pen
    ENDDO
    CLOSE(10)

  END SUBROUTINE save_output


  SUBROUTINE update_memory()
    use m_global
    T_old = T
  end subroutine update_memory
END MODULE m_routines

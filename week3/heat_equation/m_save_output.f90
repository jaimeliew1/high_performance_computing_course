MODULE m_save_output
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
CONTAINS
  SUBROUTINE save_output(fileroot, T, Nx, Ny, dx, dy, step_no)
    ! Saves the temperature field to file for a given fileroot. If a step number
    ! is provided, the step number is appended to the fileroot.
    ! The output filename receives the extension .dat.
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(in) :: fileroot
    REAL(MK), DIMENSION(:,:) :: T
    INTEGER :: Nx, Ny
    REAL(MK) :: dx, dy
    INTEGER, OPTIONAL :: step_no

    INTEGER i, j
    CHARACTER(LEN=20) :: filename

    IF (PRESENT(step_no)) THEN
       WRITE(filename,'(A,I2.2,A)') fileroot, step_no,'.dat'
    ELSE
       filename = fileroot // '.dat' ! // is the append command
    ENDIF

    OPEN(10, FILE=filename)
    DO i=1,Ny
       WRITE(10, *) T(:, i)
    ENDDO

    CLOSE(10)

  END SUBROUTINE save_output
END MODULE m_save_output

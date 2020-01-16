MODULE m_save_output
  INTEGER, PARAMETER :: MK = KIND(1.0D0)
CONTAINS
  SUBROUTINE save_output(fileroot, N, U, coords, step_no)
    ! Saves the temperature field to file for a given fileroot. If a step number
    ! is provided, the step number is appended to the fileroot.
    ! The output filename receives the extension .dat.
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(in) :: fileroot
    INTEGER :: N
    REAL(MK), DIMENSION(:,:) :: U
    REAL(MK), DIMENSION(:)  :: coords
    INTEGER, OPTIONAL :: step_no

    INTEGER i, j
    CHARACTER(LEN=20) :: filename

    IF (PRESENT(step_no)) THEN
       WRITE(filename,'(A,I6.6,A)') fileroot, step_no,'.dat'
    ELSE
       filename = fileroot // '.dat' ! // is the append command
    ENDIF

    OPEN(10, FILE=filename)
    DO i=1,N+2
       DO j=1,N+2
          WRITE(10,'(3E12.4)') coords(i), coords(j), U(i,j)
       ENDDO
       WRITE(10,'(A)') ! Will produce a new empty line – and tell gnuplot to lift the pen
    ENDDO
    CLOSE(10)

  END SUBROUTINE save_output
END MODULE m_save_output

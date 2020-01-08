SUBROUTINE save_output(fileroot, T, N, dx, step_no)
  ! Saves the temperature field to file for a given fileroot. If a step number
  ! is provided, the step number is appended to the fileroot.
  ! The output filename receives the extension .dat.

  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(in) :: fileroot
  REAL, DIMENSION(:,:) :: T
  INTEGER :: N
  REAL :: dx
  INTEGER, OPTIONAL :: step_no

  INTEGER i, j
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

SUBROUTINE save_output(fileroot, T, Nx, Ny, dx, dy, step_no)
  ! Saves the temperature field to file for a given fileroot. If a step number
  ! is provided, the step number is appended to the fileroot.
  ! The output filename receives the extension .dat.
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(in) :: fileroot
  REAL, DIMENSION(:,:) :: T
  INTEGER :: Nx, Ny
  REAL :: dx, dy
  INTEGER, OPTIONAL :: step_no

  INTEGER i, j
  CHARACTER(LEN=20) :: filename

  IF (PRESENT(step_no)) THEN
     WRITE(filename,'(A,I6.6,A)') fileroot, step_no,'.dat'
  ELSE
     filename = fileroot // '.dat'
  ENDIF

  OPEN(10, FILE=filename)
  DO j=1,Ny
     DO i=1,Nx
        WRITE(10,'(3E12.4)') REAL(i-1)*dx, REAL(j-1)*dy, T(i,j)
     ENDDO
     WRITE(10,'(A)') ! Will produce a new empty line – and tell gnuplot to lift the pen
  ENDDO
  CLOSE(10)

END SUBROUTINE save_output

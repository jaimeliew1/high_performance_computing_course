MODULE m_diagnostic
CONTAINS
  SUBROUTINE diagnostic(step_no, T, close_file)
    ! Writes diagnostic information (minimum value of temperature field)
    ! to an output file, diag.dat
    IMPLICIT NONE
    INTEGER, INTENT(in) :: step_no
    REAL, DIMENSION(:, :), INTENT(in) :: T
    LOGICAL, OPTIONAL :: close_file

    CHARACTER(len=20), PARAMETER :: filename = 'diag.dat'
    LOGICAL :: first = .TRUE.

    CHARACTER(len=8) :: date
    CHARACTER(len=10) :: time

    CALL DATE_AND_TIME(DATE=date, TIME=time)

    IF (first) THEN
       OPEN(20, FILE=filename)
       first = .FALSE.
       WRITE(20, '(6A)') 'Date: ', date(7:8), '/',date(5:6), '/',date(1:4)
       WRITE(20, '(6A)') 'Time: ', time(1:2), ':',time(3:4), ':',time(5:)
       WRITE(20, '(A)') 'step_no min_val'

    ENDIF

    IF (PRESENT(close_file)) THEN
       CLOSE(20)
       RETURN
    ENDIF

    WRITE(20, '(I6, E12.4)') step_no, MINVAL(T)
  END SUBROUTINE diagnostic
END MODULE m_diagnostic

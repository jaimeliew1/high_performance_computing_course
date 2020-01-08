PROGRAM main
  ! An example of using interfaces
  REAL, DIMENSION ( : ) , POINTER :: my_data
  INTEGER :: n, status

  INTERFACE
     SUBROUTINE sub (DATA, n, info)
       REAL, DIMENSION ( : ) , POINTER :: DATA
     END SUBROUTINE sub
     SUBROUTINE init (DATA, n, info)
       REAL, DIMENSION(*) :: DATA
     END SUBROUTINE
  END INTERFACE

  n = 12
  CALL sub (my_data, n, status)
  PRINT* , ' status = ', status, ASSOCIATED(my_data)

  CALL init(my_data, n, status)
  DO i=1,n
     PRINT* , 'my_data (i) = ' , my_data(i)
  ENDDO
END PROGRAM main



SUBROUTINE sub (DATA, n, info)
  REAL, DIMENSION ( : ) , POINTER :: DATA
  INTEGER :: n, info

  ALLOCATE (DATA (n) , stat=info)
END SUBROUTINE sub




SUBROUTINE init (DATA, n, info)
  REAL, DIMENSION(*) :: DATA
  INTEGER :: n, info

  DO i=1, n
    DATA(i) = -i
  ENDDO
END SUBROUTINE init

MODULE m_update_memory
CONTAINS
  ELEMENTAL SUBROUTINE update_memory(A, A_old)
    REAL, INTENT(inout) :: A, A_old
    A_old = A
  END SUBROUTINE update_memory
END MODULE m_update_memory

program test
!subroutine test  !!When using in PSSE, should be subroutine, not program
    Use SetPara ! 
    implicit none
    print *, 'Test!'
!    Call ReadFile
!    print *, "N_VSC, Pole, Sbase"
!    print *, N_VSC, Pole, Sbase

    Call writeformat
    print *, 'Test Finishes!'
!end subroutine test
end program test


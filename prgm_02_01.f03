        Program prgm_02_01
!
!this program finds the kinetic energy matrix element given by <Psi_n1|T|Psi_n2> for a 1d particle in a box of length l
!
!
       implicit none
       integer::m,l,n1,n2
       real::KE
       real::PIB1DTElement
!format statements
 1000 format(1X,'Kinetic energy matrix element ',I5,',',I5,' is ',F12.5,'.')

        write(*,*)' What is the value of m?'
        read(*,*) m

        write(*,*)' l?'
        read(*,*) l

        write(*,*)' n1?'
        read(*,*) n1

        write(*,*)' n2?'
        read(*,*) n2

!call function to find KE matrix element
        KE = PIB1DTElement(m,l,n1,n2)
!
!print out the KE matrix element
        write(*,1000)n1,n2,KE


        End Program prgm_02_01
!find KE matrix element
        Real Function PIB1DTElement(m,l,n1,n2)
        integer::m,l,n1,n2
        real::pi
        pi = float(4)*atan(float(1))
        if (n1.ne.n2) then
                PIB1DTElement = float(0)
        else
                PIB1DTElement = pi**2*n2**2/(2.*l**2*m)
        endIf
        Return
        End

        Program prgm_02_02
!
!this program find the potential energy matrix element given by <Psi_n1|V|Psi_n2> for a 1d particle in a box of length l
!
!
        implicit none
        integer::m,l,n1,n2,b
        real::V
        real::PIB1DModifiedVElement
!format statements
 1000   format(1X,'Potential energy matrix element ',I5,',',I5,' is ',F12.5,'.')

        write(*,*)' What is the value of the potential scalar b?'
        read(*,*) b

        write(*,*)' m?'
        read(*,*) m

        write(*,*)' l?'
        read(*,*) l

        write(*,*)' n1?'
        read(*,*) n1

        write(*,*)' n2?'
        read(*,*) n2
        
!call function to find V matrix element
        V = PIB1DModifiedVElement(m,l,n1,n2,b)
!
!print out the V matrix element
        write(*,1000)n1,n2,V


        End Program prgm_02_02
!find V matrix element
        Real Function PIB1DModifiedVElement(m,l,n1,n2,b)
        integer::m,l,n1,n2,b
        real::pi
        pi = float(4)*atan(float(1))
        if (n1.eq.n2) then
                PIB1DModifiedVElement = b*l/2.
        else if (mod(n1,2)>0 .and. mod(n2,2)>0) then
                PIB1DModifiedVElement = 0. 
        else if (mod(n1,2)==0 .and. mod(n2,2)==0) then
                PIB1DModifiedVElement = 0.
        else
             PIB1DModifiedVElement =-8.*b*l/pi**2*n1*n2/(n1**2-n2**2)**2
                
        endIf
        Return
        End

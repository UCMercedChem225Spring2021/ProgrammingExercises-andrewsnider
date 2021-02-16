        Program prgm_02_03
!
!this program find the hamiltonian matrix element given by <Psi_n1|V|Psi_n2> for a 1d particle in a box of length l
!
!
        implicit none
        integer::m,l,n1,n2,b
        real::H
        real::PIB1DModifiedHamiltonianElement
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
        
!call function to find H matrix element
        H = PIB1DModifiedHamiltonianElement(m,l,n1,n2,b)
!
!print out the H matrix element
        write(*,1000)n1,n2,H


        End Program prgm_02_03
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


!find the T matrix element
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


!find the H matrix element
        Real Function PIB1DModifiedHamiltonianElement(m,l,n1,n2,b)
        integer::m,l,n1,n2,b
        real::T,V
        T = PIB1DTElement(m,l,n1,n2)
        V = PIB1DModifiedVElement(m,l,n1,n2,b)
        PIB1DModifiedHamiltonianElement = T + V  
        Return
        End
        
        


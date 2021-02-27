        Program prgm_03_03


        Implicit None
        Integer,Parameter::IIn=10
        Integer::IError,NDim,i,j
        Real,Dimension(:),Allocatable::Array_Input,EVals,Temp_Vector
        Real,Dimension(:,:),Allocatable::Matrix,EVecs,Temp_Matrix
        Character(Len=256)::FileName


        Call Get_Command_Argument(1,FileName)
        Open(Unit=IIn,File=TRIM(FileName),Status='OLD',IOStat=IError)
        If(IError.ne.0) then
                Write(*,*)'Error opening input file.'
                STOP 
        endIf
        Read(IIn,*) NDim
        Allocate(Array_Input((NDim*(NDim+1))/2),Matrix(NDim,NDim))
        Allocate(EVals(NDim),EVecs(NDim,NDim),Temp_Vector(3*NDim))
        Allocate(Temp_Matrix(NDim,NDim))

        read(IIn,*) Array_Input
        write(IIn,*) Array_Input

        Close(Unit=IIn)
!       
        write(*,*) ' The matrix loaded (column-wise) lower-tri packed:' 
        call SymmetricPacked2Matrix_LowerPac(Ndim,Array_Input,Matrix)
        
        
        call Print_Matrix_Full_Real(Matrix,NDim,NDim)
        Call SSPEV(Array_Input,NDim,EVals,EVecs,NDim,  &
                Temp_Vector, NDim, NDim, IError)
        If(IError.ne.0) then
                Write(*,*)' Failure in DSPEV.'
                STOP
        endIf
        Write(*,*)' Evals:'
        Call Print_Matrix_Full_Real(RESHAPE(EVals,(/1,NDim/)),1,NDim)
        Write(*,*)' Evecs:'
        Call Print_Matrix_Full_Real(EVecs,NDim,NDim)


!        write(*,*) 'The matrix loaded (column-wise) upper-tri packed:'
!        call SymmetricPacked2Matrix_Upper(NDim,Array_Input,Matrix)
!        call Print_Matrix_Full_Real(Matrix,NDim,NDim)
        
        end Program prgm_03_03




      Subroutine SymmetricPacked2Matrix_Upper(N,ArrayIn,AMatOut)
      Implicit None
      Integer::i,j,k,d
      Integer,Intent(in)::N
      real,dimension((N*(N+1))/2),intent(in)::ArrayIn
      real,dimension(N,N)::AMatOut
        k = 1
        d = 1
        do i=1,N
                do j=1,k
                        AMatOut(j,i) = ArrayIn(d)
                        d = d + 1
                endDo 
                k = k + 1
        endDo

        k = 1
        do i=1,N
                do j=1,k
                        AMatOut(i,j) = AMatOut(j,i)
                endDo
                k = k + 1
        endDo

      end Subroutine



      Subroutine SymmetricPacked2Matrix_LowerPac(N,ArrayIn,AMatOut)
      Implicit None
      Integer,Intent(in)::N
      integer::i,j,k,d
      real,dimension((N*(N+1))/2),intent(in)::ArrayIn
      real,dimension(N,N)::AMatOut
        d = 1
        k = 0
        do i=1,N
                do j=1+k,N 
                        AMatOut(j,i) = ArrayIn(d)
                        d= d + 1
                endDo
                k = k + 1
        endDo

        
        do i=1,N
                k = 0
                do j=1+k,N 
                        AMatOut(i,j) = AMatOut(j,i)
                endDo
                k = k + 1
        endDo
        end Subroutine
  
      Subroutine Print_Matrix_Full_Real(AMat,M,N)
!
!     This subroutine prints a real matrix that is fully dimension - i.e.,
!     not stored in packed form. AMat is the matrix, which is dimensioned
!     (M,N).
!
!     The output of this routine is sent to unit number 6 (set by the local
!     parameter integer IOut).
!
!
!     Variable Declarations
!
      implicit none
      integer,intent(in)::M,N
      real,dimension(M,N),intent(in)::AMat
!
!     Local variables
      integer,parameter::IOut=6,NColumns=5
      integer::i,j,IFirst,ILast
!
 1000 Format(1x,A)
 2000 Format(5x,5(7x,I7))
 2010 Format(1x,I7,5F14.6)
!
      Do IFirst = 1,N,NColumns
        ILast = Min(IFirst+NColumns-1,N)
        write(IOut,2000) (i,i=IFirst,ILast)
        Do i = 1,M
          write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo
!
      Return
      End Subroutine Print_Matrix_Full_Real

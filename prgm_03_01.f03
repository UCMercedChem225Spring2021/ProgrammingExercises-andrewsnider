        Program prgm_03_01



        Implicit None
        Integer,Parameter::IIn=10
        Integer::IError,NDim,i,j
        Real,Dimension(:),Allocatable::Array_Input
        Real,Dimension(:,:),Allocatable::Matrix
        Character(Len=256)::FileName


        Call Get_Command_Argument(1,FileName)
        Open(Unit=IIn,File=TRIM(FileName),Status='OLD',IOStat=IError)
        If(IError.ne.0) then
                Write(*,*)'Error opening input file.'
                STOP
        endIf
        Read(IIn,*) NDim
        Allocate(Array_Input(NDim*NDim),Matrix(NDim,NDim))
        read(IIn,*) Array_Input
        write(IIn,*) Array_Input
        write(*,*) ' The matrix expanded according to a row-wise ', &
        'linear packed format' 
        call Packed2MatrixRowWise(NDim,Ndim,Array_Input,Matrix)
        call Print_Matrix_Full_Real(Matrix,NDim,NDim)
        write(*,*) ' The matrix expanded according to a column-wise ', &
                'linear packed format'
        call Packed2MatrixColumnWise(NDim,Ndim,Array_Input,Matrix)
        call Print_Matrix_Full_Real(Matrix,NDim,NDim)
        
        end Program prgm_03_01




      Subroutine Packed2MatrixRowWise(M,N,ArrayIn,AMatOut)
      Integer::i,j
      integer,intent(in)::M,N
      real,dimension(M*N),intent(in)::ArrayIn
      real,dimension(M,N)::AMatOut
        do i=1,M
                do j=1,N
                        AMatOut(j,i) = ArrayIn(i+(j-1)*N)
                endDo
        endDo
      end Subroutine

      Subroutine Packed2MatrixColumnWise(M,N,ArrayIn,AMatOut)
      Integer::i,j
      integer,intent(in)::M,N
      real,dimension(M*N),intent(in)::ArrayIn
      real,dimension(M,N)::AMatOut
        do i=1,M
                do j=1,N
                        AMatOut(i,j) = ArrayIn(i+(j-1)*N)
                endDo
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

      USE functions
      
        REAL(8), ALLOCATABLE, DIMENSION(:):: COEF
       REAL(8), ALLOCATABLE, DIMENSION(:,:):: P    

       ALLOCATE( COEF(NDIM) )

!      ALLOCATE( Y(NDIM)) 
      ALLOCATE( P(NDIM,NDIM) )       
           

! ******* ��������E ������� *******
         OPEN(7, FILE='init.txt', STATUS='OLD')
	 READ(7,*) DD1   ! ��������� ���������� �� ����������� ���������
	 READ(7,*) TT1   ! ��������� ���������� �� ����������� ���������
       READ(7,*) TEND  ! ����� ��������� �������
	CLOSE(7)
      
! ***** ����� ��������� ��������� �������������� -- ������� P(MP, NP)
!       ��� ��������� ������� ����� ������ ��� ����� � ��������� ������� � ���� ������
      Kcall=0
      
      do 12 i=1,MP
        do 11 J=1,NP
          COEF(1)=TT1/I       ! ����������� 
          COEF(2)=DD1/I       ! ��������� 
          p(i,j)=COEF(J)
11      continue    
              CALL FUNKS(COEF, FUNK, 0 ) 
        y(i)=FUNK
12    continue
      
      Kcall=1
!      CALL FUNKS(COEF, FUNK, Kcall ) 
      call amoeba(p,y,MP,NP,ndim,FTOL,FUNKs,iter)


        STOP
      END
 
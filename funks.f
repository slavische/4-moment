      SUBROUTINE FUNKS ( COEF, FUNK, Kcall )
      USE functions

      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER(4) (I-N)

         REAL(8) COEF(NDIM)

         REAL(8) T, TOUT, TEND
         REAL(8) D1,V1,T1

!      INTEGER(4) N, Nend, IHLF,IHLFM, Kcall, Ncall/0/
!      SAVE


! ******* мювюкэмшE сякнбхъ *******
!         OPEN(7, FILE='funks.txt', STATUS='OLD')
!	 READ(7,*) Nend
!	 READ(7,*) TEND
!	 READ(7,*) EPS
!	 READ(7,*) D1
!	 READ(7,*) T1
!	 READ(7,*) DU
!	CLOSE(7)

! ******* мювюкэмшE сякнбхъ *******
         OPEN(7, FILE='init.txt', STATUS='OLD')
	 READ(7,*) DD1   ! нрмньемхе окнрмняреи мю онбепумнярх хяоюпемхъ
	 READ(7,*) TT1  ! нрмньемхе релоепюрсп мю онбепумнярх хяоюпемхъ
       READ(7,*) TEND  ! дкхмю пюявермни накюярх       
	CLOSE(7)
      

        TOUT=1.0d-3

        Y(1) = TT1        ! нрмньемхе релоепюрсп
        Y(2) = DD1        ! нрмньемхе окнрмняреи

! бшвхякемхе тсмйжхи б мювюкэмшу рнвйюу лмнцнцпюммхйю 
          IF (KCALL == 0) THEN
        Y(3) = coef(1)           ! set probe coefficients (опнамше гмювемхъ мю цнпъвеи онбепумнярх)
        Y(4) = coef(2)
          END IF
!      IF (KCALL==0) 
 
!-----------------------------------------------------------------------
!      FLAG = .true.
!      IHLFM = 0

      DO WHILE (T<=TEND)

          forse=0.0d0
         CALL EQCOEF(Y, A, R, FORSE)        

       M=4
       N=1  
      
      
       EPS=1.0E-8
! ******* пеьемхе яхярелш спюбмемхи 
       call DGELG(R,A,M,N,EPS,IER)

! ***** пегскэрюрш дкъ якедсчыеи йннпдхмюрш        

      Y(1)=Y(1)+R(1,1)*TOUT
      Y(2)=Y(2)+R(2,1)*TOUT
      Y(3)=Y(3)+R(3,1)*TOUT
      Y(4)=Y(4)+R(4,1)*TOUT
 
      T=T+TOUT

!      FLUX=Y(2)*SQRT(Y(1))-Y(4)*SQRT(Y(3))
      
             ENDDO
     
!-------------------------------------------------  target function FUNK
         FUNK = 0.D0

!      IF( T1 /= 0.D0 ) then
         FUNK = FUNK + ABS(y(3)/1.0d0 - 1.D0)**2
!      ELSE
!         FUNK = FUNK + ABS(Y(3))**2
!      ENDIF


!      IF( D1 /= 0.D0 ) then
         FUNK = FUNK + ABS(Y(4)/1.0d0 - 1.D0)**2
!      ELSE
!         FUNK = FUNK + ABS(Y(4))**2
!      ENDIF

 62    FORMAT(1P,15G15.7)
!      RETURN
!      CONTAINS
       END SUBROUTINE FUNKS


!=======================================================================
!      SUBROUTINE FCT(XX,YY,DERY)
!      USE functions
!      REAL(8) XX,YY(MDIM),DERY(MDIM)
!      SAVE

!       CALL EQCOEF( YY )

! пеьемхе яхярелш спюбмемхи
!       CALL DGELG( R, A, MDIM, LDIM, EPS, IER )

!       DO  i = 1, MDIM
!	  DERY(i) = R(i,1)
!       ENDDO

!      END SUBROUTINE FCT

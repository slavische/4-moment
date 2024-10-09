      MODULE FUNCTIONS

      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER(4) (I-N)

      INTEGER(4), PARAMETER :: MDIM = 4 , LDIM = 1, NDIM = 5
      INTEGER(4), EXTERNAL :: FCT

         REAL(8) X(MDIM), R(MDIM,LDIM), A(MDIM,MDIM), Y(NDIM)

      INTEGER NP,MP
      REAL FTOL
      PARAMETER(NP=2,MP=3,FTOL=1.0E-12)
      PARAMETER (NEQ=4)
!      INTEGER i,iter,j,ndim
!      REAL famoeb,p(MP,NP), y(NDIM), Z(NEQ)               

      CONTAINS

!================================================ ÊÎÝÔÔÈÖÈÅÍÒÛ ÓÐÀÂÍÅÍÈÉ
      SUBROUTINE EQCOEF(Z, A, R, FORSE)
      REAL(8) D3, D2, D1, F1, Z(MDIM), A(MDIM,MDIM),R(MDIM,MDIM)
   
      F1=FORSE
! ******* EQ1 *************   
       A(1,1) = 0.5D0*Z(2)/SQRT(Z(1))
       A(1,2) = SQRT(Z(1))
       A(1,3) = -0.5D0*Z(4)/SQRT(Z(3))
       A(1,4) = -SQRT(Z(3))

       R(1,1) = 0.D0

! ******* EQ2 *******************
       A(2,1) = Z(2)
       A(2,2) = Z(1)
       A(2,3) = Z(4)
       A(2,4) = Z(3)

       R(2,1) = 2.0D0*F1*(Z(2)+Z(4))

! ******* EQ3 *******************
       A(3,1) = 1.5D0*Z(2)*SQRT(Z(1))
       A(3,2) = Z(1)**1.5
       A(3,3) = -1.5D0*Z(4)*SQRT(Z(3))
       A(3,4) = -Z(3)**1.5

       R(3,1) = 0.5D0*F1*(Z(2)*SQRT(Z(1))-Z(4)*SQRT(Z(3))) 

! ******* EQ4 *******************
        A(4,1) = 2.0D0*Z(2)*Z(1)
       A(4,2) = (Z(1))**2.
       A(4,3) = 2.0D0*Z(4)*Z(3)
       A(4,4) = (Z(3))**2.

       D1=Z(2)*(Z(1))**1.5-Z(4)*(Z(3))**1.5
       D1=D1*(Z(2)+Z(4))
       
       D2=Z(2)*SQRT(Z(1))-Z(4)*SQRT(Z(3))
       D2=5.0*D2*(Z(2)*Z(1)+Z(4)*Z(3))/8.0D0
       
       D3=-4.0D0*(D1-D2)/(5.0D0*SQRT(3.1415926))
       
        R(4,1) = F1*(Z(2)*Z(1)+Z(4)*Z(3))+D3
        
! ***************************************** EQ5 *******************

      END SUBROUTINE EQCOEF

!-----------------------------------------------------------------------
      END MODULE FUNCTIONS

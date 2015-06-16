! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.5.7 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 13:45 on 16.6.2015   
! ----------------------------------------------------------------------  
 
 
Module Tadpoles_MSSMCPV 
 
Use Model_Data_MSSMCPV 
Use SUSYMasses_MSSMCPV 
Use RGEs_MSSMCPV 
Use Control 

Use Mathematics 

Contains 


Subroutine SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,               & 
& ml2,mHd2,mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,Tad1Loop)

Implicit None
Real(dp),Intent(inout) :: g1,g2,g3,mHd2,mHu2,vd,vu

Complex(dp),Intent(inout) :: Yd(3,3),Ye(3,3),Yu(3,3),Mu,Td(3,3),Te(3,3),Tu(3,3),Bmu,mq2(3,3),ml2(3,3),             & 
& md2(3,3),mu2(3,3),me2(3,3),M1,M2,M3

Real(dp) :: Abs2Mu

Complex(dp), Intent(in) :: Tad1Loop(4)

! For numerical routines 
Real(dp) :: gC(215)
logical :: broycheck 
Real(dp) :: broyx(3)

Abs2Mu =  0._dp 
If (HighScaleModel.Eq."LOW") Then 
Abs2Mu = (-8*mHd2*vd**2 - g2**2*vd**4 + 8*mHu2*vu**2 + g2**2*vu**4 + g1**2*(-vd**4 +           & 
&  vu**4) + 8*vd*Tad1Loop(1) - 8*vu*Tad1Loop(2))/(8._dp*(vd**2 - vu**2))
Mu = Sqrt(Abs2Mu)*SignumMu
temporaryValue = (4*(1 + exp(2*(0.,1._dp)*eta))*mHd2*vd*vu**2 - 4*(1 + exp(2*(0.,1._dp)*eta))*mHu2*vd*vu**2 +& 
&  g1**2*vd**3*vu**2 + exp(2*(0.,1._dp)*eta)*g1**2*vd**3*vu**2 + g2**2*vd**3*vu**2 +     & 
&  exp(2*(0.,1._dp)*eta)*g2**2*vd**3*vu**2 - g1**2*vd*vu**4 - exp(2*(0.,1._dp)*eta)*g1**2*vd*vu**4 -& 
&  g2**2*vd*vu**4 - exp(2*(0.,1._dp)*eta)*g2**2*vd*vu**4 - 4*vu**2*Tad1Loop(1) -         & 
&  4*exp(2*(0.,1._dp)*eta)*vu**2*Tad1Loop(1) + 4*vd*vu*Tad1Loop(2) + 4*exp(2*(0.,        & 
& 1._dp)*eta)*vd*vu*Tad1Loop(2) - 4*(0.,1._dp)*vd**2*Tad1Loop(3) + 4*(0.,1._dp)*exp(2*(0.,& 
& 1._dp)*eta)*vd**2*Tad1Loop(3) + 4*(0.,1._dp)*vu**2*Tad1Loop(3) - 4*(0.,1._dp)*exp(2*(0.,& 
& 1._dp)*eta)*vu**2*Tad1Loop(3))/(8._dp*exp((0._dp,1._dp)*eta)*(-(vd**2*vu) + vu**3))
Bmu = Cmplx(Real(temporaryValue,dp),Aimag(Bmu),dp )
temporaryValue = (4*(0.,1._dp)*(-1 + exp(2*(0.,1._dp)*eta))*mHd2*vd*vu**2 - 4*(0.,1._dp)*(-            & 
& 1 + exp(2*(0.,1._dp)*eta))*mHu2*vd*vu**2 - (0._dp,1._dp)*g1**2*vd**3*vu**2 +           & 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g1**2*vd**3*vu**2 - (0._dp,1._dp)*g2**2*vd**3*vu**2 +& 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g2**2*vd**3*vu**2 + (0._dp,1._dp)*g1**2*vd*vu**4 -& 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g1**2*vd*vu**4 + (0._dp,1._dp)*g2**2*vd*vu**4 -   & 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g2**2*vd*vu**4 + 4*(0.,1._dp)*vu**2*Tad1Loop(1) - & 
&  4*(0.,1._dp)*exp(2*(0.,1._dp)*eta)*vu**2*Tad1Loop(1) - 4*(0.,1._dp)*vd*vu*Tad1Loop(2) +& 
&  4*(0.,1._dp)*exp(2*(0.,1._dp)*eta)*vd*vu*Tad1Loop(2) - 4*vd**2*Tad1Loop(3) -          & 
&  4*exp(2*(0.,1._dp)*eta)*vd**2*Tad1Loop(3) + 4*vu**2*Tad1Loop(3) + 4*exp(2*(0.,        & 
& 1._dp)*eta)*vu**2*Tad1Loop(3))/(8._dp*exp((0._dp,1._dp)*eta)*(-(vd**2*vu) + vu**3))
Bmu = Cmplx(real(Bmu,dp),Real(temporaryValue,dp),dp) 

 ! ----------- Check solutions for consistency  -------- 

 ! Check for NaNs 
If (Real(Bmu,dp).ne.Real(Bmu,dp)) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Bmu" 
   Call TerminateProgram  
 End If 
 If (Real(Bmu,dp).ne.Real(Bmu,dp)) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Bmu" 
   Call TerminateProgram  
 End If 
 
 ! Check for positive abs-squared 
If (Abs2Mu.Lt.0._dp) Then 
Write(*,*) "No consistent solution found for tadpoles: " 
Write(*,*) "Negative absolute squared: ,Abs2Mu "   
Write(*,*) "Result: ",Abs2Mu 
Abs2Mu = Abs(Abs2Mu)  
Mu = SignumMu* sqrt(Abs2Mu) 
SignOfMuChanged = .True.  
End If 
Else 
Abs2Mu = (-8*mHd2*vd**2 - g2**2*vd**4 + 8*mHu2*vu**2 + g2**2*vu**4 + g1**2*(-vd**4 +           & 
&  vu**4) + 8*vd*Tad1Loop(1) - 8*vu*Tad1Loop(2))/(8._dp*(vd**2 - vu**2))
Mu = Sqrt(Abs2Mu)*SignumMu
temporaryValue = (4*(1 + exp(2*(0.,1._dp)*eta))*mHd2*vd*vu**2 - 4*(1 + exp(2*(0.,1._dp)*eta))*mHu2*vd*vu**2 +& 
&  g1**2*vd**3*vu**2 + exp(2*(0.,1._dp)*eta)*g1**2*vd**3*vu**2 + g2**2*vd**3*vu**2 +     & 
&  exp(2*(0.,1._dp)*eta)*g2**2*vd**3*vu**2 - g1**2*vd*vu**4 - exp(2*(0.,1._dp)*eta)*g1**2*vd*vu**4 -& 
&  g2**2*vd*vu**4 - exp(2*(0.,1._dp)*eta)*g2**2*vd*vu**4 - 4*vu**2*Tad1Loop(1) -         & 
&  4*exp(2*(0.,1._dp)*eta)*vu**2*Tad1Loop(1) + 4*vd*vu*Tad1Loop(2) + 4*exp(2*(0.,        & 
& 1._dp)*eta)*vd*vu*Tad1Loop(2) - 4*(0.,1._dp)*vd**2*Tad1Loop(3) + 4*(0.,1._dp)*exp(2*(0.,& 
& 1._dp)*eta)*vd**2*Tad1Loop(3) + 4*(0.,1._dp)*vu**2*Tad1Loop(3) - 4*(0.,1._dp)*exp(2*(0.,& 
& 1._dp)*eta)*vu**2*Tad1Loop(3))/(8._dp*exp((0._dp,1._dp)*eta)*(-(vd**2*vu) + vu**3))
Bmu = Cmplx(Real(temporaryValue,dp),Aimag(Bmu),dp )
temporaryValue = (4*(0.,1._dp)*(-1 + exp(2*(0.,1._dp)*eta))*mHd2*vd*vu**2 - 4*(0.,1._dp)*(-            & 
& 1 + exp(2*(0.,1._dp)*eta))*mHu2*vd*vu**2 - (0._dp,1._dp)*g1**2*vd**3*vu**2 +           & 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g1**2*vd**3*vu**2 - (0._dp,1._dp)*g2**2*vd**3*vu**2 +& 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g2**2*vd**3*vu**2 + (0._dp,1._dp)*g1**2*vd*vu**4 -& 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g1**2*vd*vu**4 + (0._dp,1._dp)*g2**2*vd*vu**4 -   & 
&  (0._dp,1._dp)*exp(2*(0.,1._dp)*eta)*g2**2*vd*vu**4 + 4*(0.,1._dp)*vu**2*Tad1Loop(1) - & 
&  4*(0.,1._dp)*exp(2*(0.,1._dp)*eta)*vu**2*Tad1Loop(1) - 4*(0.,1._dp)*vd*vu*Tad1Loop(2) +& 
&  4*(0.,1._dp)*exp(2*(0.,1._dp)*eta)*vd*vu*Tad1Loop(2) - 4*vd**2*Tad1Loop(3) -          & 
&  4*exp(2*(0.,1._dp)*eta)*vd**2*Tad1Loop(3) + 4*vu**2*Tad1Loop(3) + 4*exp(2*(0.,        & 
& 1._dp)*eta)*vu**2*Tad1Loop(3))/(8._dp*exp((0._dp,1._dp)*eta)*(-(vd**2*vu) + vu**3))
Bmu = Cmplx(real(Bmu,dp),Real(temporaryValue,dp),dp) 

 ! ----------- Check solutions for consistency  -------- 

 ! Check for NaNs 
If (Real(Bmu,dp).ne.Real(Bmu,dp)) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Bmu" 
   Call TerminateProgram  
 End If 
 If (Real(Bmu,dp).ne.Real(Bmu,dp)) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Bmu" 
   Call TerminateProgram  
 End If 
 
 ! Check for positive abs-squared 
If (Abs2Mu.Lt.0._dp) Then 
Write(*,*) "No consistent solution found for tadpoles: " 
Write(*,*) "Negative absolute squared: ,Abs2Mu "   
Write(*,*) "Result: ",Abs2Mu 
Abs2Mu = Abs(Abs2Mu)  
Mu = SignumMu* sqrt(Abs2Mu) 
SignOfMuChanged = .True.  
End If 
End if 
End Subroutine SolveTadpoleEquations

Subroutine CalculateTadpoles(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,               & 
& mHd2,mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,Tad1Loop,TadpoleValues)

Real(dp),Intent(in) :: g1,g2,g3,mHd2,mHu2,vd,vu

Complex(dp),Intent(in) :: Yd(3,3),Ye(3,3),Yu(3,3),Mu,Td(3,3),Te(3,3),Tu(3,3),Bmu,mq2(3,3),ml2(3,3),             & 
& md2(3,3),mu2(3,3),me2(3,3),M1,M2,M3

Complex(dp), Intent(in) :: Tad1Loop(4)

Real(dp), Intent(out) :: TadpoleValues(4)

TadpoleValues(1) = Real((exp((0._dp,1._dp)*eta)*(g1**2 + g2**2)*vd**3 - 4*(-1*(0.,1._dp)& 
& *imBmu + reBmu + exp(2*(0.,1._dp)*eta)*((0._dp,1._dp)*imBmu + reBmu))*vu + exp((0._dp,1._dp)& 
& *eta)*vd*(8._dp*(mHd2) - (g1**2 + g2**2)*vu**2 + 8*Mu**2))/(8._dp*exp((0._dp,1._dp)    & 
& *eta)) - Tad1Loop(1),dp) 
TadpoleValues(2) = Real((-4*(-1*(0.,1._dp)*imBmu + reBmu)*vd - 4*exp(2*(0.,1._dp)     & 
& *eta)*((0._dp,1._dp)*imBmu + reBmu)*vd + exp((0._dp,1._dp)*eta)*vu*(8._dp*(mHu2)       & 
&  - (g1**2 + g2**2)*vd**2 + g1**2*vu**2 + g2**2*vu**2 + 8*Mu**2))/(8._dp*exp((0._dp,1._dp)& 
& *eta)) - Tad1Loop(2),dp) 
TadpoleValues(3) = Real((-1._dp/2._dp*(0.,1._dp)*((0._dp,1._dp)*imBmu - reBmu + exp(2*(0.,1._dp)& 
& *eta)*((0._dp,1._dp)*imBmu + reBmu))*vu)/exp((0._dp,1._dp)*eta) - Tad1Loop(3),dp) 
TadpoleValues(4) = Real((-1._dp/2._dp*(0.,1._dp)*((0._dp,1._dp)*imBmu - reBmu + exp(2*(0.,1._dp)& 
& *eta)*((0._dp,1._dp)*imBmu + reBmu))*vd)/exp((0._dp,1._dp)*eta) - (vd*Tad1Loop(3))/vu,dp) 
End Subroutine CalculateTadpoles 

End Module Tadpoles_MSSMCPV 
 

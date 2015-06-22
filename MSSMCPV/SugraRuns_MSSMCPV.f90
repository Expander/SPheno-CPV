! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.5.8b1 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 15:23 on 22.6.2015   
! ----------------------------------------------------------------------  
 
 
Module SugraRuns_MSSMCPV 
 
Use Control 
Use LoopCouplings_MSSMCPV 
Use LoopMasses_MSSMCPV 
Use LoopFunctions 
Use Mathematics 
Use Model_Data_MSSMCPV 
Use RGEs_MSSMCPV 
Use Tadpoles_MSSMCPV 
 Use StandardModel 
 
Integer, save :: YukScen 
Real(dp), save :: Lambda, MlambdaS,F_GMSB 
Real(dp),save::mGUT_save,sinW2_DR_mZ&
&,mf_l_DR_SM(3),mf_d_DR_SM(3),mf_u_DR_SM(3)
Complex(dp),save::Yl_mZ(3,3),Yu_mZ(3,3),Yd_mZ(3,3)
Real(dp),Save::vevs_DR_save(2)
Contains 
 
Subroutine BoundarySUSY(gA,gB) 
Implicit None 
Real(dp),Intent(in)::gA(:)
Real(dp),Intent(out)::gB(:)
Integer::i1,i2,i_count,kont
Complex(dp) :: Tad1Loop(4) 
Real(dp) :: comp(4) 
Complex(dp) :: cplcChaChaUhhL(2,2,4),cplcChaChaUhhR(2,2,4),cplChiChiUhhL(4,4,4),cplChiChiUhhR(4,4,4),& 
& cplcFdFdUhhL(3,3,4),cplcFdFdUhhR(3,3,4),cplcFeFeUhhL(3,3,4),cplcFeFeUhhR(3,3,4),       & 
& cplcFuFuUhhL(3,3,4),cplcFuFuUhhR(3,3,4),cplcgWmgWmUhh(4),cplcgWpCgWpCUhh(4),           & 
& cplcgZgZUhh(4),cplUhhhhhh(4,4,4),cplUhhhhVZ(4,4),cplUhhHpmcHpm(4,2,2),cplUhhHpmcVWm(4,2),& 
& cplUhhSdcSd(4,6,6),cplUhhSecSe(4,6,6),cplUhhSucSu(4,6,6),cplUhhSvcSv(4,3,3),           & 
& cplUhhcVWmVWm(4),cplUhhVZVZ(4),cplUhhUhhhhhh(4,4,4,4),cplUhhUhhHpmcHpm(4,4,2,2),       & 
& cplUhhUhhSdcSd(4,4,6,6),cplUhhUhhSecSe(4,4,6,6),cplUhhUhhSucSu(4,4,6,6),               & 
& cplUhhUhhSvcSv(4,4,3,3),cplUhhUhhcVWmVWm(4,4),cplUhhUhhVZVZ(4,4)

Real(dp) :: g1,g2,g3

Complex(dp) :: Yd(3,3),Ye(3,3),Yu(3,3)

Real(dp) :: mHd2,mHu2

Complex(dp) :: Mu,Td(3,3),Te(3,3),Tu(3,3),Bmu,mq2(3,3),ml2(3,3),md2(3,3),mu2(3,3),me2(3,3),M1,M2,M3

Iname=Iname+1
NameOfUnit(Iname)='BoundarySUSY'
Call GToParameters57(gA,g1,g2,g3,Yd,Ye,Yu)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 

 
 ! --- Boundary conditions at SUSY-scale --- 
If (HighScaleModel.ne."LOW") Then 
 eta = etaInput
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 ! ----------------------- 
 
If (.Not.FirstRun) Then 
vd = vdSUSY 
vu = vuSUSY 

 
 ! --- Boundary conditions at SUSY-scale --- 
If (HighScaleModel.ne."LOW") Then 
 eta = etaInput
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 ! ----------------------- 
 
End if 

 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 
Call ParametersToG57(g1,g2,g3,Yd,Ye,Yu,gB)

Iname=Iname-1
End Subroutine BoundarySUSY 
 
Subroutine BoundaryHS(gA,gB) 
Implicit None 
Real(dp),Intent(in)::gA(:)
Real(dp),Intent(out)::gB(:)
Integer::i1,i2
Real(dp) :: g1,g2,g3

Complex(dp) :: Yd(3,3),Ye(3,3),Yu(3,3)

Real(dp) :: mHd2,mHu2

Complex(dp) :: Mu,Td(3,3),Te(3,3),Tu(3,3),Bmu,mq2(3,3),ml2(3,3),md2(3,3),mu2(3,3),me2(3,3),M1,M2,M3

Complex(dp) :: Yd_ckm(3,3), Yu_ckm(3,3), Tu_ckm(3,3), Td_ckm(3,3), mq2_ckm(3,3), mu2_ckm(3,3), md2_ckm(3,3) 
Complex(dp) :: Yd_out(3,3), Yu_out(3,3), Tu_out(3,3), Td_out(3,3), mq2_out(3,3), mu2_out(3,3), md2_out(3,3) 
Iname=Iname+1
NameOfUnit(Iname)='BoundaryHS'
Call GToParameters57(gA,g1,g2,g3,Yd,Ye,Yu)

If (HighScaleModel.ne."LOW") Then 
 If (InputValueforg1) Then 
g1 = g1IN 
Else 
g1 = Sqrt(g1**2 + g2**2)/sqrt(2._dp)
End If 
If (InputValueforg2) Then 
g2 = g2IN 
Else 
g2 = g1
End If 
If (InputValueforTe) Then 
Te = TeIN 
Else 
Te = Azero*Ye
End If 
If (InputValueforTd) Then 
Td = TdIN 
Else 
Td = Azero*Yd
End If 
If (InputValueforTu) Then 
Tu = TuIN 
Else 
Tu = Azero*Yu
End If 
If (InputValueformq2) Then 
mq2 = mq2IN 
Else 
mq2 = 0._dp 
Do i1=1,3
mq2(i1,i1) = m0**2
End Do
End If 
If (InputValueforml2) Then 
ml2 = ml2IN 
Else 
ml2 = 0._dp 
Do i1=1,3
ml2(i1,i1) = m0**2
End Do
End If 
If (InputValueformd2) Then 
md2 = md2IN 
Else 
md2 = 0._dp 
Do i1=1,3
md2(i1,i1) = m0**2
End Do
End If 
If (InputValueformu2) Then 
mu2 = mu2IN 
Else 
mu2 = 0._dp 
Do i1=1,3
mu2(i1,i1) = m0**2
End Do
End If 
If (InputValueforme2) Then 
me2 = me2IN 
Else 
me2 = 0._dp 
Do i1=1,3
me2(i1,i1) = m0**2
End Do
End If 
If (InputValueformHd2) Then 
mHd2 = mHd2IN 
Else 
mHd2 = m0**2
End If 
If (InputValueformHu2) Then 
mHu2 = mHu2IN 
Else 
mHu2 = m0**2
End If 
If (InputValueforM1) Then 
M1 = M1IN 
Else 
M1 = m12
End If 
If (InputValueforM2) Then 
M2 = M2IN 
Else 
M2 = m12
End If 
If (InputValueforM3) Then 
M3 = M3IN 
Else 
M3 = m12
End If 
If (InputValueforMu) Then 
Mu = MuIN 
Else 
Mu = 0
End If 
If (InputValueforBmu) Then 
Bmu = BmuIN 
Else 
Bmu = 0
End If 
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 MuGUT =Mu
TdGUT =Td
TeGUT =Te
TuGUT =Tu
BmuGUT =Bmu
mq2GUT =mq2
ml2GUT =ml2
mHd2GUT =mHd2
mHu2GUT =mHu2
md2GUT =md2
mu2GUT =mu2
me2GUT =me2
M1GUT =M1
M2GUT =M2
M3GUT =M3
g1GUT =g1
g2GUT =g2
g3GUT =g3
YdGUT =Yd
YeGUT =Ye
YuGUT =Yu
If (Maxval(Abs(Yd)).gt.3._dp) Then 
Write(*,*) "Non pertubative coupling at GUT scale Yd" 
If (ErrorLevel.gt.0) Call TerminateProgram 
End if 
If (Maxval(Abs(Ye)).gt.3._dp) Then 
Write(*,*) "Non pertubative coupling at GUT scale Ye" 
If (ErrorLevel.gt.0) Call TerminateProgram 
End if 
If (Maxval(Abs(Yu)).gt.3._dp) Then 
Write(*,*) "Non pertubative coupling at GUT scale Yu" 
If (ErrorLevel.gt.0) Call TerminateProgram 
End if 
If (Abs(g1).gt.3._dp) Then 
Write(*,*) "Non pertubative coupling at GUT scale appearing for: g1" 
If (ErrorLevel.gt.0) Call TerminateProgram 
End if 
If (Abs(g2).gt.3._dp) Then 
Write(*,*) "Non pertubative coupling at GUT scale appearing for: g2" 
If (ErrorLevel.gt.0) Call TerminateProgram 
End if 
If (Abs(g3).gt.3._dp) Then 
Write(*,*) "Non pertubative coupling at GUT scale appearing for: g3" 
If (ErrorLevel.gt.0) Call TerminateProgram 
End if 



! Translate input form SCKM to electroweak basis 
If (SwitchToSCKM) Then
Yd_ckm = Yd(1:3,1:3) 
Yu_ckm = Yu(1:3,1:3) 
Td_ckm = Td(1:3,1:3) 
Tu_ckm = Tu(1:3,1:3) 
mq2_ckm = mq2(1:3,1:3) 
md2_ckm = md2(1:3,1:3) 
mu2_ckm = mu2(1:3,1:3) 
Call Switch_from_superCKM(Yd_ckm, Yu_ckm, Td_ckm, Tu_ckm, md2_ckm, mq2_ckm, mu2_ckm& 
&, Td_out, Tu_out, md2_out, mq2_out, mu2_out,.True.) 
If (InputValueforTd) Td = Td_out 
If (InputValueforTu) Tu = Tu_out 
If (InputValueformq2) mq2 = mq2_out 
If (InputValueformd2) md2 = md2_out 
If (InputValueformu2) mu2 = mu2_out 
End If 



Call ParametersToG213(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,            & 
& md2,mu2,me2,M1,M2,M3,gB)

Iname=Iname-1
Contains 

Subroutine Switch_from_superCKM(Y_d, Y_u, Ad_in, Au_in, MD_in, MQ_in, MU_in &
                      &, Ad_out, Au_out, MD_out, MQ_out, MU_out, tr        &
                      &, RSd_in, RSu_in, RSd_out, RSu_out, CKM_out, Yd, Yu )
 !---------------------------------------------------------------------------
 ! shifts the parameter from the  super CKM basis to the electroweak basis
 ! written by werner Porod, 12.03.08
 !---------------------------------------------------------------------------
 Implicit None
  Complex(dp), Intent(in), Dimension(3,3) :: Y_d, Y_u, Au_in, Ad_in, MD_in &
        & , MQ_in, MU_in
  Complex(dp), Optional, Intent(in), Dimension(6,6) :: RSu_in, RSd_in
  Logical, Intent(in) :: tr  ! if true, then the matrices are transposed 
                             ! compared to low energy definition
  Complex(dp), Intent(out), Dimension(3,3) :: Au_out, Ad_out, MD_out, MQ_out &
        & , MU_out
  Complex(dp), Optional, Intent(out), Dimension(6,6) :: RSu_out, RSd_out
  Complex(dp), Optional, Intent(out) :: CKM_out(3,3)
  Complex(dp), Optional, Intent(out) :: Yd(3), Yu(3)

  Complex(dp), Dimension(3,3) :: uU_L, uU_R, uD_L, uD_R, CKM_Q
  Complex(dp) :: rot(6,6), Ephi

  Real(dp) :: mf(3), s12, s23, aR, aI, s13, c13
  Integer :: ierr

  !------------------------------------------
  ! diagonalizing d- and u-Yukawa couplings
  ! I am only interested in the mixing matrices
  !------------------------------------------
  If (tr) Then
   Call FermionMass(Transpose(Y_u), 1._dp, mf, uU_L, uU_R, ierr)
   If (Present(Yu)) Yu = sqrt2 * mf
   Call FermionMass(Transpose(Y_d), 1._dp, mf, uD_L, uD_R, ierr)
   If (Present(Yd)) Yd = sqrt2 * mf
  Else
   Call FermionMass(Y_u, 1._dp, mf, uU_L, uU_R, ierr)
   If (Present(Yu)) Yu = sqrt2 * mf
   Call FermionMass(Y_d, 1._dp, mf, uD_L, uD_R, ierr)
   If (Present(Yd)) Yd = sqrt2 * mf
  End If
  !---------------------------------------------------------
  ! CKM matrix at Q, shifting phases according to PDG form
  !---------------------------------------------------------
  CKM_Q =  Matmul(uU_L, Transpose(Conjg(ud_L)) )
  uD_L(1,:) = uD_L(1,:) / Conjg(CKM_Q(1,1)) * Abs(CKM_Q(1,1))
  uD_L(2,:) = uD_L(2,:) / Conjg(CKM_Q(1,2)) * Abs(CKM_Q(1,2))
  uU_L(2,:) = uU_L(2,:) / CKM_Q(2,3) * Abs(CKM_Q(2,3))
  uU_L(3,:) = uU_L(3,:) / CKM_Q(3,3) * Abs(CKM_Q(3,3))
  !-------------------------------------------------------------------
  ! also the right quark must be multiplied with the conjugate phase
  ! as otherwise the masses get complex
  !-------------------------------------------------------------------
  uD_R(1,:) = uD_R(1,:) / CKM_Q(1,1) * Abs(CKM_Q(1,1))
  uD_R(2,:) = uD_R(2,:) / CKM_Q(1,2) * Abs(CKM_Q(1,2))
  uU_R(2,:) = uU_R(2,:) / Conjg(CKM_Q(2,3)) * Abs(CKM_Q(2,3))
  uU_R(3,:) = uU_R(3,:) / Conjg(CKM_Q(3,3)) * Abs(CKM_Q(3,3))
  CKM_Q =  Matmul(uU_L, Transpose(Conjg(ud_L)) )

  !--------------------------------------------------------------
  ! one more freedom left
  !--------------------------------------------------------------
  s13 = Abs(CKM_Q(1,3))
  c13 = sqrt(1._dp - s13**2)
  s23 = Abs(CKM_Q(2,3))/c13
  s12 = Abs(CKM_Q(1,2))/c13

  aR = Real(CKM_Q(2,2),dp) + s12 * s23 * Real(CKM_Q(1,3),dp)
  aI =  s12 * s23 * Aimag(CKM_Q(1,3)) - Aimag(CKM_Q(2,2))
  Ephi = Cmplx(aR/Sqrt(aR**2+aI**2),aI/Sqrt(aR**2+aI**2),dp)

  uU_L(2:3,:) = Ephi * uU_L(2:3,:)
  uD_L(3,:) = Ephi * uD_L(3,:)
  Ephi = Conjg(Ephi)
  uU_R(2:3,:) = Ephi * uU_R(2:3,:)
  uD_R(3,:) = Ephi * uD_R(3,:)

  CKM_Q =  Matmul(uU_L, Transpose(Conjg(ud_L)) )

  If (Present(CKM_out)) CKM_out = CKM_Q
  !-------------------------------------------------------------------
  ! shifting the parameters from the super CKM basis
  !-------------------------------------------------------------------
  If (tr) Then
   Au_out = Matmul( Matmul(Transpose(uU_R), Au_in), uU_L)
   Ad_out = Matmul( Matmul(Transpose(uD_R), Ad_in), uD_L)

   MD_out = Matmul( Matmul( Transpose(Conjg(uD_R)), MD_in), uD_R)
   MU_out = Matmul( Matmul( Transpose(Conjg(uU_R)), MU_in), uU_R)
   MQ_out = Matmul( Matmul( Transpose(uD_L), MQ_in), Conjg(uD_L) )

  Else
   Au_out = Matmul( Matmul(Transpose(uU_L), Au_in), uU_R)
   Ad_out = Matmul( Matmul(Transpose(uD_L), Ad_in), uD_R)

   MD_out = Matmul( Matmul( Transpose(uD_R), MD_in), Conjg(uD_R))
   MU_out = Matmul( Matmul( Transpose(uU_R), MU_in), Conjg(uU_R))
   MQ_out = Matmul( Matmul( Transpose(Conjg(uD_L)), MQ_in), uD_L )

  End If
  !------------------------------------------------------------------
  ! to avoid numerical problems ensure that matrices are hermitian
  !-----------------------------------------------------------------
  MD_out = 0.5_dp * ( MD_out + Conjg(Transpose(MD_out)) )
  MU_out = 0.5_dp * ( MU_out + Conjg(Transpose(MU_out)) )
  MQ_out = 0.5_dp * ( MQ_out + Conjg(Transpose(MQ_out)) )

   If (Present(RSu_in).And.Present(RSu_out)) Then
    rot = 0._dp
    rot(1:3,1:3) = Conjg(uU_L)
    rot(4:6,4:6) = uU_R
    RSu_out = Matmul(RSu_in, rot)
   End If
   If (Present(RSd_in).And.Present(RSd_out)) Then
    rot = 0._dp
    rot(1:3,1:3) = Conjg(uD_L)
    rot(4:6,4:6) = uD_R
    RSd_out = Matmul(RSd_in, rot)
   End If

 End Subroutine Switch_from_superCKM
End Subroutine BoundaryHS 
 
Subroutine BoundaryEW(i_run,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,              & 
& MFu2,MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,               & 
& MVWm,MVWm2,MVZ,MVZ2,pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,               & 
& ZUR,ZV,ZW,ZZ,vd,vu,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,            & 
& mu2,me2,M1,M2,M3,delta0,gMZ,kont)

Implicit None 
Real(dp),Intent(out)::gMZ(:)
Real(dp),Intent(inout) :: g1,g2,g3,mHd2,mHu2

Complex(dp),Intent(inout) :: Yd(3,3),Ye(3,3),Yu(3,3),Mu,Td(3,3),Te(3,3),Tu(3,3),Bmu,mq2(3,3),ml2(3,3),             & 
& md2(3,3),mu2(3,3),me2(3,3),M1,M2,M3

Real(dp),Intent(inout) :: MCha(2),MCha2(2),MChi(4),MChi2(4),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),               & 
& MFu2(3),MGlu,MGlu2,Mhh(4),Mhh2(4),MHpm(2),MHpm2(2),MSd(6),MSd2(6),MSe(6),              & 
& MSe2(6),MSu(6),MSu2(6),MSv(3),MSv2(3),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZH(4,4),ZZ(2,2)

Complex(dp),Intent(inout) :: pG,UM(2,2),UP(2,2),ZD(6,6),ZDL(3,3),ZDR(3,3),ZE(6,6),ZEL(3,3),ZER(3,3),               & 
& ZN(4,4),ZP(2,2),ZU(6,6),ZUL(3,3),ZUR(3,3),ZV(3,3),ZW(2,2)

Real(dp),Intent(inout) :: vd,vu

Complex(dp) :: cplcChaChaVZL(2,2),cplcChaChaVZR(2,2),cplChiChiVZL(4,4),cplChiChiVZR(4,4),            & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcgWmgWmVZ,cplcgWpCgWpCVZ,        & 
& cplhhhhVZ(4,4),cplhhVZVZ(4),cplHpmcHpmVZ(2,2),cplHpmcVWmVZ(2),cplSdcSdVZ(6,6),         & 
& cplSecSeVZ(6,6),cplSucSuVZ(6,6),cplSvcSvVZ(3,3),cplcVWmVWmVZ,cplhhhhVZVZ(4,4),         & 
& cplHpmcHpmVZVZ(2,2),cplSdcSdVZVZ(6,6),cplSecSeVZVZ(6,6),cplSucSuVZVZ(6,6),             & 
& cplSvcSvVZVZ(3,3),cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplChiChacVWmL(4,2), & 
& cplChiChacVWmR(4,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFvFecVWmL(3,3),          & 
& cplcFvFecVWmR(3,3),cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,          & 
& cplhhHpmcVWm(4,2),cplhhcVWmVWm(4),cplHpmcVWmVP(2),cplSdcSucVWm(6,6),cplSecSvcVWm(6,3), & 
& cplcVWmVPVWm,cplhhhhcVWmVWm(4,4),cplHpmcHpmcVWmVWm(2,2),cplSdcSdcVWmVWm(6,6),          & 
& cplSecSecVWmVWm(6,6),cplSucSucVWmVWm(6,6),cplSvcSvcVWmVWm(3,3),cplcVWmVPVPVWm1,        & 
& cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3

Complex(dp) :: cplcUFeChaSvL(3,2,3),cplcUFeChaSvR(3,2,3),cplcUFeChiSeL(3,4,6),cplcUFeChiSeR(3,4,6),  & 
& cplcUFeFehhL(3,3,4),cplcUFeFehhR(3,3,4),cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),           & 
& cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),cplcUFeFvHpmL(3,3,2),cplcUFeFvHpmR(3,3,2),         & 
& cplcUFeFvVWmL(3,3),cplcUFeFvVWmR(3,3),cplcUFdChaSuL(3,2,6),cplcUFdChaSuR(3,2,6),       & 
& cplcUFdChiSdL(3,4,6),cplcUFdChiSdR(3,4,6),cplcUFdFdhhL(3,3,4),cplcUFdFdhhR(3,3,4),     & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHpmL(3,3,2),cplcUFdFuHpmR(3,3,2),         & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3),cplcUFdGluSdL(3,6),cplcUFdGluSdR(3,6),           & 
& cplcUFuChiSuL(3,4,6),cplcUFuChiSuR(3,4,6),cplcUFuFdcHpmL(3,3,2),cplcUFuFdcHpmR(3,3,2), & 
& cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),cplcUFuFuhhL(3,3,4),cplcUFuFuhhR(3,3,4),       & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3),cplcUFuGluSuL(3,6),cplcUFuGluSuR(3,6),             & 
& cplcChacUFuSdL(2,3,6),cplcChacUFuSdR(2,3,6)

Real(dp) :: Abs2Mu

Integer, Intent(in) :: i_run 
Real(dp), Intent(in) :: delta0 
Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4 
Complex(dp) ::uU_L(3,3),uU_R(3,3),uD_L(3,3),uD_R(3,3), NoMatrix(3,3) &
&,uL_L(3,3),uL_R(3,3)
Real(dp)::mW2_run,mZ2_run,test, D_mat(3,3)
Real(dp)::alphaMZ,alpha3,gSU2,rho,delta_rho,delta_rho0,sinW2_DR,vev2&
&,vevs_DR(2),mZ2_mZ,CosW2SinW2,gauge(3),delta,sinW2_old,delta_r&
&,p2,gSU3,tanb,xt2,fac(2),SigQCD,delta_rw,sinW2,cosW2,cosW
Real(dp),Dimension(3)::mf_d_DR,mf_l_DR,mf_u_DR
Complex(dp) :: dmZ2,dmW2,dmW2_0,yuk_tau,yuk_t,yuk_b
Complex(dp) :: SigS_u(3,3),sigR_u(3,3),SigL_u(3,3)
Complex(dp) :: SigS_d(3,3),SigR_d(3,3),SigL_d(3,3)
Complex(dp) :: SigS_l(3,3),sigR_l(3,3),SigL_l(3,3)
Complex(dp) :: Y_u(3,3),Y_d(3,3),Y_l(3,3),adCKM(3,3),Y_l_old(3,3),Y_d_old(3,3),Y_u_old(3,3) 
Complex(dp) :: uU_L_T(3,3),uU_R_T(3,3)&
&,uD_L_T(3,3),uD_R_T(3,3),uL_L_T(3,3),uL_R_T(3,3)
Logical::converge
Integer :: i_loop, i_loop_max 
Real(dp),Parameter::&
& as2loop=1._dp/24._dp+2011._dp*oo32Pi2/12._dp&
&+Log2/12._dp-oo8Pi2*Zeta3&
&,log2loop_a=123._dp*oo32Pi2,log2loop_b=33._dp*oo32Pi2
Real(dp)::Q2,logQ 


Complex(dp) ::MassFu(3,3),MassFd(3,3),MassFe(3,3) 
Iname=Iname+1
NameOfUnit(Iname)='BoundaryEW'
tanb = tanbetaMZ 
sinW2 = 1 - mW**2/mZ**2 
test = SetRenormalizationScale(mZ2) 
!-----------------
!sin(theta_W)^2
!-----------------
If (i_run.Eq.1) Then
   sinW2_DR=sinW2
   sinW2_old=sinW2_DR
   Y_l=0._dp
   Do i1=1,3
       y_l(i1,i1)=sqrt2*mf_l_mZ(i1)/vevSM(1)
   End Do
   mf_l2=mf_l_mZ**2
   mf_d2=mf_d_mZ**2
   mf_u2=mf_u_mZ**2
Else
   sinW2_DR=sinW2_DR_mZ
   sinW2_old=sinW2_DR
   Y_l=Yl_mZ
   Call FermionMass(Yd_mZ,vevs_DR_save(1),mf_d2,uD_L_T,uD_R_T,kont)
   Call FermionMass(Yl_mZ,vevs_DR_save(1),mf_l2,uL_L_T,uL_R_T,kont)
   Call FermionMass(Yu_mZ,vevs_DR_save(2),mf_u2,uU_L_T,uU_R_T,kont)
   mf_l2=mf_l2**2
   mf_d2=mf_d2**2
   mf_u2=mf_u2**2
End If
MFe2(1:3) = mf_l**2 
MFd2(1:3) = mf_d**2 
MFu2(1:3) = mf_u**2 
MFe = sqrt(MFe2) 
MFd = sqrt(MFd2) 
MFu = sqrt(MFu2) 
alphaMZ = AlphaEwDR(mZ,MVWm,MSd,MSu,MSe,MHpm,MCha,MFe,MFd,MFu) 
 
MFe2(1:3) = mf_l2 
MFd2(1:3) = mf_d2 
MFu2(1:3) = mf_u2 
MFe = sqrt(MFe2) 
MFd = sqrt(MFd2) 
MFu = sqrt(MFu2) 
alpha3 = AlphaSDR(mZ,MSd,MSu,MGlu,MFd,MFu) 
gSU3 = Sqrt(4._dp*pi*alpha3) 
 
!--------------------
!for 2-loop parts
!--------------------
xt2=3._dp*(G_F*mf_u2(3)*oo8pi2*oosqrt2)**2&
    &*Abs(ZH(1,2))**2*rho_2(Sqrt(Mhh2(1))/mf_U(3))&
    &*((1._dp+tanb**2)/tanb**2)
fac(1)=alphaMZ*alphaS_mZ*oo4pi&
      &*(2.145_dp*mf_u2(3)/mZ2+0.575*Log(mf_u(3)/mZ)-0.224_dp&
      &-0.144_dp*mZ2/mf_u2(3))/Pi
fac(2)=alphamZ*alphaS_mZ*oo4pi&
      &*(-2.145_dp*mf_u2(3)/mW2+1.262*Log(mf_u(3)/mZ)-2.24_dp&
      &-0.85_dp*mZ2/mf_u2(3))/Pi
Do i1=1,100 
gSU2 = Sqrt( 4._dp*pi*alphamZ/sinW2_DR) 
g1 =gSU2*Sqrt(sinW2_DR/(1._dp-sinW2_DR)) 
g2 = gSU2 
TW= Asin(Sqrt(sinw2_dr)) 

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 
MVZ2 = mZ2 
MVZ= Sqrt(MVZ2) 
MVWm2 = mW2 
MVWm= Sqrt(MVWm2) 
Mhh(1)=MVZ
Mhh2(1)=MVZ2
MHpm(1)=MVWm
MHpm2(1)=MVWm2
Call CouplingsForVectorBosons(g1,g2,UM,UP,TW,ZN,ZH,vd,vu,ZP,ZD,ZE,ZU,ZDL,             & 
& ZUL,ZEL,ZV,cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,          & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhhhVZ,cplhhVZVZ,cplHpmcHpmVZ,              & 
& cplHpmcVWmVZ,cplSdcSdVZ,cplSecSeVZ,cplSucSuVZ,cplSvcSvVZ,cplcVWmVWmVZ,cplhhhhVZVZ,     & 
& cplHpmcHpmVZVZ,cplSdcSdVZVZ,cplSecSeVZVZ,cplSucSuVZVZ,cplSvcSvVZVZ,cplcVWmVWmVZVZ1,    & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,           & 
& cplcFuFdcVWmR,cplcFvFecVWmL,cplcFvFecVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,  & 
& cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplSdcSucVWm,cplSecSvcVWm,       & 
& cplcVWmVPVWm,cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSecSecVWmVWm,         & 
& cplSucSucVWmVWm,cplSvcSvcVWmVWm,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,       & 
& cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)

Call Pi1LoopVZ(mZ2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,              & 
& Mhh2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,               & 
& cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,cplcFdFdVZR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,cplcFvFvVZR,               & 
& cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhhhVZ,cplhhVZVZ,cplHpmcHpmVZ,cplHpmcVWmVZ,             & 
& cplSdcSdVZ,cplSecSeVZ,cplSucSuVZ,cplSvcSvVZ,cplcVWmVWmVZ,cplhhhhVZVZ,cplHpmcHpmVZVZ,   & 
& cplSdcSdVZVZ,cplSecSeVZVZ,cplSucSuVZVZ,cplSvcSvVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,   & 
& cplcVWmVWmVZVZ3,kont,dmZ2)

mZ2_mZ = Real(dmZ2 + mZ2,dp) 
If (mZ2_mZ.Lt.0._dp) Then
    Iname=Iname-1
    kont=-402
    Call AddError(402)
    Write(*,*) " MZ  getting negative at EW scale" 
    Call TerminateProgram
End If

mZ2_run=mZ2_mZ
mW2_run=mZ2_mZ*(1._dp-sinW2_DR) +0  
MVZ2 = mZ2_run 
MVZ= Sqrt(MVZ2) 
MVWm2 = mW2_run 
MVWm= Sqrt(MVWm2) 
Mhh(1)=MVZ
Mhh2(1)=MVZ2
MHpm(1)=MVWm
MHpm2(1)=MVWm2
CosW2SinW2=(1._dp-sinW2_DR)*sinW2_DR
vev2=mZ2_mZ*CosW2SinW2/(pi*alphamZ) -0 
vevs_DR(1)=Sqrt(vev2/(1._dp+tanb**2))
vevs_DR(2)=tanb*vevs_DR(1)
vd=vevs_DR(1)
vu=vevs_DR(2) 
Call CouplingsForVectorBosons(g1,g2,UM,UP,TW,ZN,ZH,vd,vu,ZP,ZD,ZE,ZU,ZDL,             & 
& ZUL,ZEL,ZV,cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,          & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhhhVZ,cplhhVZVZ,cplHpmcHpmVZ,              & 
& cplHpmcVWmVZ,cplSdcSdVZ,cplSecSeVZ,cplSucSuVZ,cplSvcSvVZ,cplcVWmVWmVZ,cplhhhhVZVZ,     & 
& cplHpmcHpmVZVZ,cplSdcSdVZVZ,cplSecSeVZVZ,cplSucSuVZVZ,cplSvcSvVZVZ,cplcVWmVWmVZVZ1,    & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,           & 
& cplcFuFdcVWmR,cplcFvFecVWmL,cplcFvFecVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,  & 
& cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplSdcSucVWm,cplSecSvcVWm,       & 
& cplcVWmVPVWm,cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSecSecVWmVWm,         & 
& cplSucSucVWmVWm,cplSvcSvcVWmVWm,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,       & 
& cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)

Call Pi1LoopVZ(mZ2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,              & 
& Mhh2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,               & 
& cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,cplcFdFdVZR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,cplcFvFvVZR,               & 
& cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhhhVZ,cplhhVZVZ,cplHpmcHpmVZ,cplHpmcVWmVZ,             & 
& cplSdcSdVZ,cplSecSeVZ,cplSucSuVZ,cplSvcSvVZ,cplcVWmVWmVZ,cplhhhhVZVZ,cplHpmcHpmVZVZ,   & 
& cplSdcSdVZVZ,cplSecSeVZVZ,cplSucSuVZVZ,cplSvcSvVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,   & 
& cplcVWmVWmVZVZ3,kont,dmZ2)

mZ2_mZ = Real(dmZ2 + mZ2,dp) 
If (mZ2_mZ.Lt.0._dp) Then
    Iname=Iname-1
    kont=-402
    Call AddError(402)
    Write(*,*) " MZ  getting negative at EW scale" 
    Call TerminateProgram
End If

mZ2_run=mZ2_mZ
mW2_run=mZ2_mZ*(1._dp-sinW2_DR) +0  
MVZ2 = mZ2_run 
MVZ= Sqrt(MVZ2) 
MVWm2 = mW2_run 
MVWm= Sqrt(MVWm2) 
Mhh(1)=MVZ
Mhh2(1)=MVZ2
MHpm(1)=MVWm
MHpm2(1)=MVWm2
CosW2SinW2=(1._dp-sinW2_DR)*sinW2_DR
vev2=mZ2_mZ *CosW2SinW2/(pi*alphamZ) -0 
vevs_DR(1)=Sqrt(vev2/(1._dp+tanb**2))
vevs_DR(2)=tanb*vevs_DR(1)
vd=vevs_DR(1)
vu=vevs_DR(2) 
Call CouplingsForVectorBosons(g1,g2,UM,UP,TW,ZN,ZH,vd,vu,ZP,ZD,ZE,ZU,ZDL,             & 
& ZUL,ZEL,ZV,cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,          & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhhhVZ,cplhhVZVZ,cplHpmcHpmVZ,              & 
& cplHpmcVWmVZ,cplSdcSdVZ,cplSecSeVZ,cplSucSuVZ,cplSvcSvVZ,cplcVWmVWmVZ,cplhhhhVZVZ,     & 
& cplHpmcHpmVZVZ,cplSdcSdVZVZ,cplSecSeVZVZ,cplSucSuVZVZ,cplSvcSvVZVZ,cplcVWmVWmVZVZ1,    & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,           & 
& cplcFuFdcVWmR,cplcFvFecVWmL,cplcFvFecVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,  & 
& cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplSdcSucVWm,cplSecSvcVWm,       & 
& cplcVWmVPVWm,cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSecSecVWmVWm,         & 
& cplSucSucVWmVWm,cplSvcSvcVWmVWm,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,       & 
& cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)

Call Pi1LoopVWm(mW2,MChi,MChi2,MCha,MCha2,MFu,MFu2,MFd,MFd2,MFe,MFe2,MHpm,            & 
& MHpm2,Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,MSv,MSv2,MSe,MSe2,cplChiChacVWmL, & 
& cplChiChacVWmR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFvFecVWmL,cplcFvFecVWmR,cplcgWpCgAcVWm, & 
& cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,     & 
& cplHpmcVWmVZ,cplSdcSucVWm,cplSecSvcVWm,cplcVWmVPVWm,cplcVWmVWmVZ,cplhhhhcVWmVWm,       & 
& cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSecSecVWmVWm,cplSucSucVWmVWm,cplSvcSvcVWmVWm,     & 
& cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3, & 
& cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,dmW2)

Call Pi1LoopVWm(0._dp,MChi,MChi2,MCha,MCha2,MFu,MFu2,MFd,MFd2,MFe,MFe2,               & 
& MHpm,MHpm2,Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,MSv,MSv2,MSe,MSe2,           & 
& cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFvFecVWmL,               & 
& cplcFvFecVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,               & 
& cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplHpmcVWmVZ,cplSdcSucVWm,cplSecSvcVWm,         & 
& cplcVWmVPVWm,cplcVWmVWmVZ,cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,            & 
& cplSecSecVWmVWm,cplSucSucVWmVWm,cplSvcSvcVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,       & 
& cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,dmW2_0)

rho=(1._dp+Real(dmZ2,dp)/mZ2)/(1._dp+Real(dmW2,dp)/mW2)  
delta_rho=1._dp-1._dp/rho
delta_rho0=0
rho=1._dp/(1._dp-delta_rho-delta_rho0)
CosW2SinW2=(1._dp-sinW2_DR)*sinW2_DR
If (IncludeDeltaVB) Then 
Call DeltaVB(sinW2,sinW2_dr,rho,MCha,MChi,MFe,Mhh,MHpm,MSe,MSv,MVWm,g1,               & 
& g2,UM,UP,vd,vu,Ye,ZE,ZEL,ZER,ZH,ZN,ZP,ZV,delta)

Else 
delta = 0._dp 
End if 
delta_r=rho*Real(dmW2_0,dp)/mW2-Real(dmZ2,dp)/mZ2+delta
rho=1._dp/(1._dp-delta_rho-delta_rho0-fac(2)/sinW2_DR-xt2)
delta_r=rho*Real(dmW2_0,dp)/mW2-Real(dmZ2,dp)/mZ2+delta&
        &+fac(1)/CosW2SinW2-xt2*(1-delta_r)*rho
CosW2SinW2=pi*alphamZ/(sqrt2*mZ2*G_F*(1-delta_r))
sinW2_DR=0.5_dp-Sqrt(0.25_dp-CosW2SinW2)

If (sinW2_DR.Lt.0._dp) Then
    kont=-403
    Call AddError(403)
    Iname=Iname-1
    Write(*,*) " sinW2 getting negtive at EW scale " 
    Call TerminateProgram
End If
 
If (Abs(sinW2_DR-sinW2_old).Lt.0.1_dp*delta0) Exit

sinW2_old=sinW2_DR
delta_rw=delta_rho*(1._dp-delta_r)+delta_r
If ((0.25_dp-alphamz*pi/(sqrt2*G_F*mz2*rho*(1._dp-delta_rw))).Lt.0._dp) Then
    kont=-404
    Call AddError(404)
    Iname=Iname-1
     Return
End If

mW2=mZ2*rho*(0.5_dp&
    &+Sqrt(0.25_dp-alphamz*pi/(sqrt2*G_F*mz2*rho*(1._dp-delta_rw))))
Mhh(1)=MVZ
Mhh2(1)=MVZ2
MHpm(1)=MVWm
MHpm2(1)=MVWm2
cosW2=mW2/mZ2
cosW=Sqrt(cosW2)
sinW2=1._dp-cosW2
End Do

Mhh(1)=MVZ
Mhh2(1)=MVZ2
MHpm(1)=MVWm
MHpm2(1)=MVWm2
delta_rw=delta_rho*(1._dp-delta_r)+delta_r
mW2=mZ2*rho*(0.5_dp& 
   &+Sqrt(0.25_dp-alphamz*pi/(sqrt2*G_F*mz2*rho*(1._dp-delta_rw))))
mW=Sqrt(mW2)
cosW2=mW2/mZ2
cosW=Sqrt(cosW2)
sinW2=1._dp-cosW2
gauge(1)=Sqrt(4._dp*pi*alphamZ/(1._dp-sinW2_DR))
gauge(2)=Sqrt(4._dp*pi*alphamZ/sinW2_DR)
gauge(3)=Sqrt(4._dp*pi*alpha3)
vev2=mZ2_mZ*CosW2SinW2/(pi*alphamZ) -0 
vevs_DR(1)=Sqrt(vev2/(1._dp+tanb**2))
vevs_DR(2)=tanb*vevs_DR(1)
vdMZ = vevs_DR(1)
vuMZ = vevs_DR(2)
vd = vevs_DR(1)
vu = vevs_DR(2)
g1MZ = gauge(1)
g2MZ = gauge(2)


! -------------------------
!  Calculate Yukawas
! -------------------------
uU_L=id3C
uU_R=id3C
uD_L=id3C
uD_R=id3C
uL_L=id3C
uL_R=id3C
If (GenerationMixing) Then
    Call Adjungate(CKM,adCKM)
 If (YukawaScheme.Eq.1) Then
    uU_L(1:3,1:3)=CKM
 Else
    uD_L(1:3,1:3)=adCKM
 End If
End If
If (i_run.Eq.1) Then
mf_l_DR_SM=&
            & mf_l_mZ*(1._dp-oo8pi2*3._dp*(gauge(1)**2-gauge(2)**2)/16._dp)
mf_d_DR_SM=mf_d_mZ*(1._dp-alpha3/(3._dp*pi)&
           &-23._dp*alpha3**2/(72._dp*Pi2)&
           &+oo8pi2*3._dp*gauge(2)**2/16._dp&
           &-oo8pi2*13._dp*gauge(1)**2/144._dp)
mf_u_DR_SM(1:2)=mf_u_mZ(1:2)*(1._dp-alpha3/(3._dp*pi)&
               &-23._dp*alpha3**2/(72._dp*Pi2)&
               &+oo8pi2*3._dp*gauge(2)**2/16._dp&
               &-oo8pi2*7._dp*gauge(1)**2/144._dp)
mf_u_DR_SM(3)=mf_u(3)! QCD+QED shift will be added later
mf_l_DR=mf_l_DR_SM
mf_d_DR=mf_d_DR_SM
mf_u_DR=mf_u_DR_SM
Y_d=0._dp
Y_u=0._dp
Y_l=0._dp
Do i1=1,3
    Y_u(i1,i1)=sqrt2*mf_u_DR_SM(i1)/vevs_DR(2)
    Y_l(i1,i1)=sqrt2*mf_l_DR_SM(i1)/vevs_DR(1)
    Y_d(i1,i1)=sqrt2*mf_d_DR_SM(i1)/vevs_DR(1)
End Do
If (GenerationMixing) Then
  If (YukawaScheme.Eq.1) Then
    Y_u=Matmul(Transpose(uU_L(1:3,1:3)),Y_u)
  Else
    Y_d=Matmul(Transpose(uD_L(1:3,1:3)),Y_d)
  End If
End If
Else
Y_l=Yl_MZ
Y_d=Yd_MZ
Y_u=Yu_MZ
End If! i_run.eq.1

converge= .False.
Y_l_old=Y_l
Y_d_old=Y_d
Y_u_old=Y_u


! -------------------------
!  Main Loop
! -------------------------
if (FermionMassResummation) then
  i_loop_max=100! this should be sufficient
else
  i_loop_max=1
end if
Do i_loop=1,i_loop_max
p2=0._dp! for off-diagonal elements

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 


! Full one-loop corrections
Call CouplingsForSMfermions(g2,Ye,ZV,UM,UP,g1,ZE,ZN,ZH,ZEL,ZER,TW,ZP,Yd,              & 
& Yu,ZU,ZD,ZDL,ZDR,g3,eta,ZUL,ZUR,pG,cplcUFeChaSvL,cplcUFeChaSvR,cplcUFeChiSeL,          & 
& cplcUFeChiSeR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,        & 
& cplcUFeFeVZR,cplcUFeFvHpmL,cplcUFeFvHpmR,cplcUFeFvVWmL,cplcUFeFvVWmR,cplcUFdChaSuL,    & 
& cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,      & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHpmL,        & 
& cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFdGluSdL,cplcUFdGluSdR,cplcUFuChiSuL,   & 
& cplcUFuChiSuR,cplcUFuFdcHpmL,cplcUFuFdcHpmR,cplcUFuFdcVWmL,cplcUFuFdcVWmR,             & 
& cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,cplcUFuFuVPR,         & 
& cplcUFuFuVZL,cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,cplcChacUFuSdL,cplcChacUFuSdR)

Call Sigma1LoopFeMZ(p2,MSv,MSv2,MCha,MCha2,MSe,MSe2,MChi,MChi2,Mhh,Mhh2,              & 
& MFe,MFe2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,cplcUFeChaSvL,cplcUFeChaSvR,cplcUFeChiSeL,     & 
& cplcUFeChiSeR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,        & 
& cplcUFeFeVZR,cplcUFeFvHpmL,cplcUFeFvHpmR,cplcUFeFvVWmL,cplcUFeFvVWmR,sigR_l,           & 
& sigL_l,sigS_l)

Call Sigma1LoopFdMZ(p2,MSu,MSu2,MCha,MCha2,MSd,MSd2,MChi,MChi2,Mhh,Mhh2,              & 
& MFd,MFd2,MVZ,MVZ2,MHpm,MHpm2,MFu,MFu2,MVWm,MVWm2,MGlu,MGlu2,cplcUFdChaSuL,             & 
& cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,      & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHpmL,        & 
& cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFdGluSdL,cplcUFdGluSdR,sigR_d,          & 
& sigL_d,sigS_d)

Call Sigma1LoopFuMZ(p2,MSu,MSu2,MChi,MChi2,MHpm,MHpm2,MFd,MFd2,MVWm,MVWm2,            & 
& Mhh,Mhh2,MFu,MFu2,MVZ,MVZ2,MGlu,MGlu2,MCha,MCha2,MSd,MSd2,cplcUFuChiSuL,               & 
& cplcUFuChiSuR,cplcUFuFdcHpmL,cplcUFuFdcHpmR,cplcUFuFdcVWmL,cplcUFuFdcVWmR,             & 
& cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,cplcUFuFuVPR,         & 
& cplcUFuFuVZL,cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,cplcChacUFuSdL,cplcChacUFuSdR,   & 
& sigR_u,sigL_u,sigS_u)



! SM two-loop corrections
Q2=GetRenormalizationScale()
logQ=Log(Q2/MFu(3)**2)
SigQCD=-4._dp/3._dp*gSU3**2*MFu(3)*(5._dp+3._dp*LogQ&
&+(as2loop+log2loop_a*logQ&
&+log2loop_b*logQ**2)*gSU3**2)&
&-MFu(3)*(-2._dp/3._dp*gSU2)**2*sinW2_DR*(5._dp+3._dp*LogQ)
SigQCD=oo16pi2*SigQCD

mf_u_DR_SM(3)=mf_u(3)+SigQCD



! Construct tree-level masses
! Needed for models with additional states mixing with SM particles
Call CalculateMFe(Ye,vd,ZEL,ZER,MFe,kont)

MassFe=0._dp 
Do i1 = 1,3
 MassFe(i1,i1)=MFe(i1) 
End do
MassFe = MatMul(Transpose(ZEL),MatMul(MassFe,Conjg(ZER))) 
MFe(1:3) =mf_l_DR_SM 
Call CalculateMFu(Yu,eta,vu,ZUL,ZUR,MFu,kont)

MassFu=0._dp 
Do i1 = 1,3
 MassFu(i1,i1)=MFu(i1) 
End do
MassFu = MatMul(Transpose(ZUL),MatMul(MassFu,Conjg(ZUR))) 
MFu(1:3) =mf_u_DR_SM 
Call CalculateMFd(Yd,vd,ZDL,ZDR,MFd,kont)

MassFd=0._dp 
Do i1 = 1,3
 MassFd(i1,i1)=MFd(i1) 
End do
MassFd = MatMul(Transpose(ZDL),MatMul(MassFd,Conjg(ZDR))) 
MFd(1:3) =mf_d_DR_SM 


! Obtain Yukawas
Call Yukawas3(MFu,vevs_DR(2),uU_L,uU_R,SigS_u,SigL_u,SigR_u&
      &,massFu,Y_u, FermionMassResummation,kont) 
If (kont.Ne.0) Then 
    Iname=Iname-1
    Write(*,*) " Fit of Yukawa couplings at EW scale failed" 
    Call TerminateProgram
End If
Call Yukawas3(MFd,vevs_DR(1),uD_L,uD_R,SigS_d,SigL_d,SigR_d& 
      &,massFd,Y_d,FermionMassResummation,kont)
If (kont.Ne.0) Then
    Iname=Iname-1
    Write(*,*) " Fit of Yukawa couplings at EW scale failed" 
    Call TerminateProgram
End If 
Call Yukawas3(MFe,vevs_DR(1),uL_L,uL_R,SigS_l,SigL_l,SigR_l&
     &,massFe,Y_l,.False.,kont) 
If (kont.Ne.0) Then
    Iname=Iname-1
    Write(*,*) " Fit of Yukawa couplings at EW scale failed" 
    Call TerminateProgram
End If


! Re-calculate (s)quarks with new Yukawas
Yu = Transpose(Y_u) 
Yd = Transpose(Y_d) 
Ye = Transpose(Y_l) 
Call CalculateMFe(Ye,vd,ZEL,ZER,MFe,kont)

Call CalculateMFu(Yu,eta,vu,ZUL,ZUR,MFu,kont)

Call CalculateMFd(Yd,vd,ZDL,ZDR,MFd,kont)

mf_l_DR  = MFe(1:3) 
mf_d_DR  = MFd(1:3) 
mf_u_DR  = MFu(1:3) 
Call CalculateMSe(g1,g2,Mu,Ye,Te,ml2,me2,eta,vd,vu,ZE,MSe,MSe2,kont)

Call CalculateMSu(g1,g2,Mu,Yu,Tu,mq2,mu2,eta,vd,vu,ZU,MSu,MSu2,kont)

Call CalculateMSd(g1,g2,Mu,Yd,Td,mq2,md2,eta,vd,vu,ZD,MSd,MSd2,kont)



! Check convergence 
converge= .True. 
D_mat=Abs(Abs(Y_l)-Abs(Y_l_old))
Where (Abs(Y_l).Ne.0._dp) D_mat=D_mat/Abs(Y_l)
Do i1=1,3
 If (D_mat(i1,i1).Gt.0.1_dp*delta0) converge= .False. 
  Do i2=i1+1,3 
   If (D_mat(i1,i2).Gt.delta0) converge= .False. 
   If (D_mat(i2,i1).Gt.delta0) converge= .False. 
 End Do 
End Do 
D_mat=Abs(Abs(Y_d)-Abs(Y_d_old))
Where (Abs(Y_d).Ne.0._dp) D_mat=D_mat/Abs(Y_d)
Do i1=1,3 
 If (D_mat(i1,i1).Gt.0.1_dp*delta0) converge= .False. 
   Do i2=i1+1,3 
    If (D_mat(i1,i2).Gt.10._dp*delta0) converge= .False. 
    If (D_mat(i2,i1).Gt.10._dp*delta0) converge= .False. 
   End Do 
End Do 
D_mat=Abs(Abs(Y_u)-Abs(Y_u_old))
Where (Abs(Y_u).Ne.0._dp) D_mat=D_mat/Abs(Y_u)
Do i1=1,3
 If (D_mat(i1,i1).Gt.0.1_dp*delta0) converge= .False. 
  Do i2=i1+1,3 
   If (D_mat(i1,i2).Gt.10._dp*delta0) converge= .False. 
   If (D_mat(i2,i1).Gt.10._dp*delta0) converge= .False. 
  End Do 
End Do
If (converge) Exit
  Y_l_old=Y_l
  Y_u_old=Y_u
  Y_d_old=Y_d
!-------------------------------------------------- 
!Either we have run into a numerical problem or 
!perturbation theory breaks down 
!-------------------------------------------------- 
If ((Minval(Abs(mf_l_DR/mf_l)).Lt.0.1_dp)&
&.Or.(Maxval(Abs(mf_l_DR/mf_l)).Gt.10._dp)) Then
Iname=Iname-1
kont=-405
Call AddError(405)
    Write(*,*) " Loop corrections to Yukawa couplings at EW scale too large!" 
    Call TerminateProgram
Else If ((Minval(Abs(mf_d_DR/mf_d)).Lt.0.1_dp)&
&.Or.(Minval(Abs(mf_d_DR/mf_d)).Gt.10._dp)) Then
Iname=Iname-1
kont=-406
Call AddError(406)
    Write(*,*) " Loop corrections to Yukawa couplings at EW scale too large!" 
    Call TerminateProgram
Else If ((Minval(Abs(mf_u_DR/mf_u)).Lt.0.1_dp)&
&.Or.(Minval(Abs(mf_u_DR/mf_u)).Gt.10._dp)) Then
Iname=Iname-1
kont=-407
Call AddError(407)
    Write(*,*) " Loop corrections to Yukawa couplings at EW scale too large!" 
    Call TerminateProgram
End If
End Do! i_loop
If ((.Not.converge).and.FermionMassResummation) Then
Write (ErrCan,*)'Problem in subroutine BoundaryEW!!'
Write (ErrCan,*) "After-1 + (i_loop)iterations no convergence of Yukawas"
End If
!----------------------------------------------------------------
!Transpose Yukawas if necessary
!----------------------------------------------------------------
Yl_MZ=Y_l
Yd_MZ=Y_d
Yu_MZ=Y_u
Y_u=Transpose(Y_u)
Y_d=Transpose(Y_d)
Y_l=Transpose(Y_l)
Ye=Y_l
Yd=Y_d
Yu=Y_u
sinW2_DR_mZ=sinW2_DR
vevs_DR_Save=vevs_DR
gauge_mZ=gauge
g1 = gauge(1) 
g2 = gauge(2) 
g3 = gauge(3) 
vdMZ = vevs_DR(1) 
vuMZ = vevs_DR(2) 
YuMZ = Y_u 
YdMZ = Y_d 
YeMZ = Y_l 

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))


 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 
Call ParametersToG57(g1,g2,g3,Yd,Ye,Yu,gMZ)

test=SetRenormalizationScale(test)
Iname=Iname-1

Contains

Real(dp) Function rho_2(r)
Implicit None
Real(dp),Intent(in)::r
Real(dp)::r2,r3
r2=r*r
r3=r2*r
rho_2=19._dp-16.5_dp*r+43._dp*r2/12._dp&
&+7._dp*r3/120._dp&
&-Pi*Sqrt(r)*(4._dp-1.5_dp*r+3._dp*r2/32._dp&
&+r3/256._dp)&
&-Pi2*(2._dp-2._dp*r+0.5_dp*r2)&
&-Log(r)*(3._dp*r-0.5_dp*r2)
End Function rho_2


Subroutine Yukawas3(mf,vev,uL,uR,SigS,SigL,SigR,MassMatrix,Y,ReSum,kont)
Implicit None
Integer,Intent(inout)::kont
Real(dp),Intent(in)::mf(3),vev
Complex(dp),Dimension(3,3),Intent(in)::uL,uR,SigS,SigL,SigR
Logical,Intent(in)::ReSum
Complex(dp),Intent(inout)::MassMatrix(3,3)
Complex(dp),Intent(out)::Y(3,3)
Integer::i1
Complex(dp),Dimension(3,3)::mass,uLa,uRa,f,invf,invMass2,Ytemp
Call Adjungate(uL,uLa)
Call Adjungate(uR,uRa)
mass=ZeroC
Do i1=1,3
mass(i1,i1)=mf(i1)
End Do
mass=Matmul(Transpose(uL),Matmul(mass,uR))
If (ReSum) Then
kont=0
Call chop(MassMatrix)
invMass2=MassMatrix
Call gaussj(kont,invMass2,3,3)
If (kont.Ne.0) Return
f=id3C-Matmul(SigS,invMass2)-Transpose(SigL)-Matmul(MassMatrix,Matmul(SigR,invMass2))
invf=f
Call gaussj(kont,invf,3,3)
If (kont.Ne.0) Return
Ytemp=Matmul(invf,mass)
Else
Ytemp=mass+SigS+Matmul(Transpose(SigL),MassMatrix)+Matmul(MassMatrix,SigR)
End If
Y=sqrt2*Ytemp(1:3,1:3)/vev
Call chop(y)
End Subroutine Yukawas3

End Subroutine BoundaryEW 
 
Subroutine Sugra(delta0,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,             & 
& MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,MVWm,               & 
& MVWm2,MVZ,MVZ2,pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,ZUR,ZV,             & 
& ZW,ZZ,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,mu2,me2,M1,              & 
& M2,M3,mGut,kont,WriteComment,niter)

Implicit None
Logical,Intent(in) :: WriteComment
Integer,Intent(inout) :: kont
Integer,Intent(in) :: niter
Real(dp) :: delta0,deltaG0, gA(213), gB(57)
Real(dp) :: gC(215),  gD(215) 
Real(dp),Intent(out) :: mGUT
Complex(dp) :: Tad1Loop(4) 
Real(dp) :: comp(4), tanbQ, vev2 
Complex(dp) :: cplcChaChaUhhL(2,2,4),cplcChaChaUhhR(2,2,4),cplChiChiUhhL(4,4,4),cplChiChiUhhR(4,4,4),& 
& cplcFdFdUhhL(3,3,4),cplcFdFdUhhR(3,3,4),cplcFeFeUhhL(3,3,4),cplcFeFeUhhR(3,3,4),       & 
& cplcFuFuUhhL(3,3,4),cplcFuFuUhhR(3,3,4),cplcgWmgWmUhh(4),cplcgWpCgWpCUhh(4),           & 
& cplcgZgZUhh(4),cplUhhhhhh(4,4,4),cplUhhhhVZ(4,4),cplUhhHpmcHpm(4,2,2),cplUhhHpmcVWm(4,2),& 
& cplUhhSdcSd(4,6,6),cplUhhSecSe(4,6,6),cplUhhSucSu(4,6,6),cplUhhSvcSv(4,3,3),           & 
& cplUhhcVWmVWm(4),cplUhhVZVZ(4),cplUhhUhhhhhh(4,4,4,4),cplUhhUhhHpmcHpm(4,4,2,2),       & 
& cplUhhUhhSdcSd(4,4,6,6),cplUhhUhhSecSe(4,4,6,6),cplUhhUhhSucSu(4,4,6,6),               & 
& cplUhhUhhSvcSv(4,4,3,3),cplUhhUhhcVWmVWm(4,4),cplUhhUhhVZVZ(4,4)

Real(dp),Intent(inout) :: MCha(2),MCha2(2),MChi(4),MChi2(4),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),               & 
& MFu2(3),MGlu,MGlu2,Mhh(4),Mhh2(4),MHpm(2),MHpm2(2),MSd(6),MSd2(6),MSe(6),              & 
& MSe2(6),MSu(6),MSu2(6),MSv(3),MSv2(3),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZH(4,4),ZZ(2,2)

Complex(dp),Intent(inout) :: pG,UM(2,2),UP(2,2),ZD(6,6),ZDL(3,3),ZDR(3,3),ZE(6,6),ZEL(3,3),ZER(3,3),               & 
& ZN(4,4),ZP(2,2),ZU(6,6),ZUL(3,3),ZUR(3,3),ZV(3,3),ZW(2,2)

Real(dp),Intent(inout) :: g1,g2,g3,mHd2,mHu2

Complex(dp),Intent(inout) :: Yd(3,3),Ye(3,3),Yu(3,3),Mu,Td(3,3),Te(3,3),Tu(3,3),Bmu,mq2(3,3),ml2(3,3),             & 
& md2(3,3),mu2(3,3),me2(3,3),M1,M2,M3

Real(dp) ::mass_new(34),mass_old(34),diff_m(34)
Real(dp) :: tz,dt,q,q2,mudim,mudimNew, vev, sinW2 
Logical::FoundResult, SignMassChangedSave 
Integer::j,n_tot, i_count, i1, i2 
Iname=Iname+1
NameOfUnit(Iname)='Sugra'
kont=0
FoundResult= .False.
n_tot =1
mass_old(n_tot:n_tot+5) = MSd
n_tot = n_tot + 6 
mass_old(n_tot:n_tot+5) = MSu
n_tot = n_tot + 6 
mass_old(n_tot:n_tot+5) = MSe
n_tot = n_tot + 6 
mass_old(n_tot:n_tot+2) = MSv
n_tot = n_tot + 3 
mass_old(n_tot:n_tot+3) = Mhh
n_tot = n_tot + 4 
mass_old(n_tot:n_tot+1) = MHpm
n_tot = n_tot + 2 
mass_old(n_tot:n_tot+3) = MChi
n_tot = n_tot + 4 
mass_old(n_tot:n_tot+1) = MCha
n_tot = n_tot + 2 
mass_old(n_tot:n_tot+0) = MGlu
If (.Not.UseFixedScale) Then 
mudim=Max(mZ**2,Abs(Sqrt(Real((mq2(3,3) + (vu**2*Conjg(Yu(3,3))*Yu(3,3))/2._dp)*(mu2(3,3) + (vu**2*Conjg(Yu(3,3))*Yu(3,3))/2._dp) - ((vd*Mu*Conjg(Yu(3,3)) - vu*Conjg(Tu(3,3)))*(vd*Conjg(Mu)*Yu(3,3) - vu*Tu(3,3)))/2._dp,dp)))) 
Call SetRGEScale(mudim) 
UseFixedScale= .False. 
End If 
Write(*,*) "Calculating mass spectrum" 
CalculateOneLoopMassesSave = CalculateOneLoopMasses 
CalculateOneLoopMasses = .false. 
Do j=1,niter 
Write(*,*) "  ", j,".-iteration" 
Write(ErrCan,*) "sugra ", j,".-iteration" 
Call BoundaryEW(j,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MGlu,              & 
& MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,MVWm,MVWm2,              & 
& MVZ,MVZ2,pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,ZUR,ZV,ZW,ZZ,             & 
& vd,vu,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,mu2,me2,M1,              & 
& M2,M3,delta0,gB,kont)

If (kont.Ne.0) Then
Iname=Iname-1
    Write(*,*) " Problem with boundary conditions at EW scale" 
    Call TerminateProgram
End If
 
Call RunRGE(kont,0.1_dp*delta0,gB,gA,mGUT)
 
Call GToParameters213(gA,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,              & 
& mHu2,md2,mu2,me2,M1,M2,M3)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 
If (kont.Ne.0) Then
Iname=Iname-1
    Write(*,*) " RGE running not possible. Errorcode:", kont 
    Call TerminateProgram
End If
mudim=GetRenormalizationScale() 
Q=Sqrt(mudim) 
Q2=mudim 
tz=Log(Q/mZ)
dt=-tz/50._dp
vd=1._dp
vu=tanbeta 

 
 ! --- Boundary conditions at SUSY-scale --- 
If (HighScaleModel.ne."LOW") Then 
 eta = etaInput
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 ! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))


 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 
Call ParametersToG215(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,            & 
& md2,mu2,me2,M1,M2,M3,vd,vu,gC)

Call odeint(gC,215,tz,0._dp,0.1_dp*delta0,dt,0._dp,rge215,kont)
Call GToParameters215(gC,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,              & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 
If(SPA_Convention) Then 
  tanbetaMZ = vu/vd 
Else 
  tanbetaMZ = tanbeta 
End If 
vd = vdMZ
vu = vuMZ

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))


 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 
Call ParametersToG215(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,            & 
& md2,mu2,me2,M1,M2,M3,vd,vu,gD)

tz=Log(mZ/Q)
dt=-tz/50._dp
Call odeint(gD,215,tz,0._dp,0.1_dp*delta0,dt,0._dp,rge215,kont)
Call GToParameters215(gD,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,              & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 


If(SPA_Convention) Then 
  tanbQ = tanbeta 
Else 
  tanbQ = vu/vd 
End If 
vev2=4._dp*Real(mZ2,dp)/(g1**2+g2**2) -0._dp
vd=Sqrt(vev2/(1._dp+tanbQ**2))
vu=tanbQ*vd
vdSUSY = vd 
vuSUSY = vu 

 
 ! --- Boundary conditions at SUSY-scale --- 
If (HighScaleModel.ne."LOW") Then 
 eta = etaInput
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 ! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))

Call OneLoopMasses(MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MGlu,             & 
& MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,MVWm,MVWm2,              & 
& MVZ,MVZ2,pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,ZUR,ZV,ZW,ZZ,             & 
& vd,vu,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,mu2,me2,M1,              & 
& M2,M3,kont)

 FirstRun = .False. 
If (kont.Ne.0) Then
Iname=Iname-1
    Write(*,*) " Problem in SugraRuns. Errorcode:", kont 
    If (kont.eq.-12) Then 
      Write(*,*) "Stepsize underflow in rkqs (most likely due to a Landau pole) " 
    Else If ((kont.eq.-1).or.(kont.eq.-5).or.(kont.eq.-9)) Then 
      Write(*,*) "Stepsize smaller than minimum." 
    Else If ((kont.eq.-2).or.(kont.eq.-6).or.(kont.eq.-10)) Then 
      Write(*,*) "Running values larger 10^36." 
    Else If ((kont.eq.-3).or.(kont.eq.-7).or.(kont.eq.-11)) Then 
      Write(*,*) "Too many steps: Running has not converged." 
    Else If ((kont.eq.-4).or.(kont.eq.-8)) Then 
      Write(*,*) "No GUT scale found." 
End If
    Call TerminateProgram
End If
n_tot =1
mass_new(n_tot:n_tot+5) = MSd
n_tot = n_tot + 6 
mass_new(n_tot:n_tot+5) = MSu
n_tot = n_tot + 6 
mass_new(n_tot:n_tot+5) = MSe
n_tot = n_tot + 6 
mass_new(n_tot:n_tot+2) = MSv
n_tot = n_tot + 3 
mass_new(n_tot:n_tot+3) = Mhh
n_tot = n_tot + 4 
mass_new(n_tot:n_tot+1) = MHpm
n_tot = n_tot + 2 
mass_new(n_tot:n_tot+3) = MChi
n_tot = n_tot + 4 
mass_new(n_tot:n_tot+1) = MCha
n_tot = n_tot + 2 
mass_new(n_tot:n_tot+0) = MGlu
Where (mass_new.lt.1E-10_dp) mass_new=0._dp 
diff_m=Abs(mass_new-mass_old)
Where (Abs(mass_old).Gt.0._dp) diff_m=diff_m/Abs(mass_old)
deltag0=Maxval(diff_m)
Write(*,*) "  ... reached precision:", deltag0 
If (WriteComment) Write(*,*) "Sugra,Comparing",deltag0
If ((deltag0.Lt.delta0).And.(j.Gt.1)) Then! require at least two iterations
   FoundResult= .True.
If (SignOfMassChanged) Then
  If (.Not.IgnoreNegativeMasses) Then
  Write(*,*) " Mass spectrum converged, but negative mass squared present." 
   Call TerminateProgram 
  Else 
   SignOfMassChanged = .False. 
   kont = 0 
  End If
End If
If (SignOfMuChanged) Then
  If (.Not.IgnoreMuSignFlip) Then
  Write(*,*) " Mass spectrum converged, but negative |mu|^2 from tadpoles." 
   Call TerminateProgram 
  Else 
   SignOfMuChanged = .False. 
   kont = 0 
  End If
End If
Exit
Else
If (SignOfMassChanged) Then
  If ((j.ge.MinimalNumberIterations).And.(.Not.IgnoreNegativeMasses)) Then
  Write(*,*) " Still a negative mass squared after ",MinimalNumberIterations," iterations. Stop calculation. "  
   Call TerminateProgram 
  Else 
   SignOfMassChanged = .False. 
   kont = 0 
  End If
End If
If (SignOfMuChanged) Then
  If ((j.ge.MinimalNumberIterations).And.(.Not.IgnoreMuSignFlip)) Then
  Write(*,*) " Still a negative |mu|^2 after ",MinimalNumberIterations," iterations. Stop calculation. "  
   Call TerminateProgram 
  Else 
   SignOfMuChanged = .False. 
   kont = 0 
  End If
End If
mass_old=mass_new 
If (.Not.UseFixedScale) Then 
mudimNew=Max(mZ**2,Abs(Sqrt(Real((mq2(3,3) + (vu**2*Conjg(Yu(3,3))*Yu(3,3))/2._dp)*(mu2(3,3) + (vu**2*Conjg(Yu(3,3))*Yu(3,3))/2._dp) - ((vd*Mu*Conjg(Yu(3,3)) - vu*Conjg(Tu(3,3)))*(vd*Conjg(Mu)*Yu(3,3) - vu*Tu(3,3)))/2._dp,dp)))) 
If (HighScaleModel.eq."LOW") GUT_Scale = sqrt(mudimNew) 
 UseFixedScale= .False. 
End If 
If (j.lt.niter) Then 
Call GToParameters215(gC,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,              & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu)

vd = vdMZ 
vu = vuMZ 

 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 

 
 ! --- Boundary conditions at EW-scale when running down --- 
! ----------------------- 
 

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))

If (IgnoreNegativeMassesMZ) Then 
  SignMassChangedSave = SignOfMassChanged 
End if 
Call TreeMasses(MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MGlu,MGlu2,          & 
& Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,MVWm,MVWm2,MVZ,MVZ2,           & 
& pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,ZUR,ZV,ZW,ZZ,vd,vu,g1,             & 
& g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,mu2,me2,M1,M2,M3,GenerationMixing,kont)

If (IgnoreNegativeMassesMZ) Then 
  SignOfMassChanged = SignMassChangedSave  
End if 
If (.Not.UseFixedScale) Then 
Call SetRGEScale(mudimNew) 
UseFixedScale= .False. 
End If 
Else
  FoundIterativeSolution = .False. 
End if
End If
End Do
If (CalculateOneLoopMassesSave) Then 
CalculateOneLoopMasses =  CalculateOneLoopMassesSave 
Write(*,*) "Calculate loop corrected masses " 
Call OneLoopMasses(MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MGlu,             & 
& MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,MVWm,MVWm2,              & 
& MVZ,MVZ2,pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,ZUR,ZV,ZW,ZZ,             & 
& vd,vu,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,mu2,me2,M1,              & 
& M2,M3,kont)

If (SignOfMassChanged) Then
  If (.Not.IgnoreNegativeMasses) Then
  Write(*,*) " Mass spectrum converged, but negative mass squared present." 
   Call TerminateProgram 
  Else 
   SignOfMassChanged = .False. 
   kont = 0 
  End If
End If
If (SignOfMuChanged) Then
  If (.Not.IgnoreMuSignFlip) Then
  Write(*,*) " Mass spectrum converged, but negative |mu|^2 from tadpoles." 
   Call TerminateProgram 
  Else 
   SignOfMuChanged = .False. 
   kont = 0 
  End If
End If
End if 
Iname=Iname-1
 
End Subroutine Sugra
 
Subroutine RunRGE(kont, delta0, g1A, g1C, mGUT)
Implicit None
Integer,Intent(inout)::kont
Real(dp),Intent(in)::delta0
Integer :: i1, i2, i3, i4 
Real(dp),Intent(inout)::g1A(57)
Real(dp),Intent(out)::g1C(213),mGUT
Real(dp)::tz,dt,t_out 
Real(dp)::mudim,gGUT,gA_h(57),g1b(57),m_hi,m_lo
Real(dp)::g1S(57)
Logical :: FoundUnification, unified, greater 

Iname=Iname+1
NameOfUnit(Iname)='runRGE'

If (.Not.UseFixedGUTScale) Then
m_lo=5.e12_dp
Else 
m_lo=Min(5.e12_dp,GUT_Scale)
End If 
mudim=GetRenormalizationScale()
mudim=Max(mudim,mZ2)
tz=0.5_dp*Log(mZ2/mudim)
dt=tz/100._dp
Call odeint(g1A,57,tz,0._dp,delta0,dt,0._dp,rge57,kont)

If (kont.Ne.0) Then 
  Iname=Iname-1 
  Write(*,*) " Problem with RGE running. Errorcode:", kont 
  Call TerminateProgram 
End If 
Call BoundarySUSY(g1a,g1B)

FoundUnification= .False. 
tz=Log(sqrt(mudim)/m_lo)
dt=-tz/50._dp

Call odeint(g1B,57,tz,0._dp,delta0,dt,0._dp,rge57,kont)

If (kont.Ne.0) Then 
Iname=Iname-1 
  Write(*,*) " Problem with RGE running. Errorcode:", kont 
Call TerminateProgram 
End If 
If (.Not.UseFixedGUTScale) Then
tz=Log(m_lo/1.e20_dp)
dt=-tz/50._dp

Call odeintB2(g1B,57,tz,0._dp,delta0,dt,0._dp,rge57,checkGUT57,t_out,kont)

If (kont.Eq.0) Then
FoundUnification= .True. 
mGUT=1.e20_dp*Exp(t_out)
gGUT=Sqrt(0.5_dp*(g1b(1)**2+g1b(2)**2))
If (StrictUnification) g1b(3)=gGUT
Else
Write(ErrCan,*) "kont",kont,delta0,tz,dt
Write (ErrCan,*) "t_out",t_out,1.e20_dp*Exp(t_out)
Write(ErrCan,*) " "
Iname=Iname-1
Return
End If
Else
  tz=Log(m_lo/GUT_scale)
  mGUT=GUT_scale
  dt=-tz/50._dp
Call odeint(g1B,57,tz,0._dp,delta0,dt,0._dp,rge57,kont)

If (kont.Ne.0) Then 
Iname=Iname-1 
  Write(*,*) " Problem with RGE running. Errorcode:", kont 
Call TerminateProgram 
End If 
End If

mGUT_Save=mGUT
Call BoundaryHS(g1B,g1c)

mudim=GetRenormalizationScale()
mudim=Max(mudim,mZ2)
tz=0.5_dp*Log(mudim/mGUT_save**2)
dt=tz/100._dp
Call odeint(g1c,213,0._dp,tz,delta0,dt,0._dp,rge213,kont)
Iname=Iname-1
Contains 

Function InverseMatrix(M) Result(Inv) 
Implicit None
Complex(dp), Intent(in) :: M(:,:) 
Complex(dp) :: Inv(Size(M,1),Size(M,2)) 

inv = M 
Call gaussj(kont,inv,Size(M,1),Size(M,2)) 
End Function InverseMatrix 

End Subroutine RunRGE

Subroutine checkGUT57(y,eps,unified,greater)  
Implicit None 
Real(dp), Intent(in) :: y(57), eps 
Logical, Intent(out) :: unified, greater 
Call GToParameters57(y,g1,g2,g3,Yd,Ye,Yu)

If ((((g1)-(g2)).Gt.0._dp).And.(((g1)-(g2)).Lt.eps)) Then 
  unified = .True. 
Else 
   unified = .False. 
End If 
If ((g1).Gt.(g2)) Then 
  greater = .True. 
Else 
  greater = .False. 
End If 
End Subroutine checkGUT57
 
Subroutine FirstGuess(MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,               & 
& MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,MVWm,               & 
& MVWm2,MVZ,MVZ2,pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,ZUR,ZV,             & 
& ZW,ZZ,vd,vu,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,mu2,               & 
& me2,M1,M2,M3,kont)

Implicit None 
Real(dp),Intent(out) :: g1,g2,g3,mHd2,mHu2

Complex(dp),Intent(out) :: Yd(3,3),Ye(3,3),Yu(3,3),Mu,Td(3,3),Te(3,3),Tu(3,3),Bmu,mq2(3,3),ml2(3,3),             & 
& md2(3,3),mu2(3,3),me2(3,3),M1,M2,M3

Real(dp),Intent(out) :: MCha(2),MCha2(2),MChi(4),MChi2(4),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),               & 
& MFu2(3),MGlu,MGlu2,Mhh(4),Mhh2(4),MHpm(2),MHpm2(2),MSd(6),MSd2(6),MSe(6),              & 
& MSe2(6),MSu(6),MSu2(6),MSv(3),MSv2(3),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZH(4,4),ZZ(2,2)

Complex(dp),Intent(out) :: pG,UM(2,2),UP(2,2),ZD(6,6),ZDL(3,3),ZDR(3,3),ZE(6,6),ZEL(3,3),ZER(3,3),               & 
& ZN(4,4),ZP(2,2),ZU(6,6),ZUL(3,3),ZUR(3,3),ZV(3,3),ZW(2,2)

Integer,Intent(inout)::kont
Integer :: i1, i2
Real(dp),Intent(inout) :: vd,vu

Real(dp):: gauge(3),vev,vevs(2),mgut,mudim,mudimNew,sigma(2),mt,mb,cosW,cosW2,sinW2 
Complex(dp):: Y_l(3,3), Y_d(3,3), Y_u(3,3) 
Real(dp) :: k_fac 
Real(dp), Parameter :: oo2pi=1._dp/(2._dp*pi),oo6pi=oo2pi/3._dp 
Real(dp):: gA(57), gB(213), Scale_Save 
Logical::TwoLoopRGE_save, UseFixedScale_save 
Iname=Iname+1 
NameOfUnit(Iname)="FirstGuess" 
If (HighScaleModel.eq."LOW") UseFixedGUTScale = .true. 

If (tanbeta.gt.3._dp) Then 
 tanb = tanbeta 
 tanbetaMZ = tanbeta 
Else 
 tanb = 5._dp 
 tanbetaMZ = 5._dp 
End if 
mW2=mZ2*(0.5_dp+Sqrt(0.25_dp-Alpha_Mz*pi/(sqrt2*G_F*mZ2)))
mW=Sqrt(mW2) 
cosW2=mw2/mZ2 
sinW2=1._dp-cosW2 
cosW=Sqrt(cosW2) 
 
If (tanbeta.gt.5._dp) Then 
 k_fac=1._dp-alpha*(oo6pi & 
  &-oo2pi*(57._dp*Log(10._dp)+16._dp*Log(mf_u(3)/mZ))/9._dp) 
Else 
 k_fac=1._dp 
End if 
gauge(1)=Sqrt(20._dp*pi*alpha_mZ/(k_fac*3._dp*(1._dp-sinW2))) 
gauge(2)=Sqrt(4._dp*pi*alpha_mZ/(k_fac*sinW2)) 
If (tanbeta.gt.5._dp) Then 
 k_fac=1-AlphaS_mZ*oo2pi*(0.5_dp-4._dp*Log(10._dp) &
  &-2._dp*Log(mf_u(3)/mZ)/3._dp) 
Else 
 k_fac=1._dp 
End if 
gauge(3)=Sqrt(4._dp*pi*alphas_mZ) 
gauge(3)=Sqrt(4._dp*pi*alphas_mZ/k_fac) 
 
vev=2._dp*mW/gauge(2) 
vevs(1)=vev/Sqrt(1._dp+tanb**2) 
vevs(2)=tanb*vevs(1) 
vevSM=vev 
vd = vevs(1) 
vu = vevs(2) 
Y_l=0._dp 
Y_d=0._dp 
Y_u=0._dp 
Do i1=1,3 
  y_l(i1,i1)=sqrt2*mf_L_mZ(i1)/vevS(1) 
  If (i1.Eq.3) Then! top and bottom are special: 
  ! TanBeta Aufsummierung fehlt bei Yd!! 
  y_u(i1,i1)=sqrt2*mf_U(i1)/vevS(2)& 
    &*(1._dp-oo3pi*alphas_mZ*(5._dp+3._dp*Log(mZ2/mf_u2(3)))) 
  y_d(i1,i1)=sqrt2*mf_D_mZ(i1)/(vevS(1) * (1._dp + 0.015*tanb)) 
Else 
  y_u(i1,i1)=sqrt2*mf_U_mZ(i1)/vevS(2) 
  y_d(i1,i1)=sqrt2*mf_D_mZ(i1)/vevS(1) 
End If  
End Do 
If (GenerationMixing) Then 
  If (YukawaScheme.Eq.1) Then 
    Y_u=Matmul(Transpose(CKM),Y_u) 
    Y_u=Transpose(Y_u) 
  Else 
    Y_d=Matmul(Conjg(CKM),Y_d) 
    Y_d=Transpose(Y_d) 
  End If 
End If 
TwoLoopRGE_save=TwoLoopRGE 
UseFixedScale_save = UseFixedScale 
Scale_save=GetRenormalizationScale() 
UseFixedScale = .True. 
Call SetRGEScale(mZ2) 
g1 = gauge(1)*Sqrt(3._dp/5._dp) 
g2 = gauge(2) 
g3 = gauge(3) 
Yu = Y_u 
Yd = Y_d 
Ye = Y_l 
 
 ! 1. Run 
 

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 

 
 ! --- Boundary conditions at SUSY-scale --- 
If (HighScaleModel.ne."LOW") Then 
 eta = etaInput
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 ! ----------------------- 
 
Mu = 0._dp 
Bmu = 0._dp 
Bmu = 0._dp 

 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 


Call ParametersToG57(g1,g2,g3,Yd,Ye,Yu,gA)

If (.Not.UseFixedScale) Then 
  If (HighScaleModel.eq."LOW") Then 
     mudim=1000._dp**2 
   Else 
    mudim= Real(m0**2 + 4*m12**2, dp) 
    mudim=Max(mf_u2(3),mudim) 
  End if  
   Call SetRGEScale(mudim) 
   UseFixedScale= .False. 
Else 
  mudim=GetRenormalizationScale() 
End If 
If (HighScaleModel.eq."LOW") GUT_Scale = sqrt(mudim) 
 TwoLoopRGE= .False. 
kont=0 
Call RunRGE(kont,0.001_dp,gA,gB,mGUT) 
If (kont.Ne.0) Then 
Iname=Iname-1 
    Write(*,*) " Problem with RGE running. Errorcode:", kont 
    Call TerminateProgram
End if 
Call GToParameters213(gB,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,              & 
& mHu2,md2,mu2,me2,M1,M2,M3)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 



 
 ! --- Boundary conditions at EW-scale when running down --- 
! ----------------------- 
 
g1 = gauge(1)*Sqrt(3._dp/5._dp) 
g2 = gauge(2) 
g3 = gauge(3) 
Yu = Y_u 
Yd = Y_d 
Ye = Y_l 
 
 ! 2. Run 
 

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 

 
 ! --- Boundary conditions at SUSY-scale --- 
If (HighScaleModel.ne."LOW") Then 
 eta = etaInput
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 ! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))


 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 


Call ParametersToG57(g1,g2,g3,Yd,Ye,Yu,gA)

If (.Not.UseFixedScale) Then 
  If (HighScaleModel.eq."LOW") Then 
     mudim=1000._dp**2 
   Else 
    mudim= Real(m0**2 + 4*m12**2, dp) 
    mudim=Max(mf_u2(3),mudim) 
  End if  
   Call SetRGEScale(mudim) 
   UseFixedScale= .False. 
Else 
  mudim=GetRenormalizationScale() 
End If 
If (HighScaleModel.eq."LOW") GUT_Scale = sqrt(mudim) 
 TwoLoopRGE= .False. 
kont=0 
Call RunRGE(kont,0.001_dp,gA,gB,mGUT) 
If (kont.Ne.0) Then 
Iname=Iname-1 
    Write(*,*) " Problem with RGE running. Errorcode:", kont 
    Call TerminateProgram
End if 
Call GToParameters213(gB,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,              & 
& mHu2,md2,mu2,me2,M1,M2,M3)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 



 
 ! --- Boundary conditions at EW-scale when running down --- 
! ----------------------- 
 
g1 = gauge(1)*Sqrt(3._dp/5._dp) 
g2 = gauge(2) 
g3 = gauge(3) 
Yu = Y_u 
Yd = Y_d 
Ye = Y_l 
 
 ! 3. Run 
 

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 

 
 ! --- Boundary conditions at SUSY-scale --- 
If (HighScaleModel.ne."LOW") Then 
 eta = etaInput
else If (HighScaleModel.Eq."LOW") Then 
 ! Setting values 
 Mu = MuIN 
 Td = TdIN 
 Te = TeIN 
 Tu = TuIN 
 Bmu = BmuIN 
 mq2 = mq2IN 
 ml2 = ml2IN 
 mHd2 = mHd2IN 
 mHu2 = mHu2IN 
 md2 = md2IN 
 mu2 = mu2IN 
 me2 = me2IN 
 M1 = M1IN 
 M2 = M2IN 
 M3 = M3IN 
 eta = etaInput
vd = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2)))/Sqrt(1 + TanBeta**2)
vu = (sqrt(2._dp)*Sqrt(mz2/(g1**2 + g2**2))*TanBeta)/Sqrt(1 + TanBeta**2)
End if
 
 ! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))


 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 


Call ParametersToG57(g1,g2,g3,Yd,Ye,Yu,gA)

Call SetRGEScale(Scale_save) 
UseFixedScale = UseFixedScale_save 
If (.Not.UseFixedScale) Then 
  If (HighScaleModel.eq."LOW") Then 
     mudim=1000._dp**2 
   Else 
    mudim= Real(m0**2 + 4*m12**2, dp) 
    mudim=Max(mf_u2(3),mudim) 
  End if  
   Call SetRGEScale(mudim) 
   UseFixedScale= .False. 
Else 
  mudim=GetRenormalizationScale() 
End If 
If (HighScaleModel.eq."LOW") GUT_Scale = sqrt(mudim) 
 TwoLoopRGE= .False. 
kont=0 
Call RunRGE(kont,0.001_dp,gA,gB,mGUT) 
If (kont.Ne.0) Then 
Iname=Iname-1 
    Write(*,*) " Problem with RGE running. Errorcode:", kont 
    Call TerminateProgram
End if 
Call GToParameters213(gB,g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,              & 
& mHu2,md2,mu2,me2,M1,M2,M3)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 



 
 ! --- Boundary conditions at EW-scale when running down --- 
! ----------------------- 
 
TwoLoopRGE = TwoLoopRGE_save 
MuMZ = Mu 
TdMZ = Td 
TeMZ = Te 
TuMZ = Tu 
BmuMZ = Bmu 
mq2MZ = mq2 
ml2MZ = ml2 
mHd2MZ = mHd2 
mHu2MZ = mHu2 
md2MZ = md2 
mu2MZ = mu2 
me2MZ = me2 
M1MZ = M1 
M2MZ = M2 
M3MZ = M3 
vd = vevs(1) 
vu = vevs(2) 

 
 ! --- Boundary conditions at EW-scale --- 
! ----------------------- 
 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,            & 
& mHu2,md2,mu2,me2,M1,M2,M3,vd,vu,(/ ZeroC, ZeroC, ZeroC, ZeroC /))

Call TreeMasses(MCha,MCha2,MChi,MChi2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MGlu,MGlu2,          & 
& Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSe,MSe2,MSu,MSu2,MSv,MSv2,MVWm,MVWm2,MVZ,MVZ2,           & 
& pG,TW,UM,UP,v,ZD,ZDL,ZDR,ZE,ZEL,ZER,ZH,ZN,ZP,ZU,ZUL,ZUR,ZV,ZW,ZZ,vd,vu,g1,             & 
& g2,g3,Yd,Ye,Yu,Mu,Td,Te,Tu,Bmu,mq2,ml2,mHd2,mHu2,md2,mu2,me2,M1,M2,M3,GenerationMixing,kont)

tanb = tanbeta 
tanbetaMZ = tanbeta 
MVWm = mW 
MVWm2 = mW2 
MVZ = mZ 
MVZ2 = mZ2 
MFe(1:3) = mf_l 
MFe2(1:3) = mf_l**2 
MFu(1:3) = mf_u 
MFu2(1:3) = mf_u**2 
MFd(1:3) = mf_d 
MFd2(1:3) = mf_d**2 
Iname=Iname-1 
End subroutine FirstGuess 
End Module SugraRuns_MSSMCPV 

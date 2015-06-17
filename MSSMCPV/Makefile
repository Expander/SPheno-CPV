modname = MSSMCPV  # as an example  
PreDef = -DGENERATIONMIXING -DONLYDOUBLE
# setting various paths  
InDir = ../include
Mdir = ${InDir}
name = ../lib/libSPhenoMSSMCPV.a
 
# check if SARAH module and SPheno are compatibel  
minV=330.00 
cVersion =$(shell expr $(version) \>= $(minV))
#  
# options for various compilers  
#  
# Default Compiler  
F90=gfortran
comp= -c -O -module ${Mdir} -I${InDir}  
LFlagsB= -O  
# Intels ifort,debug modus  
ifeq (${F90},ifortg)  
F90=ifort  
comp= -c -g -module ${Mdir} -I${InDir}  
LFlagsB= -g  
endif  
# gfortran  
ifneq (,$(findstring gfortran,${F90}))
comp= -c -g -ffree-line-length-none -J${Mdir} -I${InDir}  
LFlagsB= -g  
endif  
# g95  
ifeq (${F90},g95)  
comp= -c -O -fmod=${Mdir} -I${InDir}  
LFlagsB= -O  
endif  
# Lahey F95 compiler  
ifeq (${F90},lf95)  
comp=-c -O -M ${Mdir} -I${InDir}  
LFlagsB=-O  
endif  
# NAG f95/2003  
ifeq (${F90},nagfor)  
comp= -c -O -mdir ${Mdir} -I${InDir}  
LFlagsB= -O -DONLYDOUBLE -mdir ${MDir} -I${InDir}  
endif   
.SUFFIXES : .o .ps .f90 .F90 .a 
bin/SPhenoMSSMCPV:
ifeq (${cVersion},1)
	 cd ../src ; ${MAKE} F90=${F90} 
	 ${MAKE} F90=${F90} ${name} 
	 ${MAKE} F90=${F90} SPhenoMSSMCPV.o 
	 ${F90} -o SPhenoMSSMCPV ${LFlagsB} SPhenoMSSMCPV.o ../lib/libSPhenoMSSMCPV.a ../lib/libSPheno.a
	 mv SPhenoMSSMCPV ../bin
	 rm SPhenoMSSMCPV.o  
${name}:  ${name}(Model_Data_MSSMCPV.o)  \
 ${name}(RGEs_MSSMCPV.o)   \
 ${name}(Couplings_MSSMCPV.o) ${name}(SusyMasses_MSSMCPV.o) ${name}(TadpoleEquations_MSSMCPV.o) \
 ${name}(LoopCouplings_MSSMCPV.o) ${name}(CouplingsForDecays_MSSMCPV.o) \
 ${name}(SusyDecays_MSSMCPV.o) \
 ${name}(Glu_MSSMCPV.o)  ${name}(Chi_MSSMCPV.o)  ${name}(Cha_MSSMCPV.o)  ${name}(Sd_MSSMCPV.o)  ${name}(Su_MSSMCPV.o)  ${name}(Se_MSSMCPV.o)  ${name}(Sv_MSSMCPV.o)  \
 ${name}(EffPotFunctions.o) ${name}(DerivativesEffPotFunctions.o) ${name}(EffectivePotential_MSSMCPV.o) \
 ${name}(2LPoleFunctions.o) ${name}(2LPole_MSSMCPV.o) \
 ${name}(BranchingRatios_MSSMCPV.o) ${name}(LoopMasses_MSSMCPV.o) \
 ${name}(HiggsCS_MSSMCPV.o) ${name}(RunSM_MSSMCPV.o) \
 ${name}(LowEnergy_MSSMCPV.o) \
${name}(FlavorKit_LFV_MSSMCPV.o) ${name}(FlavorKit_QFV_MSSMCPV.o) ${name}(FlavorKit_Observables_MSSMCPV.o)\
 ${name}(SugraRuns_MSSMCPV.o)  ${name}(InputOutput_MSSMCPV.o) 
else 
	 @echo -------------------------------------------------------------------  
	 @echo ERROR:  
	 @echo The installed SPheno is version not compatibel with this module 
	 @echo Please, upgrade at least to SPheno version 3.3.0.  
	 @echo The current SPheno version can be downloaded from 
	 @echo http://www.hepforge.org/downloads/spheno 
	 @echo ------------------------------------------------------------------- 
endif 
clean: 
	 rm -f *.o *~ */*.o */*~
cleanall: 
	 rm -f bin/SPheno3 lib/*.a *~ */*.o */*~ include/*
#
# Suffix rules
#
.f90.a:
	 ${F90} ${comp} $< 
	 ar -ruc $@ $*.o
	 rm -f $*.o
.F90.a:
	 ${F90} ${comp} ${PreDef} $< 
	 ar -ruc $@ $*.o
	 rm -f $*.o
.f90.o:
	 ${F90} ${comp} $<
.f90.ps:
	 a2ps -o $*.ps $<
.h.ps:
	 a2ps -o $*.ps $<

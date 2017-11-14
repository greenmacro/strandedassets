t=t(-1)+1
initLowCarb=(t==(n+1))
firstPeriodLowcarb=(t==(n+2))
lowcarbExisting=(t>(n+1))
beforeIPO=(t<=(n+entry)&(t>(n+1)))
exith=ifelse(exith(-1)==1,1,ifelse(yh==0|Fh<0|phe<=0,1,0))
hasExited=(exith(-1)==1)
yc=cf+cw
yl=invltot
yh=invhtot
Yc=yc*pc
Yl=yl*pkl
Yh=yh*pkh
yce = ycmean + ycmean*ycgr*eta+epsilon * (yc(-1) - yce(-1))
yle = ifelse(lowcarbExisting, ylmean + ylmean*ylgr*eta + epsilon * (yl(-1) - yle(-1)), 0)
yhe = ifelse(hasExited, 0, yhmean + yhmean*yhgr*eta + epsilon * (yh(-1) - yhe(-1)))
ycgr = ifelse(yc(-2) == 0, 0, (yc(-1) - yc(-2))/yc(-2))
ylgr = ifelse(yl(-2) == 0, 0, (yl(-1) - yl(-2))/yl(-2))
yhgr = ifelse(yh(-2) == 0, 0, (yh(-1) - yh(-2))/yh(-2))
ycmean = mean(c(yc(-4), yc(-3), yc(-2), yc(-1)))
ylmean = mean(c(yl(-4), yl(-3), yl(-2), yl(-1)))
yhmean = mean(c(yh(-4), yh(-3), yh(-2), yh(-1)))
khc=khc(-1)+invch-depkhc
khl=khl(-1)+invlh-depkhl
khh=ifelse(hasExited,0,khh(-1)+invhh-depkhh)
klc=klc(-1)+invcl-depklc
kll=kll(-1)+invll-depkll
khcgr=ifelse(khc(-1)==0,0,(khc-khc(-1))/khc(-1))
khhgr=ifelse(khh(-1)==0,0,(khh-khh(-1))/khh(-1))
khlgr=ifelse(khl(-1)==0,0,(khl-khl(-1))/khl(-1))
klcgr=ifelse(klc(-1)==0,0,(klc-klc(-1))/klc(-1))
kllgr=ifelse(kll(-1)==0,0,(kll-kll(-1))/kll(-1))
totkcgr=ifelse(khc(-1)+klc(-1)==0,0,(khc+klc-khc(-1)-klc(-1))/(khc(-1)+klc(-1)))
totkhgr=khhgr
totklgr=ifelse(khl(-1)+kll(-1)==0,0,(khl+kll-khl(-1)-kll(-1))/(khl(-1)+kll(-1)))
totkgr=ifelse(khh(-1)+khl(-1)+kll(-1)==0,0,(khh+khl+kll-khh(-1)-khl(-1)-kll(-1))/(khh(-1)+khl(-1)+kll(-1)))
totkhhllgr = ifelse(khh(-1) + kll(-1) == 0, 0, (khh + kll - khh(-1) - kll(-1))/(khh(-1) + kll(-1)))
uc=yc/(khc(-1)*prkh +klc(-1)*prkl)
ul=ifelse(lowcarbExisting,yl/(khl(-1)*prkh+kll(-1)*prkl),0)
uh=ifelse(hasExited,0,yh/(khh(-1)*prkh))
uce=yce/(khc(-1)*prkh+klc(-1)*prkl)
ule=ifelse(lowcarbExisting,(yle/(kll(-1)*prkl+khl(-1)*prkh)),0)
uhe=ifelse(hasExited,0,yhe/(khh(-1)*prkh))
ulc=ifelse(klc(-1)==0, 0, max(0,min(1,yc/(prkl*klc(-1)))))
ull=ifelse(lowcarbExisting&kll(-1)!=0, max(0,min(1,yl/(prkl*kll(-1)))),0)
ulce=ifelse(klc(-1)==0, 0,min(yce/(prkl*klc(-1)),1))
ulle=ifelse(lowcarbExisting&kll(-1)!=0,min(yle/(prkl*kll(-1)),1),0)
uhc=ifelse(khc(-1)>0,max(0,(yc-ulc*prkl*klc(-1))/(prkh*khc(-1))),0)
uhl=ifelse(lowcarbExisting&khl(-1)!=0,max(0,(yl-ull*prkl*kll(-1))/(prkh*khl(-1))),0)
uhh=ifelse(hasExited,0,yh/(prkh*khh(-1)))
uhce=ifelse(khc(-1)>0,min((yce-ulce*prkl*klc(-1))/(prkh*khc(-1)),1),0)
uhle=ifelse(lowcarbExisting&khl(-1)!=0,max(0,min((yle-ulle*prkl*kll(-1))/(prkh*khl(-1)),1),0),0)
uhhe=ifelse(hasExited,0,yhe/(prkh*khh(-1)))
Nc=ulc*klc(-1)/caplabratiolc + uhc*khc(-1)/caplabratiohc
Nl=ifelse(lowcarbExisting,ull*kll(-1)/caplabratioll+uhl*khl(-1)/caplabratiohl,0)
Nh=ifelse(hasExited,0,uhh*khh(-1)/caplabratiohh)
Ncmax=klc(-1)/caplabratiolc + khc(-1)/caplabratiohc
Ntot=Nc+Nh+Nl
Aprc=yc/Nc
Aprl=ifelse(Nl>0,yl/Nl,0.001)
Aprh=ifelse(!hasExited&Nh>0,yh/Nh,0)
pc=pc(-1)+zeta*((1+upsilonc)*NUCc-pc(-1))
pkl=ifelse(initLowCarb,(1+upsilonl)*NUCl,pkl(-1)+zeta*((1+upsilonl)*NUCl-pkl(-1)))
pkh=ifelse(hasExited,pkh(-1),pkh(-1)+zeta*((1+upsilonh)*NUCh-pkh(-1)))
UCc=Wc*(ulc*klc(-1)*caplabratiohc+uhc*khc(-1)*caplabratiolc)/((uhc*prkh*khc(-1) +ulc*prkl*klc(-1))*caplabratiolc*caplabratiohc)
UCl=ifelse(lowcarbExisting,Wl*(ull*kll(-1)*caplabratiohl+uhl*khl(-1)*caplabratioll)/((uhl*prkh*khl(-1)+ull*prkl*kll(-1))*caplabratioll*caplabratiohl),0)
UCh=ifelse(!hasExited&invhtot>0,Wh/(prkh*caplabratiohh),0)
UChe=ifelse(hasExited,0,Wh(-1)/(prkh*caplabratiohh))
UCle=ifelse(initLowCarb|firstPeriodLowcarb,Wl/(prkh*caplabratiohl),ifelse(khl(-1)==0&kll(-1)==0,0,Wl(-1)*(ulle*kll(-1)*caplabratiohl+uhle*khl(-1)*caplabratioll)/((uhle*prkh*khl(-1)+ulle*prkl*kll(-1))*caplabratioll*caplabratiohl)))
UCce=Wc(-1)*(ulce*klc(-1)*caplabratiohc+uhce*khc(-1)*caplabratiolc)/((uhce*prkh*khc(-1)+ulce*prkl*klc(-1))*caplabratiolc*caplabratiohc)
NUCh=ifelse(hasExited,0,NUCh(-1)+zeta*(UChe-NUCh(-1)))
NUCl=ifelse(initLowCarb|firstPeriodLowcarb,UCle,NUCl(-1)+zeta*(UCle-NUCl(-1)))
NUCc=NUCc(-1)+zeta*(UCce-NUCc(-1))
upsilonl=ifelse(firstPeriodLowcarb,rlT*pkh/(NUCl*prkh),ifelse(NUCl==0|yle==0,0,rlT*(pkh(-1)*khl(-1)+pkl(-1)*kll(-1))/(NUCl*yle)))
upsilonc=rcT*(pkh(-1)*khc(-1) +pkl(-1)*klc(-1))/(NUCc*yce)
upsilonh=ifelse(hasExited,0,rhT*(pkh(-1)*khh(-1))/(UChe*yhe))
pic=(pc-pc(-1))/pc(-1)
pikl=ifelse(pkl(-1)==0,0,(pkl-pkl(-1))/pkl(-1))
pikh=ifelse(hasExited,0,(pkh-pkh(-1))/pkh(-1))
pickhl=(khc(-1)*pikh + klc(-1)*pikl)/(khc(-1)+ klc(-1))
Ms=Mw+Mf+Mc+Mh+Ml
el=ifelse(t<(n+entry), 0, 100)
Vw=Vw(-1)+(YDw-Cw)
Vf=Vf(-1)+(YDf-Cf)+CG-bailout
vf=Vf/pc
vw=Vw/pc
Mw=Vw
Vfe=Vf(-1) + YDfe - Cf
Mh=ifelse(exith(-1)!=1&invhtot>0,Mh(-1)+Fh-REh-FDh,0)
Ml=Ml(-1)+Fl-REl-FDl
Mc=Mc(-1)+Fc-REc-FDc
CGc=ec*(pce-pce(-1))
CGh=ifelse(yh!=0,eh*(phe-phe(-1)),0)
CGl=ifelse(el(-1)!=0&ple(-1)!=0,el(-1)*(ple-ple(-1)),0)
CG = ifelse(el(-1)==0&el>0,CGc+CGh,CGc+CGh+CGl)
cg=CG/pc
cgc=CGc/(pce(-1)*ec)
cgh=ifelse(yh!=0&Fh>=0,CGh/(phe(-1)*eh),0)
cgl=ifelse(el(-1)!=0&ple(-1)!=0,CGl/(ple(-1)*el(-1)),0)
CGce= CGcmean+CGcmean*CGcgr*eta+iota*(CGc(-1)-CGce(-1))
CGhe=ifelse(hasExited,0,CGhmean+CGhmean*CGhgr*eta+iota*(CGh(-1)-CGhe(-1)))
CGle =CGlmean+CGlmean*CGlgr*eta+iota*(CGl(-1)-CGle(-1))
cgce = CGce/(ec*pce(-1))
cghe=ifelse(hasExited,0,CGhe/(eh*phe(-1)))
cgle=ifelse(el(-1)==0|ple(-1)==0,0,(CGle/(el(-1)*ple(-1))))
CGcgr=ifelse(CGc(-2)==0,0,(CGc(-1)-CGc(-2))/CGc(-2))
CGlgr=ifelse(CGl(-2)==0,0,(CGl(-1)-CGl(-2))/CGl(-2))
CGhgr=ifelse(CGh(-2)==0,0,(CGh(-1)-CGh(-2))/CGh(-2))
CGcmean=mean(c(CGc(-4),CGc(-3),CGc(-2),CGc(-1)))
CGlmean=mean(c(CGl(-4),CGl(-3),CGl(-2),CGl(-1)))
CGhmean=mean(c(CGh(-4),CGh(-3),CGh(-2),CGh(-1)))
qc= (ec*pce)/(pkh*khc +pkl*klc-Loansc)
qh=ifelse(yh!=0&Fh>=0,(eh*phe)/(pkh*khh-Loansh),0)
ql=ifelse((khl==0&kll==0)|el==0,1,(el*ple)/(pkh*khl +pkl*kll-Loansl))
qcprev=(qc(-4)+qc(-3)+qc(-2)+qc(-1))/4
qhprev=(qh(-4)+qh(-3)+qh(-2)+qh(-1))/4
qlprev=(ql(-4)+ql(-3)+ql(-2)+ql(-1))/4
YDw=Wc*Nc+Wh*Nh+Wl*Nl
ydw=YDw/pc-pic*Vw(-1)/pc
YDwe= YDwmean+YDwmean*YDwgr*eta+epsilon*(YDw(-1)-YDwe(-1))
ydwe= YDwe/pc-(pic*Vw(-1))/pc
YDwgr=ifelse(YDw(-2)==0,0,(YDw(-1)-YDw(-2))/YDw(-2))
YDwmean=mean(c(YDw(-4),YDw(-3),YDw(-2),YDw(-1)))
YDf=FDc+FDh+FDb+FDl
YDfe1=FDce+FDhe+FDle+FDbe
YDfe=(YDfe1+abs(YDfe1))/2
ydf=YDf/pc - pic*Vf(-1)/pc
ydfe=YDfe/pc-(pic*Vf(-1))/pc
omegaTc=Omega0+Omega1*log(Aprc)+ Omega2*log(Ntot/LF)
omegaTh=ifelse(!hasExited&Nh>0,Omega0+Omega1*log(Aprh)+Omega2*log(Ntot/LF),0)
omegaTl=Omega0+Omega1*log(Aprl)+Omega2*log(Ntot/LF)
Wc=Wc(-1) + Omega3*(omegaTc(-1) - Wc(-1)/pc(-1))
Wh=ifelse(hasExited,0,Wh(-1) + Omega3*(omegaTh(-1) - Wh(-1)/pc(-1)))
Wl=ifelse(lowcarbExisting,Wl(-1) + Omega3*(omegaTl(-1) - Wl(-1)/pc(-1)),Wh(-1))
Fc=Yc-Wc*Nc-intrc(-1)*Loansc(-1)
Fl=ifelse(lowcarbExisting,Yl-Wl*Nl-intrl(-1)*Loansl(-1),0)
Fh=ifelse(hasExited,0,Yh-Wh*Nh-intrh(-1)*Loansh(-1))
Fce=Fcmean+Fcmean*Fcgr*eta+nu*(Fc(-1)-Fce(-1))
Fle=Flmean+Flmean*Flgr*eta+nu*(Fl(-1)- Fle(-1))
Fhe=ifelse(hasExited,0,Fhmean+Fhmean*Fhgr*eta+nu*(Fh(-1)-Fhe(-1)))
Fcgr=ifelse(Fc(-2)==0,0,(Fc(-1)-Fc(-2))/Fc(-2))
Flgr=ifelse(Fl(-2)==0,0,(Fl(-1)-Fl(-2))/Fl(-2))
Fhgr=ifelse(Fh(-2)==0,0,(Fh(-1)-Fh(-2))/Fh(-2))
Fcmean=mean(c(Fc(-4),Fc(-3),Fc(-2),Fc(-1)))
Flmean=mean(c(Fl(-4),Fl(-3),Fl(-2),Fl(-1)))
Fhmean=mean(c(Fh(-4),Fh(-3),Fh(-2),Fh(-1)))
profratec=Fc/(khc(-1)*pkh(-1)+klc(-1)*pkl(-1))
profratel=ifelse(lowcarbExisting,Fl/(khl(-1)*pkh(-1)+kll(-1)*pkl(-1)),0)
profrateh=ifelse(hasExited,0,Fh/(khh(-1)*pkh(-1)))
expprofratec=Fce/(ec*pce(-1))
expprofratel=ifelse(t>=(n+entry),ifelse(ple(-1)==0,Fle/(khl(-1)*pkh(-1)+kll(-1)*pkl(-1)),Fle/(el(-1)*ple(-1))),0)
expprofrateh=ifelse(hasExited,0,Fhe/(eh*phe(-1)))
profrhistc=ifelse(sumkc>0,(Fc(-5)+Fc(-4)+Fc(-3)+Fc(-2)+Fc(-1))/sumkc,0)
sumkc=pkh(-5)*khc(-5)+pkl(-5)*klc(-5)+pkh(-4)*khc(-4)+pkl(-4)*klc(-4)+pkh(-3)*khc(-3)+pkl(-3)*klc(-3)+pkh(-2)*khc(-2)+pkl(-2)*klc(-2)+pkh(-1)*khc(-1)+pkl(-1)*klc(-1)
profrhistl=ifelse(sumkl>0,(Fl(-5)+Fl(-4)+Fl(-3)+Fl(-2)+Fl(-1))/sumkl,0)
sumkl=pkh(-5)*khl(-5)+pkl(-5)*kll(-5)+pkh(-4)*khl(-4)+pkl(-4)*kll(-4)+pkh(-3)*khl(-3)+pkl(-3)*kll(-3)+pkh(-2)*khl(-2)+pkl(-2)*kll(-2)+pkh(-1)*khl(-1)+pkl(-1)*kll(-1)
profrhisth=ifelse(sumkh>0,(Fh(-5)+Fh(-4)+Fh(-3)+Fh(-2)+Fh(-1))/sumkh,0)
sumkh=pkh(-5)*khh(-5)+pkh(-4)*khh(-4)+pkh(-3)*khh(-3)+pkh(-2)*khh(-2)+pkh(-1)*khh(-1)
REhT=ifelse(hasExited,0,Loansh(-1)+Invh-lambdahT*khh*pkh)
RElT=ifelse(lowcarbExisting,Loansl(-1)+Invl-lambdalT*(khl*pkh+kll*pkl),0)
REcT=Loansc(-1)+Invc-lambdacT*(khc*pkh+klc*pkl)
REh=ifelse((Mh(-1)+Fh>FDhT+REhT)|(REhT<0),REhT,REhT*(Mh(-1)+Fh)/(FDhT+REhT))
REl=ifelse(lowcarbExisting,ifelse((Ml(-1)+Fl>FDcT+REcT)|(RElT<0),RElT,RElT*(Ml(-1)+Fl)/(FDlT+RElT)),0)
REc=ifelse((Mc(-1)+Fc>FDcT+REcT)|(REcT<0),REcT,REcT*(Mc(-1)+Fc)/(FDcT+REcT))
FDhT=max(0,ifelse(hasExited,0,RRhT*eh*phe(-1)-CGhe))
FDlT=max(0,ifelse(lowcarbExisting,RRlT*el*ple(-1)-CGle,0))
FDcT=max(0,RRcT*ec*pce(-1)-CGce)
FDl=ifelse(t>(n+entry),ifelse(Ml(-1)+Fl>FDlT+RElT,FDlT,ifelse(RElT<0,Ml(-1)+Fl-RElT,FDlT*(Ml(-1)+Fl)/(FDlT+RElT))),0)
FDc=ifelse(Mc(-1)+Fc>FDcT+REcT,FDcT,ifelse(REcT<0,Mc(-1)+Fc-REcT,FDcT*(Mc(-1)+Fc)/(FDcT+REcT)))
FDh=ifelse(exith(-1)!=1&invhtot>0,ifelse(Mh(-1)+Fh>FDhT+REhT,FDhT,ifelse(REhT<0,Mh(-1)+Fh-REhT,FDhT*(Mh(-1)+Fh)/(FDhT+REhT))),0)
FDb=max(0,FDbraw)
FDbraw= intrc(-1)*Loansc(-1) +ifelse(yh!=0&Fh>=0&phe>0,intrh(-1)*Loansh(-1),-(Loansh(-1)-Mh(-1))) + intrl(-1)*Loansl(-1)
bailout=FDb-FDbraw
FDbe= FDbmean+FDbmean*FDbgr*eta+chi*(FDb(-1)- FDbe(-1))
FDle= FDlmean+FDlmean*FDlgr*eta+chi*(FDl(-1)-FDle(-1))
FDce=FDcmean+FDcmean*FDcgr*eta+chi*(FDc(-1)-FDce(-1))
FDhe=ifelse(hasExited,0,FDhmean+FDhmean*FDhgr*eta+chi*(FDh(-1)-FDhe(-1)))
FDbgr=ifelse(FDb(-2)==0,0,(FDb(-1)-FDb(-2))/FDb(-2))
FDcgr=ifelse(FDc(-2)==0,0,(FDc(-1)-FDc(-2))/FDc(-2))
FDlgr=ifelse(FDl(-2)==0,0,(FDl(-1)-FDl(-2))/FDl(-2))
FDhgr=ifelse(FDh(-2)==0,0,(FDh(-1)-FDh(-2))/FDh(-2))
FDbmean=mean(c(FDb(-4),FDb(-3),FDb(-2),FDb(-1)))
FDcmean=mean(c(FDc(-4),FDc(-3),FDc(-2),FDc(-1)))
FDlmean=mean(c(FDl(-4),FDl(-3),FDl(-2),FDl(-1)))
FDhmean=mean(c(FDh(-4),FDh(-3),FDh(-2),FDh(-1)))
expdivratec=FDce/(ec*pce(-1))
expdivrateh=ifelse(hasExited,0,FDhe/(eh*phe(-1)))
expdivratel=ifelse(t>=(n+entry),ifelse(ple(-1)==0,FDle/(khl(-1)*pkh(-1)+kll(-1)*pkl(-1)),FDle/(el(-1)*ple(-1))),0)
cw1= xiw1*ydwe + xiw2*vw(-1)
cf1=max(0,xif1*ydfe+xif2*vf(-1)+xif3*cg(-1))
cs=ifelse(LF-Nl-Nh<Ncmax,(LF-Nl-Nh)/Ncmax,1)*(khc(-1)*prkh+klc(-1)*prkl)
exc=min(0,cs-cw1-cf1)
cw=cw1+cw1/(cw1+cf1)*exc
cf=cf1+cf1/(cw1+cf1)*exc
Cw=cw*pc
Cf=cf*pc
gkh=min(gkmax,ifelse(hasExited,0,eta0k+eta1*uhe-eta2*rintrh*lambdahprev+eta3*qhprev))
gkl=min(gkmax,ifelse(beforeIPO,tau,eta0k+eta1*ule-eta2*rintrl*lambdalprev+eta3*qlprev))
gc=min(gkmax,eta0c+eta1*uce-eta2*rintrc*lambdacprev+eta3*qcprev)
invyc=min(max(gc*yc(-1)+depkhc*prkh+depklc*prkl,0),khsold*prkh+klsold*prkl)
invcld = beta_inv * invyc/prkl
invchd = (1 - beta_inv) * invyc/prkh
beta_invd = 1/(beta0 + exp(beta1 * (tuclc - tuchc)/tuchc))
tuclc = Wc/(prkl * caplabratiolc) + pkl/(deltal * prkl)
tuchc = Wc/(prkh * caplabratiohc) + pkh/(deltah * prkh)
invcl = min(invcld+max(0,invchd-khsold)*prkh/prkl,klsold)
invch = min(invchd+max(0,invcld-klsold)*prkl/prkh,khsold)
beta_inv = invcl*prkl/invyc
Invc=invch*pkh+invcl*pkl
invhh=ifelse(hasExited,0,min(khh(-1)*prkh,max(0,khh(-1)*gkh/prkh + depkhh)))
khsold=ifelse(hasExited,0,khh(-1)*prkh-invhh)
invhtot=invhh+invch+invlh
Invh=ifelse(hasExited,0, invhh*pkh)
invll=min(kll(-1)*prkl+khl(-1)*prkh,max(((gkl*(kll(-1)*prkl+khl(-1)*prkh)+depkll*prkl+depkhl*prkh)/prkl),0))
invlh=ifelse(initLowCarb,rho*Yh(-1)*prkl/((1-rho)*pkl*prkh*(prkl-exp(1-n)-tau)),0)
klsold=kll(-1)*prkl+khl(-1)*prkh-invll
invltot=invcl+invll
Invl=ifelse(initLowCarb,invlh*pkh,invll*pkl)
depkhh=invhh(-1)*(-5.602796e-09+1.522998e-08)+invhh(-2)*(-1.522998e-08+4.139938e-08)+invhh(-3)*(-4.139938e-08+1.125352e-07)+invhh(-4)*(-1.125352e-07+3.059023e-07)+invhh(-5)*(-3.059023e-07+8.315287e-07)+invhh(-6)*(-8.315287e-07+2.260329e-06)+invhh(-7)*(-2.260329e-06+6.144212e-06)+invhh(-8)*(-6.144212e-06+1.67017e-05)+invhh(-9)*(-1.67017e-05+4.539993e-05)+invhh(-10)*(-4.539993e-05+0.0001234098)+invhh(-11)*(-0.0001234098+0.0003354626)+invhh(-12)*(-0.0003354626+0.000911882)+invhh(-13)*(-0.000911882+0.002478752)+invhh(-14)*(-0.002478752+0.006737947)+invhh(-15)*(-0.006737947+0.01831564)+invhh(-16)*(-0.01831564+0.04978707)+invhh(-17)*(-0.04978707+0.1353353)+invhh(-18)*(-0.1353353+0.3678794)+invhh(-19)*(1-0.3678794)+invhh(-1)*5.602796e-09
depkhl=invlh(-1)*(-5.602796e-09+1.522998e-08)+invlh(-2)*(-1.522998e-08+4.139938e-08)+invlh(-3)*(-4.139938e-08+1.125352e-07)+invlh(-4)*(-1.125352e-07+3.059023e-07)+invlh(-5)*(-3.059023e-07+8.315287e-07)+invlh(-6)*(-8.315287e-07+2.260329e-06)+invlh(-7)*(-2.260329e-06+6.144212e-06)+invlh(-8)*(-6.144212e-06+1.67017e-05)+invlh(-9)*(-1.67017e-05+4.539993e-05)+invlh(-10)*(-4.539993e-05+0.0001234098)+invlh(-11)*(-0.0001234098+0.0003354626)+invlh(-12)*(-0.0003354626+0.000911882)+invlh(-13)*(-0.000911882+0.002478752)+invlh(-14)*(-0.002478752+0.006737947)+invlh(-15)*(-0.006737947+0.01831564)+invlh(-16)*(-0.01831564+0.04978707)+invlh(-17)*(-0.04978707+0.1353353)+invlh(-18)*(-0.1353353+0.3678794)+invlh(-19)*(1-0.3678794)+invlh(-1)*5.602796e-09
depkll=invll(-1)*(-5.602796e-09+1.522998e-08)+invll(-2)*(-1.522998e-08+4.139938e-08)+invll(-3)*(-4.139938e-08+1.125352e-07)+invll(-4)*(-1.125352e-07+3.059023e-07)+invll(-5)*(-3.059023e-07+8.315287e-07)+invll(-6)*(-8.315287e-07+2.260329e-06)+invll(-7)*(-2.260329e-06+6.144212e-06)+invll(-8)*(-6.144212e-06+1.67017e-05)+invll(-9)*(-1.67017e-05+4.539993e-05)+invll(-10)*(-4.539993e-05+0.0001234098)+invll(-11)*(-0.0001234098+0.0003354626)+invll(-12)*(-0.0003354626+0.000911882)+invll(-13)*(-0.000911882+0.002478752)+invll(-14)*(-0.002478752+0.006737947)+invll(-15)*(-0.006737947+0.01831564)+invll(-16)*(-0.01831564+0.04978707)+invll(-17)*(-0.04978707+0.1353353)+invll(-18)*(-0.1353353+0.3678794)+invll(-19)*(1-0.3678794)+invll(-1)*5.602796e-09
depkhcT=invch(-1)*(-5.602796e-09+1.522998e-08)+invch(-2)*(-1.522998e-08+4.139938e-08)+invch(-3)*(-4.139938e-08+1.125352e-07)+invch(-4)*(-1.125352e-07+3.059023e-07)+invch(-5)*(-3.059023e-07+8.315287e-07)+invch(-6)*(-8.315287e-07+2.260329e-06)+invch(-7)*(-2.260329e-06+6.144212e-06)+invch(-8)*(-6.144212e-06+1.67017e-05)+invch(-9)*(-1.67017e-05+4.539993e-05)+invch(-10)*(-4.539993e-05+0.0001234098)+invch(-11)*(-0.0001234098+0.0003354626)+invch(-12)*(-0.0003354626+0.000911882)+invch(-13)*(-0.000911882+0.002478752)+invch(-14)*(-0.002478752+0.006737947)+invch(-15)*(-0.006737947+0.01831564)+invch(-16)*(-0.01831564+0.04978707)+invch(-17)*(-0.04978707+0.1353353)+invch(-18)*(-0.1353353+0.3678794)+invch(-19)*(1-0.3678794)+invch(-1)*5.602796e-09
depkhc=ifelse(depkhcT==0&khc(-1)>0,depkhc(-1),depkhcT)
depklc=invcl(-1)*(-5.602796e-09+1.522998e-08)+invcl(-2)*(-1.522998e-08+4.139938e-08)+invcl(-3)*(-4.139938e-08+1.125352e-07)+invcl(-4)*(-1.125352e-07+3.059023e-07)+invcl(-5)*(-3.059023e-07+8.315287e-07)+invcl(-6)*(-8.315287e-07+2.260329e-06)+invcl(-7)*(-2.260329e-06+6.144212e-06)+invcl(-8)*(-6.144212e-06+1.67017e-05)+invcl(-9)*(-1.67017e-05+4.539993e-05)+invcl(-10)*(-4.539993e-05+0.0001234098)+invcl(-11)*(-0.0001234098+0.0003354626)+invcl(-12)*(-0.0003354626+0.000911882)+invcl(-13)*(-0.000911882+0.002478752)+invcl(-14)*(-0.002478752+0.006737947)+invcl(-15)*(-0.006737947+0.01831564)+invcl(-16)*(-0.01831564+0.04978707)+invcl(-17)*(-0.04978707+0.1353353)+invcl(-18)*(-0.1353353+0.3678794)+invcl(-19)*(1-0.3678794)+invcl(-1)*5.602796e-09
Loansl=Loansl(-1)+Invl-REl
Loansc=Loansc(-1)+Invc-REc
Loansh=ifelse(yh!=0&Fh>=0,Loansh(-1)+Invh-REh,0)
LoansDem=Loansc+Loansh+Loansl
intrc=intrbase*(1+1/(1 +exp(kappa*(profrhistc-profrbench))))
intrh=ifelse(hasExited,0,intrbase*(1+1/(1+exp(kappa*(profrhisth-profrbench)))))
intrl=intrbase*(1+1/(1 +exp(kappa*(profrhistl-profrbench))))
rintrc=(1+intrc)/(1+pickhl)-1
rintrl=ifelse(lowcarbExisting,0,ifelse(pikl==0,0,(1+intrl)/(1+pikl)-1))
rintrh=ifelse(hasExited,0,(1+intrh)/(1+pikh)-1)
lambdac=Loansc/(khc*pkh + klc*pkl)
lambdal=ifelse(khl!=0|kll!=0,Loansl/(khl*pkh + kll*pkl),0)
lambdah=ifelse(hasExited,0,Loansh/(khh*pkh))
lambdacprev=(lambdac(-4)+lambdac(-3)+lambdac(-2)+lambdac(-1))/4
lambdalprev=(lambdal(-4)+lambdal(-3)+lambdal(-2)+lambdal(-1))/4
lambdahprev=(lambdah(-4)+lambdah(-3)+lambdah(-2)+lambdah(-1))/4
sharekh=round(khh(-1)/(khh(-1)+kll(-1)),digit=10)
sharekl=round(kll(-1)/(khh(-1)+kll(-1)),digit=10)
kllgre = ifelse(sharekh == 1, 0, ifelse(kll(-1) == 0, 0, (1 - theta) * kllgr))
khhgre = ifelse(sharekh == 0, 0, (totkhhllgr - (1 - sharekl) * kllgre)/sharekh)
khhgrmeane=mean(c(khhgre(-4),khhgre(-3),khhgre(-2),khhgre(-1)))
khlgrmeane=mean(c(khlgre(-4),khlgre(-3),khlgre(-2),khlgre(-1)))
kllgrmeane=mean(c(kllgre(-4),kllgre(-3),kllgre(-2),kllgre(-1)))
khcgrmeane=mean(c(khcgr(-4),khcgr(-3),khcgr(-2),khcgr(-1)))
klcgrmeane=mean(c(klcgr(-4),klcgr(-3),klcgr(-2),klcgr(-1)))
khcperc=khc
klcperc=klc
khlperc=khl
kllperc=ifelse(kll==0,0,(1-phi)*kll + phi*kll(-1)*(1+kllgrmeane))
khhperc=ifelse(khh==0,0,(1-phi)*khh + phi*khh(-1)*(1+khhgrmeane))
xkhc=(1+khcgrmeane)/(1+intrbase)
xklc=(1+klcgrmeane)/(1+intrbase)
xkhh=(1+khhgrmeane)/(1+intrbase)
xkhl=(1+khlgrmeane)/(1+intrbase)
xkll=(1+kllgrmeane)/(1+intrbase)
PVyc=ifelse(khcperc>0,khcperc*(1-xkhc^deltah)/(1-xkhc)+klcperc*(1-xklc^deltah)/(1-xklc),klcperc*(1-xklc^deltah)/(1-xklc))
PVyh=khhperc*(1-xkhh^deltah)/(1-xkhh)
PVyl=ifelse(khlperc>0,khlperc*(1-xkhl^deltah)/(1-xkhl)+kllperc*(1-xkll^deltah)/(1-xkll),kllperc*(1-xkll^deltah)/(1-xkll))
ypotce=PVyc*(1-lambdacT)
ypothe=PVyh*(1-lambdahT)
ypotle=PVyl*(1-lambdalT)
pce=ifelse(hasExited,lambda20*Vfe/ec +(lambda21*RRm+lambda22*RRc+lambda23*RRl)*Vfe/ec,ifelse(el==0,lambda20*Vfe/ec +(lambda21*RRm+lambda22*RRc+lambda23*RRh)*Vfe/ec,lambda20a*Vfe/ec+(lambda21a*RRm+lambda22a*RRc+lambda23a*RRh+lambda24a*RRl)*Vfe/ec))
ple=ifelse(hasExited,lambda30*Vfe/el+(lambda31*RRm+lambda32*RRc+lambda33*RRl)*Vfe/el,ifelse(el>0,lambda40a*Vfe/el+(lambda41a*RRm+lambda42a*RRc+lambda43a*RRh+lambda44a*RRl)*Vfe/el,0))
phe=max(0,ifelse(hasExited,0,ifelse(el==0,lambda30*Vfe/eh +(lambda31*RRm+lambda32*RRc+lambda33*RRh)*Vfe/eh,lambda30a*Vfe/eh+(lambda31a*RRm+lambda32a*RRc+lambda33a*RRh+lambda34a*RRl)*Vfe/eh)))
Mf=ifelse(hasExited,lambda10*Vfe +(lambda11*RRm+lambda12*RRc+lambda13*RRl)*Vfe,ifelse(el==0,lambda10*Vfe +(lambda11*RRm+lambda12*RRc+lambda13*RRh)*Vfe,lambda10a*Vfe +(lambda11a*RRm+lambda12a*RRc+lambda13a*RRh+lambda14a*RRl)*Vfe))
lambda20=ifelse(yh==0|Fh<0,ypotce/(ypotce+ypotle),ifelse(el==0,ypotce/(ypotce+ypothe),0))*(1-lambda10)
lambda30=1-lambda20-lambda10
lambda20a= ifelse(hasExited,0,ifelse(el==0,0,ypotce*(1-lambda10a)/(ypotce+ypothe+ypotle)))
lambda30a= ifelse(hasExited,0,ifelse(el==0,0,ypothe*(1-lambda10a)/(ypotce+ypothe+ypotle)))
lambda40a=1-(lambda20a+lambda30a+lambda10a)
RRm=-pic/(1+pic)
RRc=ifelse(round(cgce,digit=6)==0,chi1a*((expdivratec+1)/(1+pic)-1)+chi1a*((1+expprofratec)/(1+pic)-1),(chi1b*((1+cgce)/(1+pic)-1)+chi1*((1+expdivratec)/(1+pic)-1)+chi1*((1+expprofratec)/(1+pic)-1)))
RRh=ifelse(yh!=0&Fh>=0,ifelse(round(cghe,digit=6)==0,chi1a*((expdivrateh+1)/(1+pic)-1)+chi1a*((1+expprofrateh)/(1+pic)-1),(chi1b*((1+cghe)/(1+pic)-1)+chi1*((1+expdivrateh)/(1+pic)-1)+chi1*((1+expprofrateh)/(1+pic)-1))),0)
RRl=ifelse(el>0,ifelse(round(cgle,digit=6)==0,chi1a*((expdivratel+1)/(1+pic)-1)+chi1a*((1+expprofratel)/(1+pic)-1),(chi1b*((1+cgle)/(1+pic)-1)+chi1*((1+expdivratel)/(1+pic)-1)+chi1*((1+expprofratel)/(1+pic)-1))),0)
timeline 1 500

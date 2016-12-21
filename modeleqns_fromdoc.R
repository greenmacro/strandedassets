# Equations begin
t=t(-1)+1
firstPeriodInov=(t==(n+1))
exitk=ifelse(exitk(-1)==1,1,ifelse(yk==0|Fk<0,1,0))
Vc=Vc(-1) + (YDc-Cc)
Vw=Vw(-1)+(YDw-Cw)
Vce=Vc(-1) + YDce - Cc
Vec=pce*ec+pke*ek +pie*ei +McDf
Vece=Vce-McD
Nc=uic*ic(-1)/li + ukc*kc(-1)/lk
Ni=ifelse(t<=(n+entry)&t>(n+1),ifelse((inii+inci)!=0,uii*ii(-1)/li + uki*ki(-1)/lk,0),0)+ifelse(t>(n+entry),ifelse((inii+inci)!=0,uii*ii(-1)/li+uki*ki(-1)/lk,0),0)
Nk=ifelse(exitk(-1)!=1&inktot>0,ukk*kk(-1)/lk,0)
Ntot=Nc+Nk+Ni
Aprc=yc/Nc
Apri=ifelse(Ni>0,yi/Ni,0.001)
Aprk=ifelse(exitk(-1)!=1&Nk>0,yk/Nk,0)
omegaTc=Omega0+Omega1*log(Aprc)+ Omega2*log(Ntot/LF)
omegaTk=ifelse(exitk(-1)!=1&Nk>0,Omega0+Omega1*log(Aprk)+Omega2*log(Ntot/LF),0)
omegaTi=Omega0+Omega1*log(Apri)+Omega2*log(Ntot/LF)
Wc=Wc(-1) + Omega3*(omegaTc(-1) - Wc(-1)/pc(-1))
Wk=ifelse(exitk(-1)==0,Wk(-1) + Omega3*(omegaTk(-1) - Wk(-1)/pc(-1)),0)
Wi=ifelse(t<=(n+1),Wk(-1),Wi(-1) + Omega3*(omegaTi(-1) - Wi(-1)/pc(-1)))
Fc=Yc-Wc*Nc-rlc*Lc(-1)
Fi=ifelse(t<=(n+entry)&t>(n+1),Yi-Wi*Ni-rli*Li(-1),0)+ifelse(t>(n+entry),Yi-Wi*Ni-rli*Li(-1),0)
Fk=ifelse(exitk(-1)!=1&inktot>0,Yk-Wk*Nk-rlk*Lk(-1),0)
Fce=Fcmean+Fcmean*Fcgr*eta+nu*(Fc(-1)-Fce(-1))
Fie=Fimean+Fimean*Figr*eta+nu*(Fi(-1)- Fie(-1))
Fke=ifelse(exitk(-1)!=1,Fkmean+Fkmean*Fkgr*eta+nu*(Fk(-1)-Fke(-1)),0)
Fcgr=ifelse(Fc(-2)==0,0,(Fc(-1)-Fc(-2))/Fc(-2))
Figr=ifelse(Fi(-2)==0,0,(Fi(-1)-Fi(-2))/Fi(-2))
Fkgr=ifelse(Fk(-2)==0,0,(Fk(-1)-Fk(-2))/Fk(-2))
Fcmean=mean(c(Fc(-4),Fc(-3),Fc(-2),Fc(-1)))
Fimean=mean(c(Fi(-4),Fi(-3),Fi(-2),Fi(-1)))
Fkmean=mean(c(Fk(-4),Fk(-3),Fk(-2),Fk(-1)))
FDi=ifelse(t<=(n+entry)&t>(n+1),max(Fi-Ii,0),0)+ifelse(t>(n+entry),ifelse(Ii-Fi-Mi(-1)<=0,max(Fi-Ii,0),0),0)
FDc=max(Fc-Ic,0)
FDk=ifelse(exitk(-1)!=1&inktot>0,max(Fk-Ik,0),0)
FDb= rlc*Lc(-1) +ifelse(yk!=0&Fk>=0,rlk*Lk(-1),-Lk(-1)) + rli*Li(-1)
FDbe= FDbmean+FDbmean*FDbgr*eta+chi*(FDb(-1)- FDbe(-1))
FDie= FDimean+FDimean*FDigr*eta+chi*(FDi(-1)-FDie(-1))
FDce=FDcmean+FDcmean*FDcgr*eta+chi*(FDc(-1)-FDce(-1))
FDke=ifelse(exitk(-1)!=1,FDkmean+FDkmean*FDkgr*eta+chi*(FDk(-1)-FDke(-1)),0)
FDbgr=ifelse(FDb(-2)==0,0,(FDb(-1)-FDb(-2))/FDb(-2))
FDcgr=ifelse(FDc(-2)==0,0,(FDc(-1)-FDc(-2))/FDc(-2))
FDigr=ifelse(FDi(-2)==0,0,(FDi(-1)-FDi(-2))/FDi(-2))
FDkgr=ifelse(FDk(-2)==0,0,(FDk(-1)-FDk(-2))/FDk(-2))
FDbmean=mean(c(FDb(-4),FDb(-3),FDb(-2),FDb(-1)))
FDcmean=mean(c(FDc(-4),FDc(-3),FDc(-2),FDc(-1)))
FDimean=mean(c(FDi(-4),FDi(-3),FDi(-2),FDi(-1)))
FDkmean=mean(c(FDk(-4),FDk(-3),FDk(-2),FDk(-1)))
cw1= xiw1*ydwe + xiw2*vw(-1)
cc0=xic1*ydce+xic2*vc(-1)
cc1=(cc0+abs(cc0))/2
cs=kc(-1)*prk+ic(-1)*pri
exc1=cs-cw1-cc1
exc2=(exc1-abs(exc1))/2
cw=cw1+cw1/(cw1+cc1)*exc2
cc=cc1+cc1/(cw1+cc1)*exc2
Cw=cw*pc
Cc=cc*pc
vc=Vc/pc
vw=Vw/pc
YDw=Wc*Nc+Wk*Nk+Wi*Ni
ydw=YDw/pc-pic*Vw(-1)/pc
YDwe= YDwmean+YDwmean*YDwgr*eta+epsilon*(YDw(-1)-YDwe(-1))
ydwe= YDwe/pc-(pic*Vw(-1))/pc
YDwgr=ifelse(YDw(-2)==0,0,(YDw(-1)-YDw(-2))/YDw(-2))
YDwmean=mean(c(YDw(-4),YDw(-3),YDw(-2),YDw(-1)))
YDc=YPc + CG
ydc=YDc/pc - pic*Vc(-1)/pc
YDce1=YPce + CGce+CGke+CGie
YDce=(YDce1+abs(YDce1))/2
ydce=YDce/pc-(pic*Vc(-1))/pc
YPc=FDc+FDk+FDb+FDi
YPce=FDce+FDke+FDie+FDbe
CG = ifelse(ei(-1)==0&ei>0,ec(-1)*(pce-pce(-1))+ek(-1)*(pke-pke(-1)),ec(-1)*(pce-pce(-1))+ek(-1)*(pke-pke(-1))+ ei(-1)*(pie-pie(-1)))
CGc=ec(-1)*(pce-pce(-1))
CGk=ifelse(yk!=0,ek(-1)*(pke-pke(-1)),0)
CGi=ifelse(ei(-1)!=0&pie(-1)!=0,ei(-1)*(pie-pie(-1)),0)
cgc=CGc/(pce(-1)*ec(-1))
cgk=ifelse(yk!=0&Fk>=0,CGk/(pke(-1)*ek(-1)),0)
cgi=ifelse(ei(-1)!=0&pie(-1)!=0,CGi/(pie(-1)*ei(-1)),0)
CGce= CGcmean+CGcmean*CGcgr*eta+theta*(CGc(-1)-CGce(-1))
CGke=ifelse(exitk(-1)!=1,CGkmean+CGkmean*CGkgr*eta+theta*(CGk(-1)-CGke(-1)),0)
CGie =CGimean+CGimean*CGigr*eta+theta*(CGi(-1)-CGie(-1))
cgce = CGce/(ec(-1)*pce(-1))
cgke=ifelse(exitk(-1)!=1,CGke/(ek(-1)*pke(-1)),0)
cgie=ifelse(t<(n+1),ifelse(ei(-1)==0|pie(-1)==0,0,(CGie/(ei(-1)*pie(-1)))),0)
CGcgr=ifelse(CGc(-2)==0,0,(CGc(-1)-CGc(-2))/CGc(-2))
CGigr=ifelse(CGi(-2)==0,0,(CGi(-1)-CGi(-2))/CGi(-2))
CGkgr=ifelse(CGk(-2)==0,0,(CGk(-1)-CGk(-2))/CGk(-2))
CGcmean=mean(c(CGc(-4),CGc(-3),CGc(-2),CGc(-1)))
CGimean=mean(c(CGi(-4),CGi(-3),CGi(-2),CGi(-1)))
CGkmean=mean(c(CGk(-4),CGk(-3),CGk(-2),CGk(-1)))
Ms=Mw+Mc+Mi
Mw=Vw
McD=beta*Cc
Mc=McDf+Vc -Vec
Mileft=Mi(-1)+Fi+esi*pie-Ii-FDi
Mi=max(Mileft-Li(-1)/nLoans,0)
Li=Li(-1)-min(Li(-1)/nLoans,Mileft)
Lc=Lc(-1)+Ifc-esc*pce
Lk=ifelse(yk!=0&Fk>=0,Lk(-1)+Ifk-esk*pke,0)
Ld=Lc+Lk+Li
rlc=rl*(1+1/(1 +exp(kappa*(rca-rb))))
rlk=ifelse(exitk(-1)==0,rl*(1+1/(1+exp(kappa*(rka-rb)))),0)
rli=rl*(1+1/(1 +exp(kappa*(ria-rb))))
rrlc=(1+rlc)/(1+picki)-1
rrli=ifelse(firstPeriodInov,0,ifelse(pii==0,0,(1+rli)/(1+pii)-1))
rrlk=ifelse(exitk(-1)==0,(1+rlk)/(1+pik(-1))-1,0)
lambdac=Lc/(kc*pk + ic*pi)
lambdai=ifelse(ki!=0|ii!=0,Li/(ki*pk + ii*pi),0)
lambdak=ifelse(exitk(-1)==0,Lk/(kk*pk),0)
Ic=inck*pk+inci*pi
Ik=ifelse(exitk(-1)==0, ink*pk,0)
Ii=ifelse(firstPeriodInov,inik*pk,inii*pi)
Ifk=ifelse(exitk(-1)!=1&inktot>0,max(Ik-Fk,0),0)
Ifi=ifelse(t<=(n+entry)&t>(n+1),max(Ii-Fi,0),0)+ifelse(t>(n+entry),max(Ii-Fi-Mi(-1),0),0)+ifelse(t==(n+1),Ii,0)
Ifc=max(Ic-Fc,0)
Psik=ifelse(exitk(-1)!=1&inktot>0,1/(1+exp(psik*(rkT-cgk(-1)-rlk))),0)
Psii=0+(t>(n+entry))*(1/(1+exp(psii*(riT-cgi(-1)-rli))))
Psic=1/(1+exp(psic*(rcT-cgc(-1)-rlc)))
esc=(Psic*Ifc)/pce(-1)
esk=ifelse(exitk(-1)!=1&inktot>0,(Psik*Ifk)/pke(-1),0)
esi=ifelse(t>(n+entry),(psii*Ifi)/pie(-1),ifelse(t==(n+entry),100,0))
ec=ec(-1)+esc
ek=ek(-1)+esk
ei=ifelse(t<=(n+entry)&t>(n+1),ei(-1)+esi,0)+ifelse(t>(n+entry),ei(-1)+esi,0)
REk=ifelse(exitk(-1)!=1&inktot>0,Fk-FDk,0)
REi=ifelse(t<=(n+entry)&t>(n+1),Fi-FDi,0)+ifelse(t>(n+entry),Fi-FDi,0)
REc=Fc-FDc
kc=kc(-1)+inck-depkc
ki=ifelse(firstPeriodInov,inik,ki(-1)-depki)
kk=ifelse(exitk(-1)==0,kk(-1)+ink-depkk,0)
ic=ic(-1)+inci-depic
ii=ifelse(firstPeriodInov,0,ii(-1)-depii+inii)
depkk=ink(-1)*(-5.602796e-09+1.522998e-08)+ink(-2)*(-1.522998e-08+4.139938e-08)+ink(-3)*(-4.139938e-08+1.125352e-07)+ink(-4)*(-1.125352e-07+3.059023e-07)+ink(-5)*(-3.059023e-07+8.315287e-07)+ink(-6)*(-8.315287e-07+2.260329e-06)+ink(-7)*(-2.260329e-06+6.144212e-06)+ink(-8)*(-6.144212e-06+1.67017e-05)+ink(-9)*(-1.67017e-05+4.539993e-05)+ink(-10)*(-4.539993e-05+0.0001234098)+ink(-11)*(-0.0001234098+0.0003354626)+ink(-12)*(-0.0003354626+0.000911882)+ink(-13)*(-0.000911882+0.002478752)+ink(-14)*(-0.002478752+0.006737947)+ink(-15)*(-0.006737947+0.01831564)+ink(-16)*(-0.01831564+0.04978707)+ink(-17)*(-0.04978707+0.1353353)+ink(-18)*(-0.1353353+0.3678794)+ink(-19)*(1-0.3678794)+ink(-1)*5.602796e-09
depki=inik(-1)*(-5.602796e-09+1.522998e-08)+inik(-2)*(-1.522998e-08+4.139938e-08)+inik(-3)*(-4.139938e-08+1.125352e-07)+inik(-4)*(-1.125352e-07+3.059023e-07)+inik(-5)*(-3.059023e-07+8.315287e-07)+inik(-6)*(-8.315287e-07+2.260329e-06)+inik(-7)*(-2.260329e-06+6.144212e-06)+inik(-8)*(-6.144212e-06+1.67017e-05)+inik(-9)*(-1.67017e-05+4.539993e-05)+inik(-10)*(-4.539993e-05+0.0001234098)+inik(-11)*(-0.0001234098+0.0003354626)+inik(-12)*(-0.0003354626+0.000911882)+inik(-13)*(-0.000911882+0.002478752)+inik(-14)*(-0.002478752+0.006737947)+inik(-15)*(-0.006737947+0.01831564)+inik(-16)*(-0.01831564+0.04978707)+inik(-17)*(-0.04978707+0.1353353)+inik(-18)*(-0.1353353+0.3678794)+inik(-19)*(1-0.3678794)+inik(-1)*5.602796e-09
depii=inii(-1)*(-5.602796e-09+1.522998e-08)+inii(-2)*(-1.522998e-08+4.139938e-08)+inii(-3)*(-4.139938e-08+1.125352e-07)+inii(-4)*(-1.125352e-07+3.059023e-07)+inii(-5)*(-3.059023e-07+8.315287e-07)+inii(-6)*(-8.315287e-07+2.260329e-06)+inii(-7)*(-2.260329e-06+6.144212e-06)+inii(-8)*(-6.144212e-06+1.67017e-05)+inii(-9)*(-1.67017e-05+4.539993e-05)+inii(-10)*(-4.539993e-05+0.0001234098)+inii(-11)*(-0.0001234098+0.0003354626)+inii(-12)*(-0.0003354626+0.000911882)+inii(-13)*(-0.000911882+0.002478752)+inii(-14)*(-0.002478752+0.006737947)+inii(-15)*(-0.006737947+0.01831564)+inii(-16)*(-0.01831564+0.04978707)+inii(-17)*(-0.04978707+0.1353353)+inii(-18)*(-0.1353353+0.3678794)+inii(-19)*(1-0.3678794)+inii(-1)*5.602796e-09
depkc=inck(-1)*(-5.602796e-09+1.522998e-08)+inck(-2)*(-1.522998e-08+4.139938e-08)+inck(-3)*(-4.139938e-08+1.125352e-07)+inck(-4)*(-1.125352e-07+3.059023e-07)+inck(-5)*(-3.059023e-07+8.315287e-07)+inck(-6)*(-8.315287e-07+2.260329e-06)+inck(-7)*(-2.260329e-06+6.144212e-06)+inck(-8)*(-6.144212e-06+1.67017e-05)+inck(-9)*(-1.67017e-05+4.539993e-05)+inck(-10)*(-4.539993e-05+0.0001234098)+inck(-11)*(-0.0001234098+0.0003354626)+inck(-12)*(-0.0003354626+0.000911882)+inck(-13)*(-0.000911882+0.002478752)+inck(-14)*(-0.002478752+0.006737947)+inck(-15)*(-0.006737947+0.01831564)+inck(-16)*(-0.01831564+0.04978707)+inck(-17)*(-0.04978707+0.1353353)+inck(-18)*(-0.1353353+0.3678794)+inck(-19)*(1-0.3678794)+inck(-1)*5.602796e-09
depic=inci(-1)*(-5.602796e-09+1.522998e-08)+inci(-2)*(-1.522998e-08+4.139938e-08)+inci(-3)*(-4.139938e-08+1.125352e-07)+inci(-4)*(-1.125352e-07+3.059023e-07)+inci(-5)*(-3.059023e-07+8.315287e-07)+inci(-6)*(-8.315287e-07+2.260329e-06)+inci(-7)*(-2.260329e-06+6.144212e-06)+inci(-8)*(-6.144212e-06+1.67017e-05)+inci(-9)*(-1.67017e-05+4.539993e-05)+inci(-10)*(-4.539993e-05+0.0001234098)+inci(-11)*(-0.0001234098+0.0003354626)+inci(-12)*(-0.0003354626+0.000911882)+inci(-13)*(-0.000911882+0.002478752)+inci(-14)*(-0.002478752+0.006737947)+inci(-15)*(-0.006737947+0.01831564)+inci(-16)*(-0.01831564+0.04978707)+inci(-17)*(-0.04978707+0.1353353)+inci(-18)*(-0.1353353+0.3678794)+inci(-19)*(1-0.3678794)+inci(-1)*5.602796e-09
gk=ifelse(exitk(-1)==0,eta0k+eta1*uke-eta2*rrlk*lambdakprev+eta3*qkprev,0)
gi=ifelse(t>n+1,ifelse(t<=(n+entry),tau,eta0k+eta1*uie-eta2*rrli*lambdaiprev+eta3*qiprev),0)
gc=eta0c+eta1*uce-eta2*rrlc*lambdacprev+eta3*qcprev
lambdacprev=(lambdac(-4)+lambdac(-3)+lambdac(-2)+lambdac(-1))/4
lambdaiprev=(lambdai(-4)+lambdai(-3)+lambdai(-2)+lambdai(-1))/4
lambdakprev=(lambdak(-4)+lambdak(-3)+lambdak(-2)+lambdak(-1))/4
invyc=max(gc*yc(-1)+depkc*prk+depic*pri,0)
inci=(invyc>0)*(z11*(z21*is+(1-z21)*invyc/pri)+(1-z11)*z31*(z41*is+(1-z41)*(invyc-ks*prk)/pri))
inck=(invyc>0)*(z11*z21*(z41*ks+(1-z41)*(invyc-is*pri)/prk)+(1-z11)*(z31*ks+(1-z31)*invyc/prk))
z11=(invyc>0)*(costk>costi)
z21=(invyc>0)*(invyc>is*pri)
z31=(invyc>0)*(invyc>ks*prk)
z41=(invyc>0)*(invyc>is*pri+ks*prk)
costk=pk/prk
costi=pi/pri
gkk=ifelse(exitk(-1)==0,gk/prk,0)
gii=ifelse(firstPeriodInov,0,gi/pri)
ink=ifelse(exitk(-1)==0,min(kk(-1)*prk,max(0,gkk*kk(-1) + depkk)),0)
ks=ifelse(exitk(-1)==0,kk(-1)*prk-ink,0)
inktot=ink+inck+inik
inii=ifelse(firstPeriodInov,0,min(ii(-1)*pri+ki(-1)*prk,max(((gi*yi(-1)+depii*pri+depki*prk)/pri),0)))
inik=ifelse(firstPeriodInov,rho*Yk(-1)*pri/((1-rho)*pi*prk*(pri-exp(1-n)-tau)),0)
is=ifelse(firstPeriodInov,0,ii(-1)*pri+ki(-1)*prk-inii)
yc=cc+cw
yi=ifelse(firstPeriodInov,inik*prk,ifelse(t<=(n+entry)&t>(n+1),inii+inci,0)+ifelse(t>(n+entry),inii+inci,0))
yk=ifelse(exitk(-1)!=1&inktot>0,ink+inck+inik,0)
Yc=yc*pc
Yi=ifelse(t<=(n+entry)&t>(n+1),yi*pi,0)+ifelse(t>(n+entry),yi*pi,0)
Yk=ifelse(exitk(-1)!=1&inktot>0,yk*pk,0)
yce=yc(-1)+epsilon*(yce(-1)-yc(-1))
yie=ifelse(!firstPeriodInov,yi(-1)+epsilon*(yie(-1)-yi(-1)),0)
yke=ifelse(exitk(-1)==0,yk(-1)+epsilon*(yke(-1)-yk(-1)),0)
uc=yc/(kc(-1)*prk +ic(-1)*pri)
ui=ifelse(t<=(n+entry)&t>(n+1),yi/(ki(-1)*prk+ii(-1)*pri),0)+ifelse(t>(n+entry),yi/(ki(-1)*prk+ii(-1)*pri),0)
uk=ifelse(exitk(-1)!=1&inktot>0,yk/(kk(-1)*prk),0)
uce=yce/(kc(-1)*prk+ic(-1)*pri)
uie=ifelse(!firstPeriodInov,ifelse(ii(-1)==0,0,(yie/(ii(-1)*pri+ki(-1)*prk))),0)
uke=ifelse(exitk(-1)==0,yke/(kk(-1)*prk),0)
uic1=ifelse(ic(-1)==0,1,0)+ifelse(ic(-1)>0,max(0,min(yc/(pri*ic(-1)),1)),0)
uic=uic1
uii1=ifelse(t<=(n+entry)&t>(n+1),ifelse(ii(-1)!=0,max(0,min(1,yi/(pri*ii(-1)))),1),ifelse(t>(n+entry),max(0,min(1,yi/(pri*ii(-1)))),0))
uii=ifelse(t<=(n+entry)&t>(n+1),ifelse((inii+inci)!=0,ifelse(ii(-1)==0,1,uii1),0),0)+ifelse(t>(n+entry),ifelse((inii+inci)!=0,uii1,0),0)
uice=min(yce/(pri*ic(-1)),1)
uiie=ifelse(!firstPeriodInov,ifelse(ii(-1)==0,1,min(yie/(pri*ii(-1)),1)),0)
ukc=ifelse(kc(-1)==0,1,0)+ifelse(kc(-1)>0,max(0,(yc-uic*pri*ic(-1))/(prk*kc(-1))),0)
uki=ifelse(t>(n+1),ifelse((inii+inci)==0,0,ifelse(ki(-1)==0,0,max(0,(yi-uii*pri*ii(-1))/(prk*ki(-1))))),0)
ukk=ifelse(exitk(-1)!=1&inktot>0,yk/(prk*kk(-1)),0)
ukce=min((yce-uice*pri*ic(-1))/(prk*kc(-1)),1)
ukie=ifelse(!firstPeriodInov,ifelse(ki(-1)==0,0,min((yie-uiie*pri*ii(-1))/(prk*ki(-1)),1)),0)
ukke=ifelse(exitk(-1)==0,yke/(prk*kk(-1)),0)
pc=pc(-1)+zeta*((1+phic)*NUCc-pc(-1))
pi=ifelse(firstPeriodInov,(1+phii)*NUCi,pi(-1)+zeta*((1+phii)*NUCi-pi(-1)))
pk=ifelse(exitk(-1)==0,pk(-1)+zeta*((1+phik)*NUCk-pk(-1)),0)
UCc=Wc*(uic*ic(-1)*lk+ukc*kc(-1)*li)/((ukc*prk*kc(-1) +uic*pri*ic(-1))*li*lk)
UCi=ifelse(t<=(n+entry)&t>(n+1),ifelse((inii+inci)!=0,Wi*(uii*ii(-1)*lk+uki*ki(-1)*li)/((uki*prk*ki(-1) +uii*pri*ii(-1))*li*lk),0),0)+ifelse(t>(n+entry),ifelse((inii+inci)!=0,Wi*(uii*ii(-1)*lk+uki*ki(-1)*li)/((uki*prk*ki(-1)+uii*pri*ii(-1))*li*lk),0),0)
UCk=ifelse(exitk(-1)!=1&inktot>0,Wk/(prk*lk),0)
UCke=ifelse(exitk(-1)==0,Wk(-1)/(prk*lk),0)
UCie=ifelse(firstPeriodInov,Wi/(prk*lk),ifelse(ki(-1)==0&ii(-1)==0,0,Wi(-1)*(uiie*ii(-1)*lk+ukie*ki(-1)*li)/((ukie*prk*ki(-1)+uiie*pri*ii(-1))*li*lk)))
UCce=Wc(-1)*(uice*ic(-1)*lk+ukce*kc(-1)*li)/((ukce*prk*kc(-1)+uice*pri*ic(-1))*li*lk)
NUCk = ifelse(exitk(-1)==0,NUCk(-1)+zeta*(UCke-NUCk(-1)),0)
NUCi=ifelse(firstPeriodInov,UCie,NUCi(-1)+zeta*(UCie-NUCi(-1)))
NUCc=NUCc(-1)+zeta*(UCce-NUCc(-1))
phii=ifelse(firstPeriodInov,riT*pk/(NUCi*prk),ifelse(NUCi==0|yie==0,0,riT*(pk(-1)*ki(-1)+pi(-1)*ii(-1))/(NUCi*yie)))
phic=rcT*(pk(-1)*kc(-1) +pi(-1)*ic(-1))/(NUCc*yce)
phik=ifelse(exitk(-1)==0,rkT*(pk(-1)*kk(-1))/(UCke*yke),0)
pic=(pc-pc(-1))/pc(-1)
pii=ifelse(pi(-1)==0,0,(pi-pi(-1))/pi(-1))
pik=ifelse(exitk(-1)==0,(pk-pk(-1))/pk(-1),0)
picki=(kc(-1)*pik + ic(-1)*pii)/(kc(-1)+ ic(-1))
pce=ifelse(exitk(-1)==1,lambda20*Vece/ec +(lambda21*RRm+lambda22*RRc+lambda23*RRi)*Vece/ec,ifelse(ei==0,lambda20*Vece/ec +(lambda21*RRm+lambda22*RRc+lambda23*RRk)*Vece/ec,lambda20a*Vece/ec+(lambda21a*RRm+lambda22a*RRc+lambda23a*RRk+lambda24a*RRi)*Vece/ec))
pie=ifelse(exitk(-1)==1,lambda30*Vece/ei+(lambda31*RRm+lambda32*RRc+lambda33*RRi)*Vece/ei,ifelse(ei>0,lambda40a*Vece/ei+(lambda41a*RRm+lambda42a*RRc+lambda43a*RRk+lambda44a*RRi)*Vece/ei,0))
pke=ifelse(exitk(-1)==1,0,ifelse(ei==0,lambda30*Vece/ek +(lambda31*RRm+lambda32*RRc+lambda33*RRk)*Vece/ek,lambda30a*Vece/ek+(lambda31a*RRm+lambda32a*RRc+lambda33a*RRk+lambda34a*RRi)*Vece/ek))
McDf=ifelse(exitk(-1)==1,lambda10*Vece +(lambda11*RRm+lambda12*RRc+lambda13*RRi)*Vece,ifelse(ei==0,lambda10*Vece +(lambda11*RRm+lambda12*RRc+lambda13*RRk)*Vece,lambda10a*Vece +(lambda11a*RRm+lambda12a*RRc+lambda13a*RRk+lambda14a*RRi)*Vece))
templambda20=ifelse(yk==0|Fk<0,(kc*prk+ic*pri)/(kc*prk+ic*prk+ki*prk+ii*pri),ifelse(ei==0,(kc*prk+ic*pri)/(kc*prk+ic*prk+kk*prk),0))
lambda20=templambda20*(1-lambda10)
lambda30=1-lambda20-lambda10
lambda20a= ifelse(exitk(-1)==0,ifelse(ei==0,0,ifelse(lambda20a(-1)==0,(kc*prk+ic*pri-irrational2*(kk*prk+kc*prk+ic*pri+ii*pri+ki*prk))*(1-lambda10a-irrational3)/(kc*prk+ic*prk+ki*prk+ii*pri+kk*prk+ii*pri+ki*prk),irrational*lambda20a(-1)+(1-irrational)*(kc*prk+ic*pri-irrational2*(kk*prk+kc*prk+ic*pri))*(1-lambda10a-irrational3)/(kc*prk+ic*prk+ki*prk+ii*pri+kk*prk))),0)
lambda30a= ifelse(exitk(-1)==0,ifelse(ei==0,0,ifelse(lambda30a(-1)==0,(kk*prk+irrational2*(kk*prk+kc*prk+ic*pri+ii*pri+ki*prk))*(1-lambda10a-irrational3)/(kc*prk+ic*prk+ki*prk+ii*pri+kk*prk+ii*pri+ki*prk),irrational*lambda30a(-1)+(1-irrational)*(kk*prk+irrational2*(kk*prk+kc*prk+ic*pri))*(1-lambda10a-irrational3)/(kc*prk+ic*prk+ki*prk+ii*pri+kk*prk))),0)
lambda40a=1-(lambda20a+lambda30a+lambda10a)
qc= (ec*pce)/(pk*kc +pi*ic )
qk=ifelse(yk!=0&Fk>=0,(ek*pke)/(pk*kk),0)
qi=ifelse((ki==0&ii==0)|ei==0,1,(ei*pie)/(pk*ki +pi*ii))
qcprev=(qc(-4)+qc(-3)+qc(-2)+qc(-1))/4
qkprev=(qk(-4)+qk(-3)+qk(-2)+qk(-1))/4
qiprev=(qi(-4)+qi(-3)+qi(-2)+qi(-1))/4
rc=Fc/(kc(-1)*pk(-1)+ic(-1)*pi(-1))
ri=ifelse(t<=(n+entry)&t>(n+1),Fi/(ki(-1)*pk(-1)+ii(-1)*pi(-1)),0)+ifelse(t>(n+entry),Fi/(ki(-1)*pk(-1)+ii(-1)*pi(-1)),0)
rk=ifelse(exitk(-1)!=1&inktot>0,Fk/(kk(-1)*pk(-1)),0)
rca=ifelse(sumkc>0,(Fc(-5)+Fc(-4)+Fc(-3)+Fc(-2)+Fc(-1))/sumkc,0)
sumkc=pk(-5)*kc(-5)+pi(-5)*ic(-5)+pk(-4)*kc(-4)+pi(-4)*ic(-4)+pk(-3)*kc(-3)+pi(-3)*ic(-3)+pk(-2)*kc(-2)+pi(-2)*ic(-2)+pk(-1)*kc(-1)+pi(-1)*ic(-1)
ria=ifelse(sumki>0,(Fi(-5)+Fi(-4)+Fi(-3)+Fi(-2)+Fi(-1))/sumki,0)
sumki=pk(-5)*ki(-5)+pi(-5)*ii(-5)+pk(-4)*ki(-4)+pi(-4)*ii(-4)+pk(-3)*ki(-3)+pi(-3)*ii(-3)+pk(-2)*ki(-2)+pi(-2)*ii(-2)+pk(-1)*ki(-1)+pi(-1)*ii(-1)
rka=ifelse(sumkk>0,(Fk(-5)+Fk(-4)+Fk(-3)+Fk(-2)+Fk(-1))/sumkk,0)
sumkk=pk(-5)*kk(-5)+pk(-4)*kk(-4)+pk(-3)*kk(-3)+pk(-2)*kk(-2)+pk(-1)*kk(-1)
rcge=Fce/(ec(-1)*pce(-1))
rige=ifelse(t>=(n+entry),ifelse(pie(-1)==0,Fie/(ki(-1)*pk(-1)+ii(-1)*pi(-1)),Fie/(ei(-1)*pie(-1))),0)
rkge=ifelse(exitk(-1)!=1,Fke/(ek(-1)*pke(-1)),0)
rce=FDce/(ec(-1)*pce(-1))
rke=ifelse(exitk(-1)!=1,FDke/(ek(-1)*pke(-1)),0)
rie=ifelse(t>=(n+entry),ifelse(pie(-1)==0,FDie/(ki(-1)*pk(-1)+ii(-1)*pi(-1)),FDie/(ei(-1)*pie(-1))),0)
RRm=-pic/(1+pic)
RRc=ifelse(round(cgce,digit=6)==0,chi1a*((rce+1)/(1+pic)-1)+chi1a*((1+rcge)/(1+pic)-1),(chi1b*((1+cgce)/(1+pic)-1)+chi1*((1+rce)/(1+pic)-1)+chi1*((1+rcge)/(1+pic)-1)))
RRk=ifelse(yk!=0&Fk>=0,ifelse(round(cgke,digit=6)==0,chi1a*((rke+1)/(1+pic)-1)+chi1a*((1+rkge)/(1+pic)-1),(chi1b*((1+cgke)/(1+pic)-1)+chi1*((1+rke)/(1+pic)-1)+chi1*((1+rkge)/(1+pic)-1))),0)
RRi=ifelse(ei>0,ifelse(round(cgie,digit=6)==0,chi1a*((rie+1)/(1+pic)-1)+chi1a*((1+rige)/(1+pic)-1),(chi1b*((1+cgie)/(1+pic)-1)+chi1*((1+rie)/(1+pic)-1)+chi1*((1+rige)/(1+pic)-1))),0)
#Calib
CALIBRATION
#END OF MODEL
timeline 1 500

save(bas,file="baselineAnalysis.Rdata")

load("baselineAnalysis.Rdata")

analysis<-bas[21:60,]

View(analysis[c("yk","inck","ink","inik","yc","cc","cw","Nc","Nk","Ni","yi","inci","inii","invyc","gc","lambdacprev","rrlc","rlc","qcprev")])

View(analysis[c("gk","lambdakprev","rrlk","rlk","qkprev","uk")])

View(analysis[c("pk","phik","pik","pc","phic","pic","pi","phii","pii")])

plot(analysis$yk,type='l')

temp<-analysis[1:40,c("lambdakprev","rrlk","qkprev","uk","rlk","pik")]
temp$lambdakprev<-temp$lambdakprev/temp$lambdakprev[1]
temp$rrlk<-temp$rrlk/temp$rrlk[1]
temp$rlk<-temp$rlk/temp$rlk[1]
# temp$pik<-temp$pik/temp$pik[1]
temp$uk<-temp$uk/temp$uk[1]
temp$qkprev<-temp$qkprev/temp$qkprev[1]
matplot(temp,type='l',lty=1,lwd=2)
legend("bottomleft",col=1:5,lwd=2,bty='n',legend=c("lambdakprev","rrlk","qkprev","uk","rlk","pik"))

analysis$incishare<-analysis$inci/analysis$yi
View(analysis[c("gi","lambdaiprev","rrli","rli","qiprev","ui","yi","inii","incishare")])

View(analysis[c("gc","lambdacprev","rrlc","rlc","qcprev","uc","yc","cc","cw")])

plot(analysis$gc,type='l')

temp<-analysis[1:40,c("lambdacprev","rrlc","qcprev","uc","rlc","picki")]
temp$lambdakprev<-temp$lambdacprev/temp$lambdakprev[1]
temp$rrlc<-temp$rrlc/temp$rrlc[1]
temp$rlc<-temp$rlc/temp$rlc[1]
# temp$pik<-temp$pik/temp$pik[1]
temp$uc<-temp$uc/temp$uc[1]
temp$qcprev<-temp$qcprev/temp$qcprev[1]
matplot(temp,type='l',lty=1,lwd=2)
legend("bottomleft",col=1:6,lwd=2,bty='n',legend=c("lambdacprev","rrlc","qcprev","uc","rlc","pic"))

temp<-analysis[1:40,c("yc","cc","cw")]
temp$yc<-temp$yc/temp$yc[1]
temp$cc<-temp$cc/temp$cc[1]
temp$cw<-temp$cw/temp$cw[1]
matplot(temp,type='l',lty=1,lwd=2)
legend("bottomleft",col=1:5,lwd=2,bty='n',legend=c("yc","cc","cw"))

temp<-analysis[1:40,c("cw","YDw","Vw","pc","Nc","Nk","Ni")]
temp$YDw<-temp$YDw/temp$YDw[1]
temp$pc<-temp$pc/temp$pc[1]
temp$cw<-temp$cw/temp$cw[1]
temp$Vw<-temp$Vw/temp$Vw[1]
temp$Nc<-temp$Nc/temp$Nc[1]
temp$Ni<-temp$Ni/temp$Ni[1]
temp$Nk<-temp$Nk/temp$Nk[1]
matplot(temp,type='l',lty=1,lwd=2,ylim=c(0.8,1.2))
legend("bottomleft",col=1:6,lwd=2,bty='n',legend=colnames(temp))

analysis$NcShare<-analysis$Nc/analysis$Ntot
analysis$NiShare<-analysis$Ni/analysis$Ntot
analysis$NkShare<-analysis$Nk/analysis$Ntot

matplot(analysis[1:40,c("NcShare","NkShare","NiShare")],type='l',lty=1,lwd=2)


temp<-analysis[1:40,c("cc","YDc","FDc","FDk","FDb","Vc")]
temp$cc<-temp$cc/temp$cc[1]
temp$YDc<-temp$YDc/temp$YDc[1]
temp$FDc<-temp$FDc/temp$FDc[1]
temp$FDk<-temp$FDk/temp$FDk[1]
temp$FDb<-temp$FDb/temp$FDb[1]
temp$Vc<-temp$Vc/temp$Vc[1]
matplot(temp,type='l',lty=1,lwd=2)
legend("bottomleft",col=1:6,lwd=2,bty='n',legend=colnames(temp))

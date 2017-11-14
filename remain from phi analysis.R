
```{r, echo=FALSE}
temp<-fail[40:58,c("lambdakprev","rrlk","qkprev","uke","rlk","eta2","eta1","eta3","eta0k","gk")]
temp$debtburden<--temp$eta2*temp$rrlk*temp$lambdakprev
temp$capacity<-temp$eta1*temp$uke
temp$finance<-temp$eta3*temp$qkprev
tempf<-temp
temp<-suc[40:58,c("lambdakprev","rrlk","qkprev","uke","rlk","eta2","eta1","eta3","eta0k","gk")]
temp$debtburden<--temp$eta2*temp$rrlk*temp$lambdakprev
temp$capacity<-temp$eta1*temp$uke
temp$finance<-temp$eta3*temp$qkprev
matplot(rownames(temp),temp[c("debtburden","capacity","finance","gk")]-tempf[c("debtburden","capacity","finance","gk")],col=2:5,type='l',lty=c(1,1,1,1,2,2,2,2),lwd=2,main="factors of brown growth",xlab="",ylab="")
abline(h=0,col=1)
abline(v=63,col=1)
grid()
legend("bottomleft",col=2:5,lwd=2,bty='n',legend=c("debtburden","capacity","finance","growth"))
legend(50,-0.05,col=1,lwd=2,lty=1:2,bty='n',legend=c("success","failure"))
```

```{r, echo=FALSE}
tempf<-fail[40:63,c("shareki","sharekk")]
temp<-suc[40:63,c("shareki","sharekk")]
# temp$pie<-temp$pie/temp$pie[1]
# temp$pke<-temp$pke/temp$pke[1]
# temp$pce<-temp$pce/temp$pce[1]
matplot(rownames(temp),cbind(temp,tempf),type='l',lty=c(1,1,2,2),lwd=2,ylab="",xlab="",col=1:2)
abline(h=0)
abline(v=63,col=1)
legend("bottomleft",col=1:5,lwd=2,bty='n',legend=colnames(temp))

kable(cbind(temp,tempf))
```

```{r, echo=FALSE}
tempf<-fail[40:63,c("kkgre","kigre","iigre","kigr")]
temp<-suc[40:63,c("kkgre","kigre","iigre","kigr")]
# temp$pie<-temp$pie/temp$pie[1]
# temp$pke<-temp$pke/temp$pke[1]
# temp$pce<-temp$pce/temp$pce[1]
matplot(rownames(temp),cbind(temp,tempf),type='l',lty=c(1,1,1,2,2,2),lwd=2,ylab="",xlab="",col=1:3)
abline(h=0)
abline(v=63,col=1)
legend("bottomleft",col=1:5,lwd=2,bty='n',legend=colnames(temp))

kable(cbind(temp,tempf))
```

```{r, echo=FALSE}
tempf<-fail[40:63,c("xkk","xki","xii")]
temp<-suc[40:63,c("xkk","xki","xii")]
# temp$pie<-temp$pie/temp$pie[1]
# temp$pke<-temp$pke/temp$pke[1]
# temp$pce<-temp$pce/temp$pce[1]
matplot(rownames(temp),cbind(temp,tempf),type='l',lty=c(1,1,1,2,2,2),lwd=2,ylab="",xlab="",col=1:3)
abline(h=0)
abline(v=63,col=1)
legend("bottomleft",col=1:5,lwd=2,bty='n',legend=colnames(temp))

kable(cbind(temp,tempf))
```

Difference on Price

```{r, echo=FALSE}
temp<-fail[40:63,c("pk","phik","yke","UCke")]
temp$pk<-temp$pk/temp$pk[1]
temp$phik<-temp$phik/temp$phik[1]
temp$yke<-temp$yke/temp$yke[1]
temp$UCke<-temp$UCke/temp$UCke[1]
tempf<-temp
temp<-suc[40:63,c("pk","phik","yke","UCke")]
temp$pk<-temp$pk/temp$pk[1]
temp$phik<-temp$phik/temp$phik[1]
temp$yke<-temp$yke/temp$yke[1]
temp$UCke<-temp$UCke/temp$UCke[1]
matplot(rownames(temp),cbind(temp,tempf),col=2:5,type='l',lty=c(1,1,1,1,2,2,2,2),lwd=2,ylab="",xlab="")
abline(v=63,col=1)
legend("bottomleft",col=2:5,lwd=2,bty='n',legend=c("pk","phik","yke","UCke"))
legend("topleft",col=1,lty=1:2,lwd=2,bty='n',legend=c("Success","Failure"))
```

### Exit of brown

ypotie is below in the fail case


```{r, echo=FALSE}
temp<-fail[40:63,c("ypotce","ypotke","ypotie")]
temp$ypotke<-temp$ypotke/temp$ypotke[1]
temp$ypotie<-temp$ypotie/temp$ypotie[1]
temp$ypotce<-temp$ypotce/temp$ypotce[1]
tempf<-temp
temp<-suc[40:63,c("ypotce","ypotke","ypotie")]
temp$ypotke<-temp$ypotke/temp$ypotke[1]
temp$ypotie<-temp$ypotie/temp$ypotie[1]
temp$ypotce<-temp$ypotce/temp$ypotce[1]
matplot(rownames(temp),cbind(temp,tempf),type='l',lty=c(1,1,1,2,2,2),col=2:4,lwd=2,ylim=c(-70,70),ylab="",xlab="")
abline(h=1)
abline(v=63,col=1)
legend("bottomleft",col=2:4,lwd=2,bty='n',legend=colnames(temp))

kable(cbind(temp,tempf))
```

PVyi is below in the fail case

```{r, echo=FALSE}
temp<-fail[40:63,c("PVyc","PVyk","PVyi")]
temp$PVyk<-temp$PVyk/temp$PVyk[1]
temp$PVyi<-temp$PVyi/temp$PVyi[1]
temp$PVyc<-temp$PVyc/temp$PVyc[1]
tempf<-temp
temp<-suc[40:63,c("PVyc","PVyk","PVyi")]
temp$PVyk<-temp$PVyk/temp$PVyk[1]
temp$PVyi<-temp$PVyi/temp$PVyi[1]
temp$PVyc<-temp$PVyc/temp$PVyc[1]
matplot(rownames(temp),cbind(temp,tempf),type='l',lty=c(1,1,1,2,2,2),col=2:4,lwd=2,ylim=c(-70,70),ylab="",xlab="")
abline(h=1)
abline(v=63,col=1)
legend("bottomleft",col=2:4,lwd=2,bty='n',legend=colnames(temp))
```

But somehow pie is above in the fail case

```{r, echo=FALSE}
tempf<-fail[40:63,c("pce","pke","pie")]
temp<-suc[40:63,c("pce","pke","pie")]
# temp$pie<-temp$pie/temp$pie[1]
# temp$pke<-temp$pke/temp$pke[1]
# temp$pce<-temp$pce/temp$pce[1]
matplot(rownames(temp),cbind(temp,tempf),type='l',lty=c(1,1,1,2,2,2),lwd=2,ylab="",xlab="",col=1:3)
abline(h=0)
abline(v=63,col=1)
legend("bottomleft",col=1:5,lwd=2,bty='n',legend=colnames(temp))
```

This is due to higher wealth in both households sectors (needs to be investigated)

```{r, echo=FALSE}
tempf<-fail[40:63,c("Vw","Vc","Vec")]
temp<-suc[40:63,c("Vw","Vc","Vec")]
# temp$pie<-temp$pie/temp$pie[1]
# temp$pke<-temp$pke/temp$pke[1]
# temp$pce<-temp$pce/temp$pce[1]
matplot(rownames(temp),cbind(temp,tempf),type='l',lty=c(1,1,1,2,2,2),lwd=2,ylab="",xlab="",col=1:3)
abline(h=0)
abline(v=63,col=1)
legend("bottomleft",col=1:5,lwd=2,bty='n',legend=colnames(temp))
```


## Green sector

Fail case shows higher growth in the green sector, mainly due to higher q.

```{r, echo=FALSE}
temp<-fail[40:63,c("lambdaiprev","rrli","qiprev","uie","rli","eta2","eta1","eta3","eta0i","gi")]
temp$debtburden<--temp$eta2*temp$rrli*temp$lambdaiprev
temp$capacity<-temp$eta1*temp$uie
temp$finance<-temp$eta3*temp$qiprev
tempf<-temp
temp<-suc[40:63,c("lambdaiprev","rrli","qiprev","uie","rli","eta2","eta1","eta3","eta0i","gi")]
temp$debtburden<--temp$eta2*temp$rrli*temp$lambdaiprev
temp$capacity<-temp$eta1*temp$uie
temp$finance<-temp$eta3*temp$qiprev
matplot(rownames(temp),cbind(temp[c("debtburden","capacity","finance","gi")],tempf[c("debtburden","capacity","finance","gi")]),col=2:5,type='l',lty=c(1,1,1,1,2,2,2,2),lwd=2,main="factors of green growth",xlab="",ylab="",ylim=c(-0.01,0.32))
abline(h=0,col=1)
abline(v=63,col=1)
grid()
legend("center",col=2:5,lwd=2,bty='n',legend=c("debtburden","capacity","finance","growth"))
```


# Appendix

## before IPO graphs

> The following graphs show the difference between the failing case (less blindness) and the succesfull case (more blindness). Not sure we need these but showing them FYI

> Growth rate in brown


```{r, echo=FALSE}
temp<-fail[21:41,c("lambdakprev","rrlk","qkprev","uke","rlk","eta2","eta1","eta3","eta0k","gk")]
temp$debtburden<--temp$eta2*temp$rrlk*temp$lambdakprev
# temp$pik<-temp$pik/temp$pik[1]
temp$capacity<-temp$eta1*temp$uk
temp$finance<-temp$eta3*temp$qkprev
tempf<-temp
temp<-suc[21:41,c("lambdakprev","rrlk","qkprev","uke","rlk","eta2","eta1","eta3","eta0k","gk")]
temp$debtburden<--temp$eta2*temp$rrlk*temp$lambdakprev
# temp$pik<-temp$pik/temp$pik[1]
temp$capacity<-temp$eta1*temp$uk
temp$finance<-temp$eta3*temp$qkprev
matplot(rownames(temp),temp[c("debtburden","capacity","finance","gk")]-tempf[c("debtburden","capacity","finance","gk")],col=2:5,type='l',lwd=2,main="Difference in factors of brown growth",xlab="",ylab="",lty=c(1,1,1,1,2,2,2,2))
abline(h=0,col=1)
grid()
legend("bottomleft",col=2:5,lwd=2,bty='n',legend=c("debtburden","capacity","finance","growth"))
# legend(20,0.03,col=1,lwd=2,lty=1:2,bty='n',legend=c("success","failure"))
```

> Growth rate in consumption

```{r, echo=FALSE}
temp<-fail[21:41,c("lambdacprev","rrlc","qcprev","uce","rlc","eta2","eta1","eta3","eta0i","gc")]
temp$debtburden<--temp$eta2*temp$rrlc*temp$lambdacprev
temp$capacity<-temp$eta1*temp$uce
temp$finance<-temp$eta3*temp$qcprev
tempf<-temp
temp<-suc[21:41,c("lambdacprev","rrlc","qcprev","uce","rlc","eta2","eta1","eta3","eta0i","gc")]
temp$debtburden<--temp$eta2*temp$rrlc*temp$lambdacprev
temp$capacity<-temp$eta1*temp$uce
temp$finance<-temp$eta3*temp$qcprev
matplot(rownames(temp),temp[c("debtburden","capacity","finance","gc")]-tempf[c("debtburden","capacity","finance","gc")],col=2:5,type='l',lty=c(1,1,1,1,2,2,2,2),lwd=2,main="factors of consumption growth",xlab="",ylab="")
abline(h=0,col=1)
abline(v=63,col=1)
grid()
legend("topleft",col=2:5,lwd=2,bty='n',legend=c("debtburden","capacity","finance","growth"))
```

> Output in consumption sector

```{r, echo=FALSE}
temp<-fail[21:41,c("yc","cc","cw")]
temp$yc<-temp$yc/temp$yc[1]
temp$cc<-temp$cc/temp$cc[1]
temp$cw<-temp$cw/temp$cw[1]
tempf<-temp
temp<-suc[21:41,c("yc","cc","cw")]
temp$yc<-temp$yc/temp$yc[1]
temp$cc<-temp$cc/temp$cc[1]
temp$cw<-temp$cw/temp$cw[1]
matplot(rownames(temp),temp-tempf,type='l',lty=c(1,1,1,2,2,2),lwd=2,ylab="",xlab="",col=2:4)
abline(h=0)
grid()
legend("bottomleft",col=2:4,lwd=2,bty='n',legend=colnames(temp))
```

> Labor income and consumption decision

```{r, echo=FALSE}
temp<-fail[21:41,c("cw","ydwe","pc","ydw","Ntot")]
temp$ydwe<-temp$ydwe/temp$ydwe[1]
temp$cw<-temp$cw/temp$cw[1]
temp$pc<-temp$pc/temp$pc[1]
temp$ydw<-temp$ydw/temp$ydw[1]
temp$Ntot<-temp$Ntot/temp$Ntot[1]
tempf<-temp
temp<-suc[21:41,c("cw","ydwe","pc","ydw","Ntot")]
temp$ydwe<-temp$ydwe/temp$ydwe[1]
temp$cw<-temp$cw/temp$cw[1]
temp$pc<-temp$pc/temp$pc[1]
temp$ydw<-temp$ydw/temp$ydw[1]
temp$Ntot<-temp$Ntot/temp$Ntot[1]
matplot(rownames(temp),temp-tempf,type='l',lty=c(1,1,1,1,1,2,2,2,2,2),lwd=2,ylab="",xlab="",col=2:6)
abline(h=0)
grid()
legend("bottomleft",col=2:6,lwd=2,bty='n',legend=colnames(temp))
```

> capitalist income and consumption decision

```{r, echo=FALSE}
temp<-fail[21:41,c("cc","ydce","ydc","vc","pc","cg")]
temp$ydc<-temp$ydc/temp$ydc[1]
temp$ydce<-temp$ydce/temp$ydce[1]
temp$cc<-temp$cc/temp$cc[1]
temp$pc<-temp$pc/temp$pc[1]
temp$vc<-temp$vc/(temp$vc[1])
temp$cg<-1+temp$cg/(10e16*temp$cg[1])
tempf<-temp
temp<-suc[21:41,c("cc","ydce","ydc","vc","pc","cg")]
temp$ydc<-temp$ydc/temp$ydc[1]
temp$ydce<-temp$ydce/temp$ydce[1]
temp$cc<-temp$cc/temp$cc[1]
temp$pc<-temp$pc/temp$pc[1]
temp$vc<-temp$vc/(temp$vc[1])
temp$cg<-1+temp$cg/(10e16*temp$cg[1])
matplot(rownames(temp),temp-tempf,type='l',lty=c(1,1,1,1,1,1,2,2,2,2,2,2),col=2:7,lwd=2,ylab="",xlab="")
abline(h=0)
grid()
legend("bottomleft",col=2:7,lwd=2,bty='n',legend=c("cc","ydce","ydc","vc","pc","cg (divided by 10e16)"))
```

> Dividends distribution


```{r, echo=FALSE}
tempf<-fail[21:41,c("FDb","FDc","FDk","FDi")]
temp<-suc[21:41,c("FDb","FDc","FDk","FDi")]
matplot(rownames(temp),temp-tempf,type='l',lty=c(1,1,1,1,2,2,2,2),lwd=2,ylab="",xlab="",col=2:5)
abline(h=0)
grid()
legend("bottomleft",col=2:5,lwd=2,bty='n',legend=c("FDb","FDc","FDk","FDi"))
```

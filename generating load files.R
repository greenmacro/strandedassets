temp<-bas[1:80,c("Yc","Yk","Yi")]
temp$GDP<-temp$Yc+temp$Yk+temp$Yi
temp$Ykgr<-c(NA,(temp$Yk[-1]-temp$Yk[-80]))
temp$Ycgr<-c(NA,(temp$Yc[-1]-temp$Yc[-80]))
temp$Yigr<-c(NA,(temp$Yi[-1]-temp$Yi[-80]))
temp$GDPgr<-c(NA,(temp$GDP[-1]-temp$GDP[-80]))


temppos<-temp[20:80,c("Ycgr","Ykgr","Yigr")]
temppos[temppos<0]<-0
tempneg<-temp[20:80,c("Ycgr","Ykgr","Yigr")]
tempneg[tempneg>0]<-0


df.bar<-barplot(t(as.matrix(temppos)), main="", xlab="",ylim=range(temp[20:80,c("Ycgr","Ykgr","Yigr")]),col=2:4)
df.bar<-barplot(t(as.matrix(tempneg)), main="", xlab="",add=TRUE,col=2:4)
points(x = df.bar, y=temp$GDPgr[20:80],lwd=2,pch=5)
legend("bottomleft",legend=c("Ycgr","Ykgr","Yigr"),bty='n',col=c(1:3),fill=c(rainbow(3)))
legend("bottomright",legend=c("GDPGr"),bty='n',col=c(1),pch=5)

View(temp[58:60,])

matplot()
stackpoly(temp[c("Ycgr","Ykgr","Yigr")],stack=T,xaxlab=rownames(temp))
#add legend
abline(v=21,col=1,lwd=2,lty=2)
abline(v=41,col=1,lwd=2,lty=3)
abline(v=58,col=1,lwd=2)
legend("bottomleft",c("Yc","Yk","Yi"),fill=rainbow(dim(temp)[2]),bty='n')

load("datatestphi.Rdata")
fail<-as.data.frame(datatest[[27]])
suc<-as.data.frame(datatest[[34]])
save(fail,suc,file="PhiAnalysis.Rdata")

load("datatest004.Rdata")
suc<-as.data.frame(datatest[[1]])
fail<-as.data.frame(datatest[[18]])
matplot(fail[1:80,c("yc","yk","yi")],type="l")
save(fail,suc,file="CrashAnalysis.Rdata")

fail<-as.data.frame(datatest[[4]])
save(fail,suc,file="NoTransitionAnalysis.Rdata")

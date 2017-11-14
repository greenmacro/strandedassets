# This looks at capital gain expectations
library(PKSFC)
library(beepr)
source("GenCalib.R")
modelLines<-readLines("AllEquations2.r")
calibrationLines<-readLines("calibration.txt")
indexCalib<-grep("CALIBRATION",modelLines)
totModel<-c(modelLines[1:(indexCalib-1)],calibrationLines,modelLines[(indexCalib+1):length(modelLines)])
writeLines(totModel,"modelAuto.sfc")
WholeModel<-sfc.model("modelAuto.sfc")
vars<-list(c("irrational4"))
values2<-vector("list",11^2*3)
datatest2<-vector("list",11^2*3)
counter=1
for(j in seq(0,0.5,0.05)){
	for(k in seq(0.04,0.12,0.008)){
		ptm<-proc.time()
		# WholeModelTemp<-sfc.editVars(WholeModel,list(list(var="irrational2",init=j),
		# 														 list(var="irrational3",init=k)))
		# WholeModelTemp<-sfc.addScenario(model=WholeModelTemp,vars="irrational4",values=0.3,inits=50,ends=51)
		# WholeModelTemp<-sfc.addScenario(model=WholeModelTemp,vars="irrational4",values=0.1,inits=50,ends=55)
		# datatestTemp<-simulate(WholeModelTemp)
		values2[[counter]]<-c(j,k,0)
		# datatest2[[counter]]<-datatestTemp$baseline
		counter=counter+1;
		values2[[counter]]<-c(j,k,0.3)
		# datatest2[[counter]]<-datatestTemp$scenario_1
		counter=counter+1;
		values2[[counter]]<-c(j,k,0.1)
		# datatest2[[counter]]<-datatestTemp$scenario_2
		counter=counter+1;
		print(paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds"))
	}
}
beep()
save(datatest2,values2,file="datatest2.RData")
load("datatest2.RData")
# for(i in 1:length(WholeModel$blocks)){
# 	common<-colnames(origBaselin)[colnames(origBaselin)%in%WholeModel$equations[WholeModel$blocks[[i]],1]]
# 	if(length(common)>0){
# 		dataSim=as.data.frame(results[20:40,common])
# 		dataOrig=as.data.frame(origBaselin[19:39,common])
# 		colnames(dataOrig)<-paste(common,"orig",sep="_")
# 		towrite<-cbind(dataSim,dataOrig)
# 		write.csv(t(towrite),file=paste("block",i,".csv",sep=""))
# 	}
# }
# 
# 
# for(j in 2:nrow(results)){
# 	if(!complete.cases(results[j,])){
# 		print(j)
# 		break
# 	}
# }
# 
# for(i in 1:length(WholeModel$blocks)){
# 	list<-WholeModel$equations[WholeModel$blocks[[i]],1]
# 	nas<-WholeModel$blocks[[i]][which(is.na(results[j,list]))]
# 	if(length(nas)>0)
# 		print(colnames(results)[nas])
# }

#REAL OUTPUT

toplot<-c("yc","yk","yi")
filename<-"output_1.png"
listscen=c(1:9)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("yc","yk","yi")
filename<-"output_2.png"
listscen=c(10:18)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("yc","yk","yi")
filename<-"output_3.png"
listscen=c(1,3)
#jpeg(filename=filename,width=1440,height=960,pointsize = 20)
	results<-as.data.frame(datatest2[[1]])
	results2<-as.data.frame(datatest2[[3]])
	matplot(cbind(results[,toplot],results2[,toplot]),type="l",lwd=2,main="",ylab="",col=1:3,lty=c(1,1,1,2,2,2))
	abline(v=which(results$exitk==1)[1])
	grid()
dev.off()

#PRICES

toplot<-c("pc","pk","pi")
filename<-"prices_1.png"
listscen=c(1:9)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("pc","pk","pi")
filename<-"prices_2.png"
listscen=c(10:18)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("pc","pk","pi")
filename<-"prices_3.png"
listscen=c(19:27)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

#LAMBDAS

toplot<-c("lambda10a","lambda20a","lambda30a","lambda40a")
filename<-"lambda_1.png"
listscen=c(1:9)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	init=results$n[1]+results$entry[1]
	timeframe=init:ifelse(length(which(results$exitk==1))==0,length(results$exitk),which(results$exitk==1)[1])
	matplot(results$t[timeframe],results[timeframe,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	grid()
}
dev.off()

toplot<-c("lambda10a","lambda20a","lambda30a","lambda40a")
filename<-"lambda_2.png"
listscen=c(10:18)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	init=results$n[1]+results$entry[1]
	timeframe=init:ifelse(length(which(results$exitk==1))==0,length(results$exitk),which(results$exitk==1)[1])
	matplot(results$t[timeframe],results[timeframe,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	grid()
}
dev.off()

toplot<-c("lambda10a","lambda20a","lambda30a","lambda40a")
filename<-"lambda_3.png"
listscen=c(19:27)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	init=results$n[1]+results$entry[1]
	timeframe=init:ifelse(length(which(results$exitk==1))==0,length(results$exitk),which(results$exitk==1)[1])
	matplot(results$t[timeframe],results[timeframe,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	grid()
}
dev.off()

#EQUITY PRICES

toplot<-c("pce","pke","pie")
filename<-"Equityprices_1.png"
listscen=c(1:9)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("pce","pke","pie")
filename<-"Equityprices_2.png"
listscen=c(10:18)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("pce","pke","pie")
filename<-"Equityprices_3.png"
listscen=c(19:27)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

#GROWTH

toplot<-c("gc","gk","gi")
filename<-"growth_1.png"
listscen=c(1:9)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="")
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("gc","gk","gi")
filename<-"growth_2.png"
listscen=c(10:18)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="")
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("gc","gk","gi")
filename<-"growth_3.png"
listscen=c(19:27)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="")
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

#EMPLOYMENT

toplot<-c("Nc","Nk","Ni")
filename<-"employment_1.png"
listscen=c(1:9)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="")
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("Nc","Nk","Ni")
filename<-"employment_2.png"
listscen=c(10:18)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="")
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

toplot<-c("Nc","Nk","Ni")
filename<-"employment_3.png"
listscen=c(19:27)
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="")
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

shocks<-as.data.frame(matrix(nrow=length(datatest),ncol=14,dimnames = list(values,c("EmplLength","EmplSize","OutputLength","OutputSize","FinancialVolatility","OutputVolatility","ExitP","ValueStranded","QuantityStranded","LoansDef","FinancialCap","param1","param2","param3"))))
for(j in 1:length(datatest)){
	results<-as.data.frame(datatest[[j]])
	if(length(exitPeriod<-which(results$exitk==1))>0&sum(is.na(results$yc))==0){
		exitPeriod<-which(results$exitk==1)[1]
		shocks$EmplLength[j]=0
		shocks$OutputLength[j]=0
		initEmpl=results$Ntot[exitPeriod]
		initOutput=results$yc[exitPeriod]
		endEmpl=results$Ntot[exitPeriod]
		endOutput=results$yc[exitPeriod]
		stopEmpl=F
		stopOutput=F
		for(i in (exitPeriod+1):length(results$exitk)){
			empl<-results$Ntot[i]
			output<-results$yc[i]
			if(empl<endEmpl){
				endEmpl<-empl
				shocks$EmplLength[j]<-shocks$EmplLength[j]+1
			}else{
				stopEmpl=T
			}
			if(output<endOutput){
				endOutput<-output
			}else{
				stopOutput=T
				shocks$OutputLength[j]<-shocks$OutputLength[j]+1
			}
			if(stopOutput&stopEmpl){
				break
			}
		}
		shocks$OutputSize[j]=(initOutput-endOutput)/endOutput
		shocks$EmplSize[j]=(initEmpl-endEmpl)/endEmpl
		shocks$FinancialVolatility[j]=(sd(results$pce)/mean(results$pce)+sd(results$pke)/mean(results$pke)+sd(results$pie)/mean(results$pie))/3
		shocks$OutputVolatility[j]=(sd(results$yc)/mean(results$yc))#+sd(results$yk)/mean(results$yk)+sd(results$yi)/mean(results$yi))/3
		shocks$ExitP[j]=exitPeriod
		shocks$ValueStranded[j]=results$kk[exitPeriod]*results$pk[exitPeriod]
		shocks$QuantityStranded[j]=results$kk[exitPeriod]
		shocks$FinancialCap[j]=results$pke[exitPeriod]*results$ek[exitPeriod]
		shocks$LoansDef[j]=results$Lk[exitPeriod-1]
		shocks$param1[j]<-values[[j]][1]
		shocks$param2[j]<-values[[j]][2]
		shocks$param3[j]<-values[[j]][3]
	}
}
shocks$col1<-(1+shocks$param1/0.1)
shocks$col2<-(1+shocks$param2/0.05)
shocks$col3<-1+(shocks$param3-0.04)/0.008



filename="plots/param1vscol2.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param1,shocks$EmplSize,xlab="Apathy",ylab="",main="Employment Size",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$OutputSize,xlab="Apathy",ylab="",main="Output Size",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$EmplLength,xlab="Apathy",ylab="",main="Employment Length",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$ExitP,xlab="Apathy",ylab="",main="Exit Period",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$FinancialVolatility,xlab="Apathy",ylab="",main="Financial Volatility",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$OutputVolatility,xlab="Apathy",ylab="",main="Output Volatility",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$ValueStranded,xlab="Apathy",ylab="",main="Value Real Capital",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$FinancialCap,xlab="Apathy",ylab="",main="Value Financial Capital",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$LoansDef,xlab="Apathy",ylab="",main="Non Performing Loans",col=shocks$col2,pch=19)
dev.off()

filename="plots/param1vscol3.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param1,shocks$EmplSize,xlab="Apathy",ylab="",main="Employment Size",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$OutputSize,xlab="Apathy",ylab="",main="Output Size",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$EmplLength,xlab="Apathy",ylab="",main="Employment Length",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$ExitP,xlab="Apathy",ylab="",main="Exit Period",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$FinancialVolatility,xlab="Apathy",ylab="",main="Financial Volatility",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$OutputVolatility,xlab="Apathy",ylab="",main="Output Volatility",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$ValueStranded,xlab="Apathy",ylab="",main="Value Real Capital",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$FinancialCap,xlab="Apathy",ylab="",main="Value Financial Capital",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$LoansDef,xlab="Apathy",ylab="",main="Non Performing Loans",col=shocks$col3,pch=19)
dev.off()


filename="plots/param2vscol1.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param2,shocks$EmplSize,xlab="Information",ylab="",main="Employment Size",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$OutputSize,xlab="Information",ylab="",main="Output Size",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$EmplLength,xlab="Information",ylab="",main="Employment Length",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$ExitP,xlab="Information",ylab="",main="Exit Period",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$FinancialVolatility,xlab="Information",ylab="",main="Financial Volatility",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$OutputVolatility,xlab="Information",ylab="",main="Output Volatility",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$ValueStranded,xlab="Information",ylab="",main="Value Real Capital",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$FinancialCap,xlab="Information",ylab="",main="Value Financial Capital",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$LoansDef,xlab="Information",ylab="",main="Non Performing Loans",col=shocks$col1,pch=19)
dev.off()

filename="plots/param2vscol3.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param2,shocks$EmplSize,xlab="Information",ylab="",main="Employment Size",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$OutputSize,xlab="Information",ylab="",main="Output Size",col=shocks$col3,pch=19,ylim=c(0,0.61))
plot(shocks$param2,shocks$EmplLength,xlab="Information",ylab="",main="Employment Length",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$ExitP,xlab="Information",ylab="",main="Exit Period",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$FinancialVolatility,xlab="Information",ylab="",main="Financial Volatility",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$OutputVolatility,xlab="Information",ylab="",main="Output Volatility",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$ValueStranded,xlab="Information",ylab="",main="Value Real Capital",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$FinancialCap,xlab="Information",ylab="",main="Value Financial Capital",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$LoansDef,xlab="Information",ylab="",main="Non Performing Loans",col=shocks$col3,pch=19)
dev.off()


filename="plots/param3vscol1.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)

plot(shocks$param3,shocks$EmplSize,xlab="Trust",ylab="",main="Employment Size",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$OutputSize,xlab="Trust",ylab="",main="Output Size",col=shocks$col1,pch=19,ylim=c(0,0.61))
plot(shocks$param3,shocks$EmplLength,xlab="Trust",ylab="",main="Employment Length",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$ExitP,xlab="Trust",ylab="",main="Exit Period",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$FinancialVolatility,xlab="Trust",ylab="",main="Financial Volatility",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$OutputVolatility,xlab="Trust",ylab="",main="Output Volatility",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$ValueStranded,xlab="Trust",ylab="",main="Value Real Capital",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$FinancialCap,xlab="Trust",ylab="",main="Value Financial Capital",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$LoansDef,xlab="Trust",ylab="",main="Non Performing Loans",col=shocks$col1,pch=19)
dev.off()

filename="plots/param3vscol2.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)

plot(shocks$param3,shocks$EmplSize,xlab="Trust",ylab="",main="Employment Size",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$OutputSize,xlab="Trust",ylab="",main="Output Size",col=shocks$col2,pch=19,ylim=c(0,0.61))
plot(shocks$param3,shocks$EmplLength,xlab="Trust",ylab="",main="Employment Length",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$ExitP,xlab="Trust",ylab="",main="Exit Period",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$FinancialVolatility,xlab="Trust",ylab="",main="Financial Volatility",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$OutputVolatility,xlab="Trust",ylab="",main="Output Volatility",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$ValueStranded,xlab="Trust",ylab="",main="Value Real Capital",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$FinancialCap,xlab="Trust",ylab="",main="Value Financial Capital",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$LoansDef,xlab="Trust",ylab="",main="Non Performing Loans",col=shocks$col2,pch=19)
dev.off()

matplot(results$t[timeframe],datatoplot[timeframe,],type="l",lwd=2,lty=1,main="Scenario 8 - 1",ylab="",xlab="")
legend("bottomright",col=1:8,lwd=2,lty=1,bty='n',legend=paste("Scenario",1:8))
# matplot(results[,c("pc","pi","pk")],type="l",lwd=2,lty=1)
# matplot(results[,c("NUCc","NUCi","NUCk")],type="l",lwd=2,lty=1)
# matplot(results[,c("phic","phii","phik")],type="l",lwd=2,lty=1)
# matplot(results[,c("yce","yie","yke")],type="l",lwd=2,lty=1)
# 
# time=100:130
# 
# results[time,c("yi","inci","inii","gi","uie","lambdaiprev","qiprev")]
# 
# write.csv(results[100:130,],file="towrite.csv")
# 
# write.csv(WholeModel$equations,file="equ.csv")
# write.csv(WholeModel$endogenous,file="end.csv")
# 
# results[2,WholeModel$equations[WholeModel$blocks[[2]],1]]
# 
# eq<-WholeModel$equations
# end<-WholeModel$endogenous
# 
# modelLines<-readLines("ShortModel.R")
# calibrationLines<-readLines("calibration.txt")
# indexCalib<-grep("CALIBRATION",modelLines)
# totModel<-c(modelLines[1:(indexCalib-1)],calibrationLines,modelLines[(indexCalib+1):length(modelLines)])
# writeLines(totModel,"modelAuto.sfc")
# WholeModel<-sfc.model("modelAuto.sfc")
# #plot_graph_hierarchy(WholeModel,main="Stranded Assets")
# datatest<-simulate(WholeModel)
shocks2<-as.data.frame(matrix(nrow=length(datatest2),ncol=14,dimnames = list(values2,c("EmplLength","EmplSize","OutputLength","OutputSize","FinancialVolatility","OutputVolatility","ExitP","ValueStranded","QuantityStranded","LoansDef","FinancialCap","param4","param2","param3"))))
for(j in 1:length(datatest2)){
	results<-as.data.frame(datatest2[[j]])
	if(length(exitPeriod<-which(results$exitk==1))>0&sum(is.na(results$yc))==0){
		exitPeriod<-which(results$exitk==1)[1]
		shocks2$EmplLength[j]=0
		shocks2$OutputLength[j]=0
		initEmpl=results$Ntot[exitPeriod]
		initOutput=results$yc[exitPeriod]
		endEmpl=results$Ntot[exitPeriod]
		endOutput=results$yc[exitPeriod]
		stopEmpl=F
		stopOutput=F
		for(i in (exitPeriod+1):length(results$exitk)){
			empl<-results$Ntot[i]
			output<-results$yc[i]
			if(empl<endEmpl){
				endEmpl<-empl
				shocks2$EmplLength[j]<-shocks2$EmplLength[j]+1
			}else{
				stopEmpl=T
			}
			if(output<endOutput){
				endOutput<-output
			}else{
				stopOutput=T
				shocks2$OutputLength[j]<-shocks2$OutputLength[j]+1
			}
			if(stopOutput&stopEmpl){
				break
			}
		}
		shocks2$OutputSize[j]=(initOutput-endOutput)/endOutput
		shocks2$EmplSize[j]=(initEmpl-endEmpl)/endEmpl
		shocks2$FinancialVolatility[j]=(sd(results$pce)/mean(results$pce)+sd(results$pke)/mean(results$pke)+sd(results$pie)/mean(results$pie))/3
		shocks2$OutputVolatility[j]=(sd(results$yc)/mean(results$yc))#+sd(results$yk)/mean(results$yk)+sd(results$yi)/mean(results$yi))/3
		shocks2$ExitP[j]=exitPeriod
		shocks2$ValueStranded[j]=results$kk[exitPeriod]*results$pk[exitPeriod]
		shocks2$QuantityStranded[j]=results$kk[exitPeriod]
		shocks2$FinancialCap[j]=results$pke[exitPeriod]*results$ek[exitPeriod]
		shocks2$LoansDef[j]=results$Lk[exitPeriod-1]
		shocks2$param4[j]<-values2[[j]][1]
		shocks2$param2[j]<-values2[[j]][2]
		shocks2$param3[j]<-values2[[j]][3]
	}
}
shocks2$col1<-ifelse(shocks2$param4==0,0,ifelse(shocks2$param4==0.1,1,2))
shocks2$col2<-(1+shocks2$param2/0.05)
shocks2$col3<-1+(shocks2$param3-0.04)/0.008



filename="plots/param1vscol2.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param1,shocks$EmplSize,xlab="Apathy",ylab="",main="Employment Size",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$OutputSize,xlab="Apathy",ylab="",main="Output Size",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$EmplLength,xlab="Apathy",ylab="",main="Employment Length",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$ExitP,xlab="Apathy",ylab="",main="Exit Period",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$FinancialVolatility,xlab="Apathy",ylab="",main="Financial Volatility",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$OutputVolatility,xlab="Apathy",ylab="",main="Output Volatility",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$ValueStranded,xlab="Apathy",ylab="",main="Value Real Capital",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$FinancialCap,xlab="Apathy",ylab="",main="Value Financial Capital",col=shocks$col2,pch=19)
plot(shocks$param1,shocks$LoansDef,xlab="Apathy",ylab="",main="Non Performing Loans",col=shocks$col2,pch=19)
dev.off()

filename="plots/param1vscol3.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param1,shocks$EmplSize,xlab="Apathy",ylab="",main="Employment Size",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$OutputSize,xlab="Apathy",ylab="",main="Output Size",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$EmplLength,xlab="Apathy",ylab="",main="Employment Length",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$ExitP,xlab="Apathy",ylab="",main="Exit Period",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$FinancialVolatility,xlab="Apathy",ylab="",main="Financial Volatility",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$OutputVolatility,xlab="Apathy",ylab="",main="Output Volatility",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$ValueStranded,xlab="Apathy",ylab="",main="Value Real Capital",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$FinancialCap,xlab="Apathy",ylab="",main="Value Financial Capital",col=shocks$col3,pch=19)
plot(shocks$param1,shocks$LoansDef,xlab="Apathy",ylab="",main="Non Performing Loans",col=shocks$col3,pch=19)
dev.off()


filename="plots/param2vscol1.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param2,shocks$EmplSize,xlab="Information",ylab="",main="Employment Size",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$OutputSize,xlab="Information",ylab="",main="Output Size",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$EmplLength,xlab="Information",ylab="",main="Employment Length",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$ExitP,xlab="Information",ylab="",main="Exit Period",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$FinancialVolatility,xlab="Information",ylab="",main="Financial Volatility",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$OutputVolatility,xlab="Information",ylab="",main="Output Volatility",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$ValueStranded,xlab="Information",ylab="",main="Value Real Capital",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$FinancialCap,xlab="Information",ylab="",main="Value Financial Capital",col=shocks$col1,pch=19)
plot(shocks$param2,shocks$LoansDef,xlab="Information",ylab="",main="Non Performing Loans",col=shocks$col1,pch=19)
dev.off()

filename="plots/param2vscol3.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param2,shocks$EmplSize,xlab="Information",ylab="",main="Employment Size",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$OutputSize,xlab="Information",ylab="",main="Output Size",col=shocks$col3,pch=19,ylim=c(0,0.61))
plot(shocks$param2,shocks$EmplLength,xlab="Information",ylab="",main="Employment Length",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$ExitP,xlab="Information",ylab="",main="Exit Period",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$FinancialVolatility,xlab="Information",ylab="",main="Financial Volatility",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$OutputVolatility,xlab="Information",ylab="",main="Output Volatility",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$ValueStranded,xlab="Information",ylab="",main="Value Real Capital",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$FinancialCap,xlab="Information",ylab="",main="Value Financial Capital",col=shocks$col3,pch=19)
plot(shocks$param2,shocks$LoansDef,xlab="Information",ylab="",main="Non Performing Loans",col=shocks$col3,pch=19)
dev.off()


filename="plots/param3vscol1.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)

plot(shocks$param3,shocks$EmplSize,xlab="Trust",ylab="",main="Employment Size",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$OutputSize,xlab="Trust",ylab="",main="Output Size",col=shocks$col1,pch=19,ylim=c(0,0.61))
plot(shocks$param3,shocks$EmplLength,xlab="Trust",ylab="",main="Employment Length",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$ExitP,xlab="Trust",ylab="",main="Exit Period",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$FinancialVolatility,xlab="Trust",ylab="",main="Financial Volatility",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$OutputVolatility,xlab="Trust",ylab="",main="Output Volatility",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$ValueStranded,xlab="Trust",ylab="",main="Value Real Capital",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$FinancialCap,xlab="Trust",ylab="",main="Value Financial Capital",col=shocks$col1,pch=19)
plot(shocks$param3,shocks$LoansDef,xlab="Trust",ylab="",main="Non Performing Loans",col=shocks$col1,pch=19)
dev.off()

filename="plots/param3vscol2.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)

plot(shocks$param3,shocks$EmplSize,xlab="Trust",ylab="",main="Employment Size",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$OutputSize,xlab="Trust",ylab="",main="Output Size",col=shocks$col2,pch=19,ylim=c(0,0.61))
plot(shocks$param3,shocks$EmplLength,xlab="Trust",ylab="",main="Employment Length",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$ExitP,xlab="Trust",ylab="",main="Exit Period",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$FinancialVolatility,xlab="Trust",ylab="",main="Financial Volatility",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$OutputVolatility,xlab="Trust",ylab="",main="Output Volatility",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$ValueStranded,xlab="Trust",ylab="",main="Value Real Capital",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$FinancialCap,xlab="Trust",ylab="",main="Value Financial Capital",col=shocks$col2,pch=19)
plot(shocks$param3,shocks$LoansDef,xlab="Trust",ylab="",main="Non Performing Loans",col=shocks$col2,pch=19)
dev.off()

library(PKSFC)
library(beepr)
library(knitr)
source("GenCalib.R") # Generates calibration.txt built around steady-state model.
# Generate the model equations from the R markdown document
purl('model_doc_v2.Rmd',documentation=0,output='modeleqns_fromdoc.R')
modelLines<-readLines("modeleqns_fromdoc.R")
calibrationLines<-readLines("calibration.txt")
indexCalib<-grep("CALIBRATION",modelLines)
totModel<-c(modelLines[1:(indexCalib-1)],calibrationLines,modelLines[(indexCalib+1):length(modelLines)])
writeLines(totModel,"modelAuto.sfc")
WholeModel<-sfc.model("modelAuto.sfc")
vars<-list(c("irrational","irrational2","irrational3"))
values<-list(
	c(0,0,0.08),
	c(0,0.2,0.08),
	c(0,0.5,0.08),
	c(0,0,0.04),
	c(0,0.2,0.04),
	c(0,0.5,0.04),
	c(0,0,0.12),
	c(0,0.2,0.12),
	c(0,0.5,0.12),
	c(0.5,0,0.08),
	c(0.5,0.2,0.08),
	c(0.5,0.5,0.08),
	c(0.5,0,0.04),
	c(0.5,0.2,0.04),
	c(0.5,0.5,0.04),
	c(0.5,0,0.12),
	c(0.5,0.2,0.12),
	c(0.5,0.5,0.12),
	c(1,0,0.08),
	c(1,0.2,0.08),
	c(1,0.5,0.08),
	c(1,0,0.04),
	c(1,0.2,0.04),
	c(1,0.5,0.04),
	c(1,0,0.12),
	c(1,0.2,0.12),
	c(1,0.5,0.12)
)
#for(i in 1:(length(values)-1)){
# While testing, only 4 scenarios
for(i in 1:4){
	WholeModel<-sfc.addScenario(model=WholeModel,vars=vars,values=list(values[[i]]),inits=2,ends=500)
}

#plot_graph_hierarchy(WholeModel,main="Stranded Assets")
datatest<-simulate(WholeModel) # A list with dataframes, one for each scenario
beep()

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

# Four-scenario display
toplot<-c("yc","yk","yi")
filename<-"output_1.png"
listscen=c(1:4) # These scenarios
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:4),2,2,byrow = T))
layout.show(4)
for(i in listscen){
	results<-as.data.frame(datatest[[i]])
	matplot(results[,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(i-1),"- (",paste(values[[i]],collapse=','),")"),ylab="",ylim=c(0,max(results[,toplot],na.rm=T)))
	abline(v=which(results$exitk==1)[1])
	grid()
}
dev.off()

# Nine-scenario display
toplot<-c("yc","yk","yi")
filename<-"output_1.png"
listscen=c(1:9) # These scenarios
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

filename="param1.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param1,shocks$EmplSize,xlab="Apathy",ylab="",main="Employment Size")
plot(shocks$param1,shocks$OutputSize,xlab="Apathy",ylab="",main="Output Size")
plot(shocks$param1,shocks$EmplLength,xlab="Apathy",ylab="",main="Employment Length")
plot(shocks$param1,shocks$ExitP,xlab="Apathy",ylab="",main="Exit Period")
plot(shocks$param1,shocks$FinancialVolatility,xlab="Apathy",ylab="",main="Financial Volatility")
plot(shocks$param1,shocks$OutputVolatility,xlab="Apathy",ylab="",main="Output Volatility")
plot(shocks$param1,shocks$ValueStranded,xlab="Apathy",ylab="",main="Value Real Capital")
plot(shocks$param1,shocks$FinancialCap,xlab="Apathy",ylab="",main="Value Financial Capital")
plot(shocks$param1,shocks$LoansDef,xlab="Apathy",ylab="",main="Non Performing Loans")
dev.off()

filename="param2.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param2,shocks$EmplSize,xlab="Information",ylab="",main="Employment Size")
plot(shocks$param2,shocks$OutputSize,xlab="Information",ylab="",main="Output Size")
plot(shocks$param2,shocks$EmplLength,xlab="Information",ylab="",main="Employment Length")
plot(shocks$param2,shocks$ExitP,xlab="Information",ylab="",main="Exit Period")
plot(shocks$param2,shocks$FinancialVolatility,xlab="Information",ylab="",main="Financial Volatility")
plot(shocks$param2,shocks$OutputVolatility,xlab="Information",ylab="",main="Output Volatility")
plot(shocks$param2,shocks$ValueStranded,xlab="Information",ylab="",main="Value Real Capital")
plot(shocks$param2,shocks$FinancialCap,xlab="Information",ylab="",main="Value Financial Capital")
plot(shocks$param2,shocks$LoansDef,xlab="Information",ylab="",main="Non Performing Loans")
dev.off()

filename="param3.png"
jpeg(filename=filename,width=1440,height=960,pointsize = 20)
layout(matrix(c(1:9),3,3,byrow = T))
layout.show(9)
plot(shocks$param3,shocks$EmplSize,xlab="Trust",ylab="",main="Employment Size")
plot(shocks$param3,shocks$OutputSize,xlab="Trust",ylab="",main="Output Size")
plot(shocks$param3,shocks$EmplLength,xlab="Trust",ylab="",main="Employment Length")
plot(shocks$param3,shocks$ExitP,xlab="Trust",ylab="",main="Exit Period")
plot(shocks$param3,shocks$FinancialVolatility,xlab="Trust",ylab="",main="Financial Volatility")
plot(shocks$param3,shocks$OutputVolatility,xlab="Trust",ylab="",main="Output Volatility")
plot(shocks$param3,shocks$ValueStranded,xlab="Trust",ylab="",main="Value Real Capital")
plot(shocks$param3,shocks$FinancialCap,xlab="Trust",ylab="",main="Value Financial Capital")
plot(shocks$param3,shocks$LoansDef,xlab="Trust",ylab="",main="Non Performing Loans")
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

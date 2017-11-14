library(PKSFC)
library(beepr)
library(akima)
library(lattice)
library(DiceKriging)
library(DiceEval)
library(DiceOptim)
library(rgenoud)
library(sensitivity)
library(plot3Drgl)
library(knitr)

#purl('Model Explanatory doc 09_October.Rmd',documentation=0,output='AllEquations_v3.0.R')

source("GenCalib_v3.0.R") 
modelLines<-readLines("AllEquations_v3.0.R")
calibrationLines<-readLines("calibration.txt")
indexCalib<-grep("#CALIBRATION",modelLines)
totModel<-c(modelLines[1:(indexCalib-1)],calibrationLines,modelLines[(indexCalib+1):length(modelLines)])
writeLines(totModel,"modelAuto.sfc")
WholeModel<-sfc.model("modelAuto.sfc")
# plot_graph_hierarchy(WholeModel,main="Stranded Assets")
# databas<-simulate(WholeModel)
# beep()
# bas<-as.data.frame(databas$baseline)
# save(bas,file="baselineAnalysis.Rdata")
# matplot(results[,c("yc","yk","yi")],type="l",lwd=2,lty=1,main="",ylab="",ylim=c(0,max(results[,c("yc","yk","yi")],na.rm=T)))

DoE<-as.data.frame(read.csv("DOE_model2.2.csv"))
params<-colnames(DoE)
vars<-list(params)
values<-vector("list",nrow(DoE))
values[[1]]<-c(0,0)
for(i in 1:nrow(DoE)){
	values[[i+1]]<-c(DoE$theta[i],DoE$phi[i])
	WholeModel<-sfc.addScenario(model=WholeModel,vars=vars,values=list(values[[i+1]]),inits=2,ends=200)
}


#plot_graph_hierarchy(WholeModel,main="Stranded Assets")
datatest<-simulate(WholeModel)
save(datatest,file="datatestphi.Rdata")
beep()

# load("datatest2004.Rdata")
#Generating sensitivity results


shocks<-as.data.frame(matrix(nrow=length(datatest),ncol=14,dimnames = list(NULL,c("scen","EmplLength","EmplSize","OutputLength","OutputSize","FinancialVolatility","OutputVolatility","ExitP","ValueStranded","QuantityStranded","LoansDef","FinancialCap",params))))
for(j in 1:length(datatest)){
	shocks$scen[j]<-j
	results<-as.data.frame(datatest[[j]])
	shocks[j,params[1]]<-results[1,params[1]]
	shocks[j,params[2]]<-results[1,params[2]]
	if(length(exitPeriod<-which(results$exith==1))>0){
		exitPeriod<-which(results$exith==1)[1]
		endSim<-min(exitPeriod+40,ifelse(length(which(is.na(results$yc)))>0,(which(is.na(results$yc))[1]-1),Inf))
		results<-results[20:endSim,]
		exitPeriod<-exitPeriod-19
		shocks$EmplLength[j]=0
		shocks$OutputLength[j]=0
		initEmpl=results$Ntot[exitPeriod]
		initOutput=results$yc[exitPeriod]
		endEmpl=results$Ntot[exitPeriod]
		endOutput=results$yc[exitPeriod]
		stopEmpl=F
		stopOutput=F
		for(i in (exitPeriod+1):length(results$exith)){
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
		shocks$FinancialVolatility[j]=(sd(results$pce)/mean(results$pce)+sd(results$phe)/mean(results$phe)+sd(results$ple)/mean(results$ple))/3
		shocks$OutputVolatility[j]=(sd(results$yc)/mean(results$yc))#+sd(results$yk)/mean(results$yk)+sd(results$yi)/mean(results$yi))/3
		shocks$ExitP[j]=exitPeriod+19
		shocks$ValueStranded[j]=results$khh[exitPeriod]*results$pkh[exitPeriod]
		shocks$QuantityStranded[j]=results$khh[exitPeriod]
		shocks$FinancialCap[j]=results$phe[exitPeriod]*results$eh[exitPeriod]
		shocks$LoansDef[j]=results$Loansh[exitPeriod-1]
	}
}

shocks<-shocks[with(shocks, order(theta)), ]
values<-shocks[c("theta","phi")]

#Saving the results

for(i in 1:length(datatest)){
	write.csv(datatest[[i]],file=paste("csv/scenario_",i,".csv",sep=""))
}
write.csv(shocks,file="csv/shocks.csv")




# Plot Time Series --------------------------------------------------------

#Function to generate plots

plotScenarios<-function(toplot,datasource,name,folder,order,values){
	for(k in 1:4){
		filename<-paste("plots/",folder,name,"_",k,".jpg",sep="")
		listscen=c(((k-1)*9+1):((k-1)*9+9))
		jpeg(filename=filename,width=1440,height=960,pointsize = 20)
		layout(matrix(c(1:9),3,3,byrow = T))
		layout.show(9)
		for(i in listscen){
			if(i<length(datasource)){
				results<-as.data.frame(datasource[[order[i]]])
				exitp=ifelse(length(which(results$exith==1))>0,which(results$exith==1)[1],60)
				timeframe=(1:(exitp+40))
				matplot(results[timeframe,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(order[i]),"- (",paste(values[i,1],",",values[i,2],collapse=','),")"),ylab="",ylim=c(0,max(results[timeframe,toplot],na.rm=T)))
				abline(v=which(results$exith==1)[1])
				grid()
			}
		}
		dev.off()
	}
}

scenName<-"scenariosphi/"

plotScenarios(c("yc","yh","yl"),datatest,"Output",scenName,shocks$scen,values)
plotScenarios(c("Nc","Nh","Nl","LF"),datatest,"Employment",scenName,shocks$scen,values)
plotScenarios(c("pce","phe","ple"),datatest,"EquityPrice",scenName,shocks$scen,values)
plotScenarios(c("qc","qh","ql"),datatest,"TobinQ",scenName,shocks$scen,values)
plotScenarios(c("khc","klc","khh","kll"),datatest,"CapitalStock",scenName,shocks$scen,values)
plotScenarios(c("pc","pkh","pkl"),datatest,"Price",scenName,shocks$scen,values)
plotScenarios(c("lambda10a","lambda20a","lambda30a","lambda40a"),datatest,"Lambdas",scenName,shocks$scen,values)

# Plot Indicators by Parameter --------------------------------------------


# filename="plots/param1vscol2.png"
# jpeg(filename=filename,width=1440,height=960,pointsize = 20)
# layout(matrix(c(1:9),3,3,byrow = T))
# layout.show(9)
# plot(shocks$theta,shocks$EmplSize,xlab="Apathy",ylab="",main="Employment Size",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$OutputSize,xlab="Apathy",ylab="",main="Output Size",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$EmplLength,xlab="Apathy",ylab="",main="Employment Length",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$ExitP,xlab="Apathy",ylab="",main="Exit Period",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$FinancialVolatility,xlab="Apathy",ylab="",main="Financial Volatility",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$OutputVolatility,xlab="Apathy",ylab="",main="Output Volatility",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$ValueStranded,xlab="Apathy",ylab="",main="Value Real Capital",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$FinancialCap,xlab="Apathy",ylab="",main="Value Financial Capital",col=shocks$phi,pch=19)
# plot(shocks$theta,shocks$LoansDef,xlab="Apathy",ylab="",main="Non Performing Loans",col=shocks$phi,pch=19)
# dev.off()
# 
# filename="plots/param2vscol1.png"
# jpeg(filename=filename,width=1440,height=960,pointsize = 20)
# layout(matrix(c(1:9),3,3,byrow = T))
# layout.show(9)
# plot(shocks$param2,shocks$EmplSize,xlab="Information",ylab="",main="Employment Size",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$OutputSize,xlab="Information",ylab="",main="Output Size",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$EmplLength,xlab="Information",ylab="",main="Employment Length",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$ExitP,xlab="Information",ylab="",main="Exit Period",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$FinancialVolatility,xlab="Information",ylab="",main="Financial Volatility",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$OutputVolatility,xlab="Information",ylab="",main="Output Volatility",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$ValueStranded,xlab="Information",ylab="",main="Value Real Capital",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$FinancialCap,xlab="Information",ylab="",main="Value Financial Capital",col=shocks$col1,pch=19)
# plot(shocks$param2,shocks$LoansDef,xlab="Information",ylab="",main="Non Performing Loans",col=shocks$col1,pch=19)
# dev.off()


# Kriging -----------------------------------------------------------------

#Building up datasets

kriging.mean<-function(X,m) predict.km(m,X,"UK",se.compute = FALSE)$mean

sensitivityAnalysis<-function(indicator,foldername,resultsShock,params,plotrgl=FALSE){
	
	min<-min(resultsShock[,params[1]])
	max<-max(resultsShock[,params[1]])
	
	#Building up the X and y dataframes
	allX <- as.data.frame(resultsShock[which(!is.na(shocks$ExitP)),params])
	ally<- as.data.frame(resultsShock[which(!is.na(shocks$ExitP)),indicator])
	
	#We'll split up the whole dataset into a sample and validation sample (1/3 of the side roughly)
	all<-nrow(allX)
	valid<-round(nrow(allX)/4)
	
	X<- allX[1:(all-valid),]
	y<-ally[1:(all-valid),]
	
	XValid <- allX[(all-valid+1):all,]
	yvalid <- ally[(all-valid+1):all,]
	
	# Runing the Kriging models
	sink("convergence.txt")
	m1 <- km(~1,design=X,response=y,covtype = "matern5_2")#,noise.var = DataVar$totDist)
	m2 <- km(~.,design=X,response=y,covtype = "matern5_2")#,noise.var = DataVar$totDist)
	m3 <- km(~1,design=X,response=y,covtype = "gauss")#,noise.var = DataVar$totDist)
	m4 <- km(~.,design=X,response=y,covtype = "gauss")#,noise.var = DataVar$totDist)
	m5 <- km(~1,design=X,response=y,covtype = "exp")#,noise.var = DataVar$totDist)
	m6 <- km(~.,design=X,response=y,covtype = "exp")#,noise.var = DataVar$totDist)
	sink()
	#Testing the prediction
	
	test1<-predict(m1,newdata = XValid, type="UK")
	test2<-predict(m2,newdata = XValid, type="UK")
	test3<-predict(m3,newdata = XValid, type="UK")
	test4<-predict(m4,newdata = XValid, type="UK")
	test5<-predict(m5,newdata = XValid, type="UK")
	test6<-predict(m6,newdata = XValid, type="UK")
	
	#testing the fitness of the prediction
	
	RMSE1<-RMSE(yvalid,test1$mean)
	RMSE2<-RMSE(yvalid,test2$mean)
	RMSE3<-RMSE(yvalid,test3$mean)
	RMSE4<-RMSE(yvalid,test4$mean)
	RMSE5<-RMSE(yvalid,test5$mean)
	RMSE6<-RMSE(yvalid,test6$mean)
	
	allmodels<-c(m1,m2,m3,m4,m5,m6)
	allRMSES<-c(RMSE1,RMSE2,RMSE3,RMSE4,RMSE5,RMSE6)
	minRMSES<-which(allRMSES==min(allRMSES))[1]
	minModel<-allmodels[[minRMSES]]
	
	# SA.metamodel<-fast99(model = kriging.mean,n=18,M=2,factors=colnames(X),q.arg=list(list(min=0.6,max=1),list(min=0.6,max=1)),m=minModel)
	# 
	# plot(SA.metamodel)
	
	n.grid<-12
	x.grid<-seq(min,max,length=n.grid)
	y.grid<-seq(min,max,length=n.grid)
	X.grid<-expand.grid(x=x.grid,y=y.grid)
	
	colnames(X.grid)<-params
	
	pred.m<-predict(minModel,X.grid,"UK")
	fitpoints<-predict(minModel,allX,"UK")
	
	data<-data.frame(x = as.matrix(X.grid[params[1]]), y = as.matrix(X.grid[params[2]]), z = as.matrix(pred.m$mean))
	dataOriginal<-data.frame(x = as.matrix(allX[params[1]]), y = as.matrix(allX[params[2]]), z = as.matrix(ally))
	colnames(dataOriginal)<-c("x","y","z")
	z.pred<-matrix(pred.m$mean,nrow=12,ncol=12)
	
	jpeg(filename=paste("plots/",foldername,"/",indicator,".png",sep=""),width=1440,height=960,pointsize = 20)
	scatter3D(z = dataOriginal$z, x = dataOriginal$x, y = dataOriginal$y, pch = 18, cex = 2, 
						theta = 20, phi = 20, ticktype = "detailed",
						xlab = params[1], ylab = params[2], zlab = indicator, clab = "mpg", 
						surf = list(x = x.grid, y = y.grid, z = z.pred, facets=NA),
						colkey = list(length = 0.8, width = 0.4),            
						main = "")
	dev.off()
	
	if(plotrgl){
		plotrgl()
	}
}

#params<-c("theta","phi")
foldername<- "sensitivityphi/"
sensitivityAnalysis("ExitP",foldername,shocks,params,TRUE)
sensitivityAnalysis("OutputSize",foldername,shocks,params,TRUE)
sensitivityAnalysis("OutputLength",foldername,shocks,params,TRUE)
sensitivityAnalysis("EmplSize",foldername,shocks,params,TRUE)
sensitivityAnalysis("EmplLength",foldername,shocks,params,TRUE)
sensitivityAnalysis("FinancialVolatility",foldername,shocks,params,TRUE)
sensitivityAnalysis("OutputVolatility",foldername,shocks,params,TRUE)
sensitivityAnalysis("ValueStranded",foldername,shocks,params,TRUE)
sensitivityAnalysis("QuantityStranded",foldername,shocks,params,TRUE)
sensitivityAnalysis("LoansDef",foldername,shocks,params,TRUE)
sensitivityAnalysis("FinancialCap",foldername,shocks,params,TRUE)

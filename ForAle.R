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


load("datatest.Rdata")
params<-c("thetashock","phi")

#Generating sensitivity results

#This is the datastructure which will contain the different indicators we use, param contains the two parameters for whcih we run a sensitivity analysis.

shocks<-as.data.frame(matrix(nrow=length(datatest),ncol=14,dimnames = list(NULL,c("scen","EmplLength","EmplSize","OutputLength","OutputSize","FinancialVolatility","OutputVolatility","ExitP","ValueStranded","QuantityStranded","LoansDef","FinancialCap",params))))
for(j in 1:length(datatest)){
	shocks$scen[j]<-j
	results<-as.data.frame(datatest[[j]])
	shocks[j,params[1]]<-results[1,params[1]]
	shocks[j,params[2]]<-results[1,params[2]]
	if(length(exitPeriod<-which(results$exitk==1))>0){
		exitPeriod<-which(results$exitk==1)[1]
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
		shocks$ExitP[j]=exitPeriod+19
		shocks$ValueStranded[j]=results$kk[exitPeriod]*results$pk[exitPeriod]
		shocks$QuantityStranded[j]=results$kk[exitPeriod]
		shocks$FinancialCap[j]=results$pke[exitPeriod]*results$ek[exitPeriod]
		shocks$LoansDef[j]=results$Lk[exitPeriod-1]
	}
}

shocks<-shocks[with(shocks, order(thetashock)), ]

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


sensitivityAnalysis("ExitP","sensitivity/",shocks,params,TRUE)
sensitivityAnalysis("OutputSize","sensitivity/",shocks,params,TRUE)
sensitivityAnalysis("OutputLength","sensitivity/",shocks,params,TRUE,TRUE)
sensitivityAnalysis("EmplSize","sensitivity/",shocks,params,TRUE,TRUE)
sensitivityAnalysis("EmplLength","sensitivity/",shocks,params,TRUE)
sensitivityAnalysis("FinancialVolatility","sensitivity/",shocks,params,TRUE)
sensitivityAnalysis("OutputVolatility","sensitivity/",shocks,params,TRUE)
sensitivityAnalysis("ValueStranded","sensitivity/",shocks,params,TRUE,TRUE)
sensitivityAnalysis("QuantityStranded","sensitivity/",shocks,params,TRUE)
sensitivityAnalysis("LoansDef","sensitivity/",shocks,params,TRUE,TRUE)
sensitivityAnalysis("FinancialCap","sensitivity/",shocks,params,TRUE,TRUE)

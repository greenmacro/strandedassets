
# shocks01<-shocks
# shocks004<-shocks

# load("datatest004.Rdata")
# datatest004<-datatest
# load("datatest01.Rdata")
# datatest01<-datatest
suc<-as.data.frame(datatest[[18]])
suc<-as.data.frame(datatest[[18]])

suc<-as.data.frame(datatest004[[30]])
fail<-as.data.frame(datatest01[[23]])
save(suc,fail,file="scenarioAnalysis.Rdata")

datatest<-datatest01


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
values<-shocks[c("thetashock","phi")]
shocks$scen[2]
plotScenarios<-function(toplot,datasource,name,folder,order,values){
	for(k in 1:4){
		filename<-paste("plots/",folder,name,"_",k,".png",sep="")
		listscen=c(((k-1)*9+1):((k-1)*9+9))
		jpeg(filename=filename,width=1440,height=960,pointsize = 20)
		layout(matrix(c(1:9),3,3,byrow = T))
		layout.show(9)
		for(i in listscen){
			if(i<length(datasource)){
				results<-as.data.frame(datasource[[order[i]]])
				exitp=ifelse(length(which(results$exitk==1))>0,which(results$exitk==1)[1],60)
				timeframe=(1:(exitp+40))
				matplot(results[timeframe,toplot],type="l",lwd=2,lty=1,main=paste("Scenario",(order[i]-1),"- (",paste(values[i,1],",",values[i,2],collapse=','),")"),ylab="",ylim=c(0,max(results[timeframe,toplot],na.rm=T)))
				abline(v=which(results$exitk==1)[1])
				grid()
			}
		}
		dev.off()
	}
}

plotScenarios(c("yc","yk","yi"),datatest,"Output","scenarios/",shocks$scen,values)
plotScenarios(c("Nc","Nk","Ni","LF"),datatest,"Employment","scenarios/",shocks$scen,values)
plotScenarios(c("pce","pke","pie"),datatest,"EquityPrice","scenarios/",shocks$scen,values)
plotScenarios(c("qc","qk","qi"),datatest,"TobinQ","scenarios/",shocks$scen,values)
plotScenarios(c("kc","ic","kk","ii"),datatest,"CapitalStock","scenarios/",shocks$scen,values)
plotScenarios(c("pc","pk","pi"),datatest,"Price","scenarios/",shocks$scen,values)
plotScenarios(c("lambda10a","lambda20a","lambda30a","lambda40a"),datatest,"Lambdas","scenarios/",shocks$scen,values)
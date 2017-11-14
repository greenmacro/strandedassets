data_baseline<-as.data.frame(databas$baseline)
View(data_baseline)
blocks<-WholeModel$blocks
i=30
while(i<=53){
	for(block in blocks){
		vars<-WholeModel$equations[block,1]
		data_toview<-data_baseline[i,vars]
		if(sum(is.na(data_toview))>0){
			print(i)
			print(is.na(data_toview))
			print(data_toview[1,is.na(data_toview)])
			i<-53
			break
		}
	}
	i<-i+1
}

matplot(data_baseline[,c("yc","yh","yl")],type='l',lty=1,lwd=2)

blocks<-WholeModel$blocks
for(block in blocks){
	vars<-WholeModel$equations[block,1]
	equations<-WholeModel$equations[block,2]
	data_toview<-data_baseline[i,vars]
	if(sum(is.na(data_toview))>0){
		print(i)
		print(is.na(data_toview))
		print(data_toview[1,is.na(data_toview)])
		i<-23
		break
	}
}

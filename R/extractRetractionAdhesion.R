extractRetractionAdhesion=function(case,percentFlat=0.8){
    curve=stripApr(case$data,"zSensr")
		## plot(curve$zSensr,curve$force)
    minIndex=which.min(curve$force)
    curve=curve[(-1):(-minIndex+1),]
    minIndex=1
    curve$index=c(1:dim(curve)[1])
    retractionFit=lm(force~index,curve[floor(dim(curve)[1]*(1-percentFlat)):dim(curve)[1],])
   		## print("min force")
   		## print(curve$force[minIndex])
   		## print("baseline")
   		## print(as.numeric(coef(approachFit)[1]+coef(approachFit)[2]*minIndex))
    return(as.numeric(coef(retractionFit)[1]+coef(retractionFit)[2]*minIndex-curve$force[minIndex]))
}

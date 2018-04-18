extractApproachAdhesion=function(case,percentFlat=0.8){
    curve=stripRet(case$data,"zSensr")
    minIndex=which.min(curve$force)
    curve=curve[1:minIndex,]
    curve$index=c(1:dim(curve)[1])
    approachFit=lm(force~index,curve[1:floor(dim(curve)[1]*percentFlat),])
    ## print("min force")
    ## print(curve$force[minIndex])
    ## print("baseline")
    ## print(as.numeric(coef(approachFit)[1]+coef(approachFit)[2]*minIndex))
    return(as.numeric(coef(approachFit)[1]+coef(approachFit)[2]*minIndex-curve$force[minIndex]))
}

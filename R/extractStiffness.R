extractStiffness=function(case,r,approachLength=.1,contactLength=.1,searchWidth=.2,maxF=.5,weight=4,zPos="zSensr",force="force"){
    eq="F~(4/3)*EStar*(r^(1/2))*indent^(3/2)"
                                        #First fit a line to the approach section of the curve and to the end and find their intersection, this will be our rough contact point
    data=data.frame(zSensr=case$data[,zPos],force=case$data[,force])
    data=stripRet(data,"zSensr")
    dataLength=dim(data)[1]
    dataThreshold=min(data$force)+maxF*(max(data$force)-min(data$force))
    trimmedDataLength=which.min(abs(data$force-dataThreshold))
    trimmedData=data[1:trimmedDataLength,]
    approachData=trimmedData[1:floor(trimmedDataLength*approachLength),]
    contactData=trimmedData[(floor(trimmedDataLength-(trimmedDataLength*contactLength))+1):trimmedDataLength,]
    approachFit=lm("force~zSensr",approachData)
    linearContact=lm("force~zSensr",contactData)
    zIntercept=(coef(linearContact)[[1]]-coef(approachFit)[[1]])/(coef(approachFit)[[2]]-coef(linearContact)[[2]])
    fIntercept=coef(approachFit)[[2]]*zIntercept+coef(approachFit)[[1]]
    endContact=trimmedData[trimmedDataLength,]
    approxE=(3/4)*(endContact$force-fIntercept)*((endContact$zSensr-zIntercept)^(-3/2))*((r)^(-.5))

                                        #    contactFit=nls(roughEq,contactData,start=c(A=approxA,z0=zIntercept,F0=fIntercept),control=nls.control(warnOnly=TRUE,maxiter=1000,minFactor=1/1000000))
    nominalCPIndex=which.min(abs(trimmedData$zSensr-zIntercept))
    startIndex=nominalCPIndex-floor(searchWidth*trimmedDataLength)
    stopIndex=nominalCPIndex+floor(searchWidth*trimmedDataLength)
    startIndex=max(startIndex,1)
    stopIndex=min(stopIndex,trimmedDataLength-10) #make sure there are enough points for the last putative cp to converge
    cpToCheck=c(startIndex:stopIndex)
    bestFit=list()
    lowestError=Inf
    contactPoint=c()
    allError=c()
    iteration=1
    for(cpIndex in cpToCheck){
        iteration=iteration+1
        fitData=data.frame(F=trimmedData$force-trimmedData$force[cpIndex],indent=trimmedData$zSensr-trimmedData$zSensr[cpIndex],r=r)
        linearRegion=fitData[1:cpIndex,]
        nonLinearRegion=fitData[cpIndex:trimmedDataLength,]
        linearFit=lm("F~indent",linearRegion)
        nonLinearFit=nls(eq,nonLinearRegion,start=c(EStar=approxE),control=nls.control(warnOnly=TRUE))
        totalError=weight*sum(residuals(linearFit)^2)+sum(residuals(nonLinearFit)^2)
        if(totalError<lowestError){
            lowestError=totalError
            fits=list(nonLinear=nonLinearFit,linear=linearFit)
            bestFit=fits
            contactPoint=trimmedData[cpIndex,c("zSensr","force")]
            contactPoint=cbind(contactPoint,data.frame(cpIndex=cpIndex))
        }
#        allFits=c(allFits,fits)
        allError=c(allError,totalError)
    }
    correctedData=data.frame(F=data$force-contactPoint$force,zPos=data$zSensr-contactPoint$zSensr)
    modelZPos=correctedData$zPos[contactPoint$cpIndex:dim(correctedData)[1]]
    modelForce=(4/3)*coef(bestFit$nonLinear)["EStar"]*(r^(1/2))*(modelZPos^(3/2))
    modelCurve=data.frame(F=modelForce,zPos=modelZPos)
    fitInfo=data.frame(contactIndex=contactPoint$cpIndex,contactPos=contactPoint$zSensr,contactForce=contactPoint$force,residual=lowestError,EStar=coef(bestFit$nonLinear)["EStar"],converged=bestFit$nonLinear$convInfo$isConv)
   
    correctedData$curve="measured"
    modelCurve$curve="model"
    curves=rbind(correctedData,modelCurve)
    curves$curve=factor(curves$curve)

    fit=list(fit=fitInfo,curves=curves)
    
    return(list(fit=fit,ident=case$ident))
}

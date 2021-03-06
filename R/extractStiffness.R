extractStiffness=function(case,r,approachLength=.1,contactLength=.1,searchWidth=.2,maxF=.5,weight=4,correctVirtDefl=TRUE,zPos="zSensr",force="force"){
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

                                        #Correct for virtual deflection by subtracting the approachFit line from the force data
    if(correctVirtDefl){
        virtDeflForce=coef(approachFit)["zSensr"]*data$zSensr+coef(approachFit)[1]
        data$force=data$force-virtDeflForce
        trimmedData$force=trimmedData$force-virtDeflForce[1:trimmedDataLength]
    }
    
    nominalCPIndex=which.min(abs(trimmedData$zSensr-zIntercept))
    startIndex=nominalCPIndex-floor(searchWidth*trimmedDataLength)
    stopIndex=nominalCPIndex+floor(searchWidth*trimmedDataLength)
    startIndex=max(startIndex,1)
    maxStop=trimmedDataLength-10 #make sure there are enough points for the last putative cp to converge

    stopIndex=min(stopIndex,maxStop) 
    cpToCheck=c(startIndex:stopIndex)
    bestFit=list()
    lowestError=Inf
    contactPoint=c()
    allError=c()
    for(cpIndex in cpToCheck){
        fitData=data.frame(F=trimmedData$force-trimmedData$force[cpIndex],indent=trimmedData$zSensr-trimmedData$zSensr[cpIndex],r=r)
        linearRegion=fitData[1:cpIndex,]
        nonLinearRegion=fitData[cpIndex:trimmedDataLength,]
        fitList=tryCatch({ #If one of the fits throws an error, treat it as a fit with infinite residual
            linearFit=lm("F~indent",linearRegion)
            nonLinearFit=nls(eq,nonLinearRegion,start=c(EStar=approxE),control=nls.control(warnOnly=TRUE))            
            totalError=weight*sum(residuals(linearFit)^2)+sum(residuals(nonLinearFit)^2)
            list(linear=linearFit,nonLinear=nonLinearFit,totalError=totalError)
        },error=function(e){
            return(list(linear=NA,nonLinear=NA,totalError=Inf))
        })
        if(fitList$totalError<lowestError){
            lowestError=fitList$totalError
            fits=list(nonLinear=fitList$nonLinear,linear=fitList$linear)
            bestFit=fits
            contactPoint=trimmedData[cpIndex,c("zSensr","force")]
            contactPoint=cbind(contactPoint,data.frame(cpIndex=cpIndex))
        }
#        allFits=c(allFits,fits)
        allError=c(allError,fitList$totalError)
    }
    if(lowestError==Inf){
        thisIdent=""
        for(part in case$ident){
            thisIdent=paste(thisIdent,part) #change to remove extra space in warning message
        }
        warning(paste("No fit found for",thisIdent,"Check that curves and fitting parameters are reasonable"))
        return(NA)
    }
    locationInWindow=(contactPoint$cpIndex-cpToCheck[1])/length(cpToCheck) #location of the best fit inside the search width in terms of percent of the search width
    if((locationInWindow<.25 && (!startIndex==1)) || (locationInWindow>.75 &&(!stopIndex==maxStop))){ #Don't throw warning if search window is already pegged out, because increasing searchWidth won't do anything
        warning("Best fit is near the edge of the search width. Consider increasing searchWidth")
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

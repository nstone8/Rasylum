stiffnessSphereOnSphere=function(rBead, extZ, extForce,percentToFit=0.2){
                                        #Extract Stiffness, Contact point and radius of cell
                                        #Get Contact Point and initial estimate of stiffness from sphere-plane contact model
    flatFit=stiffnessSphereOnPlane(rBead,extZ,extForce,percentToFit)$fit
    extContact=data.frame(indent=extZ[flatFit$contactIndex:length(extZ)],F=extForce[flatFit$contactIndex:length(extForce)],rProbe=rBead,EStar=as.numeric(flatFit$EStar))
    extContact$F=extContact$F-extContact$F[1]
    extContact$indent=extContact$indent-extContact$indent[1]
    eq="F~(4/3)*EStar*((1/((1/rProbe)+(1/rCell)))^(1/2))*indent^(3/2)"
    result=nls(eq,extContact,start=list(rCell=rBead),trace=TRUE,control=nls.control(warnOnly=TRUE))
    tVal=-1 #We will report a t of -1 (impossible) if not converged
    
    if(result$convInfo$isConv){
        tVal=coef(summary(result))[3]
    }
    
    bestValues=data.frame(contactIndex=flatFit$contactIndex,contactPos=extZ[flatFit$contactIndex],contactForce=extForce[flatFit$contactIndex],tValue=tVal, EStar=as.numeric(flatFit$EStar), rCell=as.numeric(coef(result)["rCell"]))
    
    
    fitCurve=data.frame(zPos=extZ[bestValues$contactIndex:length(extForce)], F=((4/3)*bestValues$EStar*(rBead^(1/2))*(extZ[bestValues$contactIndex:length(extForce)]-bestValues$contactPos)^(3/2))+bestValues$contactForce, curve="model")
    plotData=rbind(data.frame(zPos=extZ, F=extForce, curve="measured"),fitCurve)

    return(list(fit=bestValues,curves=plotData,conv=result$convInfo$isConv))
}

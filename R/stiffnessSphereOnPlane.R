stiffnessSphereOnPlane=function(rBead, extZ, extForce, CPMaxF=.05, percentToFit=0.2, roughness=.01, Q=.5, approachTrim=0.2, debug=FALSE){
                                        # Extract the stiffness and contact point for sphere on plane indentation. Assume that the bead is much stiffer than the cell. rBead is the bead radius, extZ and extForce are column vectors containing the position and force data, respectively. CPMaxF is the percentage of the maximum force beyond which this function will not test for the contact point. percentToFit is the percentage of the extension curve that will be used to perform the fits. roughness is the percent of the force range that features will have to be larger than to not be considered noise. Q is the minimum ratio of height to width a trough on a residual vs. contact point plot has to have to be considered a local minima. approachTrim is the percentage of the beginning of a curve to be trimmed off before performing fitting. Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.

                                        #First find slightly overestimated CP
    minForce=min(extForce)
    thresh=minForce+CPMaxF*(max(extForce)-minForce)
    roughCPIndex=length(extForce)
    for(i in length(extForce):1){
        if(extForce[i]<thresh){
            roughCPIndex=i
            break
        }
    }
    eq="F~(4/3)*EStar*(r^(1/2))*indent^(3/2)"
    trimmedStart=1
    if(approachTrim>0 && (roughCPIndex-approachTrim*length(extForce)>1)){
                                        #remove long approach curve
        trimmedStart=floor(approachTrim*length(extForce))
    }
    roughCPIndex=roughCPIndex-(trimmedStart-1)
    extZ=extZ[trimmedStart:length(extZ)]
    extForce=extForce[trimmedStart:length(extForce)]
    
    lengthForce=length(extForce)
    fitLength=floor(lengthForce*percentToFit)
    stopIndex=lengthForce-fitLength+1
    numRow=min(stopIndex,roughCPIndex)
    fits=data.table(contactIndex=rep(-1,times=numRow),contactPos=rep(-1,times=numRow),contactForce=rep(-1,times=numRow),residual=rep(-1,times=numRow), sigma=rep(-1,times=numRow),EStar=rep(-1,times=numRow),converged=rep(FALSE,times=numRow))
    curRow=0

    for(i in 1:numRow){
                                        #Perform fits at each point, assuming F[i]=0 and indent[i]=0
        
        contactData=data.frame(indent=extZ[i:(i+fitLength-1)]-extZ[i],F=extForce[i:(i+fitLength-1)]-extForce[i],r=rep(rBead,times=fitLength))
        if(all(contactData$indent>=0)){
            result=nls(eq,contactData,start=c(EStar=1),control=nls.control(warnOnly=TRUE))
            curRow=curRow+1
            fits[curRow,contactIndex:=i]
            fits[curRow,contactPos:=extZ[i]]
            fits[curRow,contactForce:=extForce[i]]
            fits[curRow,residual:=sum(residuals(result)^2)]
            fits[curRow,sigma:=summary(result)$sigma]
            fits[curRow,EStar:=as.numeric(coef(result)["EStar"])]
            fits[curRow,converged:=result$convInfo$isConv]
        }else{
            curRow=curRow+1
            fits[curRow,names(fits):=NA]
        }
    }
    fits=na.omit(fits)
    bestValues=fits[which.min(fits$residual),] #default option
    minima=findLocalMinima(fits$residual,roughness,Q)
    minimaIndices=minima$indices
    if(length(minimaIndices)>0){
        smallestIndex=which.min(fits$residual[minimaIndices])
        bestValuesIndex=minimaIndices[smallestIndex]
        bestValues=fits[bestValuesIndex,]
        if(debug){
            print(paste("chose local minima at i=",bestValuesIndex))
            print(paste("height=",minima$heights[smallestIndex]))
            print(paste("Q=",minima$Qs[smallestIndex]))
        } 
    }else if(debug){
        print(paste("chose absolute minima at",bestValues$contactIndex))
    }
    if(debug){
        print(ggplot(data.frame(index=1:length(fits$residual),score=fits$residual),aes(x=index,y=score))+geom_path())
        readline()
    }

    fitCurve=data.frame(zPos=extZ[bestValues$contactIndex:length(extForce)], F=((4/3)*bestValues$EStar*(rBead^(1/2))*(extZ[bestValues$contactIndex:length(extForce)]-bestValues$contactPos)^(3/2))+bestValues$contactForce, curve="model")
    plotData=rbind(data.frame(zPos=extZ, F=extForce, curve="measured"),fitCurve)
    if(debug){
        print(ggplot(plotData,aes(x=zPos,y=F,color=curve))+geom_path())
        readline()
    }
    return(list(fit=bestValues,curves=plotData))
}

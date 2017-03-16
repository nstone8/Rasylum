                                        #Version 0.2.1

buildFrame=function(dataList, numRows){
                                        #Transforms dataList, which is a list of lists with the same member names into a data.table with those names as columns
    library(data.table)
    dataTable=as.data.table(matrix(nrow=numRows, ncol=length(dataList[[1]])))
    print("Allocated table")
                                        #Replace columns so they have the correct type
    colNames=names(dataList[[1]])
    names(dataTable)=colNames
    numCol=dim(dataList[[1]])[2]
    for(j in 1:numCol){
        print(paste((j/numCol)*100,"% Types Defined",sep=""))
        dataTable[,as.integer(j):=rep(as.data.frame(dataList[[1]])[1,j],numRows)]
    }
    curRow=1
    for(i in 1:length(dataList)){
        print(paste((i/length(dataList))*100,"% Written",sep=""))
        dataTable[curRow:(curRow+dim(dataList[[i]])[1]-1),names(dataTable):=dataList[[i]]]
        curRow=curRow+dim(dataList[[i]])[1]
    }
    return(dataTable)
}

saveFits=function(filename,fitData, x="zPos", y="F"){
                                        #FitData should be a object created by parExtractStiffness or parExtractTimeConst. Saves plots of the fit and original data to a pdf file filename
    graphics.off()
    library(ggplot2)
    pdf(filename)
    for(j in 1:length(fitData$fits)){
        fit=fitData$fits[[j]]
        curve=fit$fit$curves
        id=fit$ident
        fields=names(id)
        name=""
        for(i in 1:length(fields)){
            name=paste(name,fields[i],id[1,fields[i]])
        }
        newCurve=data.frame(x=curve[,x],y=curve[,y], curve=curve$curve)
        plot=ggplot(newCurve,aes(x=x,y=y,color=curve))+geom_path()+labs(title=name)
        
        print(plot)
        print(name)
        
    }
    dev.off()
}

collateFits=function(fitData){
                                        #Return a single data frame containing all fit values and identifiers. fitData should be an object created by parExtractStiffness or parExtractTimeConst
    allFits=list()
    allFits[[1]]=cbind(fitData$fits[[1]]$fit$fit,fitData$fits[[1]]$ident)
    numFits=length(fitData$fits)
    for(f in 2:numFits){
        allFits[[f]]=cbind(fitData$fits[[f]]$fit$fit,fitData$fits[[f]]$ident)
    }
    return(buildFrame(allFits,numFits))   
}

normalizeFrame=function(frame,column,identifiers,wrt,value="lowest"){
                                        #normalize the column column in the dataframe frame. Identifiers should be a list of the column names which can uniquely identify each case to be normalized. wrt and value specifies whether normalization should be with respect to the lowest or highest value of column wrt
    library(data.table)
    output=as.data.table(frame)
    output[,names(output):=NA]
    curOutputRow=1
    iterator=identIterate(frame,identifiers,1)    
    for(i in 1:length(iterator)){
        row=0
        entry=iterator[[i]]$data
        if(all(dim(entry$data)>0)){
            if(regexpr("lowest",value)>0){
                row=which.min(as.numeric(as.character(entry[,wrt])))
            }else if(regexpr("highest",value)>0){
                row=which.max(as.numeric(as.character(entry[,wrt])))
            }else{
                print("value must be 'lowest' or 'highest'")
                return(-1)
            }
            entry[,column]=entry[,column]/entry[row,column]
            lengthEntry=dim(entry)[1]
            output[curOutputRow:(curOutputRow+lengthEntry-1),names(output):=entry]            
            curOutputRow=curOutputRow+lengthEntry
        }
    }
    return(output)
}

stripRet=function(frame,zPos){
                                        #Strip the retraction curve from imported ibw data stored in frame (a data frame) zPos should be the column of that frame corresponding to the position of the z piezo (deflection would also probably work)
    zData=frame[,zPos]
                                        #remove retract curve and any tail at the start of the extension
    endPoint=which.max(zData)
    startPoint=which.min(zData[1:endPoint])
    return(frame[startPoint:endPoint,])
}

getDwell=function(frame,dwellTime,zPos,F,t){
                                        #Strip the extension curve from imported ibw data stored in frame (a data frame) zPos should be the column of that frame corresponding to the position of the z piezo (deflection would also probably work)
                                        #remove extract curve and any tail at the start of the extension
    startPoint=which.max(frame[,F])
    tStart=frame[startPoint,t]
    tEnd=tStart+dwellTime
    endPoint=which.min(abs(frame[,t]-tEnd))
    endPoint=floor(1*endPoint)
    return(frame[startPoint:endPoint,])
}

loadIBW = function(wave,asDataFrame=FALSE){
    library(IgorR)
    waveData=read.ibw(wave)
    sampleTime=attr(waveData,"WaveHeader")$sfA[1]
    notes=strsplit(attr(waveData,"Note"),"\\r")[[1]]
    inVols=notes[regexpr("^InvOLS:",notes)!=-1]
    k=notes[regexpr("^SpringConstant:",notes)!=-1]
    dwell=notes[regexpr("^DwellTime:",notes)!=-1]
                                        #Strip labels
    inVols=as.numeric(substr(inVols,regexpr(":",inVols)+1,nchar(inVols)))
    k=as.numeric(substr(k,regexpr(":",k)+1,nchar(k)))
    dwell=as.numeric(substr(dwell,regexpr(":",dwell)+1,nchar(dwell)))
                                        #Force=k*defl (the software has already taken the invOLS into account)
    if(asDataFrame){
        return(data.frame(t=c(0:(dim(waveData)[1]-1))*sampleTime,zSensr=as.numeric(waveData[,3]), rawZSensr=as.numeric(waveData[,1]),defl=as.numeric(waveData[,2]), force=k*waveData[,2],filename=wave, inVols=inVols, k=k, dwell=dwell))
    }else{
        return(list(t=c(0:(dim(waveData)[1]-1))*sampleTime,zSensr=as.numeric(waveData[,3]), rawZSensr=as.numeric(waveData[,1]),defl=as.numeric(waveData[,2]), force=k*waveData[,2],filename=wave, inVols=inVols, k=k,dwell=dwell))
    }
}

findLocalMinima=function(vec, roughness,Q){

                                        #find local minima in a vector, increasing roughness makes the function less sensitive to shallow valleys (value should be between 0 and 1). Q is the minimum depth/width ratio of the minima. ApproachTrim is the amount of data to trim off of the beginning of the curve to remove noise. return an empty data frame if no local minima are found
    indices=c()
    thresh=(max(vec)-min(vec))*roughness
    lVec=length(vec)
    heights=c()
    if(lVec>1){
        for(i in 2:lVec){
            diffVec=vec-vec[i]
            if(max(diffVec[1:i])>thresh && max(diffVec[i:lVec])>thresh){
                                        #we found a trough
                                        #Check that there is a trough lip in between this min and the last one. Otherwise, merge them
                if(length(indices)<1){#add if this is our first min
                    indices=c(indices,i)
                    heights=c(heights,min(max(diffVec[1:i]),max(diffVec[i:lVec]))/(max(vec)-min(vec))) #heights are normalized

                }else if(max(diffVec[indices[length(indices)]:i])>thresh){ #append if there is a lip between us and the last min
                    indices=c(indices,i)
                    heights=c(heights,min(max(diffVec[1:i]),max(diffVec[i:lVec]))/(max(vec)-min(vec))) #heights are normalized
                }else if(vec[i]<vec[indices[length(indices)]]){#if there is no lip and this min is less than the last one, replace the last value
                    indices[length(indices)]=i
                    heights[length(heights)]=min(max(diffVec[1:i]),max(diffVec[i:lVec]))/(max(vec)-min(vec))
                }
                
            }
        }
    }
                                        #get the aspect ratio of each trough by searching for the maxima between each local min
    sharpIndices=c()
    sharpHeights=c()
    sharpnesses=c()
    if(length(indices>0)){
        checkPoints=c(1,indices,length(indices)+2)
        for(j in 2:(length(indices)+1)){
                                        #define sharpness as the maximum 'angle to horizon' seen on the shallowest side of the trough

            leftSide=vec[checkPoints[j-1]:checkPoints[j]]/(max(vec)-min(vec))
            rightSide=vec[checkPoints[j]:checkPoints[j+1]]/(max(vec)-min(vec))
            normVec=vec/(max(vec)-min(vec))
            leftDist=c(length(leftSide):0)/length(vec)
            rightDist=c(0:length(rightSide))/length(vec)

            
            leftSlope=(leftSide/leftDist)[(leftSide-normVec[checkPoints[j]])>roughness]
            rightSlope=(rightSide/rightDist)[(rightSide-normVec[checkPoints[j]])>roughness]
            sharpness=min(max(rightSlope),max(leftSlope))
            
            if(sharpness>Q){
                sharpIndices=c(sharpIndices,checkPoints[j])
                sharpHeights=c(sharpHeights,heights[j-1])
                sharpnesses=c(sharpnesses,sharpness)
            }
            
        }
    }
    return(list(indices=sharpIndices,heights=sharpHeights, Qs=sharpnesses))
    
}

batchLoad=function(folder,consts,suffix){
                                        #consts is a vector of strings, suffix is a string
                                        # Load .ibw files from 'folder' with names of the form const1variableRegionconst2variableregion...suffix and return a data frame with each const as a column containing it's corresponding variable region
    if(regexpr("/$",folder)==-1){
        folder=paste(folder,"/",sep="")
    }
    pat="^"
    for(const in consts){
        pat=paste(pat,const,".*",sep="")
    }
    pat=paste(pat,suffix,"$",sep="")
    filenames=list.files(folder,pat)
    data=list()
    lengthData=0
    fileNo=1
    totalNumRows=0
    for(f in filenames){
        print(paste(fileNo/length(filenames)*100,"% complete",sep=""))
        fileNo=fileNo+1
        vars=c()
        delims=c(consts,suffix)                                   
        for(i in 1:(length(delims)-1)){
            vars[i]=substr(f,regexpr(delims[i],f)+nchar(delims[i]),regexpr(delims[i+1],f)-1)
        }
        unlabeledData=loadIBW(paste(folder,f,sep=""),TRUE)
        newcol=data.frame(vars[1])
        for(v in 2:length(vars)){
            newcol=cbind(newcol,vars[v])
        }
        names(newcol)=consts
        totalNumRows=totalNumRows+dim(unlabeledData)[1]
        lengthData=lengthData+1        
        data[[lengthData]]=cbind(unlabeledData,newcol)
    }
    return(list(data=data,numRows=totalNumRows))
}

quickLoad=function(folder,consts,suffix){
                                        #Convenience function for running batchload and buildFrame with one command
    imp=batchLoad(folder,consts,suffix)
    return(buildFrame(imp$data,imp$numRows))
}

loadPreSorted=function(folder,consts,suffix){
    imp=batchLoad(folder,consts,suffix)
    iterated=list()
    for(i in 1:length(imp$data)){
        iterated[[i]]=list(data=imp$data[[i]],ident=imp$data[[i]][1,consts])
    }
    return(iterated)
}

stiffnessSphereOnPlane=function(rBead, extZ, extForce, CPMaxF=.05, percentToFit=0.2, roughness=.01, Q=.5, approachTrim=0.2, debug=FALSE){
                                        # Extract the stiffness and contact point for sphere on plane indentation. Assume that the bead is much stiffer than the cell. rBead is the bead radius, extZ and extForce are column vectors containing the position and force data, respectively. CPMaxF is the percentage of the maximum force beyond which this function will not test for the contact point. percentToFit is the percentage of the extension curve that will be used to perform the fits. roughness is the percent of the force range that features will have to be larger than to not be considered noise. Q is the minimum ratio of height to width a trough on a residual vs. contact point plot has to have to be considered a local minima. approachTrim is the percentage of the beginning of a curve to be trimmed off before performing fitting. Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    library(data.table)
    if(debug){
        library(ggplot2)
    }
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
        trimmedStart=floor(roughCPIndex-approachTrim*length(extForce))
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
            result=nls(eq,contactData,control=nls.control(warnOnly=TRUE))
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

identIterate=function(frame,identifiers,numCores=-1){
                                        #returns a list of lists defining different important groupings of observations. frame is a data frame containing the raw data, identifiers is the names of the columns of frame where unique values correspond to groups. numCores is the number of cores to use in order to do the calculation (if -1 is used, the function will use all available cores)
    frame=as.data.frame(frame)
    library(parallel)
    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }
    identifierIter=list()
    identifierIterLength=0
    identNames=c()
    for(ident in identifiers){
        identifierIterLength=identifierIterLength+1
        frame[,ident]=factor(frame[,ident])
        identifierIter[[identifierIterLength]]=list(name=ident,index=1,values=levels(frame[,ident]))
        identNames=c(identNames,ident)
    }
    doneIterating=FALSE
    allOptions=list()
    allOptionsLength=0
    while(!doneIterating){
        theseVals=data.frame(identifierIter[[1]]$values[identifierIter[[1]]$index])
        if(length(identifierIter)>1){
            for(j in 2:length(identifierIter)){
                theseVals=cbind(theseVals,data.frame(identifierIter[[j]]$values[identifierIter[[j]]$index]))
            }
        }
        names(theseVals)=identNames
        allOptionsLength=allOptionsLength+1
        allOptions[[allOptionsLength]]=theseVals
                                        #increment iterator
        identifierIter[[1]]$index=identifierIter[[1]]$index+1
        if(length(identifierIter)>1){
            for(i in 2:length(identifierIter)){
                if(identifierIter[[i-1]]$index>length(identifierIter[[i-1]]$values)){ #if the last value has rolled over, reset it and increment this one
                    identifierIter[[i-1]]$index=1
                    identifierIter[[i]]$index=identifierIter[[i]]$index+1
                }
            }
        }
                                        #Check if we've done all iterations of last value

        if(identifierIter[[length(identifierIter)]]$index>length(identifierIter[[length(identifierIter)]]$values)){
            doneIterating=TRUE
        }
    }
    trimDown=function(values){
        thisCurveBool=rep(TRUE,dim(frame)[1])
        for(i in 1:length(values)){#Select Values corresponding to this iteration
            thisCurveBool=thisCurveBool & (frame[,names(values)[i]]==as.character(values[1,i]))

        }
        thisCurve=frame[thisCurveBool,]
                                        #Get the data for this iteration and add it to our results
        if(is.numeric(dim(thisCurve)) && !any(is.na(thisCurve))){
            thisCurveSize=dim(thisCurve)
            if(all(thisCurveSize>0)){
                return(list(data=thisCurve,ident=values))
            }
        }
    }
    toRun=mclapply(allOptions,trimDown,mc.cores=numCores)
    output=list()
    outputLength=0
    if(length(toRun)!=length(allOptions)){
        print(paste("Parallel processing has resulted in dropped values, try again with a value of numCores smaller than",numCores))
        return(FALSE)
    }
    for(i in 1:length(toRun)){
        if(!is.null(toRun[[i]])){
            outputLength=outputLength+1
            output[[outputLength]]=toRun[[i]]
        }
    }
    return(output)
}

parExtractStiffness=function(rBead, cases, zPos="zSensr", force="force", CPMaxF=.05, percentToFit=0.2, roughness=0.05,Q=.5, approachTrim=0.2, debug=FALSE, minRise=0, numCores=-1){

                                        #allCases is a list of curves to fit as generated by the identIter function, rBead is the bead radius, extZ and extForce are column vectors containing the position and force data, respectively. CPMaxF is the percentage of the maximum force beyond which this function will not test for the contact point. percentToFit is the percentage of the extension curve that will be used to perform the fits. roughness is the percent of the force range that features will have to be larger than to not be considered noise. Q is the minimum ratio of height to width a trough on a residual vs. contact point plot has to have to be considered a local minima. approachTrim is the percentage of the beginning of a curve to be trimmed off before performing fitting. Defaults for zPos and force correspond to the values used in data imported using loadIBW, batchLoad and quickLoad. numCores is the number of cores to use in order to do the calculation (if -1 is used, the function will use all available cores). Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    library(parallel)
    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }

    parFun=function(case){
        print(case$ident)
        case$data=stripRet(case$data,zPos)
        toReturn=stiffnessSphereOnPlane(rBead, case$data[zPos][,], case$data[force][,], CPMaxF, percentToFit, roughness,Q,approachTrim,debug)
        return(list(fit=toReturn,ident=case$ident))
    }            
    if(debug){
        for(ca in cases){
            parFun(ca)
        }
    }
    fits=mclapply(cases,parFun,mc.cores=numCores)
    if(length(fits)!=length(cases)){
        print(paste("Parallel processing has resulted in dropped values, try again with a value of numCores smaller than",numCores))
        return(FALSE)
    }

    toReturn=list(fits=fits,rBead=rBead, zPos=zPos, force=force, CPMaxF=CPMaxF, percentToFit=percentToFit,roughness=roughness,Q=Q, approachTrim=approachTrim)
    if(minRise>0){
        fixFlatFits(toReturn,minRise,debug,numCores)

    }
    return(toReturn)
}

fixFlatFits=function(fits, minRise=.1,debug=FALSE, numCores=-1){
    library(parallel)
    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }
                                        #Sometimes the method for finding the best fit chooses to fit a portion of the approach curve before the probe is in contact (resulting in a stiffness of ~0. This function attempts to find these cases (by comparing the maximum values of the measured and modeled curves) and produce a better fit. Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    checkFit=function(i,allFits){
        fitCurves=allFits$fits[[i]]$fit$curves
        measured=fitCurves[fitCurves$curve=="measured",]
        model=fitCurves[fitCurves$curve=="model",]
        if((max(model$F)/max(measured$F))<minRise){
                                        #This fit is flat, trim the measured curve up to the current CP and refit. If this results in a different fit, check the new fit for flatness. if it doesn't, then there probably isn't a better local minimum to choose
            print("Attempting to improve fit for:")
            print(allFits$fits[[i]]$ident)
            newMeasured=measured[allFits$fits[[i]]$fit$fit$contactIndex:length(measured$F),]
            newFit=stiffnessSphereOnPlane(allFits$rBead,newMeasured$zPos,newMeasured$F,allFits$CPMaxF,(length(measured$F)/length(newMeasured$F))*allFits$percentToFit,allFits$roughness,allFits$Q,allFits$approachTrim,debug)
                                        #Check if the new CP is different and that the new predicted stiffness is higher (so the curve is less flat
            if((newFit$fit$contactPos!=allFits$fits[[i]]$fit$fit$contactPos) && (newFit$fit$EStar>allFits$fits[[i]]$fit$fit$EStar)){
                print("Fit changed")
                allFits$fits[[i]]$fit=newFit
                checkFit(i,allFits) #continue checking this fit until it stops changing
            }
            
        }
        return(allFits$fits[[i]]$fit)
    }
    if(debug){
        for(j in 1:length(fits$fits)){
            fits$fits[[j]]$fit=checkFit(j,fits)
        }
    }else{
        parFun=function(k){
            return(checkFit(k,fits))
        }
        fitList=mclapply(c(1:length(fits$fits)),parFun, mc.cores=numCores)
        if(length(fitList)!=length(fits$fits)){
            print(paste("Parallel processing has resulted in dropped values, try again with a value of numCores smaller than",numCores))
            return(FALSE)
        }
        for(j in 1:length(fits$fits)){
            fits$fits[[j]]$fit=fitList[[j]]
        }
    }
    return(fits)
}

forceNonFlatFits=function(fits,minRise=0.3,numCores=-1){
    notDone=fits
    fixedFits=fits
    fixedFits$fits=list()
    for(rise in seq(from=fits$CPMaxF,to=1,by=fits$CPMaxF)){
        print(paste("CPMaxF=",rise,sep=""))
        notDone$CPMaxF=rise
        newFits=fixFlatFits(notDone,minRise,FALSE,numCores)
        notDone=newFits
        doneIndices=c()
        for(i in 1:length(notDone$fits)){
            fit=notDone$fits[[i]]
            curves=fit$fit$curves
            measured=curves[curves$curve=="measured",]$F
            model=curves[curves$curve=="model",]$F
            if((max(model)/max(measured))>=minRise){
                                        #Done!
                fixedFits$fits[[length(fixedFits$fits)+1]]=notDone$fits[[i]]
                doneIndices=c(doneIndices,i)
            }
        }
        notDone$fits=notDone$fits[-1*doneIndices]
        if(length(notDone$fits)<1){
            break
        }
    }
    return(fixedFits)
}

extractTimeConst=function(frame, time="t", force="force", zPos="zSensr", dwellTime="dwell", debug=FALSE){
                                        #frame is the raw data for the compression as produced by the loadIBW function. time, force, zPos and dwellTime are the names of the columns of frame which contain the time, force, indentation, and dwellTime data. Defaults correspond to the values used in data imported using loadIBW, batchLoad and quickLoad. Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    if(debug){
        library(ggplot2)
    }
    ret=getDwell(frame,frame[1,dwellTime],zPos,force,time)
    decayData=data.frame(t=ret[,time]-ret[1,time],F=ret[,force],FZero=ret[1,force],zPos=ret[,zPos])
    decayFit=nls("F ~ (FZero-C)*exp(-1*t*tau) + C",decayData,start=c(tau=1,C=.1,FZero=1),control=nls.control(warnOnly=TRUE))
    fitData=data.frame(residual=sum(residuals(decayFit)^2),tau=as.numeric(coef(decayFit)["tau"]),C=as.numeric(coef(decayFit)["C"]),converged=decayFit$convInfo$isConv)
    measured=data.frame(t=decayData$t,F=decayData$F,curve="measured")
    model=data.frame(t=decayData$t,F=(ret[1,force]-fitData$C)*exp(-1*decayData$t*fitData$tau)+fitData$C,curve="model")
    if(debug){
        p=ggplot(rbind(measured,model))+geom_path(aes(x=t,y=F,color=curve))
        print(p)
        readline()
    }
    
    return(list(fit=fitData,curves=rbind(measured,model)))
}

parExtractTimeConst=function(cases, time="t", force="force", zPos="zSensr", dwell="dwell", debug=FALSE, numCores=-1){
                                        #This function extracts the viscous time constants from the curves contained in cases (which should be constructed using identIterate()). Time, force, zPos, and dwell are the names for the columns corresponding to the time, force, indentation and dwell data in the original data frame. Default values correspond to those in data imported using loadIBW, batchLoad() and quickLoad(). numCores is the number of cores to use in order to do the calculation (if -1 is used, the function will use all available cores). Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    library(parallel)
    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }
    parFun=function(case){
        toReturn=extractTimeConst(case$data, time, force, zPos, dwell, debug)
        return(list(fit=toReturn,ident=case$ident))
    }

    if(debug){
        for(ca in cases){
            parFun(ca)
        }
    }else{
        fits=mclapply(cases,parFun,mc.cores=numCores)
    }
    if(length(fits)!=length(cases)){
        print(paste("Not enough RAM for all cores, try again with numCores<",numCores,sep=""))
        return(FALSE)
    }
    return(list(fits=fits, time=time, force=force, zPos=zPos))
    
}

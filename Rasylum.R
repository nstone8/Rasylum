saveFigs = function(folder, names, figures){
    graphics.off()
                                        #figures must be a list of figure objects
    if(regexpr("/$", folder)==-1){
        folder=paste(folder,"/",sep="")
    }
    if(length(names)!=length(figures)){
        print("Number of names and figures don't match")
        return(FALSE)
    }
    for(i in 1:length(names)){
        name=names[i]
        if(regexpr("\\.",name)==-1){ #No file extension
            name=paste(name,".pdf",sep="")
        }
        pdf(paste(folder,name,sep=""))
        print(figures[[i]])
        dev.off()
    }
}

saveCurves = function(folder, basename, curves){
                                        #Curves is a list of curve objects created by the extractStiffness functions
    graphics.off()
    library(ggplot2)
    if(regexpr("/$", folder)==-1){
        folder=paste(folder,"/",sep="")
    }
    for(i in 1:length(curves)){
        name=paste(basename,i,".pdf",sep="")
        plot=ggplot(curves[[i]],aes(x=zPos,y=F,color=curve))+geom_line()+labs(title=name)
        pdf(paste(folder,name,sep=""))
        print(plot)
        dev.off()
    }
}

saveFits=function(filename,fitData){
                                        #FitData should be a object created by parExtractStiffness
    graphics.off()
    library(ggplot2)
    pdf(filename)
    for(j in 1:length(fitData$fit)){
        fit=fitData$fit[[j]]
        curve=fit$fit$curves
        id=fit$ident
        fields=names(id)
        name=""
        for(i in 1:length(fields)){
            name=paste(name,fields[i],id[1,fields[i]])
        }
        plot=ggplot(curve,aes(x=zPos,y=F,color=curve))+geom_line()+labs(title=name)
        
        print(plot)
        print(name)
        
    }
    dev.off()
}

collateFits=function(fitData){
                                        #Return a single data frame containing all fit values and identifiers
    allFits=cbind(fitData$fits[[1]]$fit$fit,fitData$fits[[1]]$ident)
                                        #  print(fitData$fits[[1]])
    for(f in 2:length(fitData$fits)){
                                        #     print(fitData$fits[[f]]$ident)
        allFits=rbind(allFits,cbind(fitData$fits[[f]]$fit$fit,fitData$fits[[f]]$ident))
    }
    return(allFits)   
}

normalizeFrame=function(frame,column,identifiers,wrt,value="lowest"){
                                        #normalize the column column in the dataframe frame. Identifiers should be a list of the column names which can uniquely identify each case to be normalized. wrt and value specifies whether normalization should be with respect to the lowest or highest value of column wrt
    output=data.frame()
    iterator=identIterate(frame,identifiers)    
    for(i in 1:length(iterator)){
        row=0
        entry=iterator[[i]]$data
        if(all(dim(entry$data)>0)){
            if(regexpr("lowest",value)>0){
                row=which.min(as.numeric(as.character(entry[,wrt])))
                print(paste("cell=",entry[row,"cell"],"minimum",wrt,"=",entry[row,wrt]))
            }else if(regexpr("highest",value)>0){
                row=which.max(as.numeric(as.character(entry[,wrt])))
            }else{
                print("value must be 'lowest' or 'highest'")
                return(-1)
            }
            entry[,column]=entry[,column]/entry[row,column]
            output=rbind(output,entry)
        }
    }
    return(output)
}

stripRet=function(frame,zPos){
                                        #Strip the retraction curve from imported ibw data stored in frame (a data frame) zPos should be the column of that frame corresponding to the position of the z piezo (deflection would also probably work)
    zData=frame[zPos]
                                        #remove retract curve and any tail at the start of the extension
    endPoint=which.max(as.numeric(zData[,]))
    startPoint=which.min(as.numeric(zData[1:endPoint,]))
    return(frame[startPoint:endPoint,])
}
loadIBW = function(wave){
    library(IgorR)
    waveData=read.ibw(wave)
    notes=strsplit(attr(waveData,"Note"),"\\r")[[1]]
    inVols=notes[regexpr("^InvOLS:",notes)!=-1]
    k=notes[regexpr("^SpringConstant:",notes)!=-1]
                                        #Strip labels
    inVols=as.numeric(substr(inVols,regexpr(":",inVols)+1,nchar(inVols)))
    k=as.numeric(substr(k,regexpr(":",k)+1,nchar(k)))
                                        #Force=k*defl (the software has already taken the invOLS into account)
    
    return(list(zSensr=as.numeric(waveData[,3]), rawZSensr=as.numeric(waveData[,1]),defl=as.numeric(waveData[,2]), force=k*waveData[,2],filename=wave, inVols=inVols, k=k))
    
    
    return(collatedData)

}

findLocalMinima=function(vec, roughness,Q,approachTrim){

                                        #find local minima in a vector, increasing roughness makes the function less sensitive to shallow valleys (value should be between 0 and 1). Q is the minimum depth/width ratio of the minima. ApproachTrim is the amount of data to trim off of the beginning of the curve to remove noise. return an empty data frame if no local minima are found
    startIndex=floor(approachTrim*length(vec))
    vec=vec[floor(approachTrim*length(vec)):length(vec)]
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
    return(list(indices=sharpIndices+(startIndex-1),heights=sharpHeights, Qs=sharpnesses))
    
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
    data=data.frame()
    fileNo=1
    for(f in filenames){
        print(paste(fileNo/length(filenames)*100,"% complete",sep=""))
        fileNo=fileNo+1
        vars=c()
        delims=c(consts,suffix)
                                        #        return(delims)
        for(i in 1:(length(delims)-1)){
            vars[i]=substr(f,regexpr(delims[i],f)+nchar(delims[i]),regexpr(delims[i+1],f)-1)
        }
        unlabeledData=loadIBW(paste(folder,f,sep=""))
        newcol=data.frame(vars[1])
        for(v in 2:length(vars)){
            newcol=cbind(newcol,vars[v])
        }
        names(newcol)=consts
        data=rbind(data,cbind(unlabeledData,newcol))
    }
    return(data)
}

stiffnessSphereOnPlane=function(rBead, extZ, extForce, CPMaxF=.05, percentToFit=.1, roughness=.01, Q=.5, approachTrim=.1, debug=FALSE){
    if(debug){
        library(ggplot2)
    }
                                        # Extract the stiffness and contact point for sphere on plane indentation. Assume that the bead is much stiffer than the cell
                                        #First find slightly overestimated CP
    minForce=min(extForce)
    thresh=minForce+CPMaxF*(max(extForce)-minForce)
                                        #roughCPIndex=which.min(abs(extForce-thresh))
    roughCPIndex=length(extForce)
    for(i in length(extForce):1){
        if(extForce[i]<thresh){
            roughCPIndex=i
            break
        }
    }
    eq="F~(4/3)*EStar*(r^(1/2))*indent^(3/2)"
    fits=data.frame()
    lengthForce=length(extForce)
                                        #        allFits=c() #Delete later
    fitLength=floor(lengthForce*percentToFit)
    stopIndex=lengthForce-fitLength+1
                                        #print(paste("fit Length=",fitLength,sep=""))

    for(i in 1:min(stopIndex,roughCPIndex)){
                                        #Perform fits at each point, assuming F[i]=0 and indent[i]=0
        
        contactData=data.frame(indent=extZ[i:(i+fitLength-1)]-extZ[i],F=extForce[i:(i+fitLength-1)]-extForce[i],r=rep(rBead,times=fitLength))
        if(all(contactData$indent>=0)){
            result=nls(eq,contactData,control=nls.control(warnOnly=TRUE))

            fits=rbind(fits,data.frame(contactIndex=i,contactPos=extZ[i],contactForce=extForce[i],residual=sum(residuals(result)^2), sigma=summary(result)$sigma,EStar=as.numeric(coef(result)["EStar"]),converged=result$convInfo$isConv))
        }
    }                           
    bestValues=fits[which.min(fits$residual),] #default option
    minima=findLocalMinima(fits$residual,roughness,Q,approachTrim)
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
        print(ggplot(data.frame(index=1:length(fits$residual),score=fits$residual),aes(x=index,y=score))+geom_line())
        readline()
    }

    fitCurve=data.frame(zPos=extZ[bestValues$contactIndex:length(extForce)], F=((4/3)*bestValues$EStar*(rBead^(1/2))*(extZ[bestValues$contactIndex:length(extForce)]-bestValues$contactPos)^(3/2))+bestValues$contactForce, curve="model")
    plotData=rbind(data.frame(zPos=extZ, F=extForce, curve="measured"),fitCurve)
    if(debug){
        print(ggplot(plotData,aes(x=zPos,y=F,color=curve))+geom_line())
        readline()
    }
    return(list(fit=bestValues,curves=plotData))
}

stiffnessSphereOnSphere=function(rBead, extZ, extForce,percentToFit=.1){
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

batchExtractStiffness=function(rBead, frame, zPos, force, identifiers, CPMaxF=.05, percentToFit=.1){
                                        #Extract info from multiple curves. frame should be a data frame containing all the data, and zPos and force should be the names of the columns corresponding to those variables. identifiers should be a vector of row names whose values uniquely identify each curve

    identifierIter=list()
    identifierIterLength=0
    for(ident in identifiers){
        identifierIterLength=identifierIterLength+1
        frame[,ident]=factor(frame[,ident])
        identifierIter[[identifierIterLength]]=list(name=ident,index=1,values=levels(frame[,ident]))
    }
    doneIterating=FALSE
    results=list(values=data.frame(), curves=list())
    while(!doneIterating){
        thisCurve=data.frame(frame)
                                        #print(paste("Initial Size",dim(thisCurve)))        
        for(id in identifierIter){#Select Values corresponding to this iteration
            if(all(dim(thisCurve)>0)){
                print(paste(id$name,id$values[id$index]))
                thisCurve=thisCurve[thisCurve[id$name]==id$values[id$index],]
            }
                                        #print(paste("trimming... size=",dim(thisCurve)))
        }
                                        #Get the data for this iteration and add it to our results
                                        #return(list(iter=identifierIter,curve=thisCurve))
        if(is.numeric(dim(thisCurve)) && !any(is.na(thisCurve))){
            thisCurveSize=dim(thisCurve)
                                        #print(paste("this curve's size=",thisCurveSize))
            if(all(thisCurveSize>0)){
                thisCurve=stripRet(thisCurve,zPos)
                thisRowFit=stiffnessSphereOnPlane(rBead,thisCurve[zPos][,],thisCurve[force][,],CPmaxF,percentToFit)
                thisRowValues=thisRowFit$fit
                for(id in identifierIter){#Build new row
                    thisRowValues=cbind(thisRowValues,data.frame(id$values[id$index]))
                    names(thisRowValues)[length(names(thisRowValues))]=id$name
                }
                results$values=rbind(results$values,thisRowValues)
                results$curves[[length(results$curves)+1]]=thisRowFit$curves
            }
        }
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
    print("Done!")
    return(results)
}

identIterate=function(frame,identifiers){
    library(parallel)
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
        thisCurve=data.frame(frame)  
        for(i in 1:length(values)){#Select Values corresponding to this iteration
            if(all(dim(thisCurve)>0)){
                thisCurve=thisCurve[thisCurve[,names(values)[i]]==as.character(values[1,i]),]

            }
        }
                                        #Get the data for this iteration and add it to our results
        if(is.numeric(dim(thisCurve)) && !any(is.na(thisCurve))){
            thisCurveSize=dim(thisCurve)
            if(all(thisCurveSize>0)){
                
                return(list(data=thisCurve,ident=values))
            }
        }
    }
    toRun=mclapply(allOptions,trimDown,mc.cores=detectCores())
    output=list()
    outputLength=0
    for(i in 1:length(toRun)){
        if(!is.null(toRun[[i]])){
            outputLength=outputLength+1
            output[[outputLength]]=toRun[[i]]
        }
    }
    return(output)
}

parExtractStiffness=function(rBead, cases, zPos, force, CPMaxF=.05, percentToFit=.1, roughness=0.05,Q=.5, approachTrim=.1, debug=FALSE, minRise=0){
    library(parallel)
                                        #allCases is a list of curves to fit as generated by the identIter function
    trimmedCases=list()
    lengthTrimmedCases=0
                                        #remove cases with 0 length data (I think these are leaking in from mclapply inserting empty entries where the subsetting function doesn't return anything
    for(i in 1:length(cases)){
        if(!is.null(cases[[i]])){ #This shouldn't be necessary anymore
            cases[[i]]$data=stripRet(cases[[i]]$data,zPos)
            lengthTrimmedCases=lengthTrimmedCases+1
            trimmedCases[[lengthTrimmedCases]]=cases[[i]]
        }
    }
    parFun=function(case){
        print(case$ident)
        toReturn=stiffnessSphereOnPlane(rBead, case$data[zPos][,], case$data[force][,], CPMaxF, percentToFit, roughness,Q,approachTrim,debug)
        return(list(fit=toReturn,ident=case$ident))
    }
                                        # return(trimmedCases)
    if(debug){
        for(ca in trimmedCases){
            parFun(ca)
        }
    }
    fits=mclapply(trimmedCases,parFun,mc.cores=detectCores())
    toReturn=list(fits=fits,rBead=rBead, zPos=zPos, force=force, CPMaxF=CPMaxF, percentToFit=percentToFit,roughness=roughness,Q=Q, approachTrim=approachTrim)
    if(minRise>0){
        fixFlatFits(toReturn,minRise)

    }
    return(toReturn)
}

fixFlatFits=function(fits, minRise=.1,debug=FALSE){
                                        #Sometimes the method for finding the best fit chooses to fit a portion of the approach curve before the probe is in contact (resulting in a stiffness of ~0. This function attempts to find these cases (by comparing the maximum values of the measured and modeled curves) and produce a better fit
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
        fitList=mclapply(c(1:length(fits$fits)),parFun, mc.cores=detectCores())
        for(j in 1:length(fits$fits)){
            fits$fits[[j]]$fit=fitList[[j]]
        }
    }
    return(fits)
}

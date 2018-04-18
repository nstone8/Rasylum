identIterate=function(frame,identifiers,numCores=-1){
                                        #returns a list of lists defining different important groupings of observations. frame is a data frame containing the raw data, identifiers is the names of the columns of frame where unique values correspond to groups. numCores is the number of cores to use in order to do the calculation (if -1 is used, the function will use all available cores)
    frame=as.data.frame(frame)
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

collateFits=function(fitData){
                                        #Return a single data frame containing all fit values and identifiers. fitData should be an object created by parExtractStiffness or parExtractTimeConst
    allFits=list()
    allFits[[1]]=cbind(fitData$fits[[1]]$fit$fit,fitData$fits[[1]]$ident)
    numFits=length(fitData$fits)
    if(numFits>1)){
        for(f in 2:numFits){        
            allFits[[f]]=cbind(fitData$fits[[f]]$fit$fit,fitData$fits[[f]]$ident)
        }
    }
    return(buildFrame(allFits,numFits))   
}

collateFits=function(fitData){
                                        #Return a single data frame containing all fit values and identifiers. fitData should be an object created by parExtractStiffness or parExtractTimeConst
    allFits=list()
    theseIdents=fitData$fits[[1]]$ident
    if(length(theseIdents)<2){
        theseIdents=data.frame(theseIdents)
        names(theseIdents)=names(fitData$fits[[1]]$ident)
    }
    allFits[[1]]=cbind(fitData$fits[[1]]$fit$fit,theseIdents)
    numFits=length(fitData$fits)
    if(numFits>1){
        for(f in 2:numFits){
            theseIdents=fitData$fits[[f]]$ident
            if(length(theseIdents)<2){
                theseIdents=data.frame(theseIdents)
                names(theseIdents)=names(fitData$fits[[f]]$ident)
            }
            allFits[[f]]=cbind(fitData$fits[[f]]$fit$fit,theseIdents)
        }
    }
    return(buildFrame(allFits,numFits))   
}

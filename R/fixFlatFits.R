fixFlatFits=function(fits, minRise=.1,debug=FALSE, numCores=-1){
    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }
                                        #Sometimes the method for finding the best fit chooses to fit a portion of the approach curve before the probe is in contact (resulting in a stiffness of ~0. This function attempts to find these cases (by comparing the maximum values of the measured and modeled curves) and produce a better fit. Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    checkFit=function(i,allFits){
        fitCurves=allFits$fits[[i]]$fit$curves
        measured=fitCurves[fitCurves$curve=="measured",]
        model=fitCurves[fitCurves$curve=="model",]
        rangeMeasured=max(measured$F)-min(measured$F)
        if(1-(abs(max(model$F)-max(measured$F))/rangeMeasured)<minRise){
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

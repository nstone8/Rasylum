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

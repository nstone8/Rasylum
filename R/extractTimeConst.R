extractTimeConst=function(frame, time="t", force="force", zPos="zSensr", dwellTime="dwell", debug=FALSE, extraTrim=0){
                                        #frame is the raw data for the compression as produced by the loadIBW function. time, force, zPos and dwellTime are the names of the columns of frame which contain the time, force, indentation, and dwellTime data. Defaults correspond to the values used in data imported using loadIBW, batchLoad and quickLoad. Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    ret=getDwell(frame,frame[1,dwellTime],zPos,force,time,extraTrim)
    decayData=data.frame(t=ret[,time]-ret[1,time],F=ret[,force],FZero=ret[1,force],zPos=ret[,zPos])
    cGuess=decayData$F[length(decayData$F)]
    #aGuess=decayData$F[1]-cGuess
    aGuess=0.9
    eThreshold=decayData$FZero[1]-.63*(decayData$FZero[1]-cGuess) #Point where force has decayed by 1/e
    eTime=decayData[decayData$F<eThreshold,]$t[1] #time where force has decayed by 1/e
    tau1guess=1/eTime
    tau2guess=tau1guess*.1
    #print(paste("cGuess=",cGuess,"aGuess=",aGuess,"tau1guess=",tau1guess,"tau2guess=",tau2guess,"F0=",decayData$FZero[1]))
    if(debug){
        print("residual: tau1 tau2 A C")
    }
    decayFit=nls("F ~ (FZero-C)*((abs(A)%%1)*exp(-1*t*tau1)+(1-(abs(A)%%1))*exp(-1*t*tau2)) + C",decayData,start=c(tau1=tau1guess,tau2=tau2guess,A=aGuess,C=cGuess),control=nls.control(warnOnly=TRUE,maxiter=1000,minFactor=1/4096),trace=debug)
    fitData=data.frame(residual=sum(residuals(decayFit)^2),tau1=as.numeric(coef(decayFit)["tau1"]),C=as.numeric(coef(decayFit)["C"]),tau2=as.numeric(coef(decayFit)["tau2"]),A=as.numeric(coef(decayFit)["A"]),converged=decayFit$convInfo$isConv)
    if(debug){
	print(paste("converged=",fitData$converged))
	print(fitData)
    }
    measured=data.frame(t=decayData$t,F=decayData$F,curve="measured")

    model=data.frame(t=decayData$t,F=(ret[1,force]-fitData$C)*(fitData$A*exp(-1*decayData$t*fitData$tau1)+(1-fitData$A)*exp(-1*decayData$t*fitData$tau2))+fitData$C,curve="model")
    if(debug){
        p=ggplot(rbind(measured,model))+geom_path(aes(x=t,y=F,color=curve))
        print(p)
        readline()
    }
    
    return(list(fit=fitData,curves=rbind(measured,model)))
}

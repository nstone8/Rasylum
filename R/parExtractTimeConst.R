parExtractTimeConst=function(cases, time="t", force="force", zPos="zSensr", dwell="dwell", debug=FALSE, numCores=-1,extraTrim=0){
                                        #This function extracts the viscous time constants from the curves contained in cases (which should be constructed using identIterate()). Time, force, zPos, and dwell are the names for the columns corresponding to the time, force, indentation and dwell data in the original data frame. Default values correspond to those in data imported using loadIBW, batchLoad() and quickLoad(). numCores is the number of cores to use in order to do the calculation (if -1 is used, the function will use all available cores). Setting debug to TRUE causes the function to plot each fit (the user should press 'enter' to cycle through fits) in order to allow for tuning of fit parameters.
    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }
    parFun=function(case){
    	print(case$ident)
        toReturn=extractTimeConst(case$data, time, force, zPos, dwell, debug,extraTrim)
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
    testFun=function(case){
	return(class(case)!="try-error")
    }
    fits=Filter(testFun,fits)
    return(list(fits=fits, time=time, force=force, zPos=zPos))
    
}

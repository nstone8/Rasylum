parExtractStiffness=function(cases,r,approachLength=.1,contactLength=.1,searchWidth=.2,maxF=.5,weight=4,correctVirtDefl=TRUE,zPos="zSensr",force="force",numCores=-1){

    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }

    parFun=function(case){
        print(case$ident)
        return(extractStiffness(case,r,approachLength,contactLength,searchWidth,maxF,weight,correctVirtDefl,zPos,force))
    }            
    fits=mclapply(cases,parFun,mc.cores=numCores)
    if(length(fits)!=length(cases)){
        stop(paste("Parallel processing has resulted in dropped values, try again with a value of numCores smaller than",numCores))
    }
    testFun=function(case){
	return(class(case)!="try-error")
    }
    fits=Filter(testFun,fits)
    toReturn=list(fits=fits,r=r,approachLength=approachLength,contactLength=contactLength,searchWidth=searchWidth,maxF=maxF,weight=weight,correctVirtDefl=correctVirtDefl,zPos=zPos,force=force)

    return(toReturn)
}

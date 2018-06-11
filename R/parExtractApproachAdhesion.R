parExtractApproachAdhesion=function(cases,percentFlat=0.8,numCores=-1){
    if(numCores<0){
        numCores=detectCores()-1 #Default to giving R a bonus core to play with
    }
    oneArgFun=function(c){
        return(extractApproachAdhesion(c,percentFlat))
    }
    adhesion=unlist(mclapply(cases,oneArgFun,mc.cores=numCores))
    idents=c()
    for(case in cases){
        idents=rbind(idents,case$ident)
    }
    return(cbind(idents,data.frame(Adhesion=adhesion)))
}

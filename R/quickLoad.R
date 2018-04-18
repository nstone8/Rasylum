quickLoad=function(folder,consts,suffix){
                                        #Convenience function for running batchload and buildFrame with one command
    imp=batchLoad(folder,consts,suffix)
    return(buildFrame(imp$data,imp$numRows))
}

loadPreSorted=function(folder,consts,suffix){
    imp=batchLoad(folder,consts,suffix)
    iterated=list()
    for(i in 1:length(imp$data)){
        iterated[[i]]=list(data=imp$data[[i]],ident=imp$data[[i]][1,consts])
        if(length(consts<2)){
            names(iterated[[i]]$ident)=consts
        }
    }
    return(iterated)
}

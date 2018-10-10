loadCase=function(filename,ident=NA){
    ibw=loadIBW(filename,asDataFrame=TRUE)
    if(!any(is.na(ident))){
        ibw=cbind(ibw,ident)
    }
    return(list(data=ibw,ident=ident))
}

normalizeFrame=function(frame,column,identifiers,wrt,value="lowest"){
                                        #normalize the column column in the dataframe frame. Identifiers should be a list of the column names which can uniquely identify each case to be normalized. wrt and value specifies whether normalization should be with respect to the lowest or highest value of column wrt
    output=as.data.table(frame)
    output[,names(output):=NA]
    curOutputRow=1
    iterator=identIterate(frame,identifiers,1)    
    for(i in 1:length(iterator)){
        row=0
        entry=iterator[[i]]$data
        if(all(dim(entry$data)>0)){
            if(regexpr("lowest",value)>0){
                row=which.min(as.numeric(as.character(entry[,wrt])))
            }else if(regexpr("highest",value)>0){
                row=which.max(as.numeric(as.character(entry[,wrt])))
            }else{
                print("value must be 'lowest' or 'highest'")
                return(-1)
            }
            entry[,column]=entry[,column]/entry[row,column]
            lengthEntry=dim(entry)[1]
            output[curOutputRow:(curOutputRow+lengthEntry-1),names(output):=entry]            
            curOutputRow=curOutputRow+lengthEntry
        }
    }
    return(output)
}

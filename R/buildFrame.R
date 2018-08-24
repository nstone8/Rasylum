buildFrame=function(dataList, numRows){
                                        #Transforms dataList, which is a list of lists with the same member names into a data.table with those names as columns
    dataTable=as.data.table(matrix(nrow=numRows, ncol=length(dataList[[1]])))
    print("Allocated table")
                                        #Replace columns so they have the correct type
    colNames=names(dataList[[1]])
    names(dataTable)=colNames
    numCol=dim(dataList[[1]])[2]
    for(j in 1:numCol){
        print(paste((j/numCol)*100,"% Types Defined",sep=""))
        dataTable[,as.integer(j):=rep(as.data.frame(dataList[[1]])[1,j],numRows)]
    }
    curRow=1
    for(i in 1:length(dataList)){
        print(paste((i/length(dataList))*100,"% Written",sep=""))
        dataTable[curRow:(curRow+dim(dataList[[i]])[1]-1),names(dataTable):=dataList[[i]]]
        curRow=curRow+dim(dataList[[i]])[1]
    }
    return(dataTable)
}

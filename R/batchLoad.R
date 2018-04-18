batchLoad=function(folder,consts,suffix){
                                        #consts is a vector of strings, suffix is a string
                                        # Load .ibw files from 'folder' with names of the form const1variableRegionconst2variableregion...suffix and return a data frame with each const as a column containing it's corresponding variable region
    if(regexpr("/$",folder)==-1){
        folder=paste(folder,"/",sep="")
    }
    pat="^"
    for(const in consts){
        pat=paste(pat,const,".*",sep="")
    }
    pat=paste(pat,suffix,"$",sep="")
    filenames=list.files(folder,pat)
    data=list()
    lengthData=0
    fileNo=1
    totalNumRows=0
    for(f in filenames){
        print(paste(fileNo/length(filenames)*100,"% complete",sep=""))
        fileNo=fileNo+1
        vars=c()
        delims=c(consts,suffix)                                   
        for(i in 1:(length(delims)-1)){
            vars[i]=substr(f,regexpr(delims[i],f)+nchar(delims[i]),regexpr(delims[i+1],f)-1)
        }
        unlabeledData=loadIBW(paste(folder,f,sep=""),TRUE)
        newcol=data.frame(vars[1])
        if(length(vars)>1){
            for(v in 2:length(vars)){
                newcol=cbind(newcol,vars[v])
            }
        }
        names(newcol)=consts
        totalNumRows=totalNumRows+dim(unlabeledData)[1]
        lengthData=lengthData+1        
        data[[lengthData]]=cbind(unlabeledData,newcol)
    }
    return(list(data=data,numRows=totalNumRows))
}

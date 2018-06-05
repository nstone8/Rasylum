loadARDF=function(ardf){
                                        # Loads Asylum force map data files in .ardf format
    mapPoints=names(ardf$data)	#Names of individual force curves in map
    sampleRate=as.numeric(ardf$headers$NumPtsPerSec)	#Sample rate for each point    
    inVols=as.numeric(ardf$headers$InvOLS)	#Deflection InvOLS in m/V
    k=as.numeric(ardf$headers$SpringConstant)	#Cantilever spring constant in N/m
    dwell=as.numeric(ardf$headers$DwellTime)	#Dwell time in seconds - need to check if towards or away*
    data=list()
    lengthData=0
    pointNo=1
    totalNumRows=0
    for(p in 1:length(mapPoints)){
        print(paste(pointNo/length(mapPoints)*100,"% complete",sep=""))
        pointNo=pointNo+1 
        vars=c()                                  
        vars[1]=substr(mapPoints[p],5,8) 	#Get the four digit line number 
        vars[2]=substr(mapPoints[p],14,17) #Get the four digit point number
        unlabeledData=data.frame(t=c(0:(length(ardf$data[[p]]$Defl)-1))*1/sampleRate, zSensr=ardf$data[[p]]$ZSnsr, rawZSensr=ardf$data[[p]]$Raw,defl=ardf$data[[p]]$Defl, force=k*ardf$data[[p]]$Defl,filename=mapPoints[p], inVols=inVols, k=k, dwell=dwell)			#Same dataframe format as loadIBW
        newcol=data.frame(vars[1])
        newcol=cbind(newcol,vars[2])
        names(newcol)=c("Line","Point")
        totalNumRows=totalNumRows+dim(unlabeledData)[1]
        lengthData=lengthData+1        
        data[[lengthData]]=cbind(unlabeledData,newcol)
    }
    imp=list(data=data,numRows=totalNumRows)
    iterated=list()
    for(i in 1:length(imp$data)){
        iterated[[i]]=list(data=imp$data[[i]],ident=imp$data[[i]][1,c("Line","Point")])
    }
    return(iterated)
}

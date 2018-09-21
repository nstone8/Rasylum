loadIBW = function(wave,asDataFrame=FALSE){
    waveData=read.ibw(wave)
    sampleTime=attr(waveData,"WaveHeader")$sfA[1]
    notes=strsplit(attr(waveData,"Note"),"\\r")[[1]]
    inVols=notes[regexpr("^InvOLS:",notes)!=-1]
    k=notes[regexpr("^SpringConstant:",notes)!=-1]
    dwell=notes[regexpr("^DwellTime:",notes)!=-1]
                                        #Strip labels
    inVols=as.numeric(substr(inVols,regexpr(":",inVols)+1,nchar(inVols)))
    k=as.numeric(substr(k,regexpr(":",k)+1,nchar(k)))
    dwell=as.numeric(substr(dwell,regexpr(":",dwell)+1,nchar(dwell)))
                                        #Force=k*defl (the software has already taken the invOLS into account)
    if(asDataFrame){
        return(data.frame(t=c(0:(dim(waveData)[1]-1))*sampleTime,zSensr=as.numeric(waveData[,3]), rawZSensr=as.numeric(waveData[,1]),defl=as.numeric(waveData[,2]), force=k*waveData[,2],filename=wave, inVols=inVols, k=k, dwell=dwell))
    }else{
        return(list(t=c(0:(dim(waveData)[1]-1))*sampleTime,zSensr=as.numeric(waveData[,3]), rawZSensr=as.numeric(waveData[,1]),defl=as.numeric(waveData[,2]), force=k*waveData[,2],filename=wave, inVols=inVols, k=k,dwell=dwell))
    }
}

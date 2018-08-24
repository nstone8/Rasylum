read.ardf=function(filename){
    textHex=c(0x54,0x45,0x58,0x54)
    channelHex=c(0x56,0x43,0x48,0x4e) 
    nameHex=c(0x56,0x4e,0x41,0x4d)
    f=file(filename,open="rb")
    fInfo=file.info(filename)
    byteStream=readBin(f,"raw",fInfo$size)
    print("Loading File")
    close(f)
                                        #search for TEXT tag
    textTagIndices=c()
    nameHexIndices=c()
    channelHexIndices=c()
    for(i in 1:(fInfo$size-4)){
        if(all(byteStream[i:(i+3)]==textHex)){
            textTagIndices=c(textTagIndices,i)
        }else if(all(byteStream[i:(i+3)]==nameHex)){
            nameHexIndices=c(nameHexIndices,i)
        }else if(all(byteStream[i:(i+3)]==channelHex)){
            channelHexIndices=c(channelHexIndices,i)
        }
    }
    textSections=getTextSections(textTagIndices,byteStream)
    channels=getChannels(channelHexIndices,byteStream)
    dataSections=getDataSections(nameHexIndices,byteStream,channels)
    return(list(headers=textSections,data=dataSections))
}

getTextSections=function(textTagIndices,byteStream){
    print("Reading Headers")
    textSections=list()
    for(index in textTagIndices){
        thisText=c()
                                        #search for 3 non-null bytes in a row, indicates that data is starting
        actualStart=index+4
        while(any(byteStream[actualStart:(actualStart+3)]==0x00)){
            actualStart=actualStart+1
        }
                                        #Now slurp up key-value pairs, separated by 0x3a and terminated by 0x0d
        currentIndex=actualStart
        while(byteStream[currentIndex]!=0x00){ #Watch for end of text block
            thisKey=c()
            while(byteStream[currentIndex]!=0x3a){ #Fill up header
                thisKey=c(thisKey,byteStream[currentIndex])
                currentIndex=currentIndex+1
            }
            currentIndex=currentIndex+1
            thisValue=c()
            while(byteStream[currentIndex]!=0x0d){
                thisValue=c(thisValue,byteStream[currentIndex])
                currentIndex=currentIndex+1
            }
            thisPair="" #If value is empty default to empty string
            if(length(thisValue)>0){ #replace default if value isn't empty
                thisPair=rawToChar(thisValue)
            }
            names(thisPair)=rawToChar(thisKey)
            thisText=c(thisText,thisPair)

            currentIndex=currentIndex+1
        }
        textSections=c(textSections,as.list(thisText))
    }
    return(textSections)
}

getDataSections=function(nameIndices,byteStream,channels){
    dataHex=c(0x56,0x44,0x41,0x54)
    nameHex=c(0x56,0x4e,0x41,0x4d)
    allPoints=list()
    pointNames=c()
    allPointsIndex=1
    for(currentIndex in (nameIndices+4)){ #nameIndices contains the indices at the beginning of VNAM, we're interested in what comes after

                                        #First find the first instance of c(0x50,0x00,0x00,0x00,0x58,0x44,0x41,0x54), this will be the end of the last VDAT section
        maxIndex=currentIndex
        while(!all(byteStream[maxIndex:(maxIndex+7)]==c(0x50,0x00,0x00,0x00,0x58,0x44,0x41,0x54))){
            maxIndex=maxIndex+1
        }
                                        #Search for 3 non null bytes in a row, indicates name is starting (may only need 2 non null bytes in a row, but 3 should work)
        while(any(byteStream[currentIndex:(currentIndex+3)]==0x00)){
            currentIndex=currentIndex+1
        }
        #Now slurp up bytes until the first sequence of 2 null bytes, this will be the name of the dataset
        nameBytes=readToSequence(byteStream,currentIndex,c(0x00,0x00))
        currentIndex=currentIndex+length(nameBytes)
        thisName=rawToChar(nameBytes)
        print(thisName)
        dataVectors=list()
        dataVectorIndex=1

                                        #Find termination sequence of the form c(x,0x00,0x00) which is the 4 bytes before the first VDAT tag
        while(!all(byteStream[currentIndex:(currentIndex+3)]==dataHex)){
            currentIndex=currentIndex+1
        }

        terminationSequence=byteStream[(currentIndex-4):(currentIndex-1)]
        
        #Read up to next VDAT or VNAM tag, then read in the data for VDAT or skip to next name (break out of loop) for VNAM
        while(TRUE){
            #print(paste("currentIndex:",currentIndex,"maxIndex",length(byteStream)))
            while(any(byteStream[currentIndex:(currentIndex+3)]!=dataHex)&&any(byteStream[currentIndex:(currentIndex+3)]!=nameHex)){
                currentIndex=currentIndex+1
                if(currentIndex>length(byteStream)){
                    #We've read past the end of the file
                    break
                }
            }
            if(all(byteStream[currentIndex:(currentIndex+3)]==nameHex)){
                break
            }else if(all(byteStream[currentIndex:(currentIndex+3)]==dataHex)){
                                        #now skip to the end of VDAT
                currentIndex=currentIndex+4
                                        #skip 44 bytes to start of data
                currentIndex=currentIndex+44
                                        #Now slurp up bytes until the terminationSequence
                dataBytes=readToSequence(byteStream,currentIndex,terminationSequence,maxIndex)
                convertedData=readBin(dataBytes,"double",length(dataBytes),4)
                dataVectors[[dataVectorIndex]]=convertedData
                dataVectorIndex=dataVectorIndex+1
                currentIndex=currentIndex+length(dataBytes)

            }else if(currentIndex>length(byteStream)){
                #We've read to the end of the file
                print("EOF")
                break
            }else{
                stop("This shouldn't happen")
            }
        }
                                        #Package up the data from this name
                                        #Make data vectors all the same length
        targetLength=0
        for(dVec in dataVectors){
            targetLength=max(targetLength,length(dVec))
        }
        dFrame=data.frame(c())
        if(length(dataVectors[[1]])<targetLength){
            dataVectors[[1]]=c(dataVectors[[1]],rep(NA,times=targetLength-length(dataVectors[[1]])))
        }
        dFrame=data.frame(dataVectors[[1]])
        for(i in 2:length(dataVectors)){
            if(length(dataVectors[[i]])<targetLength){
                dataVectors[[i]]=c(dataVectors[[i]],rep(NA,times=targetLength-length(dataVectors[[i]])))
            }
            dFrame=cbind(dFrame,data.frame(dataVectors[[i]]))
        }
        names(dFrame)=channels
        dFrame=dFrame[1:(dim(dFrame)[1]-1),] #Last point seems to be garbage, not sure why
        if(any(is.na(dFrame))){
            warning(paste("A parsing error may have occured for",thisName,"please check data integrity"))
        }
        pointNames=c(pointNames,thisName)
        allPoints[[allPointsIndex]]=dFrame
        allPointsIndex=allPointsIndex+1
    }
    names(allPoints)=pointNames
    return(allPoints)
}
readToSequence=function(byteStream,startIndex=0,stopSequence,maxIndex=NA){
                                        #Read up to the first instance of stopSequence or up to maxIndex and return the resulting byte array
    theseBytes=c()
    currentIndex=startIndex
    while(!all(byteStream[currentIndex:(currentIndex+(length(stopSequence)-1))]==stopSequence)){
        if(!is.na(maxIndex)){ #return if currentIndex>maxIndex
            if(currentIndex>maxIndex){
                break
            }
        }
        theseBytes=c(theseBytes,byteStream[currentIndex])
        currentIndex=currentIndex+1
    }
    return(theseBytes)
}
getChannels=function(channelHexIndices,byteStream){
    channels=c()
    for(currentIndex in (channelHexIndices+4)){
                                        #read up to first non-null byte
        while(byteStream[currentIndex]==0x00){
            currentIndex=currentIndex+1
        }
        thisChannelBytes=readToSequence(byteStream,currentIndex,c(0x00,0x00))
        thisChannel=rawToChar(thisChannelBytes)
        channels=c(channels,thisChannel)
    }
    return(channels)
}

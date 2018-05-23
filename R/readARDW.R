read.ardw=function(filename){
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
    return(list(textSections=textSections,dataSections=dataSections,channels=channels))
}

getTextSections=function(textTagIndices,byteStream){
    print("Reading Headers")
    textSections=list()
    textSectionIndex=1
    for(index in textTagIndices){
        thisText=c()
                                        #search for 3 non-null bytes in a row, indicates that data is starting
        actualStart=index+4
        while(any(byteStream[actualStart:(actualStart+3)]==0x00)){
            actualStart=actualStart+1
        }
                                        #Now slurp up key-value pairs, separated by 0x3a and terminated by 0x0d
        currentIndex=actualStart
        while(!all(byteStream[currentIndex:(currentIndex+1)]==c(0x00,0x00))){ #Watch for end of text block
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
        textSections[[textSectionIndex]]=as.list(thisText)
        textSectionIndex=textSectionIndex+1
    }
    return(textSections)
}

getDataSections=function(nameIndices,byteStream,channels){
    dataHex=c(0x56,0x44,0x41,0x54)
    nameHex=c(0x56,0x4e,0x41,0x4d)
    allPoints=list()
    allPointsIndex=1
    for(currentIndex in (nameIndices+4)){ #nameIndices contains the indices at the beginning of VNAM, we're interested in what comes after
                                        #Search for 3 non null bytes in a row, indicates name is starting (may only need 2 non null bytes in a row, but 3 should work)
        while(any(byteStream[currentIndex:(currentIndex+3)]==0x00)){
            currentIndex=currentIndex+1
        }
        #Now slurp up bytes until the first sequence of 2 null bytes, this will be the name of the dataset
        nameBytes=readToNull(byteStream,currentIndex)
        currentIndex=currentIndex+length(nameBytes)
        thisName=rawToChar(nameBytes)
        print(thisName)
        dataVectors=list()
        dataVectorIndex=1
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
                                        #Now find the next set of 3 non-null bytes in a row, this is the start of our data
                while(any(byteStream[currentIndex:(currentIndex+3)]==0x00)){
                currentIndex=currentIndex+1
            }
                                        #Now slurp up bytes until the first sequence of 2 null bytes, this will be the name of the dataset
                dataBytes=readToNull(byteStream,currentIndex)
                convertedData=readBin(dataBytes,"double",length(dataBytes),4)
                convertedData=convertedData[1:(length(convertedData)-1)]#Last point seems to be garbage, not sure why
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
        names(dataVectors)=channels
        thisPoint=list(name=thisName,data=dataVectors)
        allPoints[[allPointsIndex]]=thisPoint
        allPointsIndex=allPointsIndex+1
    }
    return(allPoints)
}
readToNull=function(byteStream,startIndex=0){
                                        #Read up to the first set of 2 null bytes and return the resulting byte array
    theseBytes=c()
    currentIndex=startIndex
    while(!all(byteStream[currentIndex:(currentIndex+1)]==c(0x00,0x00))){
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
        thisChannelBytes=readToNull(byteStream,currentIndex)
        thisChannel=rawToChar(thisChannelBytes)
        channels=c(channels,thisChannel)
    }
    return(channels)
}

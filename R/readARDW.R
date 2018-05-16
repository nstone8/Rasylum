read.ardw=function(filename){
    textHex=c(0x54,0x45,0x58,0x54)

    f=file(filename,open="rb")
    fInfo=file.info(filename)
    byteStream=readBin(f,"raw",fInfo$size)
    close(f)
                                        #search for TEXT tag
    textTagIndices=c()
    for(i in 1:(fInfo$size-4)){
        if(all(byteStream[i:(i+3)]==textHex)){
            textTagIndices=c(textTagIndices,i)
        }
    }
    textSections=getTextSections(textTagIndices,byteStream)
    return(textSections)
}

getTextSections=function(textTagIndices,byteStream){
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
        textSections[[textSectionIndex]]=as.list(thisText)
        textSectionIndex=textSectionIndex+1
    }
    return(textSections)
}

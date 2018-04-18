stripApr=function(frame,zPos){
                                        #Strip the approach curve from imported ibw data stored in frame (a data frame) zPos should be the column of that frame corresponding to the position of the z piezo (deflection would also probably work)
    zData=frame[,zPos]
                                        #remove approach curve and any tail at the start of the extension
    startPoint=which.max(zData)
    endPoint=length(zData)
    return(frame[startPoint:endPoint,])
}

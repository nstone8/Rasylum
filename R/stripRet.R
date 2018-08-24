stripRet=function(frame,zPos){
                                        #Strip the retraction curve from imported ibw data stored in frame (a data frame) zPos should be the column of that frame corresponding to the position of the z piezo (deflection would also probably work)
    zData=frame[,zPos]
                                        #remove retract curve and any tail at the start of the extension
    endPoint=which.max(zData)
    startPoint=which.min(zData[1:endPoint])
    return(frame[startPoint:endPoint,])
}

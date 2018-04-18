getDwell=function(frame,dwellTime,zPos,F,t){
                                        #Strip the extension curve from imported ibw data stored in frame (a data frame) zPos should be the column of that frame corresponding to the position of the z piezo (deflection would also probably work)
                                        #remove extract curve and any tail at the start of the extension
    startPoint=which.max(frame[,F])
    tStart=frame[startPoint,t]
    tEnd=tStart+dwellTime
    endPoint=which.min(abs(frame[,t]-tEnd))
    endPoint=floor(1*endPoint)
    return(frame[startPoint:endPoint,])
}

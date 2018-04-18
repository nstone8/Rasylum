findLocalMinima=function(vec, roughness,Q){

                                        #find local minima in a vector, increasing roughness makes the function less sensitive to shallow valleys (value should be between 0 and 1). Q is the minimum depth/width ratio of the minima. ApproachTrim is the amount of data to trim off of the beginning of the curve to remove noise. return an empty data frame if no local minima are found
    indices=c()
    thresh=(max(vec)-min(vec))*roughness
    lVec=length(vec)
    heights=c()
    if(lVec>1){
        for(i in 2:lVec){
            diffVec=vec-vec[i]
            if(max(diffVec[1:i])>thresh && max(diffVec[i:lVec])>thresh){
                                        #we found a trough
                                        #Check that there is a trough lip in between this min and the last one. Otherwise, merge them
                if(length(indices)<1){#add if this is our first min
                    indices=c(indices,i)
                    heights=c(heights,min(max(diffVec[1:i]),max(diffVec[i:lVec]))/(max(vec)-min(vec))) #heights are normalized

                }else if(max(diffVec[indices[length(indices)]:i])>thresh){ #append if there is a lip between us and the last min
                    indices=c(indices,i)
                    heights=c(heights,min(max(diffVec[1:i]),max(diffVec[i:lVec]))/(max(vec)-min(vec))) #heights are normalized
                }else if(vec[i]<vec[indices[length(indices)]]){#if there is no lip and this min is less than the last one, replace the last value
                    indices[length(indices)]=i
                    heights[length(heights)]=min(max(diffVec[1:i]),max(diffVec[i:lVec]))/(max(vec)-min(vec))
                }
                
            }
        }
    }
                                        #get the aspect ratio of each trough by searching for the maxima between each local min
    sharpIndices=c()
    sharpHeights=c()
    sharpnesses=c()
    if(length(indices>0)){
        checkPoints=c(1,indices,length(indices)+2)
        for(j in 2:(length(indices)+1)){
                                        #define sharpness as the maximum 'angle to horizon' seen on the shallowest side of the trough

            leftSide=vec[checkPoints[j-1]:checkPoints[j]]/(max(vec)-min(vec))
            rightSide=vec[checkPoints[j]:checkPoints[j+1]]/(max(vec)-min(vec))
            normVec=vec/(max(vec)-min(vec))
            leftDist=c(length(leftSide):0)/length(vec)
            rightDist=c(0:length(rightSide))/length(vec)

            
            leftSlope=(leftSide/leftDist)[(leftSide-normVec[checkPoints[j]])>roughness]
            rightSlope=(rightSide/rightDist)[(rightSide-normVec[checkPoints[j]])>roughness]
            sharpness=min(max(rightSlope),max(leftSlope))
            
            if(sharpness>Q){
                sharpIndices=c(sharpIndices,checkPoints[j])
                sharpHeights=c(sharpHeights,heights[j-1])
                sharpnesses=c(sharpnesses,sharpness)
            }
            
        }
    }
    return(list(indices=sharpIndices,heights=sharpHeights, Qs=sharpnesses))
    
}

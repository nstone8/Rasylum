saveFits=function(filename,fitData, x="zPos", y="F"){
                                        #FitData should be a object created by parExtractStiffness or parExtractTimeConst. Saves plots of the fit and original data to a pdf file filename
    graphics.off()
    pdf(filename)
    for(j in 1:length(fitData$fits)){
        fit=fitData$fits[[j]]
        curve=fit$fit$curves
        id=fit$ident
        fields=names(id)
        name=""
        if(length(fields)>1){
            for(i in 1:length(fields)){
                name=paste(name,fields[i],id[1,fields[i]])
            }
        }else{
            name=paste(name,fields,id)
        }
        newCurve=data.frame(x=curve[,x],y=curve[,y], curve=curve$curve)
        plot=ggplot(newCurve,aes(x=x,y=y,color=curve))+geom_path()+labs(title=name)
        
        print(plot)
        print(name)
        
    }
    dev.off()
}

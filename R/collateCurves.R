collateCurves=function(cases){
	collated=cases[[1]]$data
	for(j in 2:length(cases)){
	      collated=rbind(collated,cases[[j]]$data)
	}
	return(collated)
}
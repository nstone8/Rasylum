quickLoad=function(folder,consts,suffix){
                                        #Convenience function for running batchload and buildFrame with one command
    imp=batchLoad(folder,consts,suffix)
    return(buildFrame(imp$data,imp$numRows))
}

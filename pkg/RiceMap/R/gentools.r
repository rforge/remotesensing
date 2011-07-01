# Author: Jorrel Khalil S. Aunario
# International Rice Research Institute
# Date :  25 May 2011
# Version 0.0.1
# Licence GPL v3

modis.compute <- function(modis, funlist){
    result <- numeric(0)
    for (i in 1:length(funlist)){
        argnames <- names(formals(get(funlist[i])))
        if (sum(argnames %in% colnames(modis))!=length(argnames)) {
            assign(toupper(funlist[i]), rep(NA,nrow(modis)))
        } else {
            arglist <- list()
            for (arg in argnames){
                arglist[[arg]] <- modis[,arg] 
            }
            assign(toupper(funlist[i]), do.call(funlist[i],arglist))
        }
        result <- cbind(result, get(toupper(funlist[i])))        
    }
    colnames(result) <- funlist
    return(result)
}

getRequiredBands <- function(funlist, byfun=FALSE){
	if (byfun) argnames <- list() else argnames <- NULL  
	for (i in 1:length(funlist)){
		if (byfun) argnames[[funlist[i]]] <- names(formals(get(funlist[i]))) else argnames <- c(argnames,names(formals(get(funlist[i]))))
	}
	if (byfun) names(argnames) <- funlist else argnames <- unique(argnames)
	return(argnames)
}

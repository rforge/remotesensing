# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 15 October 2012
# Version 0.1
# Licence GPL v3
# 

if ( !isGeneric("modis.cache") ) {
	setGeneric("modis.cache", function(x, ...)
				standardGeneric("modis.cache"))
}


setMethod("modis.cache", signature(x="modis.data"),
	function(x, path=paste(getwd(),"cache",sep="/")){	
		if(!force.directories(path)) stop("cannot write to path")
		for (i in 1:2400){
			tocache <- x@imgvals[(1:2400)+(i-1)*2400,]
			save(tocache,file=paste(path,"/",deparse(substitute(x)),"_",i,".RData",sep=""))		
		}
		x@imgvals <- data.frame()
	}
)

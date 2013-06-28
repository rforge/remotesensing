# Authors: Jorrel Khalil S. Aunario
# Date: 18 May 2011

if (!isGeneric("modis.data")){
	setGeneric("modis.data", function(x,...) standardGeneric("modis.data"))
}

#setMethod('modis.data', signature(x='missing'), 
#	function(nrows=2400, ncols=2400, xmn=-180, xmx=180, ymn=-90, ymx=90, crs) {
#		e <- extent(xmn, xmx, ymn, ymx)
#		if (missing(crs)) {
#			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
#				crs ="+proj=longlat +datum=WGS84"
#			} else {
#				crs=NA
#			}
#		}
#		r <- modis.data(e, nrows=nrows, ncols=ncols, crs=crs)
#		return(r)
#	}
#)

setMethod("modis.data", signature(x='missing'), 
		function(x, ...) {
			return(new("modis.data"))
		}
)

setMethod("modis.data", signature(x="modis.data"),
		function(x){
			m <- new("modis.data", product=x@product, acqdate=x@acqdate, zone=x@zone, version=x@version, 
					proddate=x@proddate, projection=x@projection, extent=x@extent, ncols=x@ncols, nrows=x@nrows, imgvals=data.frame())
			return(m)			
		}

)

setMethod("modis.data", signature(x="data.frame"),
		function(x, cached=FALSE, cache.path="./cache"){
			mstack <- stack(x$filename)
			NAvalues(mstack) <- -26872
			
			m <- new("modis.data", product=x$product[1], acqdate=x$acqdate[1], zone=x$zone[1], version=x$version[1], 
					proddate=x$proddate, projection=projection(mstack), extent=extent(mstack), ncols=ncol(mstack), nrows=nrow(mstack), imgvals=data.frame())
			
			if (cached){
				if(!force.directories(cache.path)) stop("cannot write to path")
				st <- Sys.time()
				m@imgvals <- data.frame(row=numeric(0),cache.file=character(0))
				for (i in 1:2400){
					message("reading row ", i, "\r", appendLF=FALSE)
					tocache <- as.data.frame(values(mstack,i))
					save(tocache,file=paste(cache.path,"/",deparse(substitute(x)),"_",i,".RData",sep=""))
					m@imgvals[i,] <- NA
					m@imgvals$row[i] <- i
					m@imgvals$cache.file[i] <- paste(cache.path,"/",deparse(substitute(x)),"_",i,".RData",sep="")
				}				
				Sys.time()-st
			} else {
				m@imgvals <- as.data.frame(values(mstack))
			}
			return(m)			
		}

)

setMethod("modis.data", signature(x="RasterStack"),
		function(x){
			m <- new("modis.data", product="", acqdate="", zone="", version="", 
					proddate="", projection=projection(x), extent=extent(x), ncols=ncol(x), nrows=nrow(x), imgvals=as.data.frame(values(x)))
			return(m)			
		}

)

modis.brick <- function(modis, process=NULL, writeto=NULL, format="GTiff", skipx=FALSE, infosep=".", multiband=FALSE, ...){
	if (multiband & nrow(modis@imgvals)>500000) {
		message("Warning: Large amount of data detected. Your computer might freeze or crash if you don't have enough memory.")
	}
	
	force.directories(writeto, recursive=TRUE)
	mraster <- raster(modis@extent, ncols=modis@ncols, nrows=modis@nrows, crs=modis@projection)
	if (!is.null(process)) {
		fname <- paste(writeto, paste(modis@product, modis@acqdate, modis@zone, modis@version, modis@proddate, process, sep=infosep),sep="/")
	} else fname <- paste(writeto, paste(modis@product, modis@acqdate, modis@zone, modis@version, modis@proddate, sep=infosep),sep="/")
	
	for(i in 1:ncol(modis@imgvals)){
		mraster <- addLayer(mraster, setValues(mraster,modis@imgvals[,i]))
    }
	names(mraster) <- colnames(modis@imgvals)
	
	if(is.character(writeto) & multiband) {
		writeRaster(mraster,filename=fname, format=format, bylayer=FALSE, ...)
	} else if (is.character(writeto) & !multiband){
		writeRaster(mraster,filename=fname, format=format, bylayer=TRUE, ...)
		forrename <- dir(writeto, pattern=basename(fname),full.names=TRUE)
		for(i in 1:length(forrename)){
			idx <- as.numeric(sub(paste(basename(fname),"_",sep=""),"",sub(extension(forrename[i]),"",basename(forrename[i]))))
			file.rename(forrename[i],paste(writeto, "/", basename(fname),"_", names(mraster)[idx],extension(forrename[i]),sep=""))
		}
	} else 	mraster <- TRUE  
    return(mraster)
}

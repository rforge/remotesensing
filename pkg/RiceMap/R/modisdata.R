# Authors: Jorrel Khalil S. Aunario
# Date: 18 May 2011

#if ( !isGeneric("modis.data") ) {
#	setGeneric("modis.data", function(obj, ...)
#		standardGeneric("modis.data"))
#}	



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

modis.brick <- function(modis, intlayers=NULL, writeto=NULL, intNA=-15, fltNA=-9999.0, format="GTiff", ...){
    mraster <- raster(modis@extent, ncols=modis@ncols, nrows=modis@nrows, crs=modis@projection)
    mbrick <- brick(mraster)

    if(is.character(writeto)){
        cleanpath <- paste(writeto,"clean",sep="/")
        vegpath <- paste(writeto,"veg",sep="/")
        force.directories(cleanpath, recursive=TRUE)
        force.directories(vegpath, recursive=TRUE)
        fname <- paste(modis@product, modis@acqdate, modis@zone, modis@version, modis@proddate, colnames(modis@imgvals), formatExt(format), sep=".")        
        fname[grepl("clean",fname)] <- paste(cleanpath,fname[grepl("clean",fname)],sep="/")  
        fname[!grepl("clean",fname)] <- paste(vegpath,fname[!grepl("clean",fname)],sep="/")
    }
    for(i in 1:ncol(modis@imgvals)){
        mraster <- setValues(mraster,modis@imgvals[,i])
        
        if(is.character(intlayers)){
            dataType(mraster) <- ifelse(colnames(modis@imgvals)[i] %in% intlayers,"INT1U","FLT4S")                
        } 
        
        if(is.character(writeto)) {
            mraster@file@name <- fname[i]
            if (dataType(mraster)== "INT1U"){
                writeRaster(mraster,filename=filename(mraster), format=format, NAflag=intNA, ...)
            } else {
                writeRaster(mraster,filename=filename(mraster), format=format, NAflag=fltNA, ...)
            }
        }
        mbrick <- addLayer(mbrick,mraster)
    }
    mbrick@layernames <- colnames(modis@imgvals)
    return(mbrick)
}

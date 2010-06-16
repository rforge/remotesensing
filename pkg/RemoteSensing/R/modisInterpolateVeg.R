# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

modisInterpolateVeg <- function(inpath, informat="raster"){
    indnames <- c("evi", "ndvi", "lswi", "ndwi")
    interppath <- paste(inpath, "../interpolated",sep="/")
    files <- character(0)
    if (informat="GTiff"){
        ext <- ".tif"
    } else {
        ext <- ".grd"
    }
    for (i in 1:length(indnames)){
        files <- cbind(files, list.files(inpath, pattern=paste(indnames[i],".cleaned",ext,sep="")))
        stck <- stack(paste(inpath,files[,i],sep="/"))
        interp <- tsInterpolate(evistack, targetfolder=interppath)
        interp <- writeRaster(interp,paste(interppath,paste(indnames[i],"_na_count.grd",sep=""), sep="/"), datatype="INT1U")
        rm(interp,stck)
        gc(verbose=FALSE)    
    }
   
    for (i in 1:nrow(files)){
        base <- substring(files[i,1],1,16)
        cat(base, "\r")
        flush.console()
        indices <- list()        
        for(j in 1:ncol(files)){
            ras <- raster(paste(interppath,files[i,j],sep="/"))
            NAvalue(ras) <- -9999
            indices[[indnames[j]]] <- getValues(ras)
        }
        indices <- as.data.frame(indices)/10000
        flood <- flooded(indices$lswi,indices$ndvi,indices$evi)
        fld <- raster2SGDF(ras,vals=flood)
        fld <- writeGDAL(fld,paste(interppath,paste(base,"flooded.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
		permanent <- persistentwater(indices$ndvi,indices$lswi)
		perm <- raster2SGDF(ras,vals=permanent)
		perm <- writeGDAL(perm,paste(interppath,paste(base,"permanent.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
		nddi <- flooded(indices$lswi,indices$ndvi,indices$evi)
        nd <- raster2SGDF(ras,vals=nddi)
        nd <- writeGDAL(nd,paste(interppath,paste(base,"nddi.cleaned.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Float32")
		drought <- persistentwater(indices$ndvi,indices$lswi)
		drt <- raster2SGDF(ras,vals=drought)
		drt <- writeGDAL(drt,paste(interppath,paste(base,"drought.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
        
        rm(indices,flood, fld, permanent, perm)
        rm(indices,nddi, nd, drought, drt)
        gc(verbose=FALSE)			
    }

}
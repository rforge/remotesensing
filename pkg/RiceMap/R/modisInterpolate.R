# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

modisInterpolateVeg <- function(inpath, year, informat="raster"){
    indnames <- c("evi", "ndvi", "lswi", "ndwi")    
    
    interppath <- paste(inpath, "../interpolated",sep="/")
    extn <- formatExt(informat)
    for (i in 1:length(indnames)){
        files <- list.files(inpath, pattern=paste(year,indnames[i],"cleaned",extn,sep=".*"))
        donefiles <- list.files(interppath,pattern=paste(year,indnames[i],"cleaned.tif",sep=".*"))
        if (length(donefiles)==46) next
        imgs <- paste(inpath,files,sep="/")
        interp <- tsInterpolate(imgs, targetfolder=interppath)
        interp <- writeRaster(interp,paste(interppath,paste(year, "_", indnames[i],"_na_count.grd",sep=""), sep="/"), datatype="INT1U", overwrite=TRUE)
        rm(interp,files, donefiles)
        gc(verbose=FALSE)    
    }
	files <- cbind(list.files(interppath, pattern="evi.cleaned.tif"),list.files(interppath, pattern="ndvi.cleaned.tif"),list.files(interppath, pattern="lswi.cleaned.tif"),list.files(interppath, pattern="ndwi.cleaned.tif"))
    for (i in 1:nrow(files)){
        base <- substring(files[i,1],1,16)
        cat(base, "\r")
        flush.console()
        indices <- list()        
        for(j in 1:ncol(files)){
            ras <- raster(paste(interppath,files[i,j],sep="/"))
            NAvalue(ras) <- -9999
            indices[[indnames[j]]] <- getValues(ras)/10000
        }
        		
        flood <- flooded1(indices$lswi,indices$ndvi,indices$evi)
        fld <- raster2SGDF(ras,vals=flood)
        writeGDAL(fld,paste(interppath,paste(base,"flooded.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
		rm(flood,fld)
		gc(verbose=FALSE)
		permanent <- persistentwater(indices$ndvi,indices$lswi)
		perm <- raster2SGDF(ras,vals=permanent)
		writeGDAL(perm,paste(interppath,paste(base,"permanent.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
		rm(permanent,perm)
		gc(verbose=FALSE)
		nddi <- nddi(indices$ndvi,indices$ndwi)
        nd <- raster2SGDF(ras,vals=nddi)
        writeGDAL(nd,paste(interppath,paste(base,"nddi.cleaned.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Float32")
		rm(nddi,nd)
		gc(verbose=FALSE)		
		drght <- drought(indices$ndvi,indices$ndwi)
		drt <- raster2SGDF(ras,vals=drght)
		writeGDAL(drt,paste(interppath,paste(base,"drought.tif",sep=""),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
        rm(drght,drt,indices,ras)
		gc(verbose=FALSE)		
        }
}

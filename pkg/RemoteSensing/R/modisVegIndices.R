# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3



modisVeg <- function(inpath, informat, outformat="raster", tiles="all"){
	#creation of output director "tif" folder
	outpath <- paste(inpath,"/../veg",sep="")
	if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)
    
    IntNA <- -15
    FltNA <- -9999.0
	
	if (informat=="raster") {
        inext <- ".grd"
    } else if (informat=="GTiff") {
        inext <- ".tif"
    } else {
        stop(paste("Input format", informat, "not supported."))
    }   
	
	if (outformat=="raster"){
        ext <- ".grd"
    } else if (outformat=="GTiff"){
        if (!require(rgdal)) stop("rgdal loading failed")
        ext <- ".tif"
        opts <- c("COMPRESS=LZW", "TFW=YES")
    } else {
        cat(paste("Unsupported output format '", outformat, "'. Will write files in raster instead.", sep=""))
        ext <- ".grd"
        outformat <- "raster"                
    }

    # processing of all tiles in a directory
    if(tiles=="all"){
		cat("Acquiring available tiles in input folder.\n")
		flush.console()
		#print("Press CTRL + C to terminate.")
		tiles <- unique(substr(list.files(inpath, pattern=paste("001.*b01_clean.*", inext, sep="")), 10, 15))		
	}
	
	# looping
	for (tile in tiles) {
		# file reading
		pat <- paste(tile, ".*clean", inext, sep="")
		m <- modisFilesClean(inpath, pat)

        cat("Processing tile:", tile, "\n")
        flush.console()
        
        dates <- unique(m$date[m$zone==tile])

		for (d in dates) {
		    batch <- m[m$date==d & m$zone==tile,]
			
            dlab <- paste("Date ", d, ":", sep ="")
		    fname <- paste(outpath, "/", batch$date[1], "_", batch$zone[1], "_", sep="")
				
			cat(dlab, "loading images. \r")
			flush.console()
				
            red <- raster(paste(inpath, batch$filename[batch$band=="b01"], sep="/"))
            vred <- getValues(red)
            vred[vred==FltNA] <- NA
			nir <- raster(paste(inpath, batch$filename[batch$band=="b02"], sep="/"))
			vnir <- getValues(nir)
            vnir[vnir==FltNA] <- NA
			blue <- raster(paste(inpath, batch$filename[batch$band=="b03"], sep="/"))
			vblue <- getValues(blue)
            vblue[vblue==FltNA] <- NA
			green <- raster(paste(inpath, batch$filename[batch$band=="b04"], sep="/"))
			vgreen <- getValues(green)
            vgreen[vgreen==FltNA] <- NA
			swir1 <- raster(paste(inpath, batch$filename[batch$band=="b06"], sep="/"))
			vswir1 <- getValues(swir1)
            vswir1[vswir1==FltNA] <- NA
			swir2 <- raster(paste(inpath, batch$filename[batch$band=="b07"], sep="/"))
			vswir2 <- getValues(swir2)
            vswir2[vswir2==FltNA] <- NA
				
            # making of VIs
            cat (dlab, "Computing vegetation indices. \r")
            flush.console()
            
            indices <- list()			
            indices$ndvi.cleaned <- ndvi(vred,vnir)
			indices$lswi.cleaned <- lswi(vnir,vswir1)
			indices$evi.cleaned <- evi(vblue, vred, vnir)
			indices$ndwi.cleaned <- ndwi(vnir, vswir2)
				
			# masking of VIs
			#cat (dlab, "masking vegetation indices.   \r")
            #flush.console()
				
			#pat1 <- paste(d, "_", z, "_b03_mask.grd", sep="") 
			#bluemaskfile <- paste(inpath, paste(d, "_", tile, "_b03_mask", ext, sep=""), sep="/")
			#if(!file.exists(bluemaskfile)) stop(paste(bluemaskfile, "does not exist!"))
			#bluemask <- getValues(raster(bluemaskfile))
			#bluemask[bluemask==IntNA] <- NA
			#bluemask[bluemask==0] <- NA
				
			#pat2 <- paste(d, "_", z, "_SnowMask2.grd", sep="")
			#snowmaskfiles <- list.files(inpath, pattern=pat2)
			snowmaskfile <- paste(inpath, paste(d, "_", tile, "_SnowMask2.tif", sep=""), sep="/")
			if(!file.exists(snowmaskfile)) stop(paste(snowmaskfile, "does not exist!"))
			snowmask <- getValues(raster(snowmaskfile))
			snowmask[snowmask==IntNA] <- NA	
			snowmask[snowmask==0] <- NA
			
            indices$ndvi.cleaned <- indices$ndvi.cleaned*snowmask
			indices$lswi.cleaned <- indices$lswi.cleaned*snowmask
			indices$evi.cleaned <- indices$evi.cleaned*snowmask
			indices$ndwi.cleaned <- indices$ndwi.cleaned*snowmask
			indices$nddi.cleaned <- nddi(indices$ndvi.cleaned, indices$ndwi.cleaned)
            
			# writing of flooded,permanent water and drought
			cat (dlab, "Computing drought, flooded, and permanent water \r")
            flush.console()
            maps <- list()				
			maps$flooded <- flooded(indices$lswi.cleaned,indices$ndvi.cleaned,indices$evi.cleaned)
			maps$permanent <- persistentwater(indices$ndvi.cleaned,indices$lswi.cleaned)
			maps$drought <- drought(indices$ndvi.cleaned,indices$ndwi.cleaned)
			
			cat (dlab, "Writing output files.                           \r")
            flush.console()
            
            if (outformat=="GTiff"){
                for(i in 1:length(indices)){
                    band1 <- indices[[i]]
                    band1[is.na(band1)] <- FltNA    
                    rnew <- raster2SGDF(red,vals=band1)                    
                    bfname <- paste(fname, names(indices)[i], ext, sep="")
                    if (file.exists(bfname)) file.remove(bfname)
                    rnew <- writeGDAL(rnew,bfname, options=opts, type="Float32")
                    rm(rnew)
                }
                for(i in 1:length(maps)){
                    band1 <- maps[[i]]
                    band1[is.na(band1)] <- IntNA    
                    rnew <- raster2SGDF(red,vals=band1)                    
                    bfname <- paste(fname, names(maps)[i], ext, sep="")
                    if (file.exists(bfname)) file.remove(bfname)
                    rnew <- writeGDAL(rnew,bfname, options=opts,type = "Int16")
                    rm(rnew)
                }                                
                            
            } else {
                r <- raster(red)
                for(i in 1:length(indices)){
                    rnew <- setValues(r, indices[[i]])
                    rnew <- writeRaster(rnew,filename=paste(fname, names(indices)[i], ext, sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)
                    rm(rnew)
                }
                for(i in 1:length(maps)){
                    rnew <- setValues(r, maps[[i]])
                    rnew <- writeRaster(rnew,filename=paste(fname, names(maps)[i], ext, sep=""), format=outformat, datatype="INT1S", overwrite=TRUE)
                    rm(rnew)
                }
            }
        	
            cat (dlab, " -------------------- DONE -------------------- \n")
            flush.console()
            rm(indices, maps)
            gc(verbose=FALSE)            
		}
	}
}

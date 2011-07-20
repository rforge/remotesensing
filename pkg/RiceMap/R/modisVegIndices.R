# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3

modis.indices <- function(modis, indices=c("evi", "ndvi", "ndwi", "lswi", "ndsi"), mask="snow2", writeto="./veg", savemask=FALSE, verbose=TRUE, asbrick=FALSE){
	
	if(class(modis)!="modis.data") stop("Invalid input data. Should be of class \"modis.data\"")
	# check if 64-bit 
	is64 <- version$arch=="x86_64"

	bandnums <- as.numeric(gsub("b","",colnames(modis@imgvals)))
	colnames(modis@imgvals) <- bandnames(bandnums)
	
	if (verbose) show.message(modis@acqdate, ": Computing ", paste(indices, collapse=", "), eol="\r")
	
	vegindices <- modis.data(modis)
	if (is64) {
		vegindices@imgvals <- modis.compute(modis@imgvals,funlist=indices)
	} else {
		for (i in 1:length(indices)){
			rbands <- getRequiredBands(indices[i])
			
			if (i==1) {
				vegindices@imgvals <- modis.compute(modis@imgvals[,rbands],funlist=indices[i])
			} else {
				vegindices@imgvals[,indices[i]] <- modis.compute(modis@imgvals[,rbands],funlist=indices[i])[,indices[i]]
			}
		}
	}
	
    if (is.character(mask)){
		rbands <- getRequiredBands(mask)
		inmodis <- rbands[rbands %in% colnames(modis@imgvals)]
		inveg <- rbands[rbands %in% colnames(vegindices@imgvals)]
		maskdata <- cbind(modis@imgvals[,inmodis],vegindices@imgvals[,inveg])
		colnames(maskdata) <- c(inmodis,inveg)
		rm(modis)
		gc(verbose=FALSE)
        
		masks <- modis.data(vegindices)
		if (verbose) show.message(masks@acqdate, ": Computing ", paste(mask, collapse=", "), eol="\r")
		masks@imgvals <- modis.compute(maskdata,funlist=mask, datatype="logical")
		
		
		if (is64) {
			if (verbose) show.message(masks@acqdate, ": Applying mask(s). ", eol="\r")
			vegindices@imgvals <- modis.mask(vegindices@imgvals,masks@imgvals)
		} else {
			for (i in 1:ncol(vegindices@imgvals)){
				if (verbose) show.message(vegindices@acqdate, ": Applying masks to ", colnames(vegindices@imgvals)[i], eol="\r")
				vegindices@imgvals[,i] <- modis.mask(vegindices@imgvals[,i],masks@imgvals)
			}
			gc(verbose=FALSE)
		}
    } 
	
	if (is.character(writeto)){
		outdir <- normalizePath(writeto, mustWork=FALSE)
		force.directories(writeto, recursive=TRUE)
		
		if (verbose) show.message(masks@acqdate, ": Writing vegindices to disk.", eol="\r")
		vbrick <- modis.brick(vegindices, process="index", writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
		rm(vbrick)
		gc(verbose=FALSE)
		if(savemask){
			if (verbose) show.message(masks@acqdate, ": Writing mask rasters to disk.", eol="\r")
			mbrick <- modis.brick(masks, process="index", intlayers=1:ncol(masks@imgvals),writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
			rm(mbrick)
			gc(verbose=FALSE)
		}
	}
	if (verbose) show.message(masks@acqdate, ": --------------- DONE COMPUTING INDICES ----------------", eol="\n")
    rm(masks)
	gc(verbose=FALSE)            
	return(vegindices)
}

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
    m <- modisFiles(path=inpath, modisinfo=c("acqdate","zone","band","process", "format"))
    # processing of all tiles in a directory
    if(tiles=="all"){
		cat("Acquiring available tiles in input folder.\n")
		flush.console()
		#print("Press CTRL + C to terminate.")
		tiles <- unique(m$zone)		
	}
	
	# looping
	for (tile in tiles) {
		cat("Processing tile:", tile, "\n")
        flush.console()
        
        dates <- unique(m$acqdate[m$zone==tile])

		for (d in dates) {
		    batch <- m[m$acqdate==d & m$zone==tile,]
			
            dlab <- paste("Date ", d, ":", sep ="")
		    fname <- paste(outpath, "/", batch$acqdate[1], ".", batch$zone[1], ".", sep="")
				
			cat(dlab, "loading images. \r")
			flush.console()
				
            red <- raster(paste(inpath, batch$filename[batch$band=="b01"], sep="/"))
            NAvalue(red) <- FltNA
            vred <- getValues(red)
            nir <- raster(paste(inpath, batch$filename[batch$band=="b02"], sep="/"))
			NAvalue(nir) <- FltNA
            vnir <- getValues(nir)
            blue <- raster(paste(inpath, batch$filename[batch$band=="b03"], sep="/"))
			NAvalue(blue) <- FltNA
            vblue <- getValues(blue)
            green <- raster(paste(inpath, batch$filename[batch$band=="b04"], sep="/"))
			NAvalue(green) <- FltNA
            vgreen <- getValues(green)
            swir1 <- raster(paste(inpath, batch$filename[batch$band=="b06"], sep="/"))
			NAvalue(swir1) <- FltNA
            vswir1 <- getValues(swir1)
            swir2 <- raster(paste(inpath, batch$filename[batch$band=="b07"], sep="/"))
			NAvalue(swir2) <- FltNA
            vswir2 <- getValues(swir2)
            	
            # making of VIs
            cat (dlab, "Computing vegetation indices. \r")
            flush.console()
            
            indices <- list()			
            indices$ndvi.veg <- ndvi(vred,vnir)
			indices$lswi.veg <- lswi(vnir,vswir1)
			indices$evi.veg <- evi(vblue, vred, vnir)
			indices$ndwi.veg <- ndwi(vnir, vswir2)
				
			# masking of VIs
			cat (dlab, "masking vegetation indices.   \r")
            flush.console()
				
			#pat1 <- paste(d, "_", z, "_b03_mask.grd", sep="") 
			#bluemaskfile <- paste(inpath, paste(d, "_", tile, "_b03_mask", ext, sep=""), sep="/")
			#if(!file.exists(bluemaskfile)) stop(paste(bluemaskfile, "does not exist!"))
			#bluemask <- getValues(raster(bluemaskfile))
			#bluemask[bluemask==IntNA] <- NA
			#bluemask[bluemask==0] <- NA
				
			#pat2 <- paste(d, "_", z, "_SnowMask2.grd", sep="")
			#snowmaskfiles <- list.files(inpath, pattern=pat2)
			#snowmaskfile <- paste(inpath, paste(d, "_", tile, "_SnowMask2", ext, sep=""), sep="/")
			#if(!file.exists(snowmaskfile)) stop(paste(snowmaskfile, "does not exist!"))
			#snowmask <- getValues(raster(snowmaskfile))
			#snowmask[snowmask==IntNA] <- NA	
			#snowmask[snowmask==0] <- NA
			
            #indices$ndvi.veg <- indices$ndvi.veg*bluemask*snowmask
			#indices$lswi.veg <- indices$lswi.veg*bluemask*snowmask
			#indices$evi.veg <- indices$evi.veg*bluemask*snowmask
			#indices$ndwi.veg <- indices$ndwi.veg*bluemask*snowmask
			#indices$nddi.veg <- nddi(indices$ndvi.veg, indices$ndwi.veg)
            
			# writing of flooded,permanent water and drought
			cat (dlab, "Computing drought, flooded, and permanent water \r")
            flush.console()
            maps <- list()				
			maps$flooded.veg <- flooded1(indices$lswi.veg,indices$ndvi.veg,indices$evi.veg)
			maps$permanent.veg <- persistentwater(indices$ndvi.veg,indices$lswi.veg)
			maps$drought.veg <- drought(indices$ndvi.veg,indices$ndwi.veg)
			
			cat (dlab, "Writing output files.                           \r")
            flush.console()
            
            
            r <- raster(red)
            for(i in 1:length(indices)){
                rnew <- setValues(r, indices[[i]])
                if (outformat=="raster"){ 
                    rnew <- writeRaster(rnew,filename=paste(fname, names(indices)[i], ext, sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)
                } else if (outformat=="GTiff") {
                    rnew <- writeRaster(rnew,filename=paste(fname, names(indices)[i], ext, sep=""), format=outformat, datatype="FLT4S", NAflag=FltNA, overwrite=TRUE, options=opts)
                } 
                rm(rnew)
            }
            for(i in 1:length(maps)){
                rnew <- setValues(r, maps[[i]])
                if (outformat=="raster"){ 
                    rnew <- writeRaster(rnew,filename=paste(fname, names(maps)[i], ext, sep=""), format=outformat, datatype="INT2S", overwrite=TRUE)
                } else if (outformat=="GTiff") {
                    rnew <- writeRaster(rnew,filename=paste(fname, names(maps)[i], ext, sep=""), format=outformat, datatype="INT2S", NAflag=IntNA, overwrite=TRUE, options=opts)
                }                     
                rm(rnew)
            }                
        	
            cat (dlab, " -------------------- DONE -------------------- \n")
            flush.console()
            rm(indices, maps)
            gc(verbose=FALSE)            
		}
	}
}

# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3

modis.vegindices <- function(modis, indices=c("evi", "ndvi", "ndwi", "lswi", "ndsi"), mask=NULL){
    reqbands <- c("b01", "b02", "b03", "b04", "b06", "b07")
    techname <- c("red", "nir", "blue", "green", "swir1", "swir2")
    
    if (sum(sub(".clean", "", colnames(modis@imgvals)) %in% reqbands)<6) stop("Incomplete bands") else idx <- match(sub(".clean", "", colnames(modis@imgvals)),reqbands)
    colnames(modis@imgvals)[idx[!is.na(idx)]] <- techname    

    vegindices <- modis.compute(modis@imgvals,funlist=indices)
    colnames(vegindices) <- indices
    if (is.character(mask)){
        masks <- modis.compute(cbind(modis@imgvals,vegindices),funlist=mask)
        vegindices <- modis.mask(vegindices,masks)
        vegindices <- cbind(vegindices,masks)
        colnames(vegindices) <- c(indices,mask)
    } 
    
    vegindices <- cbind(vegindices, modis.compute(vegindices, funlist=c("flooded1", "flooded2", "flooded3", "drought","persistentwater")))
    colnames(vegindices) <- paste(colnames(vegindices),"veg",sep=".")
    rm(masks)
    gc()        
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
            indices$ndvi.cleaned <- ndvi(vred,vnir)
			indices$lswi.cleaned <- lswi(vnir,vswir1)
			indices$evi.cleaned <- evi(vblue, vred, vnir)
			indices$ndwi.cleaned <- ndwi(vnir, vswir2)
				
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
			
            #indices$ndvi.cleaned <- indices$ndvi.cleaned*bluemask*snowmask
			#indices$lswi.cleaned <- indices$lswi.cleaned*bluemask*snowmask
			#indices$evi.cleaned <- indices$evi.cleaned*bluemask*snowmask
			#indices$ndwi.cleaned <- indices$ndwi.cleaned*bluemask*snowmask
			#indices$nddi.cleaned <- nddi(indices$ndvi.cleaned, indices$ndwi.cleaned)
            
			# writing of flooded,permanent water and drought
			cat (dlab, "Computing drought, flooded, and permanent water \r")
            flush.console()
            maps <- list()				
			maps$flooded <- flooded1(indices$lswi.cleaned,indices$ndvi.cleaned,indices$evi.cleaned)
			maps$permanent <- persistentwater(indices$ndvi.cleaned,indices$lswi.cleaned)
			maps$drought <- drought(indices$ndvi.cleaned,indices$ndwi.cleaned)
			
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

#Authors: Sonia Asilo, Robert J. Hijmans, Ritsuko Fuchiyama,  Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil Aunario
# International Rice Research Institute
# Date :  Feb 2009
# Version 0.1
# Licence GPL v3


mymean <- function(x) {
	x <- na.omit(x)
	if (length(x) > 0) {
		return(mean(x)) 
	} else { 
		return( NA ) 
	}
}

mymax <- function(x) {
	x <- na.omit(x)
	if (length(x) > 0) {
		return(max(x)) 
	} else { 
		return( NA ) 
	}
}
	
Flooded <- function (flooded) {sum(flooded, na.rm=T) > 0}	#Flooded= 1  ; not flooded = 0	
Permanent <- function (permanent) { sum(permanent, na.rm=T) >= 10} # permanent = 1; not permanet = 0
Forest <- function(ndvi){ sum( ndvi >= 0.7 , na.rm=T) >= 20}	# Forest: 1, ; not forest =0
Shrub <- function(lswi){ sum(lswi < 0.1, na.rm=T) == 0 } # shrub=1; not shrub = 0
# Bare <- function(ndvi){ sum(ndvi > 0.1, na.rm=T) < 2 }

Flooded <- function (flooded) {sum(flooded, na.rm=T) > 0}	#Flooded= 1  ; not flooded = 0	
Permanent <- function (permanent) { sum(permanent, na.rm=T) >= 10} # permanent = 1; not permanet = 0
Forest <- function(ndvi){ sum( ndvi >= 0.7 , na.rm=T) >= 20}	# Forest: 1, ; not forest =0
Shrub <- function(lswi){ sum(lswi < 0.1, na.rm=T) == 0 } # shrub=1; not shrub = 0
# Bare <- function(ndvi){ sum(ndvi > 0.1, na.rm=T) < 2 }

modis.rice <- function(modis, writeto="./rice", verbose=TRUE){
	#check if 64bit
	is64 <- version$arch=="x86_64"
	
	price <- modis.data(modis)
	#finalize forest
	forest <- modis@imgvals$forest >= (20*(modis@imgvals$nimgs/46))
	#finalize shrub
	shrub <- (modis@imgvals$shrub == 46) & !forest
	#finalize wetland
	wetland <- modis@imgvals[,grep("flooded", colnames(modis@imgvals))] >= 40
	#finalize flooded
	flooded <- (modis@imgvals[,grep("flooded", colnames(modis@imgvals))] > 0) & !wetland
	#finalize permanent
	permanent <- modis@imgvals$persistentwater >= 20
	
	notrice <- (forest | shrub | permanent | wetland)
	#indicators$notrice <- (indicators$forest | indicators$shrub)
	perhapsrice <- flooded & !notrice
	
	price@imgvals <- as.data.frame(cbind(forest,shrub,wetland,flooded,permanent,notrice,perhapsrice)) 
	if(is.character(writeto)) {
		outdir <- normalizePath(writeto, mustWork=FALSE)
		force.directories(writeto, recursive=TRUE)
		modis.brick(price, process="rice", intlayers=1:ncol(price@imgvals), writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)	
		modis.brick(modis, process="rice", intlayers=1:ncol(modis@imgvals), writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
	}
	
	rm(perhapsrice,notrice,forest,shrub,permanent,wetland,modis)
	gc(verbose=FALSE)
	return(price)
}

modisRice <- function(inpath, informat, outformat="raster", tiles="all", valscale=NULL){
    
	# creation of output director "tif" folder
	outpath <- paste(inpath,"/../rice",sep="")
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
        outext <- ".grd"
    } else if (outformat=="GTiff"){
        #if (!require(rgdal)) stop("rgdal loading failed")
        outext <- ".tif"
        opts <- c("COMPRESS=LZW", "TFW=YES")
    } else {
        cat(paste("Unsupported output format '", outformat, "'. Will write files in raster instead.", sep=""))
        outext <- ".grd"
        outformat <- "raster"                
    }

    # processing of all tiles in a directory
    if(tiles=="all"){
		cat("Acquiring available tiles in input folder.\n")
		flush.console()
		#print("Press CTRL + C to terminate.")
		tiles <- unique(substr(list.files(inpath, pattern=paste("001.*ndvi.*",inext,sep="")), 10, 15))		
	}
	
	for (tile in tiles) {
	    cat("Processing tile:", tile, "\n")
        flush.console()
        
		# file reading
		pat <- paste(tile, ".*", inext, sep="")
        m <- modisFiles(path=inpath, modisinfo=c("acqdate","zone","band","process", "format"))
		m$filename <- paste(inpath, m$filename, sep="/")
        years <- unique(m$year[m$zone==tile])
		# looping				
        #mm <- subset(m, m$zone==tile)
		
        for (y in years) {
            batch <- m[m$year==y & m$zone==tile,]
		    braster <- raster(batch$filename[1])
			dlab <- paste("Year ", y, ":", sep="")
			
			
            dates <- unique(batch$acqdate)
			#dates <- sort(dates)
			
            if (length(dates) < 46) { 
				if (length(dates) < 43) { 
					stop(paste("expected 46 files, found:", length(dates))) 
				}
				warning(paste("expected 46 files, found:", length(dates))) 
			}
            
            
            bands <- c("ndvi", "lswi", "flooded", "permanent")
            indnames <- c("forest", "shrub", "flooded", "permanent")
            indicators <- list()
            for (i in 1:length(bands)){
                cat(dlab, "Delineating ", indnames[i],". \r", sep="")
			    flush.console()
                bfiles <- batch$filename[grep(bands[i],batch$band)]
                indicators[[indnames[i]]] <- 0
                for (bfile in bfiles){
                    vals <- getValues(raster(bfile))
                    vals[vals<=FltNA] <- NA
                        
                    if(!is.null(valscale)){
                        vals <- vals/valscale
                    }                    
                    if (indnames[i]=="forest"){
                        vals <- vals >= 0.7
                    }else if (indnames[i]=="shrub"){
                        vals <- vals < 0.1
                    }                
                    vals[is.na(vals)] <- 0
                    indicators[[indnames[i]]] <- indicators[[indnames[i]]]+ vals
                }
                if (indnames[i]=="forest"){
                    # if >= 20 forest
                    indicators[[indnames[i]]] <- indicators[[indnames[i]]] >= (20*(length(dates)/46))
                }else if (indnames[i]=="shrub"){
                    # if ==46 shrub
                    indicators[[indnames[i]]] <- indicators[[indnames[i]]] == 46
                    indicators[[indnames[i]]] <- indicators[[indnames[i]]] & !indicators[["forest"]] 
                }else if (indnames[i]=="flooded"){
                    #if >=40  wetland
                    indicators[[indnames[i]]] <- indicators[[indnames[i]]] > 0 & indicators[[indnames[i]]] < 40
                    indicators$wetland <- indicators[[indnames[i]]] >= 40
                }else if (indnames[i]=="permanent"){
                    # if >=20 permanent
                    indicators[[indnames[i]]] <- indicators[[indnames[i]]] >= 20
                }

            }
			indicators$notrice <- (indicators$forest | indicators$shrub | indicators$permanent | indicators$wetland)
			#indicators$notrice <- (indicators$forest | indicators$shrub)
			indicators$perhapsrice <- indicators$flooded & !indicators$notrice
			
			cat (dlab, "Writing output files.                           \r")
            flush.console()
            
            r <- raster(braster)
            for(i in 1:length(indicators)){
                rnew <- setValues(r, indicators[[i]])
                if (outformat=="raster"){
                    rnew <- writeRaster(rnew,filename=paste(outpath, paste(names(indicators)[i], "_", tile, "_", y, outext, sep=""), sep="/"), format=outformat, datatype="INT2S", NAflag=IntNA, overwrite=TRUE)                                
                } else if (outformat=="GTiff"){
                    rnew <- writeRaster(rnew,filename=paste(outpath, paste(names(indicators)[i], "_", tile, "_", y, outext, sep=""), sep="/"), format=outformat, datatype="INT2S", NAflag=IntNA, options=opts, overwrite=TRUE)
                }
                rm(rnew)
            }
            cat (dlab, " -------------------- DONE -------------------- \n")
            flush.console()
            rm(indicators,vals)
            gc(verbose=FALSE)                    	
		}
    }
}
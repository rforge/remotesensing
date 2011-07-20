# Author: Andrew Nelson, Sonia Asilo, Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3


ricefile.attribs <- function(filename, sep="\\."){
    fname <- basename(filename) 
    noExt <- substr(fname, 1, nchar(fname)-4)
    if(length(filename)==1){
        initattrib <- t(unlist(strsplit(noExt, sep)))    
    } else if (length(filename)>1){
        initattrib <- matrix(unlist(strsplit(noExt, sep)),ncol=4,byrow=TRUE)
    }
    attribs <- cbind(as.numeric(substr(initattrib[,1],2,5)), as.numeric(substr(initattrib[,1],6,8)))
    colnames(attribs) <- c("year", "doy")
    attribs <- as.data.frame(attribs, stringsAsFactors=FALSE)
    attribs$date <- format(as.POSIXct((as.integer(attribs$doy)-1)*60*60*24,origin=paste(attribs$year,1,1,sep="-")), "%Y-%m-%d")
    attribs$tile <- initattrib[,2]
    #process band
    attribs$band <- initattrib[,3]
    return(attribs)
}

validationFiles <- function(path, informat="GTiff"){
    floodfiles <- list.files(path, pattern=paste("flood",formatExt(informat),sep=".*."))
    ndvifiles <- list.files(path, pattern=paste("ndvi",formatExt(informat),sep=".*."))
    evifiles <- list.files(path, pattern=paste("evi",formatExt(informat),sep=".*."))
    if(length(floodfiles)!=length(ndvifiles) & length(floodfiles)!=length(evifiles)) {
        stop("Incomplete data")
    }
    properties <- cbind(ricefile.attribs(floodfiles), floodfiles, ndvifiles, evifiles, stringsAsFactors=FALSE)
    return(properties)
}

maxEVI <- function(evidata){
    if (class(evidata)=="matrix"){
        maxevi <- rep(NA, nrow(evidata))
        for (i in 1:ncol(evidata)){
            maxevi <- pmax(evidata[,i], maxevi, na.rm=TRUE)
        }        
    } else {
        maxevi <- max(evidata, na.rm=TRUE)
    }    
    return(maxevi)
}

rmColumn <- function(mat, colnum){
    mat <- mat[,-colnum]
    return(mat)
}

modis.validate <- function(modis, modisroot, yr, writeto="./realrice", verbose=TRUE){
	
	force.directories(writeto, recursive=TRUE)
	outdir <- normalizePath(writeto, mustWork=FALSE)
	
	if(class(modis)!="modis.data") stop("Invalid input data. 'modis' Should be of class \"modis.data\"")
	
	floodpath <- paste(modisroot, "identify", sep="/")
	idfs <- modisFiles(path=floodpath, modisinfo=c("product","acqdate","zone","version","proddate","band","process", "format"), full.names=TRUE)	
	flds <- idfs[grep("flood",idfs$band),]
	# Gets the index of the last image (DOY 361) of the previous year
	fld0 <- grep(paste("A",yr-1,"361",sep=""),flds$acqdate) 
	if (length(fld0)<1) {
		warning("Missing last flood image from year ", yr-1)
		fld0 <- min(grep(yr,flds$year))
	}	
	flds <- flds[fld0:max(grep(yr,flds$year)),]
	
	evipath <- paste(modisroot, "veg", sep="/")
	infs <- modisFiles(path=evipath, modisinfo=c("product","acqdate","zone","version","proddate","band","process", "format"), full.names=TRUE)	
	evis <- infs[infs$band=="evi",]
	# Gets the index of the 12th image (DOY 89) of the next year
	eny12 <- grep(yr+1,evis$year)
	if (length(eny12)<12) warning("12 images from year ", yr+1, " incomplete.")
	en <- max(eny12)	
	evis <- evis[min(grep(yr,flds$year)):en,]
		
	vpix <- which(modis@imgvals$perhapsrice==1)
	
	ricefreq <- rep(NA,nrow(modis@imgvals))
	rppdoy <- integer(0)
	
	for (i in 1:nrow(flds)){
		fraster <- raster(flds$filename[i])
		fldvals <- as.integer(fraster[])
		fpix <- vpix[which(fldvals[vpix]==1)]
		evivals <- values(stack(evis$filename[0:11+i]))[fpix,]			
			
		if (verbose) show.message(flds$acqdate[i], ": Identifying rice pixels", eol="\r")			
		
		max5 <- maxEVI(evivals[,1:5])
		max12 <- maxEVI(evivals[,1:12])
		ave611 <- rowMeans(evivals[, 6:11], na.rm=TRUE)
		truerice <- (max5*2 >= max12) & (ave611>0.35)
		rppdoy <- c(rppdoy, sum(truerice))
		ricefreq[fpix] <- rowSums(cbind(ricefreq[fpix], truerice), na.rm=TRUE)
		
		validatedrice <- modis.data(modis)
		validatedrice@acqdate <- flds$acqdate[i]
		rice <- rep(NA,ncell(fraster))
		rice[fpix] <- truerice
		validatedrice@imgvals <- as.data.frame(rice)
		if (verbose) show.message(flds$acqdate[i], ": Writing rice raster.", eol="\r")
		modis.brick(validatedrice, process="validate", intlayers=1,writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)	
		rm(rice,validatedrice)
		gc(verbose=FALSE)
		if (verbose) show.message(flds$acqdate[i], ": Done", eol="\n")
	}
	validatedrice <- modis.data(modis)	
	rice <- ricefreq > 0
	validatedrice@imgvals <- as.data.frame(cbind(ricefreq,rice))
	modis.brick(validatedrice, process="validate", intlayers=1:2, writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
	
	if (!is.null(dev.list())) x11()
	if(require(grDevices)){
		barplot(rppdoy, names.arg=flds$doy[fld0:max(grep(yr,flds$year))], main="Count of identified rice pixels per DOY")
		savePlot(filename=paste(writeto,paste(yr,"npix_riceplot.png", sep="_"),sep="/"), type="png")    
	} else {
		show.message(paste("Package grDevices not found. Cannot save barplot."), eol="\n")
	}
	if (verbose) show.message("------------- MODIS RICE VALIDATE DONE! -------------", eol="\n")
	return(outdir)
}

validateRice <- function(inpath, year="All", informat="GTiff", outformat="GTiff", valscale=1){
    outpath <- paste(inpath, "../rice_new", sep="/")
    ricepath <- paste(inpath, "../rice", sep="/")
    if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)
    filext <- formatExt(informat)
    if (is.na(filext)){
        stop("Invalid input format")
    }
    vfiles <- modisFiles(path=inpath, modisinfo=c("product","acqdate","zone","version", "proddate", "band","process","format"))
    #vfiles <- validationFiles(inpath,informat)
    if (year=="All"){
        years <- unique(vfiles$year) 
    } else {
        years <- year
    }
    for(y in years){
        st <- which(vfiles$year==y & vfiles$doy==1)-1
        en <- which(vfiles$year==y & vfiles$doy==361)-1
        if (st<1){
            show.message(paste("Data from last image from previous year not available.\nWill start on first image of year ", y, ".",sep=""), eol="\n")
            st <- 1            
        }
        perhaps <- paste(ricepath, paste("perhapsrice_", vfiles$tile[1], "_", y, ".", filext, sep=""), sep="/")
        pr <- raster(perhaps)
            
        counts <- vector(length=length(st:en))
        for (i in st:en){
            if(i==(nrow(vfiles)-12)){
                show.message("Succeeding files not found.", eol="\n")
                break
            }
            show.message(paste("Processing DOY", vfiles$doy[i+1], "of", y), eol="\n")
            show.message("Isolating flooded pixels.", eol="\r")
            fld <- raster(paste(inpath,vfiles$floodfiles[i],sep="/"))
            if (file.exists(perhaps)){
                mfld <- pr[]*fld[]
                fpix <- which(mfld==1)                 
            } else {
                fpix <- which(fld[]==1)                            
            }
            #if (i==st){
            #    overall <- rep(NA,ncell(fld))
            #    evi12 <- getValues(stack(paste(inpath,vfiles$evifiles[i+(1:12)],sep="/")))
            #} else {
            #    evi12 <- rmColumn(evi12,1)
            #    gc(verbose=FALSE)
            #    evi12 <- cbind(evi12,raster(paste(inpath,vfiles$evifiles[i+12],sep="/"))[])
            #}
            if(i==st){
                overall <- rep(NA,ncell(fld))
            }
            show.message(paste("Reading EVI values: Files", i+1, "to", i+12), eol="\r")
            evi12 <- getValues(stack(paste(inpath,vfiles$evifiles[i+(1:12)],sep="/")))[fpix,]
            evi12[evi12==-9999] <- NA
            evi12 <- evi12/valscale
            show.message("Calculating statistics.", eol="\r")
            max5 <- maxEVI(evi12[,1:5])*2
            max12 <- maxEVI(evi12)
            ave611 <- rowMeans(evi12[,6:11], na.rm=TRUE)
            show.message("Determining rice pixels.", eol="\r")
            ricet1 <- max5>=max12
            ricet2 <- ave611>=0.2
            rice <- fld[fpix] & ricet1 & ricet2
            counts[i-st+1] <- length(which(rice==1))
            rvals <- rep(NA,ncell(fld))
            rvals[fpix] <- as.numeric(rice)
            overall <- rowSums(cbind(overall,rvals), na.rm=TRUE)
            show.message("Writing rice map to disk.", eol="\r")
            ricegrid <- setValues(fld,rvals)
            writeRaster(ricegrid, filename=paste(outpath, paste(vfiles$tile[i+1], "_", y, "_", gsub(" ", 0, format(vfiles$doy[i+1], width=3)), "_rice.tif", sep=""), sep="/"), format=outformat, options=c("COMPRESS=LZW", "TFW=YES"), datatype="INT2S", overwrite=TRUE)
            #writeGDAL(ricegrid, paste(outpath, paste(vfiles$tile[i+1], "_", y, "_", gsub(" ", 0, format(vfiles$doy[i+1], width=3)), "_rice.tif", sep=""), sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
            show.message(paste("Done.", counts[i-st+1],"rice pixels found."), eol="\n")
            rm(ricegrid,rice, max5, max12, ave611, ricet1, ricet2, rvals,evi12)
            gc(verbose=FALSE)
        }
        totalrice <- setValues(fld,overall)
        writeRaster(totalrice, filename=paste(outpath,paste(y,"totalrice.tif", sep="_"),sep="/"), format=outformat, options=c("COMPRESS=LZW", "TFW=YES"), datatype="INT2S", overwrite=TRUE)
        #writeGDAL(totalrice, paste(outpath,paste(y,"totalrice.tif", sep="_"),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
        ricepix <- as.numeric(overall>0)
        riceofyear <- setValues(fld, ricepix)
        writeRaster(riceofyear, filename=paste(outpath,paste(y,"riceofyear.tif", sep="_"),sep="/"), format=outformat, options=c("COMPRESS=LZW", "TFW=YES"), datatype="INT2S", overwrite=TRUE)
        #writeGDAL(riceofyear, paste(outpath,paste(y,"riceofyear.tif", sep="_"),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
        if (!is.null(dev.list())) x11()
        if(require(grDevices)){
            barplot(counts, names.arg=vfiles$doy[st:en+1], main="Count of identified rice pixels per DOY")
            savePlot(filename=paste(outpath,paste(y,"npix_riceplot.png", sep="_"),sep="/"), type="png")    
        } else {
            show.message(paste("Package grDevices not found. Cannot save barplot."), eol="\n")
        } 
    }
    
}

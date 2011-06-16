# Author: Andrew Nelson, Sonia Asilo, Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3


ricefile.attribs <- function(filename){
    fname <- basename(filename) 
    noExt <- substr(fname, 1, nchar(fname)-4)
    if(length(filename)==1){
        initattrib <- t(unlist(strsplit(noExt, "_")))    
    } else if (length(filename)>1){
        initattrib <- matrix(unlist(strsplit(noExt, "_")),ncol=3,byrow=TRUE)
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
    floodfiles <- list.files(path, pattern=paste("flooded",formatExt(informat),sep=""))
    ndvifiles <- list.files(path, pattern=paste("ndvi.cleaned",formatExt(informat),sep=""))
    evifiles <- list.files(path, pattern=paste("evi.cleaned",formatExt(informat),sep=""))
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

validateRice <- function(inpath, year="All", informat="GTiff", outformat="GTiff", valscale=10000){
    outpath <- paste(inpath, "../rice_new", sep="/")
    ricepath <- paste(inpath, "../rice", sep="/")
    if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)
    filext <- formatExt(informat)
    if (is.na(filext)){
        stop("Invalid input format")
    }
    vfiles <- validationFiles(inpath,informat)
    if (year=="All"){
        years <- unique(vfiles$year) 
    } else {
        years <- year
    }
    for(y in years){
        st <- which(vfiles$year==y & vfiles$doy==1)-1
        en <- which(vfiles$year==y & vfiles$doy==361)-1
        if (st<1){
            .rsMessage(paste("Data from last image from previous year not available.\nWill start on first image of year ", y, ".",sep=""), newln=TRUE)
            st <- 1            
        }
        perhaps <- paste(ricepath, paste("perhapsrice_", vfiles$tile[1], "_", y, filext, sep=""), sep="/")
        pr <- raster(perhaps)
            
        counts <- vector(length=length(st:en))
        for (i in st:en){
            if(i==(nrow(vfiles)-12)){
                .rsMessage("Succeeding files not found.")
                break
            }
            .rsMessage(paste("Processing DOY", vfiles$doy[i+1], "of", y), newln=TRUE)
            .rsMessage("Isolating flooded pixels.")
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
            .rsMessage(paste("Reading EVI values: Files", i+1, "to", i+12))
            evi12 <- getValues(stack(paste(inpath,vfiles$evifiles[i+(1:12)],sep="/")))[fpix,]
            evi12[evi12==-9999] <- NA
            evi12 <- evi12/valscale
            .rsMessage("Calculating statistics.")
            max5 <- maxEVI(evi12[,1:5])*2
            max12 <- maxEVI(evi12)
            ave611 <- rowMeans(evi12[,6:11], na.rm=TRUE)
            .rsMessage("Determining rice pixels.")
            ricet1 <- max5>=max12
            ricet2 <- ave611>=0.35
            rice <- fld[fpix] & ricet1 & ricet2
            counts[i-st+1] <- length(which(rice==1))
            rvals <- rep(NA,ncell(fld))
            rvals[fpix] <- as.numeric(rice)
            overall <- rowSums(cbind(overall,rvals), na.rm=TRUE)
            .rsMessage("Writing rice map to disk.")
            ricegrid <- raster2SGDF(fld,rvals)
            writeGDAL(ricegrid, paste(outpath, paste(vfiles$tile[i+1], "_", y, "_", gsub(" ", 0, format(vfiles$doy[i+1], width=3)), "_rice.tif", sep=""), sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
            .rsMessage(paste("Done.", counts[i-st+1],"rice pixels found.\n"))
            rm(ricegrid,rice, max5, max12, ave611, ricet1, ricet2, rvals,evi12)
            gc(verbose=FALSE)
        }
        totalrice <- raster2SGDF(fld,overall)
        writeGDAL(totalrice, paste(outpath,paste(y,"totalrice.tif", sep="_"),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
        ricepix <- as.numeric(overall>0)
        riceofyear <- raster2SGDF(fld, vals=ricepix)
        writeGDAL(riceofyear, paste(outpath,paste(y,"riceofyear.tif", sep="_"),sep="/"), options=c("COMPRESS=LZW", "TFW=YES"), type="Int16")
    }
    if (!is.null(dev.list())) x11()
    barplot(counts, names.arg=vfiles$doy[st:en+1])
}

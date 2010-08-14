# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

.interpolate <- function(series, fillends=TRUE){
    require(timeSeries)
    #require(splines)
    valids <- which(!is.na(series))
    if (fillends){
        if(valids[1]!=1){
            series[1:(valids[1]-1)] <- series[valids[1]]
        }
        if(valids[length(valids)]!=length(series)){
            series[(valids[length(valids)]+1):length(series)] <- series[valids[length(valids)]]
        }        
    }
    series <- interpNA(series)
    return(series)
}

fillMissingDOY <- function(missingdoys, dat){
    #insert NA column for each missing DOY
    for (a in length(missingdoys):1){
        if (missingdoys[a]>ncol(dat)) {
            dat <- cbind(dat,NA)
        } else if (FALSE){
            #TODO: what if missing at the begining? way to check where missin doy is.
            dat <- cbind(NA, dat)
        } else {
            end <- dat[ ,missingdoys[a]:ncol(dat)]
            st <- dat[,1:(missingdoys[a]-1)]
            dat <- cbind(st, NA, end)            
        }
    }
    return(dat)        
}

tsInterpolate <- function(imgfiles, targetfolder=NA, rm.interm =TRUE, dataAsInt=TRUE, na=-9999 , verbose=TRUE, ...){
    attribs <- ricefile.attribs(imgfiles)
    stdoy <- 1+(0:45*8)
    fnames <- paste(paste("A",attribs$year[1],gsub(" ", 0, format(stdoy,widht=3)), sep=""),attribs$tile[1],attribs$band[1], sep="_")
    
    srcdir <- dirname(imgfiles[1])
    if(is.na(targetfolder)){
        target <- paste(srcdir, "../interpolated", sep="/")    
    } else {
        target <- targetfolder
    }
    if(!file.exists(target)){
        dir.create(target)  
    }
    fnames <- paste(target, fnames, sep="/")
    file.create(paste(fnames,".txt",sep=""))
    miss <- which(!stdoy %in% attribs$doy)

    nacount <- numeric(0)
    imgstack <- stack(imgfiles)
    
    for (i in 1:nrow(imgstack)){
        if (verbose){
            cat("Row:", i, "\r")
            flush.console()
        }
        vals <- getValues(imgstack,i)
        vals[vals==na] <- NA
        if (length(miss)>0) vals <- fillMissingDOY(miss, vals)
        nacount <- c(nacount,rowSums(is.na(vals)))
        #x <- rep(NA,nrow(vals))
        for (j in 1:nrow(vals)){
            #x[j] <- sum(!is.na(vals[j,]))
            if(sum(!is.na(vals[j,]))>=12){
              #cat("accept",j, sum(!is.na(vals[j,])),"\n")
              #flush.console()
              vals[j,] <- round(.interpolate(vals[j,],...),4)  
            }             
        }
        for (k in 1:ncol(vals)){
            write.table(t(vals[,k]), paste(fnames[k],".txt",sep="") , sep=",", na="", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
        }
        rm(vals)
        gc(verbose=FALSE)        
    }
    for (k in 1:length(fnames)){
        if (verbose){
            cat("Writing to disk:", basename(fnames[k]), "\r")
            flush.console()
        }        
            
        newraster <- raster(imgstack)        
        dat <- as.matrix(read.csv(paste(fnames[k],".txt",sep=""),header=FALSE))
        typ <- 'Float32'
        if (dataAsInt){
            dat <- round(dat*10000,0)
            typ='Int16'
        }
        dat[is.na(dat)] <- na 
        rnew <- raster2SGDF(newraster, vals=dat)
        rnew <- writeGDAL(rnew,paste(fnames[k],".tif",sep=""), options=c("COMPRESS=LZW", "TFW=YES"), type=typ)
        if(rm.interm){
            file.remove(paste(fnames[k],".txt",sep=""))
        }        
        rm(rnew,newraster,dat)
        gc(verbose=FALSE)
        
    }
    newraster <- raster(imgstack)
    newraster[] <- nacount
    return(newraster)    
}

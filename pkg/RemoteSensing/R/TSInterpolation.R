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

tsInterpolate <- function(imgstack, targetfolder=NA, rm.interm =TRUE, dataAsInt=TRUE, na=-9999 , verbose=TRUE, ...){
    fnames <- character(0)
    for (k in 1:nlayers(imgstack)){
        fname <- filename(imgstack@layers[[k]])
        srcfile <- basename(fname)
        srcdir <- dirname(fname)
        if(is.na(targetfolder)){
            target <- paste(srcdir, "../interpolated", sep="/")    
        } else {
            target <- targetfolder
        }
        
        if(!file.exists(target)){
            dir.create(target)  
        }
        fnames <- c(fnames,paste(target, srcfile,sep="/"))
        file.create(paste(fnames[k],".txt",sep=""))
    }
    nacount <- numeric(0)
    for (i in 1:nrow(imgstack)){
        if (verbose){
            cat("Row:", i, "\r")
            flush.console()
        }
        vals <- getValues(imgstack,i)
        vals[vals==na] <- NA
        nacount <- c(nacount,rowSums(is.na(vals)))
        #x <- rep(NA,nrow(vals))
        for (j in 1:nrow(vals)){
            #x[j] <- sum(!is.na(vals[j,]))
            if(sum(!is.na(vals[j,]))>=12){
              #cat("accept",j, sum(!is.na(vals[j,])),"\n")
              #flush.console()
              vals[j,] <- .interpolate(vals[j,],...)  
            }             
        }
        for (k in 1:nlayers(imgstack)){
            write.table(t(vals[,k]), paste(fnames[k],".txt",sep="") , sep=",", na="", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
        }        
    }
    for (k in 1:nlayers(imgstack)){
        if (verbose){
            cat("Writing to disk:", basename(fnames[k]), "\r")
            flush.console()
        }        
            
        newraster <- raster(imgstack@layers[[k]])        
        dat <- as.matrix(read.csv(paste(fnames[k],".txt",sep=""),header=FALSE))
        typ <- 'Float32'
        if (dataAsInt){
            dat <- round(dat*10000,0)
            typ='Int16'
        }
        dat[is.na(dat)] <- na 
        rnew <- raster2SGDF(newraster, vals=dat)
        rnew <- writeGDAL(rnew,paste(substr(fnames[k],1,nchar(fnames[k])-4),".tif",sep=""), options=c("COMPRESS=LZW", "TFW=YES"), type=typ)
        if(rm.interm){
            file.remove(paste(fnames[k],".txt",sep=""))
        }        
        rm(rnew,newraster,dat)
        gc(verbose=FALSE)
        
    }
    newraster <- raster(imgstack@layers[[k]])
    newraster[] <- nacount
    return(newraster)    
}

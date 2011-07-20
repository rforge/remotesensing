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

.simple <- function(modisfiles, yr, what="evi", writeto="./smooth", format="GTiff", ...){
    sdoy <- seq(1,361,by=8)
    if(!force.directories(writeto)){
        stop("Cannot create output folder.")
    }
    success <- FALSE
    series <- modisfiles[(modisfiles$year==yr | (modisfiles$year==(yr+1) & modisfiles$doy==1) | (modisfiles$year==(yr-1) & modisfiles$doy==361)),]
    if(series$year[1]!=(yr-1)|series$year[nrow(series)]!=(yr+1)) {
        cat("Warning: Last image of",yr-1,"and first image of",yr+1, "not found.")
        flush.console()
        sdoy <- sdoy[-c(1,length(sdoy))]
    }
    blue <- series[series$band=="b03",]
    blue <- blue[order(blue$doy),]
    this <- series[series$band==what,]
    this <- this[order(this$doy),]

    # check if series is complete
    for (sd in sdoy){
        i <- which(blue$doy==sd & blue$year==yr)            
        b <- raster(blue$filename[i])        
        w <- raster(this$filename[i])
        s <- raster(b)        
        change <- values(b)>=0.18        
        if(length(change)>0){
            s[!change] <- w[!change]
            w0 <- raster(this$filename[i-1])
            b0 <- raster(blue$filename[i-1])                 
            w2 <- raster(this$filename[i+1])
            b2 <- raster(blue$filename[i+1])
            clear0 <- b0[change]<0.18
            clear2 <- b2[change]<0.18
            
            s[change[clear0 & clear2]] <- (w0[change[clear0 & clear2]]+w2[change[clear0 & clear2]])/2
            s[change[!clear0 & clear2]] <- w2[change[!clear0 & clear2]]
            s[change[clear0 & !clear2]] <- w0[change[clear0 & !clear2]]
            s[change[!clear0 & !clear2]] <- NA
        } else s <- w
        fname <- paste(this$product[i], this$acqdate[i], this$zone[i], this$version[i], this$proddate[i], what, "smooth", formatExt(format), sep=".")
        writeRaster(s,filename=paste(writeto,fname,sep="/"),...)
        cat(fname,"\n")
        flush.console()
    }
    success <-TRUE     
    return(success)                       
}



modis.smooth <- function(modisfiles, method="simple", ...){
    if (method=="simple"){
        return(.simple(modisfiles=modisfiles,...))    
    } else {
        stop("Unsupported method")
    }
}

#sm <- stack(list.files(path=paste(modispath,"smooth",sep="/"),full.names=TRUE))
#ns <- stack(list.files(path=paste(modispath,"veg",sep="/"),pattern="evi", full.names=TRUE)[-c(1,46)])
#wat <- raster(list.files(path=paste(modispath,"veg",sep="/"),pattern="water", full.names=TRUE)[2])
#cells <- which(wat[]==1)

#rr <- 0
#for(i in cells){
#    rw <- rowFromCell(wat,i)
#    co <- colFromCell(wat,i)
#    if(rr!=rw){
#        rr <- rw
#        ss <- values(sm,rw)
#        ssn <- values(ns,rw)
#    }
#    plot(ss[co,],type="l", col="red")
#    lines(ssn[co,])
# Sys.sleep(.5)
#}


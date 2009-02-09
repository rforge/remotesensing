# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modisFiles <- function(path) {
	f <- list.files(path=path, pattern='.tif')
	x <- strsplit(f, '\\.')
	m <- matrix(, length(x), length(x[[1]]))
	for(i in 1:length(x)) { m[i,] <- x[[i]] }
	m <- cbind(m, f)
	if (dim(m)[2] != 9) { stop('oops, non standard filenames found') }
	m <- m[,-8]
	m <- m[,-6]
	m <- m[,-4]
	colnames(m) <- c('prod1', 'date', 'zone', 'prod2', 'band', 'filename')
	m <- as.data.frame(m)
	m$band <- substr(m$band, 10, 12)
	return(m)
}


modisFilesClean <- function(path) {
	f <- list.files(path=path, pattern='.grd')
	x <- strsplit(f, '_')
	m <- matrix(, length(x), length(x[[1]]))
	for(i in 1:length(x)) { m[i,] <- x[[i]] }
	m <- m[,-4] 
	m <- cbind(m, f)
	colnames(m) <- c('date', 'zone', 'band', 'filename')
	m <- as.data.frame(m)
	return(m)
}




modis.sqa500a_adj <- function (pixel) {
	pixel <- modis.sqa500a(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
    return(pixel)
}



modisClean <- function(inpath, outpath, overwrite=TRUE) {
#red = "b01"; nir = "b02"; blue = "b03"; green = "b04"; swir6 = "b06"; swir7 = "b07"; qual = "state"

	m <- modisFiles(inpath)
	dir.create(outpath, showWarnings = FALSE)
	
	zones <- as.vector(unique(m$zone))
	for (z in zones) {
		mm <- subset(m, m$zone==z)
		dates <- as.vector(unique(mm$date))
		print(paste('Zone:', z))
		for (d in dates) {
			print(d)
			qfile <- paste(inpath, as.vector(subset(mm, mm$date == d & mm$band == "sta")$filename), sep='')
			b <- subset(mm, mm$date == d & mm$band != "sta")

			rq <- rasterFromFile(qfile, TRUE)
			# mask <- coulds | snow 
			# changed cloud mask from modis.sqa500a_adj to modis.sqa500f
			mask <- calc(rq, modis.sqa500f) | calc(rq, modis.sqa500h)
			# mask <- coulds | snow | water
#			mask <- calc(rq, modis.sqa500a) | calc(rq, modis.sqa500h) | calc(rq, watermask)

			fname1 <- paste(outpath, d, '_', z, '_', sep='')
			
			for (i in 1:length(b[,1])) { 
				r <- rasterFromFile( paste(inpath, b$filename[i], sep='') )
				r <- setNAvalue(r, -28672)
				r <- readAll(r)
				r[mask == 0] <- NA  # apply mask 
				
#				r <- r / 10000  # scaling     why should we scale ?
	
# for efficient storage				
				v <- round(values(r) / 100)
				r[] <- v
				if (maxValue(r) > 32767) {
					stop(paste('maxvalue too high:', maxValue(r)))
				}
				r <- setDatatype(r, datatype="integer", datasize=4)
				r <- setFilename(r,  paste(fname1, b$band[i], '_clean.grd', sep=''))
				r <- writeRaster(r)
			}
		}
	}
}





flooded <- function(lswi, ndvi, evi) { 
	return( (lswi+0.05 >= evi) | (lswi+0.05 >= ndvi) )
}


vegIndices <- function(inpath, outpath, overwrite=TRUE, inRAM=FALSE) {
	m <- modisFilesClean(inpath)
	dir.create(outpath, showWarnings = FALSE)
	zones <- as.vector(unique(m$zone))
	for (z in zones) {
		mm <- subset(m, m$zone==z)
		dates <- as.vector(unique(mm$date))
		print(paste('Zone:', z))
		for (d in dates) {
			print(d)
			b <- subset(mm, mm$date == d)
			fname1 <- paste(outpath, b$date[1], '_', b$zone[1], '_', sep='')
			for (i in 1:length(b[,1])) { 
				if (b$band[i] == 'b01') {
					red <- rasterFromFile(paste(inpath, b$filename[i], sep=''), inRAM )
				} else if (b$band[i] == 'b02') {
					nir2 <- rasterFromFile(paste(inpath, b$filename[i], sep=''), inRAM )
				} else if (b$band[i] == 'b03') {
					blue <- rasterFromFile(paste(inpath, b$filename[i], sep=''), inRAM)
				} else if (b$band[i] == 'b04') {
					green <- rasterFromFile(paste(inpath, b$filename[i], sep=''), inRAM)
				} else if (b$band[i] == 'b05') {
					#nir5 <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else if (b$band[i] == 'b06') {
					swir6 <- rasterFromFile(paste(inpath, b$filename[i], sep=''), inRAM)
				} else if (b$band[i] == 'b07') {
					#swir7 <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else {
					stop(paste('unknown band:', b$band[i]))
				}
			}
			NDVI <- overlay(red, nir2, fun=ndvi, filename=paste(fname1, 'ndvi.grd', sep=''), overwrite=overwrite)
			LSWI <- overlay(nir2, swir6, fun=lswi,  filename=paste(fname1, 'lswi.grd', sep=''), overwrite=overwrite)
			EVI <- overlay(blue, red, nir2, fun=evi, filename=paste(fname1, 'evi.grd', sep=''), overwrite=overwrite)
			flood <- overlay(LSWI, NDVI, EVI, fun=flooded, filename=paste(fname1, 'flooded.grd', sep=''), overwrite=overwrite,  asInt=TRUE)
		}
	}
}



persistentwater <- function(ndvi, lswi){
 	return( (ndvi < 0.10) & (ndvi < lswi) )
}


watermask <- function(pixel) {
	res <- modis.sqa500c(pixel)
	res[res < 1 | res > 5] <- NA
	res[!is.na(res)] <- 1
	return(res)
}



forest <- function(ndvi) {
	result <- (ndvi > 0.7)
	return(result)
}
	



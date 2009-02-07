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
	if (dim(m)[2] != 9) { stop('oops') }
	m <- m[,-8]
	m <- m[,-6]
	m <- m[,-4]
	m <- m[,-1]
	colnames(m) <- c('date', 'zone', '', 'band', 'filename')
	m <- as.data.frame(m)
	m$band <- substr(m$band, 10, 12)
	return(m)
}


modisFilesSimple <- function(path) {
	f <- list.files(path=path, pattern='.grd')
	x <- strsplit(f, '_')
	m <- matrix(, length(x), length(x[[1]]))
	for(i in 1:length(x)) { m[i,] <- x[[i]] }
	m <- cbind(m, f)
	colnames(m) <- c('date', 'zone', 'band', 'filename')
	m <- as.data.frame(m)
	m$band <- substr(m$band, 1, 3)
	return(m)
}



flooded <- function(lswi, ndvi, evi) { 
	return( (lswi+0.05 >= evi) | (lswi+0.05 >= ndvi) )
}

persistentwater <- function(ndvi, lswi){
 	return( (ndvi < 0.10) & (ndvi < lswi) )
}


modisClean <- function(inpath, outpath, overwrite=TRUE) {
#red = "b01"; nir = "b02"; blue = "b03"; green = "b04"; swir6 = "b06"; swir7 = "b07"; qual = "state"

	m <- modisFiles(inpath)
	dir.create(outpath, showWarnings = FALSE)
	
	zones <- as.vector(unique(m$zone))
	for (z in zones) {
	mm <- subset(m, m$zone==z)
		dates <- as.vector(unique(mm$date))
		for (d in dates) {
			qfile <- paste(inpath, as.vector(subset(mm, mm$date == d & mm$band == "sta")$filename), sep='')
			b <- subset(mm, mm$date == d & mm$band != "sta")
			rq <- rasterFromFile(qfile, TRUE)
			# mask <- coulds | snow
			mask <- calc(rq, modis.sqa500a) | calc(rq, modis.sqa500h)

# water   RH I find this questionable. Let the algorithm figure this out instead.
#			r <- calc(r, modis.sqa500c)
#			r[(r > 3) | (r == 0) | (r == 2)] <- NA
#			r[!is.na(r)] <- 1

			fname1 <- paste(outpath, b$date[1], '_', b$zone[1], '_', sep='')
			
			for (i in 1:length(b[,1])) { 
				r <- rasterFromFile( paste(inpath, b$filename[i], sep='') )
				r <- setNAvalue(r, -28672)
				r <- readAll(r)
				r <- r * mask  # apply mask 
				
#				r <- r / 10000  # scaling 
# why should we scale ?

				filename <- paste(fname1, b$band[i], '.grd', sep='')
				r <- setFilename(r, filename)
				r <- setDatatype(r, datatype="integer", datasize=4)
				r <- writeRaster(r)
			}
		}
	}
}


riceMap <- function(inpath, outpath, overwrite=TRUE) {
	m <- modisFilesSimple(inpath)
	dir.create(outpath, showWarnings = FALSE)
	zones <- as.vector(unique(m$zone))
	for (z in zones) {
	mm <- subset(m, m$zone==z)
		dates <- as.vector(unique(mm$date))
		for (d in dates) {
			b <- subset(mm, mm$date == d)
			fname1 <- paste(outpath, b$date[1], '_', b$zone[1], '_', sep='')
			for (i in 1:length(b[,1])) { 
				if (b$band[i] == 'b01') {
					red <- rasterFromFile(paste(inpath, b$filename[i], sep='') )
				} else if (b$band[i] == 'b02') {
					nir <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else if (b$band[i] == 'b03') {
					blue <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else if (b$band[i] == 'b04') {
					green <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else if (b$band[i] == 'b05') {
					band5 <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else if (b$band[i] == 'b06') {
					swir6 <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else if (b$band[i] == 'b07') {
					swir7 <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else {
					
				}
			}
			ndvi <- overlay(red, nir, fun=ndvi, filename=paste(fname1, 'ndvi.grd', sep=''), overwrite=overwrite)
			lswi <- overlay(nir, swir6, fun=lswi,  filename=paste(fname1, 'lswi.grd', sep=''), overwrite=overwrite)
			evi <- overlay(blue, red, nir, fun=evi, filename=paste(fname1, 'evi.grd', sep=''), overwrite=overwrite)
			flood <- overlay(lswi,ndvi,evi, fun=flooded, filename=paste(fname1, 'flooded.grd', sep=''), overwrite=overwrite,  ForceIntOut=TRUE)
		}
	}
}



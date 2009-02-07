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
	return(m)
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
			qfile <- paste(inpath, as.vector(subset(mm, mm$date == d & mm$band == "sur_refl_state_500m")$filename), sep='')
			b <- subset(mm, mm$date == d & mm$band != "sur_refl_state_500m")
			rq <- rasterFromFile(qfile, TRUE)
			# mask <- coulds | snow
			mask <- calc(rq, modis.sqa500a) | calc(rq, modis.sqa500h)

# water   RH I find this questionable. Let the algorithm figure this out instead.
#			r <- calc(r, modis.sqa500c)
#			r[(r > 3) | (r == 0) | (r == 2)] <- NA
#			r[!is.na(r)] <- 1

			fname1 <- paste(outpath, 'clean_', b$date[1], '_', b$zone[1], '_', sep='')
			
			for (i in length(b[,1])) { 
				r <- rasterFromFile( paste(inpath, b$filename[i], sep='') )
				r <- setNAvalue(r, -28672)
				r <- readAll(r)
				
				#r[r < -28671] <- NA  # set NA value
				r <- r * mask  # apply mask 
				r <- r / 10000  # scaling 

				filename <- paste(fname1, b$band[i], '.grd', sep='')
				r <- setFilename(r, filename)
				cat(filename, "\n")
				r <- writeRaster(r, overwrite=overwrite)
				if (i>1) {return(r)}
			}			
		}
	}
}


	
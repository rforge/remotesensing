# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3



.modis.sqa500a_adj <- function (pixel) {
	pixel <- modis.sqa500a(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
    return(pixel)
}


.watermask <- function(pixel) {
	res <- modis.sqa500c(pixel)
	res[res < 1 | res > 5] <- 0
	res[res > 0] <- 1
	return(res)
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

			# changed cloud mask from modis.sqa500a_adj to modis.sqa500f
			# mask <- coulds | snow | water
			mask <- calc(rq, modis.sqa500a) | calc(rq, modis.sqa500h) | calc(rq, .watermask)

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
				r <- setDatatype(r, datatype="INT4S")
				r <- setFilename(r,  paste(fname1, b$band[i], '_clean.grd', sep=''))
				r <- writeRaster(r, overwrite=overwrite)
			}
		}
	}
}



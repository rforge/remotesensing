# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


# Internal cloud  algorithm flag
.internalcloud <- function (pixel) {
	pixel <- modis.sqa500f(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
    return(pixel)
}

# Cloud shadow mask
.cloudshadow <- function (pixel) {
	pixel <- modis.sqa500b(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
    return(pixel)
}

# Water mask (includes land and inland water)
.watermask <- function(pixel) {
	res <- modis.sqa500c(pixel)
	res[res < 1 | res == 2 | res > 5 ] <- 0
	res[res >=3] <- 0
	res[res >= 1] <- 1
	return(res)
}

# Internal Snow mask
.snowmask <- function(pixel) {
	pixel <- modis.sqa500k(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
    return(pixel)
}

# possibly cloudy pixels that were not included in the internal cloud flag
.bluemask <- function(pixel) {
	pixel[pixel >= 0.2] <- NA
	pixel[pixel < 0.2] <- 1
	pixel[is.na(pixel)] <- 0
	return(pixel)
	}

modisClean <- function(inpath, outpath, overwrite=TRUE) {
#red = "b01"; nir = "b02"; blue = "b03"; green = "b04"; nir2 = "b05"; swir6 = "b06"; swir7 = "b07"; qual = "state"

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

			rq <- raster(qfile, TRUE)

			# changed cloud mask from modis.sqa500a_adj to modis.sqa500f
			# mask <- coulds | snow | water
			mask <- calc(rq, .internalcloud) * calc(rq, .cloudshadow) * calc(rq, .watermask) * calc(rq, .snowmask) 

			fname <- paste(outpath, d, '_', z, '_', sep='')
			
			for (i in 1:length(b[,1])) { 
				r <- raster( paste(inpath, b$filename[i], sep='') )
				NAvalue(r) <- -28672
				r <- readAll(r)
				r[mask == 0] <- NA  # apply mask 
				
# for efficient storage				
				r[] <- values(r) / 10000
				dataType(r) <- "FLT4S"
				filename(r) <- paste(fname, b$band[i], '_clean.grd', sep='')
				r <- writeRaster(r, overwrite=overwrite)
			}
			
			BLUE <- list.files(outpath, pattern = "b03_clean.grd")
			
			for (i in 1:length(BLUE)) {
					bluefiles <- raster(paste(outpath, BLUE[i], sep=""))
					fnameblumask <- paste(outpath, d, '_', z, '_',  "b03_mask.grd", sep="")
					blumask <- calc(bluefiles, fun=.bluemask, filename = fnameblumask, datatype='INT2S')
			}
		}
	}
}



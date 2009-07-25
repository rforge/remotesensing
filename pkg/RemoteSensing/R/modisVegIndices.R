# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modisVeg <- function(inpath, outpath, overwrite=TRUE, inRAM=FALSE) {
#NDVI: normalized difference vegetation index; EVI: enhanced vegetation index; LSWI: land surface water index; NDSI: normalized difference snow index
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
			fname <- paste(outpath, b$date[1], '_', b$zone[1], '_', sep='')
			for (i in 1:length(b[,1])) { 
				if (b$band[i] == 'b01') {
					red <- raster(paste(inpath, b$filename[i], sep=''), inRAM )
				} else if (b$band[i] == 'b02') {
					nir <- raster(paste(inpath, b$filename[i], sep=''), inRAM )
				} else if (b$band[i] == 'b03') {
					blue <- raster(paste(inpath, b$filename[i], sep=''), inRAM)
				} else if (b$band[i] == 'b04') {
					green <- raster(paste(inpath, b$filename[i], sep=''), inRAM)
				} else if (b$band[i] == 'b05') {
					#nir2 <- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else if (b$band[i] == 'b06') {
					swir1 <- raster(paste(inpath, b$filename[i], sep=''), inRAM)
				} else if (b$band[i] == 'b07') {
					#swir2<- rasterFromFile(paste(inpath, b$filename[i], sep=''))
				} else {
					stop(paste('unknown band:', b$band[i]))
				}
			}
			NDVI <- overlay(red, nir, fun=ndvi, filename=paste(fname, 'ndvi.grd', sep=''), overwrite=overwrite)
			LSWI <- overlay(nir, swir1, fun=lswi,  filename=paste(fname, 'lswi.grd', sep=''), overwrite=overwrite)
			EVI <- overlay(blue, red, nir, fun=evi, filename=paste(fname, 'evi.grd', sep=''), overwrite=overwrite)
			flood <- overlay(LSWI, NDVI, EVI, fun=flooded, filename=paste(fname, 'flooded.grd', sep=''), overwrite=overwrite,  datatype='INT2S')
			permanent <- overlay(NDVI, LSWI, fun=persistentwater, filename=paste(fname, 'permanent', sep=''), overwrite=overwrite, datatype='INT2S')
			NDSI <- overlay(green, nir, fun= ndsi, filename=paste(fname, 'NDSI', sep=''), overwrite=overwrite)
		}
	}
}

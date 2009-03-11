# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


rice <- function(inpath, outpath) {

	mysum <- function(x){ sum(x, na.rm=T) }
	sumNotNA <- function(x){ sum(!is.na(x)) }
	mymean <- function(x) {
		sumv <- mysum(x)
		sumnotna <- sumNotNA(x)
		return(sumv/sumnotna)
	}
	mymax <- function(x) {
		x <- na.omit(x)
		if (length(x) > 0) {
			return(max(x)) 
		} else { 
			return( NA ) 
		}
	}

	forest <- function(ndvi){ sum( ndvi > 0.7 , na.rm=T) > 14 }
	shrub <- function(lswi){ sum(lswi > 0.1, na.rm=T) > 1 }
	bare <- function(ndvi){ sum(ndvi > 0.1, na.rm=T) < 1 }

	m <- modisFilesClean(inpath)
	m$filename <- paste(inpath, m$filename, sep="")
	dir.create(outpath, showWarnings = FALSE)
	zones <- as.vector(unique(m$zone))
	for (z in zones) {
		mm <- subset(m, m$zone==z)
		years <- as.vector(unique(mm$year))
		for (y in years) {
			mmm <- subset(mm, mm$year == y)
			dates <- as.vector(unique(mmm$date))
			dates <- sort(dates)
			if (length(dates) < 46) { 
				if (length(dates) < 43) { 
					stop(paste('expected 46 files, found:', length(dates))) 
				}
				warning(paste('expected 46 files, found:', length(dates))) 
			}
			ndvistk <- stack( as.vector( mmm$filename[mmm$band=='ndvi']) )
			evistk <- stack( as.vector( mmm$filename[mmm$band=='evi']) )
			lswistk <- stack( as.vector( mmm$filename[mmm$band=='lswi']) )
			floodstk <- stack( as.vector( mmm$filename[mmm$band=='flooded']) )
			

			flooded <- calc(floodstk, fun=mysum,  filename="")
			permanent <- flooded > 30
			flooded[flooded > 0] <- 1
			
			forest <- calc(ndvistk, fun=forest)
			shrub <- calc(lswistk, fun=shrub)
			bare <- calc(ndvistk, fun=bare)
			shrub  <- shrub & (forest[forest==0])
			
			notrice <- max(permanent * forest * shrub)
			perhapsrice <- flooded[notrice==0]
			
			perhapsrice <- setFilename(perhapsrice, paste(outpath, 'perhapsrice.grd', sep=''))
			
#			maxevi <- which.max(evi)
#			d <- (end-5):end
#			if max(flooded[d]) == 1 then it is rice


			xiaorice <- evi
		}
	}
}
			



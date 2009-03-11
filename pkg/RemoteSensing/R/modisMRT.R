


modisMRT <- function(path, A=rep(1, TRUE), Q=c(TRUE, TRUE, TRUE) ) {
#This script uses the program resample that is part of the MODIS Reprojection Tools (MRT) suite.

oldwd <- getwd()
setwd(path)
path <- paste(path, '/', sep="")

if (!file.exists('resample.exe')) {
	stop('resample.exe not found')
}

if (length(A) != 13) { stop('A must be a vector of 13 values') }
if (length(Q) != 3) { stop('Q must be a vector of 3 values') }
A <- A * 1
Q <- Q * 1

if (sum(A) > 0) {
	f <- file('MOD09A1.parameters', open='w')
	cat('INPUT_FILENAME = null', "\n", file=f)
	cat('OUTPUT_FILENAME = null', "\n", file=f)
	cat('RESAMPLING_TYPE = NEAREST_NEIGHBOR', "\n", file=f)
	cat('SPECTRAL_SUBSET = (', as.character(A), ')', "\n", file=f)
	cat('OUTPUT_PROJECTION_TYPE = SIN', "\n", file=f)
	cat('OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )', "\n", file=f)
	close(f)
	fs <- list.files(path, glob2rx("MOD09A1*.hdf"))
	x1 <- paste("set MRTDATADIR=%CD% & ", path, "resample.exe -p MOD09A1.parameters -i ", fs, " -o ",  fs, ".tif", sep="")
} else { x1 <- "" }

if (sum(Q) > 0) {
	f <- file('MOD09Q1.parameters', open='w')
	cat('INPUT_FILENAME = null', file=f)
	cat('OUTPUT_FILENAME = null', file=f)
	cat('RESAMPLING_TYPE = NEAREST_NEIGHBOR', file=f)
	cat('SPECTRAL_SUBSET = (', as.character(Q), ')', "\n", file=f)
	cat('OUTPUT_PROJECTION_TYPE = SIN', file=f)
	cat('OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )', file=f)
	close(f)
	fs <- list.files(path, glob2rx("MOD09Q1*.hdf"))
	x2 <- paste("set MRTDATADIR=%CD% & ", path, "resample.exe -p MOD09Q1.parameters -i ", fs, " -o ",  fs, ".tif", sep="")
} else { x1 <- "" }

x <- c(x1, x2)

	for (i in 1:length(x)) {
		cat(i, "\n")
		output <- shell( x[[i]] )
	}
setwd(oldwd)
}


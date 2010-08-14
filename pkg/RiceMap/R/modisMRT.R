# Authors: Sonia Asilo, Robert J. Hijmans, Yann Chemin
# International Rice Research Institute
# Date :  March 2009
# Version 0.1
# Licence GPL v3




modisResample <- function(path, A=rep(TRUE, 13), Q=c(TRUE, TRUE, TRUE), intern=FALSE ) {
#This function uses the program 
#resample.exe
#that is part of the MODIS Reprojection Tools (MRT) .

	oldwd <- getwd()
	path <- paste(path, '/', sep="")
	setwd(path)


	if (!file.exists('resample.exe')) {
		stop('resample.exe not found')
	}
	.writeSpheroidTxt(path)



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
		cat('INPUT_FILENAME = null', "\n", file=f)
		cat('OUTPUT_FILENAME = null', "\n", file=f)
		cat('RESAMPLING_TYPE = NEAREST_NEIGHBOR', "\n", file=f)
		cat('SPECTRAL_SUBSET = (', as.character(Q), ')', "\n", file=f)
		cat('OUTPUT_PROJECTION_TYPE = SIN', "\n", file=f)
		cat('OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )', "\n", file=f)
		close(f)
		fs <- list.files(path, glob2rx("MOD09Q1*.hdf"))
		x2 <- paste("set MRTDATADIR=%CD% & ", path, "resample.exe -p MOD09Q1.parameters -i ", fs, " -o ",  fs, ".tif", sep="")
	} else { x1 <- "" }

	x <- c(x1, x2)

	for (i in 1:length(x)) {
		cat('processing image: ',i, "\n")
		bf <- file('RRRrunresampleRRR.bat', open="w")
		cat("set MRTDATADIR=%CD%\n", file=bf)
		cat("resample -p  MOD09A1.parameters -i MOD09A1.A2007073.h26v06.005.2007099220346.hdf -o test.tif\n", file=bf)
		close(bf)
		shell("RRRrunresampleRRR.bat")
		output <- system( x[[i]], intern = intern)
	}
	
	setwd(oldwd)
	
	return( TRUE )
}


.writeSpheroidTxt <- function(p) {
	f <- file('spheroid.txt', open='w')
	cat('0:CLARKE 1866:6378206.4:294.9786982:', '\n', file=f)
	cat('1:CLARKE 1880:6378249.145:293.465:', '\n', file=f)
	cat('2:BESSEL:6377397.155:299.1528128:', '\n', file=f)
	cat('3:INTERNATIONAL 1967:6378157.5:298.25:', '\n', file=f)
	cat('4:INTERNATIONAL 1909:6378388.0:297:', '\n', file=f)
	cat('5:WGS 72:6378135.0:298.26:', '\n', file=f)
	cat('6:EVEREST:6377276.3452:300.8017:', '\n', file=f)
	cat('7:WGS 66:6378145.0:298.25:', '\n', file=f)
	cat('8:GRS 1980:6378137.0:298.257222101:', '\n', file=f)
	cat('9:AIRY:6377563.396:299.3249646:', '\n', file=f)
	cat('10:MODIFIED EVEREST:6377304.063:300.8017:', '\n', file=f)
	cat('11:MODIFIED AIRY:6377340.189:299.3249646:', '\n', file=f)
	cat('12:WGS84:6378137.0:298.257223563:', '\n', file=f)
	cat('13:SOUTHEAST ASIA(MODIFIED FISCHER 1960):6378155.0:298.3:', '\n', file=f)
	cat('14:AUSTRALIAN NATIONAL:6378160.0:298.25:', '\n', file=f)
	cat('15:KRASSOVSKY:6378245.0:298.3:', '\n', file=f)
	cat('16:HOUGH:6378270.0:297.0:', '\n', file=f)
	cat('17:MERCURY 1960 (FISCHER 1960):6378166.0:298.3:', '\n', file=f)
	cat('18:MODIFIED MERCURY 1968:6378150.0:298.3:', '\n', file=f)
	cat('19:SPHERE:6370997.0::', '\n', file=f)
	cat('20:BESSEL 1841(NAMIBIA):6377483.865:299.1528128:', '\n', file=f)
	cat('21:EVEREST (SABAH & SARAWAK):6377298.556:300.802:', '\n', file=f)
	cat('22:EVEREST (INDIA 1956):6377301.243:300.8017:', '\n', file=f)
	cat('23:EVEREST (MALAYSIA 1969):6377295.664:300.8017:', '\n', file=f)
	cat('24:EVEREST (MALAY. & SINGAPORE 1948):6377304.063:300.8017:', '\n', file=f)
	cat('25:EVEREST (PAKISTAN):6377309.613:300.8017:', '\n', file=f)
	cat('26:HAYFORD:6378388.0:297.0:', '\n', file=f)
	cat('27:HELMERT 1906:6378200.0:298.3:', '\n', file=f)
	cat('28:INDONESIAN 1974:6378160.000:298.247:', '\n', file=f)
	cat('29:SOUTH AMERICAN 1969:6378160.000:298.25:', '\n', file=f)
	cat('30:WGS 60:6378165.0:298.3:', '\n', file=f)
	cat('31:MODIS SPHEROID:6371007.181:0.0:', '\n', file=f)
	close(f)
	return(TRUE)
}

	
# Author: Yann Chemin
# IRRI
# License GPL3
# Veon 1, October 2008


albedoAster <- function(green, nir, swir2, swir3, swir5, swir6) {
#ALBEDO: TERRA-Aster sensor 
# Broadband albedo Aster
# After Liang, S.L., 2001. 
# Narrowband to broadband conveon of land surface albedo 1 Algorithms.
# Remote Sensing of Environment. 2001, 76, 213-238.
# Input: Ref1, ref3, Ref5, Ref6, Ref8, Ref9
	return(0.484 * green + 0.335 * nir - 0.324 * swir2 +
				0.551 * swir3 + 0.305 * swir5 - 0.367 * swir6 - 0.0015)
}

albedoLandsat <- function(blue, green, red, nir, chan5, chan7) {
 #Broadband albedo Landsat 5TM and 7ETM+, (maybe othetoo but not sure)
	return(0.293 * blue + 0.274 * green + 0.233 * red +
			0.156 * nir + 0.033 * chan5 + 0.011 * chan7)
}

albedoModis <- function(red, nir, blue, green, swir1, swir2, swir3) {
 #Broadband albedo MODIS
	return(0.22831 * red + 0.15982 * nir + 0.09132 * (blue + green + swir1) + 0.10959 * swir2 + 0.22831 * swir3)
}

albedoAvhrr <- function(red, nir) {
 #Broadband albedo NOAA AVHRR 14 (maybe others too but not sure)
	return(0.035 + 0.545 * nir - 0.32 * red)
}

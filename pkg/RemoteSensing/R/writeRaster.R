# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2010
# Version 0.9
# Licence GPL v3


setMethod('writeRaster', signature(x='Landsat', filename='character'), 
function(x, filename, ...) {

	xr <- callNextMethod(x, filename, ...)
	
	# now write landsat metadata

	
	return(xr)  # for now; a RasterBrick 
}	
)




setClass('ModisLayer',
	representation (
		zone = 'character',
		year = 'integer',
		doy = 'integer',
		date = 'Date',
		red = 'RasterLayer',
		nir = 'RasterLayer',
		blue = 'RasterLayer',
		green = 'RasterLayer',
		swir1 = 'RasterLayer',		
		swir2 = 'RasterLayer',
		swir3 = 'RasterLayer'
	),
)

setClass('ModisStack',
	representation (
		layers = 'list',
		nlayers = 'integer',
		startdate = 'Date',
		enddate = 'Date'
	),
	prototype (
		layers = list(),
		nlayers = as.integer(0)
	),
)



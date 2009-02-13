
setClass('SatelliteImage', contains='VIRTUAL',
	representation (
		platform = 'character',
		sensor = 'character',
		date = 'character',
		time = 'character',
		zone = 'character',
		image = 'RasterStack'
	),
	prototype (
	)
)


setClass('ModisImage', contains='SatelliteImage',
	representation (
		
	),
)


setClass('AsterImage', contains='SatelliteImage',
	representation (
		
	),
)


setClass('ModisTimeSeries',
	representation (
		images = 'list',
		dates = 'vector',
		startdate = 'Date',
		enddate = 'Date'
	),
	prototype (
		images = list()
	),
)



setClass('LandsatImage', 
	contains=c('SatelliteImage', 'VIRTUAL'),
	representation (
		raster = 'RasterLayer'
	
	),
)


setClass('LandsatTM', contains='LandsatImage',
	representation (
	
	),
)




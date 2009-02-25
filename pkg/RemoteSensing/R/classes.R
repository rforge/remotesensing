# Authors: Robert Hijmans and Alice Laborte
# International Rice Research Institute
#Date: February 2009



setClass('SatelliteImage',
	representation (
		spacecraft = 'character',
		sensor = 'character',
		acquisition_date = 'character',
		acquisition_time = 'character',
		sun_elevation = 'numeric',
		sun_azimuth = 'numeric',
		zone = 'character',
		band_filenames = 'vector',
		meta_filename = 'character',
		bands = 'RasterStack'
	),
	prototype (
	)
)



setClass('LandsatImage',  contains=c('SatelliteImage', 'VIRTUAL'),	representation (
		cpf_filename =  'character',		product_creation_date = 'character',		lmax = 'vector',		lmin 	= 'vector',		qcalmax = 'vector',		qcalmin = 'vector'	),
	prototype (
		
	),
)

setClass('LandsatMSS', contains='LandsatImage',
	representation (

	),
)


setClass('LandsatTM', contains='LandsatImage',
	representation (
		thermalband = 'RasterLayer'
	
	),
)

setClass('LandsatETMp', contains='LandsatImage',
	representation (
		thermalbands = 'RasterStack',
		panchromatic = 'RasterLayer'
	),
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



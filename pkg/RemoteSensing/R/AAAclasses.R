# Authors: Robert Hijmans and Alice Laborte
# Date: February 2009


setClass('SatelliteImage', contains=c('RasterStack', 'VIRTUAL'), 
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
		callibrated = 'logical',
		callibration = 'character'
	),
	prototype (
		callibrated = FALSE,
		callibration = ''
	)
)

setClass('Landsat',  contains=c('SatelliteImage', 'VIRTUAL'),	
	representation (
		cpf_filename = 'character',		
		product_creation_date = 'character',		
		lmax	= 'vector',		
		lmin 	= 'vector',		
		qcalmax = 'vector',		
		qcalmin = 'vector'	
	),
	prototype (
		
	),
)

setClass('LandsatMSS', contains='Landsat',
	representation (
	),
)


setClass('LandsatTM', contains='Landsat',
	representation (
		thermal = 'RasterLayer',
		thermal_callibrated = 'logical'
	),
	prototype (
		thermal_callibrated = FALSE
	)
)

setClass('LandsatETMp', contains='Landsat',
	representation (
		thermal = 'RasterStackBrick',
		panchromatic = 'RasterLayer',
		thermal_callibrated = 'logical'
	),
	prototype (
		thermal_callibrated = FALSE
	)	
)


setClass('Aster', contains='Landsat',
	representation (
	),
)


setClass('Modis', contains='SatelliteImage',
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


setClass('Avhrr', contains='SatelliteImage',
	representation (
	),
)


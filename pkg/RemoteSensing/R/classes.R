
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
		meta_filename = 'vector'
		bands = 'RasterStack'
	),
	prototype (
	)
)



setClass('LandsatImage',  contains=c('SatelliteImage', 'VIRTUAL', 'RasterStack')
	representation (
		cpf_file_name 	= 'character'
		product_creation_date = 'Date',
		lmax 			= 'vector',
		lmin 			= 'vector',
		qcalmax 		= 'vector',
		qcalmin 		= 'vector',
		
#		gain 			= 'vector',
#		bias 			= 'vector',
		
#		wrs_path 			= 'integer',
#		starting_row 			= 'integer',
#		ending_row 			= 'integer',
#		nbands 				= 'integer',
#		bands				= 'vector',
		
#		raster 				= 'RasterLayer',
		
	),
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
		thermalbands = 'RasterStack'
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



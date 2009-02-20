
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



setClass('LandsatImage',  contains='SatelliteImage',
	representation (
		raster 				= 'RasterLayer',
		spacecraft_id 			= 'character',
		sensor_id 			= 'character',
		product_creation_date 	= 'Date',
		acquisition_date 		= 'Date',
		doy 					= 'integer',
		sun_elevation 			= 'numeric',
		sun_azimuth 			= 'numeric',
		wrs_path 			= 'integer',
		starting_row 			= 'integer',
		ending_row 			= 'integer',
		band_combination 		= 'character',
		nbands 				= 'integer',
		bands				= 'vector',
		band_filenames		= 'vector',
		lmax 				= 'vector',
		lmin 					= 'vector',
		qcalmax 				= 'vector',
		qcalmin 				= 'vector',
		gain 				= 'vector',
		bias 				= 'vector',
		cpf_file_name 			= 'character'
	),
	prototype (
		
	),
)


setClass('LandsatTM', contains='LandsatImage',
	representation (
	
	),
)




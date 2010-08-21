# Authors: Robert Hijmans and Alice Laborte
# Date: February 2009




setClass('SatelliteImage', contains=c('RasterStack', 'VIRTUAL'), 
	representation (
		spacecraft = 'character',
		sensor = 'character',
		
		specid = 'character',
		specname = 'character',
		speclow = 'numeric',
		spechigh = 'numeric',
		specmid = 'numeric',

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
	prototype (
		specid = c("1", "2", "3", "4"),
		specname = c('VIS', 'VIS', 'VIS', 'NIR'),
		speclow = c(0.5, 0.6, 0.7, 0.8),
		spechigh = c(0.6, 0.7, 0.8, 1.1)
	)
)


setClass('LandsatTM', contains='Landsat',
	representation (
		thermal = 'RasterLayer',
		thermal_callibrated = 'logical'
	),
	prototype (
		specid = c("1", "2", "3", "4", "5", "6", "7"),
		specname = c('VIS', 'VIS', 'VIS', 'NIR', 'SWIR','TIR', 'MWIR'),
		speclow = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.4, 2.08, 0.52),
		spechigh = c(0.52, 0.6, 0.69, 0.9, 1.75, 12.5, 2.35, 0.9),
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
		specid = c("1", "2", "3", "4", "5", "6", "7", "PAN"),
		specname = c('VIS', 'VIS', 'VIS', 'NIR', 'SWIR','TIR', 'MWIR', 'VIS'),
		speclow = c(0.45, 0.525, 0.63, 0.75, 1.55, 10.4, 2.08, 0.52),
		spechigh = c(0.515, 0.605, 0.69, 0.9, 1.75, 12.5, 2.35, 0.9),
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
	prototype (
		specid = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36"),
		specname = c("VIS", "NIR", "VIS", "VIS", "NIR", "SWIR", "SWIR", "VIS", "VIS", "VIS", "VIS", "VIS", "VIS", "VIS", "VIS", "NIR", "NIR", "NIR", "NIR", "MWIR", "MWIR", "MWIR", "MWIR", "MWIR", "MWIR", "SWIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR"),
		speclow = c(0.62, 0.841, 0.459, 0.545, 1.23, 1.628, 2.105, 0.405, 0.438, 0.483, 0.526, 0.546, 0.662, 0.673, 0.743, 0.862, 0.89, 0.931, 0.915, 3.66, 3.929, 3.929, 4.02, 4.433, 4.482, 1.36, 6.535, 7.175, 8.4, 9.58, 10.78, 11.77, 13.185, 13.485, 13.785, 14.085),
		spechigh = c(0.67, 0.876, 0.479, 0.565, 1.25, 1, 652, 2, 155, 0.42, 0.448, 0.493, 0.536, 0.556, 0.672, 0.683, 0.753, 0.877, 0.92, 0.941, 0.965, 3.84, 3, 989, 3, 989, 4.08, 4, 498, 4, 549, 1.39, 6, 895, 7, 475, 8.7, 9.88, 11.28, 12.27, 13, 485, 13, 785, 14, 085, 14, 385)
	)
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


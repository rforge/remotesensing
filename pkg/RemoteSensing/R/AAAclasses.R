# Authors: Robert Hijmans and Alice Laborte
# Date: February 2009


setClass('Sensor',  
	representation (
		name = 'character',
		spacecraft = 'character',
		metafile = 'character',	
		
		# spectral info
		specid = 'character',
		specname = 'character',
		speclow = 'numeric',
		spechigh = 'numeric',
		specmid = 'numeric',
		speccol = 'character',
		layer = 'integer',

		#acquisition info
		acquisition_date = 'character',
		acquisition_time = 'character',
		sun_elevation = 'numeric',
		sun_azimuth = 'numeric',

		product_creation_date = 'character',		
		zone = 'character'
	)
)

setClass('SatelliteImage', contains=c('RasterStack', 'VIRTUAL'), 
	representation (
		sensor = 'Sensor',
		callibrated = 'logical',
		callibration = 'character'
	),
	prototype (
		callibrated = FALSE,
		callibration = ''
	)
)


setClass('LandsatSensor',  contains='Sensor', 
	representation (
		cpf_filename = 'character',		
		lmax	= 'vector',		
		lmin 	= 'vector',		
		qcalmax = 'vector',		
		qcalmin = 'vector'		
	)
)




setClass('Landsat',  contains=c('SatelliteImage', 'VIRTUAL'),	
	representation (
	),
	prototype (	
	),
)



setClass('LandsatMSS', contains='Landsat',
	representation (
	),
	prototype (
	)
)

setMethod("initialize", "LandsatMSS",
	function(.Object, ...) {
	  	.Object@sensor <- new('LandsatSensor')
	  	.Object@sensor@specid = c("1", "2", "3", "4")
		.Object@sensor@specname = c('VIS', 'VIS', 'VIS', 'NIR')
		.Object@sensor@speclow = c(0.5, 0.6, 0.7, 0.8)
		.Object@sensor@spechigh = c(0.6, 0.7, 0.8, 1.1)
        .Object
    }
)



setClass('LandsatTM', contains='Landsat',
	representation (
		thermal = 'RasterLayer',
		thermal_callibrated = 'logical',
		thermal_callibration = 'character'
	),
	prototype (
		thermal_callibrated = FALSE,
		thermal_callibration = ''
	)
)


setMethod("initialize", "LandsatTM",
	function(.Object, ...) {
	  	.Object@sensor <- new('LandsatSensor')
	  	.Object@sensor@specid = as.character(1:7)
		.Object@sensor@specname = c('VIS', 'VIS', 'VIS', 'NIR', 'SWIR','TIR', 'MWIR')
		.Object@sensor@speclow = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.4, 2.08, 0.52)
		.Object@sensor@spechigh = c(0.52, 0.6, 0.69, 0.9, 1.75, 12.5, 2.35, 0.9)
        .Object
    }
)



setClass('LandsatETMp', contains='Landsat',
	representation (
		thermal = 'RasterStackBrick',
		panchromatic = 'RasterLayer',
		thermal_callibrated = 'logical',
		thermal_callibration = 'character'
	),
	prototype (
		thermal_callibrated = FALSE,
		thermal_callibration = ''
	)	
)


setMethod("initialize", "LandsatETMp",
	function(.Object, ...) {
	  	.Object@sensor <- new('LandsatSensor')
	  	.Object@sensor@specid = c(as.character(1:7), 'PAN')
		.Object@sensor@specname = c('VIS', 'VIS', 'VIS', 'NIR', 'SWIR','TIR', 'MIR', 'VIS')
		.Object@sensor@speclow = c(0.45, 0.525, 0.63, 0.75, 1.55, 10.4, 2.08, 0.52)
		.Object@sensor@spechigh = c(0.515, 0.605, 0.69, 0.9, 1.75, 12.5, 2.35, 0.9)
		.Object@sensor@speccol = c('blue', 'green', 'red', 'NIR', 'TIR', 'MIR', 'PAN')
		.Object@sensor@layer = as.integer(c(1:5, NA, 6, NA))
        .Object
    }
)


setClass('Aster', contains='Landsat',
	representation (
	),
)


setClass('Modis', contains='SatelliteImage',
	representation (
	),
	prototype (
	)
)


setMethod("initialize", "Modis",
	function(.Object, ...) {
	  	.Object@sensor@specid = c(as.character(1:36))
		.Object@sensor@specname = c("VIS", "NIR", "VIS", "VIS", "NIR", "SWIR", "SWIR", "VIS", "VIS", "VIS", "VIS", "VIS", "VIS", "VIS", "VIS", "NIR", "NIR", "NIR", "NIR", "MWIR", "MWIR", "MWIR", "MWIR", "MWIR", "MWIR", "SWIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", "TIR")
		.Object@sensor@speclow = c(0.62, 0.841, 0.459, 0.545, 1.23, 1.628, 2.105, 0.405, 0.438, 0.483, 0.526, 0.546, 0.662, 0.673, 0.743, 0.862, 0.89, 0.931, 0.915, 3.66, 3.929, 3.929, 4.02, 4.433, 4.482, 1.36, 6.535, 7.175, 8.4, 9.58, 10.78, 11.77, 13.185, 13.485, 13.785, 14.085)
		.Object@sensor@spechigh = c(0.67, 0.876, 0.479, 0.565, 1.25, 1, 652, 2, 155, 0.42, 0.448, 0.493, 0.536, 0.556, 0.672, 0.683, 0.753, 0.877, 0.92, 0.941, 0.965, 3.84, 3, 989, 3, 989, 4.08, 4, 498, 4, 549, 1.39, 6, 895, 7, 475, 8.7, 9.88, 11.28, 12.27, 13, 485, 13, 785, 14, 085, 14, 385)
        .Object
    }
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


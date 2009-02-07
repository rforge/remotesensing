# Author: Yann Chemin
# IRRI
# License GPL3
# Veon 1, October 2008



albedo.aster<-function(greenchan, nirchan, swirchan2, swirchan3, swirchan5, swirchan6) #ALBEDO: TERRA-Aster sensor
{
# Broadband albedo Aster
# After Liang, S.L., 2001. 
# Narrowband to broadband conveon of land surface albedo 1 Algorithms.
# Remote Sensing of Environment. 2001, 76, 213-238.
# Input: Ref1, ref3, Ref5, Ref6, Ref8, Ref9
	result <- (0.484 * greenchan + 0.335 * nirchan - 0.324 * swirchan2 +
	 0.551 * swirchan3 + 0.305 * swirchan5 - 0.367 * swirchan6 - 0.0015)
	return(result)
}

albedo.landsat<-function(bluechan, greenchan, redchan, nirchan, chan5, chan7)
 #Broadband albedo Landsat 5TM and 7ETM+, (maybe othetoo but not sure)
{
	result <- (0.293 * bluechan + 0.274 * greenchan + 0.233 * redchan +
	 0.156 * nirchan + 0.033 * chan5 + 0.011 * chan7)
	return(result)
}

albedo.modis<-function(redchan, nirchan, chan3, chan4, chan5, chan6, chan7)
 #Broadband albedo MODIS
{
	result <- (0.22831 * redchan + 0.15982 * nirchan + 0.09132 * (chan3 + chan4 + chan5) + 0.10959 * chan6 + 0.22831 * chan7)
	return(result)
}

albedo.noaa<-function(redchan, nirchan)
 #Broadband albedo NOAA AVHRR 14 (maybe others too but not sure)
{
	result <- (0.035 + 0.545 * nirchan - 0.32 * redchan)
	return(result)
}

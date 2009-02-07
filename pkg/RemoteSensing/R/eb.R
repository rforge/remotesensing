# ENERGY BALANCE CODE

deltat_wim <-function(surface_temperature)
{
	# Wim delta T (soil-air) equation
	result <- 0.3225 * surface_temperature - 91.743
	result[result<1]<-1.0
	result[result>13]<-13.0
	return(result)
}

solar_day<-function(latitude, doy, tsw )
{
	#Sun-Earth Distance (ds A.U.)
	# Average Solar Diurnal Radiation after Bastiaanssen (1995) 
	ds <- 1.0 + 0.01672 * sin(2*pi*(doy-93.5)/365.0)
	#Solar declination (delta radians)
	deltarad <- 0.4093*sin((2*pi*doy/365)-1.39)
	#Convert latitude in radians
	latrad <-  latitude * pi / 180.0
	#Convert latitude in radians
	ws <- acos(-tan(latrad)*tan(deltarad))
	cosun <- ws*sin(deltarad)*sin(latrad)+cos(deltarad)*cos(latrad)*sin(ws)
	result <- ( cosun * 1367 * tsw ) / ( pi * ds * ds )
	return(result)
}

rnet_day<-function( bbalb,  solar,  tsw )
{
	#Average Diurnal Net Radiation after Bastiaanssen (1995)
	result <- ((1.0 - bbalb)*solar)-(110.0*tsw)
	return(result)
}

etpot_day<-function( rnetd,  tempk,  roh_w )
{
	#Average Diurnal Potential ET after Bastiaanssen (1995)
	latent<-(2.501-(0.002361*(tempk-273.15)))*1000000.0
	result <- (rnetd*86400*1000.0)/(latent*roh_w)
	return(result)
}

rnet<-function( bbalb,  ndvi,  tempk,  dtair,   e0,  tsw,  doy,  utc,  sunzangle )
{
	#Instantaneous net radiation (W/m2)
	# Tsw <-  atmospheric transmissivity single-way (~0.7 -) */
	# DOY <- Day of Year */
	# utc <- UTC time of sat overpass*/
	# sunzangle <- sun zenith angle at sat. overpass */
	# tair <- air temperature (approximative, or met.station) */
	tsw_for_e_atm<-0.7 #Special tsw, consider it a coefficient
	#Atmospheric emissivity (Bastiaanssen, 1995)
	e_atm	<-  1.08 * (-log10(tsw_for_e_atm))^0.265 
	#Atmospheric emissivity (Pawan, 2004)
#	e_atm	<- 0.85 * (-log10(tsw))^0.09
	ds <- 1.0 + 0.01672 * sin(2*pi*(doy-93.5)/365)
	delta <- 0.4093*sin((2*pi*doy/365)-1.39)
	#Kin is the shortwave incoming radiation
	Kin	<- 1367.0 * (cos(sunzangle*pi/180) * tsw / (ds*ds) )
	#Lin is incoming longwave radiation
	Lin	<- (e_atm) * 5.67 * 10^(-8) * (tempk-dtair^4)
	#Lout is surface grey body emission in Longwave spectrum
	Lout	<- e0 * 5.67 * 10^(-8) * (tempk^4)
	#Lcorr is outgoing longwave radiation "reflected" by the emissivity
	Lcorr	<- (1.0 - e0) * Lin
	result	<- (1.0 - bbalb) * Kin + Lin - Lout - Lcorr  
	return(result)
}

g0<-function(  bbalb,  ndvi,  tempk,  rnet,  time=11.0, roerink=FALSE)
{
	#Soil Heat Flux
	if(time<9.0 || time>15.0){
		r0_coef <- 1.1
	} else if (time>9.0 && time<=11.0){
		r0_coef <- 1.0
	} else if (time>11.0 && time<=13.0){
		r0_coef <- 0.9
	} else if (time>13.0 && time<=15.0){
		r0_coef <- 1.0
	}
	a <- (0.0032 * (bbalb/r0_coef) + 0.0062 * (bbalb/r0_coef) * (bbalb/r0_coef))
	b <- (1 - 0.978 * (ndvi^4))
	#Spain (Bastiaanssen, 1995)
	result <- (rnet * (tempk-273.15) / bbalb) * a * b 
	#HAPEX-Sahel (Roerink, 1995)
	if(roerink==TRUE){
		result <- result * 1.430 - 0.0845
	}
	return(result)
}

dtair0<-function( t0_dem,  tempk_water=298.0,  tempk_desert=333.0)
{
	#DTAIR Initialization
	# Pixel-based input required are: tempk water & desert
	# * additionally, dtair in Desert is vaguely initialized
	ZERO<-273.15	
	if(tempk_desert >= (ZERO+48.0)){
		dtair_desert_0 <- 13.0
	} else if(tempk_desert >= (ZERO+40.0) && tempk_desert < (ZERO+48.0)){
		dtair_desert_0 <- 10.0
	} else if(tempk_desert >= (ZERO+32.0) && tempk_desert < (ZERO+40.0)){
		dtair_desert_0 <- 7.0
	} else if(tempk_desert >= (ZERO+25.0) && tempk_desert < (ZERO+32.0)){
		dtair_desert_0 <- 5.0
	} else if(tempk_desert >= (ZERO+18.0) && tempk_desert < (ZERO+25.0)){
		dtair_desert_0 <- 3.0
	} else if(tempk_desert >= (ZERO+11.0) && tempk_desert < (ZERO+18.0)){
		dtair_desert_0 <- 1.0
	} else {
		dtair_desert_0 <- 0.0
#		printf("WARNING!!! dtair_desert_0 is NOT VALID!\n")
	}
	a <- (dtair_desert_0-0.0)/(tempk_desert-tempk_water)
	b <- 0.0 - a * tempk_water
	result <- t0_dem * a + b
	return(result)
}

dtair<-function( t0_dem,  tempk_water=298.0,  tempk_desert=333.0,  dtair_desert=13.0)
{
	#DTAIR Standard equation
	# Pixel-based input required are: tempk water & desert
	# additionally, dtair in Desert point should be given
	#Only t0_dem (altitude corrected DEM) is Array
	a <- (dtair_desert-0.0)/(tempk_desert-tempk_water)
	b <- 0.0 - a * tempk_water
	result <- t0_dem * a + b
	return(result)
}

dtair_desert<-function( h_desert, roh_air_desert=1.12, rah_desert=10.0)
{
	#DTAIR Dry Pixel
	#Only h_desert input is Array
	result <- (h_desert * rah_desert)/(roh_air_desert * 1004.0)
	return(result)
}

h0<-function( roh_air, rah, dtair)
{
	#Sensible Heat Flux Standard Equation
	#All inputs are Arrays
	result <- roh_air*1004.0*(dtair) / rah
	return (result)
}

psi_h<-function( t0_dem, h, U_0, roh_air)
{
	#Psichrometric parameter for heat momentum
	#All inputs are Arrays
	if(h != 0.0){
		n5_temp <- (-1004* roh_air*(U_0^3)* t0_dem)/(0.41*9.81* h)
	} else {
		n5_temp <- -1000.0
	}
	if(n5_temp < 0.0){
		n12_mem <- ((1-16*(2/n5_temp))^0.25)
		n11_mem <- (2*log10((1+(n12_mem^2))/2))
	} else {
		n12_mem <- 1.0
		n11_mem <- -5*2/n5_temp
	}
	result <- n11_mem
	return (result)
}

rah0<-function( zom_0, u_0)
{
	#Aerodynamic resistance to heat momentum initialization
	result <- log10(2/0.01)/(u_0*0.41)
	return (result)
}

rah1<-function( psih, ustar)
{
	#Aerodynamic resistance to heat momentum standard equation
	result <- (log10(2/0.01)-psih)/(ustar*0.41)
	return (result)
}

rohair0<-function(tempk)
{
	#Air density Initialization
	A <- (18.0*(6.11*exp(17.27*34.8/(34.8+237.3)))/100.0)
	B <- (18.0*(6.11*exp(17.27*34.8/(34.8+237.3)))/100.0)
	result <- (1000.0 - A)/(tempk*2.87)+ B/(tempk*4.61)
	return(result)
}

rohair<-function( dem, tempk, dtair)
{
	#Air density standard paramterization
	a <- tempk - dtair
	b <- (( a - 0.00627*dem)/a)
	result <- 349.467 * ( b ^ 5.26)/ a 
	return(result)
}

U0<-function( zom0, u2m)
{
	#Wind Speed Initialization
	u0<-u2m*0.41*log10(200/(0.15/7))/(log10(2/(0.15/7))*log10(200/zom0)) 
	return(u0)
}

ustar<-function( t0_dem, h, ustar, roh_air, zom, u2m)
{
	#Nominal Wind Speed
# 	 n5_temp # Monin-Obukov Length 		*/
#        n10_mem # psi m 			*/
# 	 n31_mem # x for U200 (that is bh...) 	*/
	 hv<-0.15	# crop height (m) 		*/
	 bh<-200	# blending height (m) 		*/
	
	if(h != 0.0){
		n5_temp <- (-1004* roh_air*(ustar^3)* t0_dem)/(0.41*9.81* h)
	} else {
		n5_temp <- -1000.0
	}
	if(n5_temp < 0.0){
		n31_mem <- ((1-16*(200/n5_temp))^0.25)
		n10_mem <- (2*log10((1+n31_mem)/2)+log10((1+(n31_mem^2))/2)-2*atan(n31_mem)+0.5*pi)
	} else {
#		n31_mem <- 1.0
		n10_mem <- -5*2/n5_temp
	}
	result <- ((u2m*0.41/log10(2/(hv/7)))/0.41*log10(bh /(hv/7)*0.41))/(log10(bh / zom)-n10_mem)
	return(result)
}


zom0<-function( ndvi, ndvi_max)
{
	#Roughness length for heat momentum 
	hv_ndvimax<-1.5 # crop vegetation height (m) */
	hv_desert<-0.002 # desert base vegetation height (m) */
	a <- (log10(hv_desert)-((log10(hv_ndvimax/7)-log10(hv_desert))/(ndvi_max-0.02)*0.02))
	b <- (log10(hv_ndvimax/7)-log10(hv_desert))/(ndvi_max-0.02)* ndvi
	result <- exp(a+b) 
	return(result)
}


sensih<-function( iteration, tempk_water, tempk_desert, t0_dem, tempk, ndvi, ndvi_max, dem, rnet_desert, g0_desert, t0_dem_desert, u2m, dem_desert)
{
	#Sensible Heat flux determination
	# This is the main loop used in SEBAL */
	# Arrays Declarations */
	# define ITER_MAX 10
	ITER_MAX<-10
	# Arrays Declarations */
	# Fat-free junk food */
	if (iteration>ITER_MAX){
		iteration<-ITER_MAX
	}

#	dtair[0] 	<- dt_air_0(t0_dem, tempk_water, tempk_desert)
	dtair[0] <- 5.0
#	printf("*****************************dtair <- %5.3f\n",dtair[0])
	roh_air[0] 	<- rohair0(tempk)
#	printf("*****************************rohair<-%5.3f\n",roh_air[0])
	roh_air_desert 	<- rohair0(tempk_desert)
#	printf("**rohairdesert <- %5.3f\n",roh_air_desert)
	zom0 		<- zom0(ndvi, ndvi_max)
#	printf("*****************************zom <- %5.3f\n",zom0)
	u_0 		<- U0(zom0, u2m)
#	printf("*****************************u0\n")
	rah[0] 		<- rah0(zom0, u_0)
#	printf("*****************************rah <- %5.3f\n",rah[0])
	h[0] 		<- h0(roh_air[0], rah[0], dtair[0])
#	printf("*****************************h\n")

#----------------------------------------------------------------*/
#Main iteration loop of SEBAL*/
	zom[0] <- zom0
	for(ic in 1:(iteration+1)){
		# Where is roh_air[i]? */
		psih <- psi_h(t0_dem,h[ic-1],u_0,roh_air[ic-1])
		ustar[ic] <- ustar(t0_dem,h[ic-1],u_0,roh_air[ic-1],zom[0],u2m)
		rah[ic] <- rah1(psih, ustar[ic])	
		# get desert point values from maps */
		if(ic==1){
			h_desert	<- rnet_desert - g0_desert
			zom_desert	<- 0.002
			psih_desert 	<- psi_h(t0_dem_desert,h_desert,u_0,roh_air_desert)
			ustar_desert	<- ustar(t0_dem_desert,h_desert,u_0,roh_air_desert,zom_desert,u2m)
		} else {
			roh_air_desert	<- rohair(dem_desert,tempk_desert,dtair_desert)
			h_desert	<- h0(roh_air_desert,rah_desert,dtair_desert)
			ustar_desertold <- ustar_desert
			psih_desert 	<- psi_h(t0_dem_desert,h_desert,ustar_desertold,roh_air_desert)
			ustar_desert	<- ustar(t0_dem_desert,h_desert,ustar_desertold,roh_air_desert,zom_desert,u2m)
		}
		rah_desert	<- rah1(psih_desert,ustar_desert)
		dtair_desert 	<- dtair_desert(h_desert, roh_air_desert, rah_desert)
		# This should find the new dtair from inversed h equation...*/
		dtair[ic] 	<- dtair(t0_dem, tempk_water, tempk_desert, dtair_desert)
		# This produces h[ic] and roh_air[ic+1] */
		roh_air[ic] 	<- rohair(dem, tempk, dtair[ic])
		h[ic] 		<- h0(roh_air[ic], rah[ic], dtair[ic])
	}
	return(h[iteration])
}

evapfr<-function( rnet, g0, h0)
{
	#Evaporative Fraction
	result <- (rnet - g0 - h0) / (rnet - g0)
	return(result)
}


soilmoisture<-function( evapfr )
{
	#soil moisture in the root zone
	#Makin, Molden and Bastiaanssen, 2001
	result <- (exp((evapfr-1.2836)/0.4213))/0.511
	return(result)
}


eta<-function( rnet_day, evapfr, surface_temperature)
{
	#Evapotranspiration from energy balance
	t_celsius <- surface_temperature - 273.15 
	latent 	  <- 86400.0/((2.501-0.002361*t_celsius)*(10^6))
	result 	  <- rnet_day * evapfr * latent 
	return(result)
}

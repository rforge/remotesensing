################## METRIC #####################
##### ET - Energy Balance Modeling ############

###############################################
######### NON-SPATIAL FUNCTIONS ###############
###############################################

Rsky <- function(ta) #ta is near surface airtemperature from meteorological data
{
	result <- 1.807 * pow(10,-10) * pow(ta,4) * (1 - 0.26 * exp(-7.77 * pow(10,-4) * pow((273.15 - ta),2)))
	return(result)
}

###############################################
########### SPATIAL FUNCTIONS #################
###############################################
# Narrow band emissivity calculations (e_NB)
e_NB <- function(ndvi,lai)
{
	if(ndvi > 0 && ndvi < 3) 
		result <- 0.97 + 0.0033 * lai
	else if(lai >= 3)
		result <- 0.98
	else result <- 0.99
	return (result)
}
# Surface temperature calculations Landsat 5TM
T0L5 <- function(e_NB,L6rad,Rsky)
{
	#R_sky <- 1.4199 #Spreadsheet calculations, based on air temperature. Page 26 Metric manual
	T_NB <- 0.866  #Page 26 Metric manual
	RP <- 0.91 #Page 26 Metric manual
	result <- 1260.56/log(1 + (e_NB * 607.76) / L6rad)
	return(result)
}
	
# Surface emissivity calculations (e_0)
e_0 <- function(ndvi,lai)
{
	if(ndvi > 0 && ndvi < 3) 
		result <- 0.95 + 0.01 * lai
	else if(lai >= 3)
		result <- 0.98
	else result <- 0.985
	return (result)
}

#Outgoing longwave radiation (Lout)
Lout <- function(e_0,t0)
{
	result <- 5.67 * pow(10,-8) * pow(t0,4)
	return(result)
}

#Incoming longwave radiation (Lin). To be derieved locally at CSU
Lin <- function(tsw,ta)
{
	result <- 0.85 * pow(-log(tsw),0.09) * 5.67 * pow(10,-8) * pow(ta,4)
	return(result)
}

#Net longwave radiation (Lnet)
Lnet <- function(Lout,Lin)
{
	result <- Lin - Lout
	return(result)
}

#Net Radiation (Rnet)
Rnet <- function(alb,Lnet,Kin)
{
	#Kin from speadsheets
	result <- (1 - alb) * Kin + Lnet
	return(result)
}

#calculation of g0
g0 <- function(Rnet,t0,alb,ndvi)
{
	result <- Rnet * (t0 - 273.15) * ( 0.0038 + 0.0074 * alb) * (1 - 0.98 * pow(ndvi,4))
	return(result)
}

#Calculation of surface roughness of momentum
z0m <- function(lai)
{
      result <- 0.018 * lai
      return(result)
}

#Calculation of effitive wind speed (initialsation of a unique point based ustar)
ustar0 <- function(u,z,h)
{
      # u is wind speed at z meter, with vegetation height below of h meters
      #u200 calculated from equation 41, page 39
      ustar <- 0.41 * u / (log(z/(0.12*h)))
      u200 <- ustar * log (200/(0.12*h)) / 0.41
      result <- 0.41 * u200 / log(200 / (0.12*h))
      return(result)
}

#Calculation of aerodynamic resistance to heat flux momentum
rah0 <- function(ustar0) #initialsation of rah calculation
{
      result <- log(2 / 0.1)/(ustar * 0.41)
      return(result)
}

# calculation of dTair (spreadsheet calculations)
dTair <- function(eto_alf,dem_wet,t0_wet,Rnet_wet,g0_wet,dem_dry,t0_dry,Rnet_dry,g0_dry)
{
	# Somehow T0_dem is not used in the spreadsheet
	# a is slope and b is offset
	eto <- eto_alf
	LE_wet<-eto*kc*(2.501-0.002361*(t0-273.15))*pow(10,6)/3600
	LE_dry<-0.0
	h_wet<- Rnet_wet-g0_wet-LE_wet
	h_dry<- Rnet_dry-g0_dry-LE_dry
	ustar_wet<-ustar0(3.6,200,0.75)#common cereal crops average height
	ustar_dry<-ustar0(3.6,200,0.017)#standard desert sebal parameters
	rah_wet<-rah0(ustar_wet)
	rah_dry<-rah0(ustar_dry)
	dT_wet<-h_wet*rah_wet/(rohair_wet*1004)
	dT_dry<-h_dry*rah_dry/(rohair_dry*1004)
	a[0]<-(dT_dry-dT_wet)/(t0_dry-t0_wet)
	b[0]<- -a * t0_wet + dT_wet
	#dT1 is here
	roh_air <- function(dem,t0,dT)
	{
		result<-349.467*pow((((t0-dT)-0.0065*dem)/(t0-dT)),5.26)/t0
		return(result)
	}
	for (i in 8)
		airden_wet<-roh_air(dem_wet,t0_wet,dT_wet)
		airden_dry<-roh_air(dem_dry,t0_dry,dT_dry)
		h_wet<-h(airden_wet,dT_wet,rah_wet)
		h_dry<-h(airden_dry,dT_dry,rah_dry)
		if(h_wet==0)
			L_wet<- -1000
		else
			L_wet<- -1004*airden_wet*pow(ustar_wet,3)*t0_wet/(0.41*h_wet*9.81)
		if(h_dry==0)
			L_dry<- -1000
		else
			L_dry<- -1004*airden_dry*pow(ustar_dry,3)*t0_dry/(0.41*h_dry*9.81)
		psim200_wet<-psim200(L_wet)
		psim200_dry<-psim200(L_dry)
		psih2_wet<-psih2(L_wet)
		psih2_dry<-psih2(L_dry)
		psih01_wet<-psih01(L_wet)
		psih01_dry<-psih01(L_dry)
		z0m_wet<-0.12*0.75
		z0m_dry<-0.12*0.02
		ustar_wet<-ustar(3.6,z0m_wet,psim200_wet)
		ustar_dry<-ustar(3.6,z0m_dry,psim200_dry)
		rah_wet <- rah(ustar_wet,psih2_wet,psih01_wet)
		rah_dry <- rah(ustar_dry,psih2_dry,psih01_dry)
		dT_wet<-h_wet*rah_wet/(rohair_wet*1004)
		dT_dry<-h_dry*rah_dry/(rohair_dry*1004)
		a[i]<-(dT_dry-dT_wet)/(t0_dry-t0_wet)
		b[i]<- -a * t0_wet + dT_wet
	return((a,b))
}

#calculation for air density
rohair <- function(Patm,t0,dTair,eact,...)
{
	result <- Patm * eact / (C1 * (t0 - dTair) * C2)
	return(result)
}

#calculation of sensible heat flux (Single equation)
h <- function(rohair,dTair,rah)
{
	result <- rohair * 1004 * dTair / rah
	return(result)
}

#Monin-Obukov Length calculation
L <- function(rohair,ustar,t0,h)
{
	result <- rohair * 1004 * pow(ustar,3) * t0 / (0.41 * 9.807 * h)
	return(result)
}

# psychrometric momentum constant calculation
psim200 <- function(L)
{
	if (L < 0)
		x200 <- pow((1 - 16 * 200/L),0.25)
		result <- 2 * log((1 + x200)/2) + log((1 + pow(x200,2))/2) - 2 * atan(x200 + 0.5 * pi)
	else result <- -5 * 2 / L
	return(result)
}

# psychrometric heat constant calculation
psih2 <- function(L)
{
	if (L < 0)
		x2 <- pow((1 - 16 * 2/L),0.25)
		result <- 2 * log((1 + pow(x2,2))/2)
	else result <- -5 * 2 / L
	return(result)
}
		
# psychrometric heat constant calculation
psih01 <- function(L)
{
	if (L < 0)
		x01 <- pow((1 - 16 * 0.1/L),0.25)
		result <- 2 * log((1 + pow(x01,2))/2)
	else result <- -5 * 2 / L
	return(result)
}

#Calculation of effitive wind speed
ustar <- function(u200,z0m,psim200)
{
      #u200 calculated from equation 41, page 39
      result <- 0.41 * u200 / (log(200 / z0m) - psim200)
      return(result)
}

#Calculation of aerodynamic resistance to heat flux momentum
rah <- function(ustar,psih2,psih01) #initialsation of rah calculation
{
      result <- (log(2 / 0.1) - psih2 + psih01) / (ustar * 0.41)
      return(result)
}

#Sensible Heat flux Calculations
metiter <- function(dTab,t0,rah0,z0m,ustar0,Patm,eact,C1,C2)
{
	r_ah <- rah0
	for (i in 6)
		dT <- dTa[i - 1] * t0 + dTb[i - 1]
		airden <- rohair(Patm,eact,t0,dT,C1,C2)
		h0 <- h(airden,dT,r_ah)
		L0 <- L(airden,ustar,t0,h0)
		psim_200 <- psim200(L)
		psih_2 <- psih2(L)
		psih_01 <- psih01(L)
		u_star <- ustar(u200,z_0m,psim_200)
		r_ah <- rah(psih_2,psih_01,u_star)
	return(h0)
}

#ETinst
ETinst <- function(Rnet,g0,h0,t0)
{
	LHF <- Rnet - g0 - h0
	result <- 3600 * LHF / ((2.501 - 0.00236 * (t0 - 273.15)) * 1000000)
	return(result)
}



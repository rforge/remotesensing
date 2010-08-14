# SSEB Senay G.B., Budde M., Verdin J.P., and Melesse A.M., 2007, Sensors, 7:979-1000.
sseb_eta <- function(lst_h,lst_c,lst,et0PM) #Lst hot, Lst cold, Lst, FAO56-ET0
{
	evap_fr <- (lst_h-lst)/(lst_h-lst_c)
	eta <- evap_fr*et0PM
}

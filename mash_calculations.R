# Mash Infusion Calculator

library(dplyr)

#E = elevation
#Tbw = Temperature of boiling water (°F)
#r = The ratio of water to grain (quarts/pound).
#Wa = The volume of boiling water added (in quarts).
#Wm = The total volume of water in the mash (in quarts).
#T1 = The initial temperature (°F) of the mash.
#T2 = The target temperature (°F) of the mash.
#Tw = The actual temperature (°F) of the infusion water.
#G = Total grain weight (lbs).
#Va = volume of solution A whose temperature is Ta
#Vb = volume of solution B whose temperature is Tb
#Tf = the temperature of the solution after mixing.

# Elevation Correction

E <- 2411
Tbw <- 212 - 2523  * 0.0018


# Initial infusion calculator

r <- 1.5 #Quarts to Pounds
T1 <- 70 # Fahrenheit 
T2 <- 153 #F
Wm <- 0 #Volume in Mash Tun (in quarts)


Tw <- (0.2/r) * (T2 - T1) + T2


# Mash Infusion Formula

G <- 10
Wa <- ((T2 - T1) * (0.2*G + Wm))/(Tw - T2)

# Increase Temperature




# Cooling Water  
Thermal_Temp_Loss <- 2 #F
Boiling_Water_Temp <- 212 #F


Ta <- 118
Va <- 8
Tb <- 50
Tf <- 77.2

Vb <- ((Va * Ta) - (Tf * Va))/(Tf - Tb)




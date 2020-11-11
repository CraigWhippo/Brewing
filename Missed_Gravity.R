# Missed Gravity

Actual_Volumn <- 5 #gallons
Measured_Gravity <- 1.031
Target_Gravity <- 1.040

# Water_Needed (Quarts)
Water_Neded = ((Actual_Volumn * (Measured_Gravity - Target_Gravity))/(Target_Gravity - 1))*4 

# DME Needed (lbs)

DME_needed <- (Actual_Volumn * (Target_Gravity - Measured_Gravity))/0.045

# Kaminski, Colin (2019) Hot-Side Math: Geeking out on Brewhouse Efficiency, Brew Your Own. 25(4)-95.



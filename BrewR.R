# Gravity Correction
# Kaminski, Colin (2019) Hot-Side Math: Geeking out on Brewhouse Efficiency, Brew Your Own. 25(4)-95.

gravity_correction <- function(Actual_Volumn, Measured_Gravity, Target_Gravity){
  if(Measured_Gravity > Target_Gravity){
    water <- round(((Actual_Volumn * (Measured_Gravity - Target_Gravity))/(Target_Gravity - 1))*4, 1)
    result <- paste("Add", water, "quarts of water", sep = " " )
  } else {
    dme <- round((Actual_Volumn * (Target_Gravity - Measured_Gravity))/0.0450,1)
    result <- paste("Add", dme, "pounds of DME", sep = " " )
  }
  return(result)
}

# Boil-off Calculator

boiloff_gravity <- function(Begining_Gravity, Beginning_Volume, End_Volume){
  1-(Beginning_Volume * (1-Begining_Gravity))/End_Volume
}


# Volume Change Formula
target_volume <- function(Current_Gravity, Current_Volume, Target_Gravity){
  (Current_Volume * (1-Current_Gravity))/(1-Target_Gravity)
}


# ABV
get_ABV = function(og, fg){
  round((76.08 * (og - fg)/(1.775 - og)) * (fg/0.794),2)
}

# Plato Scale Conversion = %sucrose in solution

get_plato <- function(specific_gravity){
  round((-463.37) + (668.72 * specific_gravity) - (205.35 * specific_gravity^2), 2)
}

# Real Extract = the amount of sugar used

get_real_extract <- function(Pi, Pf){
  round((0.1808 * Pi) + (0.8192 * Pf),2)
}

# Attenuation

get_apparent_attenuation <- function(Pi, Pf){round(1 - (Pf/Pi), 3)}

get_real_attenuation <- function(RE, Pi){round(1- (RE/Pi), 3)}  

# ABW

get_ABW <- function(ABV, fg){round(0.79 * ABV/fg, 2)}

# Calories per 12 oz beer

get_calories <- function(ABW, RE, fg){
  round(((6.9 * ABW) + 4.0 * (RE -0.1)) * fg * 3.55, 0)
}


# Gravity Correction
# Kaminski, Colin (2019) Hot-Side Math: Geeking out on Brewhouse Efficiency, Brew Your Own. 25(4)-95.

gravity_correction <- function(Actual_Volume, Measured_Gravity, Target_Gravity){
  if(Measured_Gravity > Target_Gravity){
    water <- round(((Actual_Volume * (Measured_Gravity - Target_Gravity))/(Target_Gravity - 1))*4, 1)
    result <- paste("Add", water, "quarts of water", sep = " " )
  } else {
    dme <- round((Actual_Volume * (Target_Gravity - Measured_Gravity))/0.0450,1)
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

# Estimated Boil Gravity

get_boil_gravity <- function(final_volume, orginal_gravity, boil_volume){
  ((final_volume * (final_gravity - 1))/boil_volume) + 1
}

# Tinseth’s IBU Formula

get_tinseth <- function(decimal_AA_rating, ozs_hops, gallons_of_wort, OG, time_min){
  mg_per_L_alpha_acid <- (decimal_AA_rating * ozs_hops * 7490)/gallons_of_wort
  bigness_factor <- 1.65 * 0.000125^(OG - 1)
  boil_time_factor <- (1 - 2.71828^(-0.04 * time_min))/4.15
  utlization <- bigness_factor * boil_time_factor
  ibu <- utlization * mg_per_L_alpha_acid
  total_ibu <- round(sum(ibu), 2)
}

# Rager IBU

get_rager <- function(decimal_AA_rating, ozs_hops, gallons_of_wort, OG, time_min){
  if(OG > 1.050){
    GA <- (OG - 1.050)/2
  }else{
    GA <- 0  
  }
  utilization <- (18.11 + (13.86 * tanh((time_min - 31.32) / 18.27)))/100
  IBU <- (ozs_hops * utilization * decimal_AA_rating * 7462)/(gallons_of_wort * (1 + GA))
  Total_IBU <- round(sum(IBU), 2)
}

# Garetz IBU Formula

get_garetz <- function(tim_min, gallons_of_wort, pre_boil_volume, OG, desired_ibus, elevation, decimal_AA_rating, ozs_hops){
  utilization <- 7.2994 + (15.0746 * tanh((tim_min - 21.86) / 24.71))
  CF <- gallons_of_wort/pre_boil_volume
  BG <- (CF * (OG - 1)) + 1
  GF <- (BG - 1.050)/.2 +1
  HF <- ((CF * desired_ibus)/260) + 1
  TF = ((elevation/550) * 0.02) + 1 
  CA = GF * HF * TF
  IBU <- 100 * (utilization * decimal_AA_rating * ozs_hops * 0.749)/(gallons_of_wort * CA)
  Total_IBU <- sum(IBU)
}

# Daniels IBU Formula


get_daniels <- function(decimal_AA_rating, ozs_hops, gallons_of_wort, OG, time_min){
  bigness_factor <- 1.65 * 0.000125^(OG - 1)
  boil_time_factor <- (1 - 2.71828^(-0.04 * time_min))/4.15
  utlization <- bigness_factor * boil_time_factor
  IBU <- utlization * (decimal_AA_rating * ozs_hops * 7489)/gallons_of_wort
  Total_IBU = sum(IBU)
}

a <- c(0.064, 0.050)
b <- c(1.5, 1)
c <- 5
d <- 1.050
e <- c(45, 15)
f <- 6.5
g <- 25
h <- 251

# Mash and batch sparge calculation

strike_volume <- function(grains, mash_thickness){
  strike_volume <- grains * mash_thickness
} # Quarts


grain_absorption <- function(grains, retention_ratio){
  grain_absorption <- (grains * retention_ratio) * 4
} #Quarts


first_running_addition <- function(pre_boil_volume, grains, mash_thickness, step_water, retention_ratio){
  second_runnings_volume <- (pre_boil_volume * 4)/2
  strike_volume <- grains * mash_thickness
  finished_mash_volume <- strike_volume + step_water
  grain_absorption <- (grains * retention_ratio * 4)
  mash_out_volume <- (((pre_boil_volume * 4)/2) - ((finished_mash_volume) - grain_absorption))/4
} #Gallons

second_running_addition <- function(pre_boil_volume){
  second_running_addition <- ((pre_boil_volume * 4)/2)/4
} # Gallons


# Strike Water Temperature
W = (.2/R)(T2-T1)+T2

get_strike_temp <- function(mash_thickness, grain_temp, mash_temp, cooler_loss){
 (0.2/mash_thickness) * (mash_temp - grain_temp) + mash_temp + cooler_loss
  }

strike_teimp <- get_strike_temp(1.5, 72, 151, 7)



# Color Estimate

SRM_color <- function(grain_weight, grain_color, final_volume){
  MCU <- (grain_weight * grain_color)/final_volume 
  Total_MCU <- sum(MCU)
  SRM <- round(1.49922 * (Total_MCU^0.6859),0)
}

a <- c(10, 2)
b <- c(7,22)
c <- 5.5

SRM <- SRM_color(a,b,c)


# OG calculator

get_OG <- function(grain_weight, potential, brew_house_efficiency, final_volume){
  ppg <- abs(1 - potential) * 1000
  points <- grain_weight * ppg
  total_points <- round(sum(points), 0)
  efficiency_points <- (brew_house_efficiency/100) * total_points
  OG <- round(((efficiency_points/final_volume)/1000) + 1, 3)
}

OG <- get_OG(grain_weight, potential, 75, 5)

grain_weight <- c(5, 5)
potential <- c(1.027, 1.035)


# FG calculator

get_FG <- function(grain_weight, potential, brew_house_efficiency, final_volume, attenuation){
  ppg <- abs(1 - potential) * 1000
  points <- grain_weight * ppg
  total_points <- round(sum(points), 0)
  efficiency_points <- (brew_house_efficiency/100) * total_points
  ppg2 <- ((efficiency_points/final_volume))
  FG <- round((((100 - attenuation)/100) * ppg2/1000) + 1, 3)
}

FG <- get_FG(grain_weight, potential, 75, 5, 75)


# Priming Sugar Formula (grams of corn sugar)


get_priming_sugar <- function(volume_of_beer, volume_CO2, Tferm){
  PS <- round(15.195 * volume_of_beer * (volume_CO2 - 3.0378 + (0.050062 * Tferm) - (0.0002655 * (Tferm^2))), 1)
}


PS <- get_priming_sugar(5, 2.5, 65)

# Force Carbonation

get_carbonation_pressure <- function(volume_CO2, Tkeg, elevation, ABV, carbonation_height_in, fg){
  Pabsolute <- ((volume_CO2 + 0.003342)/(0.01821 + 0.090115 * exp(-(Tkeg - 32)/43.11))) - 14.7
  Paltitude <- (elevation/1000) * 0.53049
  Palcohol <- (ABV - 4.8) * 0.32
  Phead <- carbonation_height_in/28
  Pstarch <- (fg - 1.015) * 5
  Pcorrected <- round(Pabsolute + Paltitude + Palcohol + Phead + Pstarch,1)
}
  
carb <- get_carbonation_pressure(2.8, 40, 2500, 5.5, 30, 1.009)


# Grain PPG

MC <- 4.2
DBCG <- 85


get_grain_PPG <- function(DBCG, MC){
  ppg <- round(1 + (46.214 * (DBCG/100 - MC/100 - 0.002))/1000, 4)
}
  
  
grain_ppg <- get_grain_PPG(DBCG, MC)


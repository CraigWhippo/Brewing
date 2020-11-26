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

# Estimated Boil Gravity

get_boil_gravity <- function(final_volume, final_gravity, boil_volume){
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

a <- c(0.064, 0.050)
b <- c(1.5, 1)
c <- 5
d <- 1.050
e <- c(45, 15)
f <- 6.5
g <- 25
h <- 2513

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

s <- get_garetz(e, c, f, d, g, h, a, b)
s

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

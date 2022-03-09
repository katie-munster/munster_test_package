
#' Analyze temperature data for risk of heatstroke in dogs
#'
#' This function computes a level of risk associated with extreme temperature events.
#' When outdoor temperatures reach 24 degrees C (about 80 degrees F), this is an unsafe threshold
#' with the potential for heatstroke in dogs. Temperatures of 29 degrees C (about 85 degrees F) are
#' dangerous for dogs, and temperatures of 32 degrees C (about 90 degrees F) or higher are
#' life-threatening for dogs.
#'
#' @param temp daily temperature data (Celsius)
#' @param threshold_unsafe_potential temperature threshold that could potentially be unsafe for dogs (Celsius) (default 24)
#' @param threshold_dangerous temperature threshold of dangerous weather for dogs (Celsius) (default 29)
#' @param threshold_life_threatening temperature threshold that is life-threatening for dogs (Celsius) (default 32)
#' @param weight dog weight (pounds)
#' @param age dog age (years)
#' @return list of frequency of how often extremes reach over each threshold (days) and
#' risk of heatstroke based on temperature frequency, dog weight, and dog age

temp_risk = function(temp, threshold_unsafe_potential = 24, threshold_dangerous = 29, threshold_life_threatening = 32, weight, age) {
  # error checking
  if (weight < 1)
    return("Weight should be at least 1 pound")

  if (age < 0)
    return("Age should be at least 0 years")

  # determine frequency of extreme temperatures
  frequency_unsafe = 0
  frequency_dangerous = 0
  frequency_life_threatening = 0
  for (i in 1:length(temp)){
    if (temp[i] >= threshold_unsafe_potential & temp[i] < threshold_dangerous)
      frequency_unsafe = frequency_unsafe + 1
    if (temp[i] >= threshold_dangerous & temp[i] < threshold_life_threatening)
      frequency_dangerous = frequency_dangerous + 1
    if (temp[i] >= threshold_life_threatening)
      frequency_life_threatening = frequency_life_threatening + 1
  }

  # determine risk of heatstroke based on extreme temperature frequency, dog weight, and dog age
  risk = ((((frequency_unsafe/length(temp))*.1) + ((frequency_dangerous/length(temp))*.4) +
             ((frequency_life_threatening/length(temp))*.5)) + weight + (age*2))/100

  # return results as a list
  results = list(frequency_potentially_unsafe_temp = frequency_unsafe, frequency_dangerous_temp = frequency_dangerous,
                 frequency_life_threatening_temp = frequency_life_threatening, risk_of_heatstroke = risk)
  return(results)
}


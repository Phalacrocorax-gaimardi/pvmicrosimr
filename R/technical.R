
#' get_rooftop_solar_potential
#'
#' estimate the rofftop solar potential
#'
#' @param floor_area floor area in m2 from CER survey
#' @param housing_type housing type from CER survey 1= apartment, 5=bungalow, 2:4 = houses
#' @param bedrooms number of bedrooms
#' @param roof_floor_ratio area roof relative to footprint of house
#' @param usable_roof_fraction fraction of rooftop usable for solar PV
#' @param kW_per_m2 kWp per m2 (technical parameter)
#'
#' @return potential installed solar capacity kWp.
#' @export
#'
#' @examples
get_rooftop_solar_potential <- function(floor_area,housing_type,bedrooms,roof_floor_ratio = 1.3,usable_roof_fraction = 0.55,kW_per_m2 = 0.15){

  if(is.na(floor_area)){

    fit <- lm(floor_area~housing_type+bedrooms,cer_survey)
    floor_area <- predict(fit,tibble::tibble(bedrooms=bedrooms,housing_type=housing_type))
  }

  #roof_floor_ratio <- 1.3

  roof_area <- dplyr::case_when(housing_type == 1~0, #apartments assumed to have zero rooftop
                         housing_type %in% c(2:4)~roof_floor_ratio*floor_area/2, #assume 2 equal area storey
                         housing_type ==5~roof_floor_ratio*floor_area
  )

  #usable_roof_fraction <- 0.5

  usable_roof_area <- usable_roof_fraction*roof_area

  #kW_per_m2 <- 0.15 #SEAI 150Wp/m2

  return(usable_roof_area*kW_per_m2)
}

#cer <- cer_survey %>% rowwise() %>% mutate(rooftop_potential = get_rooftop_solar_potential(housing_type,bedrooms,floor_area))

#test %>% ggplot(aes(rooftop_potential)) + geom_histogram() #looks lognormal


pv_system_efficiency <- function(sD,yeartime){

  shockley_quisser <- 0.3316
  inverter_eff <- 0.85
  efficiency_2015 <- 0.16
  eta <- 0.5
  lambda <- (shockley_quisser*inverter_eff/efficiency_2015-1)*(1/(2015-1990))^(-eta)

shockley_quisser*inverter_eff/(1+lambda*(yeartime-1990)^(-eta))

}

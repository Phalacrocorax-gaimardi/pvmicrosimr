
#' get_rooftop_solar_potential
#'
#' generate a rooftop solar capacity potential stochastically (log normal distribution). This is calibrated to BER data up to 2023 separately for
#' bungalows and two-storey dwellings. Bungalows have a higher solar potential because of their relatively larger footprint. A stochastic shading factor is included via a shading_factor parameter.
#'
#' @param housing_type housing type is "bungalow" or "two-storey"
#' @param demand annual electricity demand in kWh
#' @param roof_floor_ratio 2/sqrt(3) for 30 degree pitch
#' @param usable_roof_fraction fraction of rooftop usable for solar PV default 0.8
#' @param kWp_per_m2 kWp per m2 (technical parameter) default 0.15
#' @param shading_factor mean capacity factor reduction factor due to shading
#'
#' @return potential installed solar capacity kWp on half-roof
#' @export
#'
#' @examples
get_rooftop_solar_potential <- function(housing_type, demand,roof_floor_ratio = 2/sqrt(3),
                                        usable_roof_fraction = 0.85, kWp_per_m2 = 0.15, shading_factor=0.85){
  #assume areas are lognormally distributed
  #parameters from BER/ demand-area correlation from CER smart meter
  ceiling_area <- dplyr::case_when(housing_type=="bungalow"~rlnorm(1,4.56+0.000033*(demand-5000),0.456),
                            housing_type!="bungalow"~rlnorm(1,4.19 + 0.000033*(demand-5000),0.485)) #bigger houses have bigger electricity use
  kW <- kWp_per_m2*ceiling_area*roof_floor_ratio*usable_roof_fraction/2 #half-roof area

  return(kW*rbeta(1,shading_factor/(1-shading_factor),1)) #Beta function B(x,1) capacity factor shading model

}

#' get_rooftop_solar_area
#'
#' generate a haf rooftop solar capacity area potential stochastically (log normal distribution). This is calibrated to BER data up to 2023 separately for
#' bungalows and two-storey dwellings. Bungalows have a higher solar potential because of their relatively larger footprint.
#'
#' @param housing_type housing type is "bungalow" or "two-storey"
#' @param demand annual electricity demand in kWh
#' @param roof_floor_ratio 2/sqrt(3) for 30 degree pitch
#' @param usable_roof_fraction fraction of rooftop usable for solar PV default 0.8
#'
#' @return potential half-roof area for solar
#' @export
#'
#' @examples
get_rooftop_solar_area <- function(housing_type, demand,roof_floor_ratio = 2/sqrt(3),
                                        usable_roof_fraction = 0.85){
  #assume areas are lognormally distributed
  #parameters from BER/ demand-area correlation from CER smart meter
  ceiling_area <- dplyr::case_when(housing_type=="bungalow"~rlnorm(1,4.56+0.000033*(demand-5000),0.456),
                                   housing_type!="bungalow"~rlnorm(1,4.19 + 0.000033*(demand-5000),0.485)) #bigger houses have bigger electricity use
  area <- ceiling_area*roof_floor_ratio*usable_roof_fraction/2 #half-roof area
  return(area) #Beta function B(x,1) capacity factor shading model

}


#cer <- cer_survey %>% rowwise() %>% mutate(rooftop_potential = get_rooftop_solar_potential(housing_type,bedrooms,floor_area))

#test %>% ggplot(aes(rooftop_potential)) + geom_histogram() #looks lognormal



#' is_bungalow
#'
#' Is this dwelling a bungalow or not based on urban/rural (qc1) and region? Consistent with overall bungalow share of 20% and lower number of bungalows in urban areas
#' in BER data.
#'
#' @param qc1 qc1=1 rural qc1=2 urban
#' @param region Dublin, Leinster, Munster, Connulster
#'
#' @return "bungalow" or "two-storey"
#' @export
#'
#' @examples
is_bungalow <- function(qc1,region){
  #probabilities from BER
  #inference using naive Bayes is a very bad idea qc1 & region not conditionally independent
  prob_b <- dplyr::case_when((region==1 & qc1==1)~0.25,(region==1 & qc1==2)~0.1,
                      (region==2 & qc1==1)~0.26,(region==2 & qc1==2)~0.15,
                      (region==3 & qc1==1)~0.3,(region==3 & qc1==2)~0.15,
                      (region==4 & qc1==1)~0.4,(region==4 & qc1==2)~0.15)
  return(sample(c("bungalow","two-storey"),size=1,prob=c(prob_b,1-prob_b)))
}



pv_system_efficiency <- function(sD,yeartime){

  shockley_quisser <- 0.3316
  inverter_eff <- 0.85
  efficiency_2015 <- 0.16
  eta <- 0.5
  lambda <- (shockley_quisser*inverter_eff/efficiency_2015-1)*(1/(2015-1990))^(-eta)

shockley_quisser*inverter_eff/(1+lambda*(yeartime-1990)^(-eta))

}

#' kWp_per_m2_fun
#'
#' solar panel efficiency (peak power per m2) at time yeartime
#'
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return  kWp per m2
#' @export
#'
#' @examples
kWp_per_m2_fun <- function(sD,yeartime){

  value_2015 <- sD %>% dplyr::filter(parameter=="kWp_per_m2_2015") %>% dplyr::pull(value)
  value_2023 <- sD %>% dplyr::filter(parameter=="kWp_per_m2_2023") %>% dplyr::pull(value)
  value_2030 <- sD %>% dplyr::filter(parameter=="kWp_per_m2_2030") %>% dplyr::pull(value)
  value_2050 <- sD %>% dplyr::filter(parameter=="kWp_per_m2_2050") %>% dplyr::pull(value)
  value <- approx(x=c(2015.5,2023.5,2030.5,2050.5), y=c(value_2015,value_2023,value_2030,value_2050),xout=yeartime,rule=2)$y
  return(value)
}


#' acceleration_fun
#'
#' accelerated solar PV "ideation" mutiplier for agents. The 2010-2022 calibrated rate p. is replaced p. * acceleration_fun to allow for the impact of advertisinga campaigns etc.
#' where acceleration = 1 for yeartime < 2023.
#'
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return
#' @export
#'
#' @examples
acceleration_fun <- function(sD,yeartime){

  value_2025 <- sD %>% dplyr::filter(parameter=="acceleration_factor_2025") %>% dplyr::pull(value)
  value_2030 <- sD %>% dplyr::filter(parameter=="acceleration_factor_2030") %>% dplyr::pull(value)
  value <- approx(x=c(2022.5,2025.5,2030.5), y=c(1,value_2025,value_2030),xout=yeartime,rule=2)$y
  return(value)
}



#' get_shaded_sys
#'
#' estimated effect on annual energy imports and exports computed from a cer_sys dataframe
#'
#' @param cer_sys cer_system data (solar1 solar2 battery imports exports) with shading parameters for roof 1 & 2
#'
#' @return cer_system data with imports and exports including effect of shading
#' @export
#'
#' @examples
get_shaded_sys <- function(cer_sys){

  #returns a table of imports and exports for a shaded roof
  sol_vals <- cer_sys$solar1 %>% unique() %>% sort()
  cer_sys <- cer_sys %>% dplyr::mutate(solar1_eff=shading1*solar1,solar2_eff=shading2*solar2)

  cer_sys <- cer_sys %>% dplyr::group_by(housecode,battery) %>% dplyr::mutate(imports = pracma::interp2(x=sol_vals,
                                                                                    y=sol_vals,
                                                                                    Z=matrix(imports,length(sol_vals),length(sol_vals)),
                                                                                    xp=solar1_eff,
                                                                                    yp=solar2_eff, method="linear"),
                                                              exports = pracma::interp2(x=sol_vals,
                                                                                    y=sol_vals,
                                                                                    Z=matrix(exports,length(sol_vals),length(sol_vals)),
                                                                                    xp=solar1_eff,
                                                                                    yp=solar2_eff, method="linear"))

 return(cer_sys %>% dplyr::select(-solar1_eff,-solar2_eff))

}

#' roundr
#'
#' stochastic round
#'
#' @param x real number to be rounded up or down
#'
#' @return integer
#' @export
#'
#' @examples
roundr <- function(x){
  x1 <- trunc(x)
  weights = c(1+x1-x,x-x1)
  return(sample(c(x1,x1+1),size=1,prob=weights))
  }

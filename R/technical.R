
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
                                        usable_roof_fraction = 0.8, kWp_per_m2 = 0.15, shading_factor=0.85){
  #assume areas are lognormally distributed
  #parameters from BER
  ceiling_area <- dplyr::case_when(housing_type=="bungalow"~rlnorm(1,4.56+0.000033*(demand-5000),0.456),
                            housing_type!="bungalow"~rlnorm(1,4.19 + 0.000033*(demand-5000),0.485)) #bigger houses have bigger electricity use
  kW <- kWp_per_m2*ceiling_area*roof_floor_ratio*usable_roof_fraction/2 #half-roof area

  return(kW*rbeta(1,shading_factor/(1-shading_factor),1)) #Beta function B(x,1) capacity factor shading model

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



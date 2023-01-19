######################
# CER data & modelling
######################

#cer_systems<- read_csv("~/Policy/AgentBasedModels/solarPV/cer_system_data.csv")
#cer_survey <- read_csv("~/Policy/AgentBasedModels/solarPV/IrelandData/cer_survey_reduced/cer_survey.csv")
#cer_survey$floor_area_unit <- NULL
#cer_survey$housing_year <- NULL
#cer_questions <- read_csv("~/Policy/AgentBasedModels/solarPV/IrelandData/cer_survey_reduced/cer_questions.csv")
#cer_qanda <- read_csv("~/Policy/AgentBasedModels/solarPV/IrelandData/cer_survey_reduced/cer_qanda.csv")
#pv_qanda <- read_csv("~/Policy/AgentBasedModels/solarPV/Survey/pv_qanda.csv")
#pv_questions <- read_csv("~/Policy/AgentBasedModels/solarPV/Survey/.csv")
#pv_survey <- readr::read_csv("~/Policy/AgentBasedModels/solarPV/Survey/pv_data.csv")
#pv_survey_oo <- pv_survey %>% dplyr::filter(q1 %in% 2:5,q3 %in% 1:2)
#pv_survey_oo <- readr::read_csv("~/Policy/AgentBasedModels/solarPV/pv_survey_oo_filled.csv")
#pv_questions <- pv_qanda %>% select(code,question) %>% distinct()
#pv_society <- readr::read_csv("~/Policy/AgentBasedModels/solarPV/Survey/society.csv")
#pv_society_oo <- readr::read_csv("~/Policy/AgentBasedModels/solarPV/Survey/pv_society_oo.csv")

#scenario_wem <- readxl::read_xlsx("~/Policy/AgentBasedModels/solarPV/scenario_parameters.xlsx", sheet="scenario_WEM")
#empirical_utils <- readr::read_csv("~/Policy/AgentBasedModels/solarPV/empirical_utils.csv")
#agents_init <- readr::read_csv("~/Policy/AgentBasedModels/solarPV/agents_init.csv")
#survey_raw <- readr::read_csv("~/Policy/AgentBasedModels/Survey/ESB Final data +LCA.csv")
#seai_elec <- read_csv("~/Policy/AgentBasedModels/solarPV/IrelandData/electricity_household_demand_price_SEAI.csv")

#seai_elec %>% filter(year==2018)
#owner-occupier bills
#
#bills_oo <- survey_raw %>% dplyr::filter(q1 %in% 2:5,q3 %in% 1:2) %>% dplyr::pull(q9_1) %>% dplyr::na_if(9999)
#bills_all <- survey_raw %>% pull(q9_1) %>% na_if(9999)


#' get_kWh_from_bills
#'
#' converts reported bi-monthly electricity bill in 2018 survey to an annual kWh consumption. Ensures that mean demand matches SEAI estimate using default seasonality factor mu=8.52
#'
#' @param b reported bill
#' @param mu kWh bill adjustment factor (seasonality). No seasonality corresponds to mu=6.
#'
#' @return kWh demand
#' @export
#'
#' @examples
get_kWh_from_bills <- function(b,mu=8.53){

  e_price <- seai_elec %>% dplyr::filter(year==2018) %>% dplyr::pull(price)
  e_demand <- seai_elec %>% dplyr::filter(year==2018) %>% dplyr::pull(kWh)
  kWh <- mu*100*(b-cost_params$standing_charge/6)/e_price
  kWh[kWh < 0] <- 0
  return(kWh)

}




#' map_survey_to_cer
#'
#' mapping of pv survey pwner-occupier data to CER housecodes based on matching annual demand. Higher lambda matches more strictly.
#' Lower lambda allows more randomness in mapping. pv_survey demand data is re-scaled to match historical mean household demand estimates
#' Apartments are excluded.
#'
#'
#' @param params parameter values for mapping (e_demand_factor)
#' @param lambda strictness parameter
#'
#' @return
#' @export
#'
#' @examples
map_survey_to_cer <-function(params,lambda=2){

  #survey_raw <- read_csv("~/Policy/AgentBasedModels/Survey/ESB Final data +LCA.csv")
  #bills <- survey_raw$q9_1 %>% na_if(9999)
  pv_survey1 <- pv_survey
  pv_survey1$bill <- bills$q9_1
  #fill in missing bills
  #replace by median bill in category
  pv_survey1 <- pv_survey1 %>% dplyr::mutate(bill=dplyr::na_if(bill,9999))
  pv_survey1 <- pv_survey1 %>% dplyr::mutate(bill=dplyr::na_if(bill,0))
  pv_survey1 <- pv_survey1 %>% dplyr::group_by(q1,qk) %>% dplyr::mutate(bill=ifelse(is.na(bill), median(bill,na.rm=T),bill))
  pv_survey1 <- pv_survey1 %>% dplyr::group_by(q1) %>% dplyr::mutate(bill=ifelse(is.na(bill), median(bill,na.rm=T),bill))
  pv_survey1 <- pv_survey1 %>% dplyr::group_by(qk) %>% dplyr::mutate(bill=ifelse(is.na(bill), median(bill,na.rm=T),bill))
  pv_survey1 <- pv_survey1 %>% dplyr::ungroup()
  #slightly adjust mu from default that excludes NAs
  pv_survey1$demand <- get_kWh_from_bills(pv_survey1$bill,mu=8.37)
  #scale survey demand to reflect annual variability
  f <- seai_elec %>% dplyr::filter(year==2018) %>% dplyr::pull(kWh)/seai_elec %>% dplyr::filter(year==2010) %>% dplyr::pull(kWh)
  pv_survey1$demand <- pv_survey1$demand*params$e_demand_factor/f

  cer_match0  <- function(demand_1,lam){
    #matches stochastically to demand
    #demand_1 is pv_survey demand inferred from latest bi-monthly bills
    #
    cer_survey_reduced0 <- cer_survey %>% dplyr::filter(housing_type != 1) %>% dplyr::select(housecode,demand)

    if(is.na(demand_1)) res <- cer_survey_reduced0 %>% dplyr::slice_sample(n=1) %>% dplyr::pull(housecode)

    if(!is.na(demand_1)) {wts <- exp(-lam*abs(demand_1-cer_survey_reduced0$demand)/1000)
    if(sum(wts)==0) wts[which.max(cer_survey_reduced0$demand)] <- 1
    res <- cer_survey_reduced0 %>% dplyr::slice_sample(n=1, weight_by = wts) %>% dplyr::pull(housecode)}
    return(res)
  }

  pv_survey1 <- pv_survey1 %>% dplyr::rowwise() %>% dplyr::mutate(housecode = cer_match0(demand,lambda))
  #
  test <- cer_survey %>% dplyr::select(housecode,demand) %>% dplyr::rename("cer_demand"=demand)
  pv_survey1 <- pv_survey1 %>% dplyr::inner_join(test) %>% dplyr::ungroup()
  pv_survey1 <- pv_survey1 %>% dplyr::filter(q1 %in% 2:5,q3 %in% 1:2)
  return(pv_survey1)

}

#map_survey_to_cer(1) %>% ggplot(aes(cer_demand,demand)) + geom_point()

#' get_cer_sys_optimal
#'
#' returns the optimally sized solar/battery system based on financial utility (NPV_0-NPV)/NPV_0 with constrained rooftop capacity (using get_rooftop_solar_potential()).
#'
#' @param params cost parameter environment object returned by fast_
#'
#' @return a dataframe
#' @export
#'
#' @examples
get_cer_sys_optimal <- function(params){
  #pv rooftop capacity constrained finacial utilities corresponding to costs in params
  cer_survey1 <- cer_survey %>% dplyr::rowwise() %>% dplyr::mutate(rooftop_capacity = get_rooftop_solar_potential(floor_area,housing_type,bedrooms))
  #missing rooftop capacity values replaced with mean for housing_type
  roof <- cer_survey1 %>% dplyr::group_by(housing_type) %>% dplyr::mutate(rooftop_capacity = ifelse(is.na(rooftop_capacity), mean(rooftop_capacity,na.rm=T),rooftop_capacity))
  roof <- roof %>% dplyr::ungroup() %>% dplyr::select(housecode,rooftop_capacity)

  cer_sys <- cer_systems %>% dplyr::inner_join(roof) %>% dplyr::filter(solar_capacity < rooftop_capacity)
  cer_sys <- cer_sys %>% dplyr::mutate(du=get_sys_util_0(params,demand,imports,exports,solar_capacity,battery_capacity))

  cer_sys_opt <- cer_sys %>% dplyr::group_by(housecode) %>% dplyr::filter(du==max(du))
  return(cer_sys_opt)
}


#df <- tibble::tibble()
#for(year in 2010:2030){
#  print(year)
#  params <- scenario_params_df(scenario_0,year+0.5) %>% fast_params
#  test <- get_cer_sys_optimal(params)
#  test <- test %>% dplyr::mutate(sf=get_self_sufficiency(imports,demand),sc=get_self_consumption(solar_capacity,exports))
#  df <- dplyr::bind_rows(df, tibble::tibble(year=year,
#                                            pv_mean = mean(test$solar_capacity),
#                                            bat_mean=mean(test$battery_capacity),
#                                            sf=mean(test$sf),
#                                           sc=mean(test$sc)))
#}

#' get_cer_sys_utils
#'
#' financial utility (NPV0-NPV)/NPV0 calculated for each combination of solar and battery capacity
#'
#' @param params parameter set (output of fast_params)
#'
#' @return dataframe with utility column du
#' @export
#'
#' @examples
get_cer_sys_utils <- function(params){
  #pv rooftop capacity constrained finacial utilities corresponding to costs in params
  cer_survey1 <- cer_survey %>% dplyr::rowwise() %>% dplyr::mutate(rooftop_capacity = get_rooftop_solar_potential(floor_area,housing_type,bedrooms))
  #missing rooftop capacity values replaced with mean for housing_type
  roof <- cer_survey1 %>% dplyr::group_by(housing_type) %>% dplyr::mutate(rooftop_capacity = ifelse(is.na(rooftop_capacity), mean(rooftop_capacity,na.rm=T),rooftop_capacity))
  roof <- roof %>% dplyr::ungroup() %>% dplyr::select(housecode,rooftop_capacity)

  cer_sys <- cer_systems %>% dplyr::inner_join(roof) %>% dplyr::filter(solar_capacity < rooftop_capacity)
  cer_sys <- cer_sys %>% dplyr::mutate(du=get_sys_util_0(params,demand,imports,exports,solar_capacity,battery_capacity))
  return(cer_sys)
}


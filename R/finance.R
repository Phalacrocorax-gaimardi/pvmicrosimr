#sD <- readxl::read_xlsx("~/Policy/AgentBasedModels/solarPV/scenario_parameters.xlsx",sheet=1)


#' amort
#'
#' amortisation or annuity payment for unit principal
#'
#' @param r interest rate, decimal
#' @param term term of loan or investment
#'
#' @return real value
#' @export
#'
#' @examples
amort <- function(r,term){
  #amortisation payment (annuity)
  res <- dplyr::case_when(r != 0~(r*(1+r)^term)/((1+r)^term-1),
                   r == 0~1/term)
  return(res)
}

#' geo_sum
#'
#' sum of a^i from i=1 to i=term
#'
#' @param a value
#' @param term sum over 1..term
#'
#' @return real value
#' @export
#'
#' @examples
geo_sum <- function(a, term){
  #the sum of a^i from i=1 to i=term
  res <- dplyr::case_when(a != 1~a*(a^term-1)/(a-1),
                   a ==1~term)
  return(res)
  }


#' seai_grant
#'
#' seai grant for domestic solar installation 2022
#'
#' @param params fast scenario parameters
#' @param s solar capacity in kW
#' @param b battery capacity in kWh
#'
#' @return grant amount in euros
#' @export
#'
#' @examples
seai_grant <- function(params,s,b){

  sol_lower_threshold <- params$sol_lower_threshold
  sol_upper_threshold <- params$sol_upper_threshold
  sol_lower_grant <- params$sol_lower_grant
  sol_upper_grant <- params$sol_upper_grant
  bat_threshold <- params$bat_threshold
  bat_grant <- params$bat_grant

  max_sol_grant <- sol_lower_threshold*sol_lower_grant + (sol_upper_threshold-sol_lower_threshold)*sol_upper_grant

  grant <- dplyr::case_when(s <= sol_lower_threshold~sol_lower_grant*s,
            s >= sol_upper_threshold & b < bat_threshold~max_sol_grant,
            s >=sol_upper_threshold & b >= bat_threshold~max_sol_grant + bat_grant,
            s > sol_lower_threshold & s < sol_upper_threshold~sol_lower_threshold*sol_lower_grant+(s-sol_lower_threshold)*sol_upper_grant)

  grant <- dplyr::case_when(params$yeartime >= 2018.5~grant,params$yeartime < 2018.5~0)
  return(grant)
}


cost_params <- tibble::tibble(
  install_cost_pv = 3000, #euros
  install_cost_bat = 2000,
  pv_cost_per_kW = 860, #per kW
  battery_cost_per_kWh = 400, #per kWh
  grid_cost_per_kWh = 0.30, #euros/kWh
  standing_charge = 100, #euros p.a.
  ceg_price_per_kWh = 0.0, #clean energy guarantee for domestic users
  ceg_tax_threshold = 200, #euros tax free threshold
  cep_price_per_kWh = 0.135, #clean export premium for non-domestic > 6.1kW
  cep_generation_cap = 0.8, #volume cap
  mss_kW_cap = 50, #kWp system
  discount_rate = 0.08,
  interest_rate = 0.05,
  term_of_loan = 10,#loan interest rate
  electricity_inflation_rate = 0.04, #i
  system_lifetime = 20, #years
  marginal_tax_rate = 0.4 #assume
)

cost_params_long <- cost_params %>% tidyr::pivot_longer(cols=everything(),names_to="parameter")

cost_params_2018 <- tibble::tibble(
  install_cost_pv = 1300, #euros
  install_cost_bat = 1000,
  pv_cost_per_kW = 1300, #per kW
  battery_cost_per_kWh = 1000, #per kWh
  grid_cost_per_kWh = 0.20, #euros/kWh
  standing_charge = 192, #euros p.a. including pso levy
  ceg_price_per_kWh = 0.0, #clean energy guarantee for domestic users
  ceg_tax_threshold = 200, #euros tax free threshold
  cep_price_per_kWh = 0.135, #clean export premium for non-domestic > 6.1kW
  cep_generation_cap = 0.8, #volume cap
  mss_kW_cap = 50, #kWp system
  discount_rate = 0.08,
  interest_rate = 0.05,
  term_of_loan = 10,#loan interest rate
  electricity_inflation_rate = 0.04, #i
  system_lifetime = 20, #years
  marginal_tax_rate = 0.4 #assume
)

cost_params_2018_long <- cost_params_2018 %>% tidyr::pivot_longer(cols=everything(),names_to="parameter")


#' get_sys_npv
#'
#' returns the npv of power bills and financing on a domestic solar + battery system
#'
#' @param params cost parameter long-format dataframe
#' @param demand annual power demand in kWh e.g. 5000 kWh
#' @param imports annual grid imports in kWh
#' @param exports annual exports to grid in kWh
#' @param solar_capacity installed solar PV capacity in kW
#' @param battery_capacity installed battery capacity in kWh
#' @param include_grant include SEAI grant T/F
#' @param customer "domestic" only at the moment
#'
#' @return dataframe with columns "capital cost", "amortisation","annual_bill","bill_saving","MSS_revenue","npv_loan_period"
#' @export
#'
#' @examples
get_sys_npv <- function(params,demand,imports,exports,solar_capacity,battery_capacity, include_grant=T, customer = "domestic"){
  #npv with cost parameters and term provided in cost_params
  #paymnents are positive, revenue is negative
  install_cost_pv <- dplyr::case_when(solar_capacity > 0~params$install_cost_pv,
                               solar_capacity==0~0)
  install_cost_bat <- dplyr::case_when(battery_capacity > 0~params$install_cost_bat,
                                battery_capacity==0~0)

  install_cost <- install_cost_pv + install_cost_bat
  pv_cost_per_kW <- params$pv_cost_per_kW
  battery_cost_per_kWh <- params$battery_cost_per_kWh
  grid_cost_per_kWh <- params$grid_cost_per_kWh
  standing_charge <- params$standing_charge
  #prorated mss for systems larger than 50kW
  #if(solar_scale > 6 & customer == "non-domestic"){
  # tariff <- ifelse(solar_scale <= cost_params$mss_kW_cap,cost_params$cep_price_per_kWh,cost_params$cep_price_per_kWh*50/solar_scale)
  # exports_cap <- solar_scale*0.1*365*24*cost_params$cep_generation_cap
  #}
  #tariff <- case_when(customer == "domestic"~cost_params$ceg_price_per_kWh,
  #                    (customer == "non-domestic" & solar_capacity = cost_params$mss_kW_cap & solar_capacity > 6)~cost_params$cep_price_per_kWh,
  #                   (customer == "non-domestic" & solar_capacity > cost_params$mss_kW_cap)~cost_params$cep_price_per_kWh*50/solar_capacity)
  #exports_cap <- case_when((customer == "non-domestic" & solar_scale > 6)~solar_scale*0.1*365*24*cost_params$cep_generation_cap)

  feedin_tariff <- params$ceg_price_per_kWh
  discount_rate <- params$discount_rate
  interest_rate <- params$interest_rate
  term_of_loan <-  params$term_of_loan
  term_of_loan <- dplyr::case_when((solar_capacity==0 & battery_capacity==0)~0,
                            (solar_capacity>0 || battery_capacity>0)~term_of_loan)
  electricity_inflation_rate <- params$electricity_inflation_rate
  system_lifetime <- params$system_lifetime
  grant <- dplyr::case_when(include_grant~seai_grant(solar_capacity,battery_capacity),
                     !include_grant~0)
  #first three year bill savings
  capital_cost <- install_cost + pv_cost_per_kW*solar_capacity + battery_cost_per_kWh*battery_capacity-grant
  #print(paste("capital cost",capital_cost))
  amort_payment <- amort(interest_rate,term_of_loan)*(install_cost+pv_cost_per_kW*solar_capacity + battery_cost_per_kWh*battery_capacity-grant)
  #print(paste("annual loan payment",amort_payment))
  first_year_bill0 <- grid_cost_per_kWh*demand + standing_charge
  first_year_bill <- grid_cost_per_kWh*imports + standing_charge
  first_year_mss_revenue0 <- feedin_tariff*exports #gross mss model (elec exports)
  first_year_mss_revenue <- dplyr::case_when((first_year_mss_revenue0 < 200)~first_year_mss_revenue0,
                                      (first_year_mss_revenue0 >= 200)~first_year_mss_revenue0*(1-params$marginal_tax_rate)+params$marginal_tax_rate*params$ceg_tax_threshold)

  first_year_mss_revenue <- dplyr::case_when((first_year_mss_revenue0 < 200)~first_year_mss_revenue0,
                                             (first_year_mss_revenue0 >= 200)~2)

  #outgoings are +ve revenue is -ve
  npv_loan_period <- geo_sum((1+electricity_inflation_rate)/(1+discount_rate),term_of_loan)*(first_year_bill-first_year_mss_revenue)+geo_sum(1/(1+discount_rate),term_of_loan)*amort_payment
  npv_noloan_period <- ((1+electricity_inflation_rate)/(1+discount_rate))^term_of_loan*geo_sum((1+electricity_inflation_rate)/(1+discount_rate),system_lifetime-term_of_loan)*(first_year_bill-first_year_mss_revenue)
  npv_noinvest <- geo_sum((1+electricity_inflation_rate)/(1+discount_rate),system_lifetime)*(first_year_bill0)

  #print(paste("first year cost savings",first_year_cost_savings))
  npv <- dplyr::case_when(term_of_loan==0~0,
                   term_of_loan > 0~npv_loan_period) + npv_noloan_period
  #print(npv)
  df <- tibble::tibble(capital_cost=capital_cost,
               amort_payment=amort_payment,
               annual_bill = first_year_bill0,
               annual_saving=first_year_bill0-first_year_bill,
               annual_mss_revenue = first_year_mss_revenue,
               npv_loan_period=npv_loan_period,
               npv_noloan_period=npv_noloan_period,
               npv=npv,
               npv_noinvest=npv_noinvest,
               d_npv = npv-npv_noinvest
  )
  return(df)
}


#' get_sys_npv_0
#'
#' same as get_sys_npv but returns a scalar only (npv)
#'
#' @param params cost parameter long-format dataframe
#' @param demand annual power demand in kWh e.g. 5000 kWh
#' @param imports annual grid imports in kWh
#' @param exports annual exports to grid in kWh
#' @param solar_capacity installed solar PV capacity in kW
#' @param battery_capacity installed battery capacity in kWh
#' @param include_grant include SEAI grant T/F
#' @param customer "domestic" only at the moment

#'
#' @return npv in euros
#' @export
#'
#' @examples
get_sys_npv_0 <- function(params,demand,imports,exports,solar_capacity,battery_capacity,include_grant=T, customer = "domestic"){
  #npv with cost parameters and term provided in cost_params
  #paymnents are positive, revenue is negative
  install_cost_pv <- dplyr::case_when(solar_capacity > 0~params$pv_install_cost,
                               solar_capacity==0~0)
  install_cost_bat <- dplyr::case_when(battery_capacity > 0~params$battery_install_cost,
                                battery_capacity==0~0)

  install_cost <- install_cost_pv + install_cost_bat
  pv_cost_per_kW <- params$pv_cost
  battery_cost_per_kWh <- params$battery_cost
  grid_cost_per_kWh <- params$e_price
  standing_charge <- params$standing_charge
  #prorated mss for systems larger than 50kW
  #if(solar_scale > 6 & customer == "non-domestic"){
  # tariff <- ifelse(solar_scale <= cost_params$mss_kW_cap,cost_params$cep_price_per_kWh,cost_params$cep_price_per_kWh*50/solar_scale)
  # exports_cap <- solar_scale*0.1*365*24*cost_params$cep_generation_cap
  #}
  #tariff <- case_when(customer == "domestic"~cost_params$ceg_price_per_kWh,
  #                    (customer == "non-domestic" & solar_capacity = cost_params$mss_kW_cap & solar_capacity > 6)~cost_params$cep_price_per_kWh,
  #                   (customer == "non-domestic" & solar_capacity > cost_params$mss_kW_cap)~cost_params$cep_price_per_kWh*50/solar_capacity)
  #exports_cap <- case_when((customer == "non-domestic" & solar_scale > 6)~solar_scale*0.1*365*24*cost_params$cep_generation_cap)

  feedin_tariff <- params$ceg
  discount_rate <- params$discount_rate
  interest_rate <- params$finance_rate
  term_of_loan <-  params$term_of_loan
  term_of_loan <- dplyr::case_when((solar_capacity==0 & battery_capacity==0)~0,
                            (solar_capacity>0 || battery_capacity>0)~term_of_loan)
  electricity_inflation_rate <- params$e_price_inflation
  system_lifetime <- params$system_lifetime
  grant <- dplyr::case_when(include_grant~seai_grant(params,solar_capacity,battery_capacity),
                     !include_grant~0)
  #first three year bill savings
  capital_cost <- install_cost + pv_cost_per_kW*solar_capacity + battery_cost_per_kWh*battery_capacity-grant
  #print(paste("capital cost",capital_cost))
  amort_payment <- amort(interest_rate,term_of_loan)*(install_cost+pv_cost_per_kW*solar_capacity + battery_cost_per_kWh*battery_capacity-grant)
  #print(paste("annual loan payment",amort_payment))
  first_year_bill0 <- grid_cost_per_kWh*demand + standing_charge
  first_year_bill <- grid_cost_per_kWh*imports + standing_charge
  first_year_mss_revenue_gross <- feedin_tariff*exports #add mss model (elec exports)
  first_year_mss_revenue_net <- dplyr::case_when(first_year_mss_revenue0 < 200~first_year_mss_revenue0,
                                      first_year_mss_revenue0 >= 200~first_year_mss_revenue0*(1-params$marginal_tax_rate)+params$marginal_tax_rate*params$ceg_tax_threshold)
  #outgoings are +ve revenue is -ve
  npv_loan_period <- geo_sum((1+electricity_inflation_rate)/(1+discount_rate),term_of_loan)*(first_year_bill-first_year_mss_revenue_net)+geo_sum(1/(1+discount_rate),term_of_loan)*amort_payment
  npv_noloan_period <- ((1+electricity_inflation_rate)/(1+discount_rate))^term_of_loan*geo_sum((1+electricity_inflation_rate)/(1+discount_rate),system_lifetime-term_of_loan)*(first_year_bill-first_year_mss_revenue_net)
  npv_noinvest <- geo_sum((1+electricity_inflation_rate)/(1+discount_rate),system_lifetime)*(first_year_bill0)

  #print(paste("first year cost savings",first_year_cost_savings))
  npv <- dplyr::case_when(term_of_loan==0~0,
                   term_of_loan > 0~npv_loan_period) + npv_noloan_period

  return(npv)
}

get_sys_npv_0 <- Vectorize(get_sys_npv_0,c("demand","imports","exports","solar_capacity","battery_capacity"))


#' get_sys_util_0
#'
#' the financial utility of a household solar PV system. This is defined as (NPV_0-NPV)/NPV_0 where NPV_0 is the net present value
#' of expected future bills (including inflation expectations, discounted out to a term) and NPV is the net present value of all future cashflows
#' when a solar PV investment is made (amortisation, feedin tariff revenues, reduced electricity bills, tax)
#'
#' @param params cost parameter long-format dataframe
#' @param demand kWh
#' @param imports kWh
#' @param exports kWh
#' @param solar_capacity 0..50 in steps of 0.5kW
#' @param battery_capacity 0..100 in steps of 2.5 kWh
#' @param include_grant True or False
#' @param customer "domestic" is only option at the moment
#'
#' @return scalar financial partial utility
#' @export
#'
#' @examples
get_sys_util_0 <- function(params,demand,imports,exports,solar_capacity,battery_capacity,include_grant=T, customer = "domestic"){
  #npv with cost parameters and term provided in cost_params
  #paymnents are positive, revenue is negative
  install_cost_pv <- dplyr::case_when(solar_capacity > 0~params$pv_install_cost,
                               solar_capacity==0~0)
  install_cost_bat <- dplyr::case_when(battery_capacity > 0~params$battery_install_cost,
                                battery_capacity==0~0)

  install_cost <- install_cost_pv + install_cost_bat
  pv_cost_per_kW <- params$pv_cost
  battery_cost_per_kWh <- params$battery_cost
  grid_cost_per_kWh <- params$e_price
  standing_charge <- params$standing_charge
  #prorated mss for systems larger than 50kW
  #if(solar_scale > 6 & customer == "non-domestic"){
  # tariff <- ifelse(solar_scale <= cost_params$mss_kW_cap,cost_params$cep_price_per_kWh,cost_params$cep_price_per_kWh*50/solar_scale)
  # exports_cap <- solar_scale*0.1*365*24*cost_params$cep_generation_cap
  #}
  #tariff <- case_when(customer == "domestic"~cost_params$ceg_price_per_kWh,
  #                    (customer == "non-domestic" & solar_capacity = cost_params$mss_kW_cap & solar_capacity > 6)~cost_params$cep_price_per_kWh,
  #                   (customer == "non-domestic" & solar_capacity > cost_params$mss_kW_cap)~cost_params$cep_price_per_kWh*50/solar_capacity)
  #exports_cap <- case_when((customer == "non-domestic" & solar_scale > 6)~solar_scale*0.1*365*24*cost_params$cep_generation_cap)

  feedin_tariff <- params$ceg
  discount_rate <- params$discount_rate
  interest_rate <- params$finance_rate
  term_of_loan <-  params$term_of_loan
  term_of_loan <- dplyr::case_when((solar_capacity==0 & battery_capacity==0)~0,
                            (solar_capacity>0 || battery_capacity>0)~term_of_loan)
  electricity_inflation_rate <- params$e_price_inflation
  system_lifetime <- params$system_lifetime
  grant <- dplyr::case_when(include_grant~seai_grant(params,solar_capacity,battery_capacity),
                     !include_grant~0)
#first three year bill savings
  capital_cost <- install_cost + pv_cost_per_kW*solar_capacity + battery_cost_per_kWh*battery_capacity-grant
  #print(paste("capital cost",capital_cost))
  amort_payment <- amort(interest_rate,term_of_loan)*(install_cost+pv_cost_per_kW*solar_capacity + battery_cost_per_kWh*battery_capacity-grant)
  #print(paste("annual loan payment",amort_payment))
  first_year_bill0 <- grid_cost_per_kWh*demand + standing_charge
  first_year_bill <- grid_cost_per_kWh*imports + standing_charge
  first_year_mss_revenue0 <- feedin_tariff*exports #add mss model (elec exports)
  first_year_mss_revenue <- dplyr::case_when(first_year_mss_revenue0 < 200~first_year_mss_revenue0,
                                             first_year_mss_revenue0 >= 200~first_year_mss_revenue0*(1-params$marginal_tax_rate)+params$marginal_tax_rate*params$ceg_tax_threshold)
  #outgoings are +ve revenue is -ve
  npv_loan_period <- geo_sum((1+electricity_inflation_rate)/(1+discount_rate),term_of_loan)*(first_year_bill-first_year_mss_revenue)+geo_sum(1/(1+discount_rate),term_of_loan)*amort_payment
  npv_noloan_period <- ((1+electricity_inflation_rate)/(1+discount_rate))^term_of_loan*geo_sum((1+electricity_inflation_rate)/(1+discount_rate),system_lifetime-term_of_loan)*(first_year_bill-first_year_mss_revenue)
  npv_noinvest <- geo_sum((1+electricity_inflation_rate)/(1+discount_rate),system_lifetime)*(first_year_bill0)

  #print(paste("first year cost savings",first_year_cost_savings))
  npv <- dplyr::case_when(term_of_loan==0~0,
                   term_of_loan > 0~npv_loan_period) + npv_noloan_period

  return((npv_noinvest-npv)/npv_noinvest)
}

get_sys_util_0 <- Vectorize(get_sys_util_0,c("demand","imports","exports","solar_capacity","battery_capacity"))


#test <- cer_systems %>% mutate(du_econ = get_sys_util_0(demand,imports,exports,solar_capacity,battery_capacity))
#test <- test %>% inner_join(cer %>% select(hcode,rooftop_potential))
#res <- test %>% group_by(hcode) %>% filter(solar_capacity <= rooftop_potential) %>% filter(du_econ==max(du_econ))

#g <- test %>% filter(hcode==1036) %>% ggplot() + geom_contour_filled(aes(x=solar_capacity,y=battery_capacity,z=du_econ), breaks=seq(0,bks[2],length.out=10))
#g <- g +geom_point(data=res %>% filter(hcode==1036),aes(solar_capacity,battery_capacity),colour="red")
#bks <- test %>% filter(hcode==1036) %>% pull(du_econ) %>% range()
#g <- g + scale_fill_brewer(type="div") + geom_vline(xintercept = res %>% filter(hcode==1036) %>% pull(rooftop_potential),linetype="dashed",colour="yellow")
#g + scale_x_continuous(trans="sqrt") + scale_y_continuous(trans="sqrt")

#hcodes <- cer_systems$hcode %>% unique()
#utils <- expand_grid(housecode=hcodes,solar_cap = seq(0,20,by=0.5), battery_cap = seq(0,20,by=2.5))


#' rooftop_solar_generation_cost
#'
#' cost per kWh of a financed PV system (no battery or grants) with a lifetime of 20 years.
#' installationa and panel costs are from cost_params
#'
#'
#' @param solar_capacity installed pv capacity
#' @param finance_rate loan interest rate
#' @param include_battery False
#'
#' @return price
#' @export
#'
#' @examples
rooftop_solar_generation_cost <- function(solar_capacity,finance_rate=0.05, include_battery = F){
  #assumes term of loan = lifetime of system
  #exclude cost of grant
  #exclude cost of battery system
  term_of_loan <- cost_params$system_lifetime
  install_cost_pv <- cost_params$install_cost_pv
  pv_cost_per_kW <- cost_params$pv_cost_per_kW
  amort_payment <- amort(finance_rate,term_of_loan)*(install_cost+pv_cost_per_kW*solar_capacity)
  total_gen <- 0.095*solar_capacity*24*265
  return(amort_payment/total_gen %>% round(3))
}


#' get_self_consumption
#'
#' self consumption metric
#'
#' @param solar_capacity install pv capacity
#' @param exports exports to grid
#' @param pv_capacity_factor capacity factor default using Birr pv1 time-series 0.094 (Dublin is 0.098)
#'
#' @return metric value
#' @export
#'
#' @examples
get_self_consumption <- function(solar_capacity,exports,pv_capacity_factor = 0.094){
  #
  self_consumption <- dplyr::case_when(solar_capacity==0~0,
                                solar_capacity > 0~1-exports/(solar_capacity*pv_capacity_factor*365*24))
  return(self_consumption)
}

#' get_self_sufficiency
#'
#' @param imports total annual imports kWh
#' @param demand  total annual demand kWh
#'
#' @return metric value
#' @export
#'
#' @examples
get_self_sufficiency <- function(imports,demand){
  #
  self_sufficiency <- 1-imports/demand
  return(self_sufficiency)
}


#df <- tibble()
#ceg0 <- cost_params$ceg_price_per_kWh
#for(ceg in seq(0,0.24,by=0.02)){
#   print(paste("ceg =", ceg))
#   cost_params$ceg_price_per_kWh <- ceg
#   test <- cer_systems %>% mutate(npv=get_sys_npv_0(demand,imports,exports,solar_capacity,battery_capacity, method="npv"))
#   test <- test %>% inner_join(cer %>% select(hcode,rooftop_potential))
#   res <- test %>% group_by(hcode) %>% filter(solar_capacity <= rooftop_potential) %>% filter(npv==min(npv,na.rm=T))
#   res$ceg <- ceg
#   df <- bind_rows(df,res)
# }

#df %>% mutate(self_sufficiency = get_self_sufficiency(imports,demand), self_consumption = get_self_consumption(solar_capacity,exports))


#' battery_cost_fun
#'
#' solar battery cost in euros/kWh
#'
#' @param sD scenario parameters dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros/kWh
#' @export
#'
#' @examples
battery_cost_fun <- function(sD,yeartime){

  cost_2010 <- sD %>% dplyr::filter(parameter=="bat_cost_2010") %>% dplyr::pull(value)
  cost_2015 <- sD %>% dplyr::filter(parameter=="bat_cost_2015") %>% dplyr::pull(value)

  cost_2022 <- sD %>% dplyr::filter(parameter=="bat_cost_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="bat_cost_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="bat_cost_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(2010.5,2015.5,2022.5,2030.5,2050.5), y=c(cost_2010,cost_2015,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)
}

#' battery_install_cost_fun
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros
#' @export
#'
#' @examples
battery_install_cost_fun <- function(sD,yeartime){

  cost_2010 <- sD %>% dplyr::filter(parameter=="bat_install_cost_2010") %>% dplyr::pull(value)
  cost_2015 <- sD %>% dplyr::filter(parameter=="bat_install_cost_2015") %>% dplyr::pull(value)
  cost_2022 <- sD %>% dplyr::filter(parameter=="bat_install_cost_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="bat_install_cost_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="bat_install_cost_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(2010.5,2015.5,2022.5,2030.5,2050.5), y=c(cost_2010,cost_2015,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)
}

#' pv_cost_fun
#'
#' solar battery cost in euros/kWh
#'
#' @param sD scenario parameters dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros/kWh
#' @export
#'
#' @examples
pv_cost_fun <- function(sD,yeartime){

  cost_2010 <- sD %>% dplyr::filter(parameter=="pv_cost_2010") %>% dplyr::pull(value)
  cost_2015 <- sD %>% dplyr::filter(parameter=="pv_cost_2015") %>% dplyr::pull(value)
  cost_2022 <- sD %>% dplyr::filter(parameter=="pv_cost_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="pv_cost_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="pv_cost_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(2010.5,2015.5,2022.5,2030.5,2050.5), y=c(cost_2010,cost_2015,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)
}

#' pv_install_cost_fun
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return scalar cost in euros
#' @export
#'
#' @examples
pv_install_cost_fun <- function(sD,yeartime){

  cost_2010 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2010") %>% dplyr::pull(value)
  cost_2015 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2015") %>% dplyr::pull(value) #inverter cost add more costs here if known

  cost_2022 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="pv_install_cost_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(2010.5,2015.5,2022.5,2030.5,2050.5), y=c(cost_2010,cost_2015,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)
}

#' electricity_price_fun
#'
#' actual path of electricity prices. For inflation expectations see electricity_price_inflation_fun
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
electricity_price_fun <- function(sD,yeartime){
  seai_elec1 <- seai_elec %>% dplyr::filter(year >=2008) #add more costs here if known

  cost_2022 <- sD %>% dplyr::filter(parameter=="electricity_price_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="electricity_price_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="electricity_price_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(seai_elec1$year+0.5,2022.5,2030.5,2050.5), y=c(seai_elec1$price/100,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)
}

#' electricity_price_inflation_fun
#'
#' inflation expectations in decimal units. This is used in NPV calculations.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return electricity inflation expectation in decimal units
#' @export
#'
#' @examples
electricity_price_inflation_fun <- function(sD,yeartime){

  inflate_2010 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2010") %>% dplyr::pull(value)
  inflate_2022 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2022") %>% dplyr::pull(value)
  inflate_2030 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2030") %>% dplyr::pull(value)
  inflate_2050 <- sD %>% dplyr::filter(parameter=="electricity_price_inflation_2050") %>% dplyr::pull(value)
  cost <- approx(c(2010.5,2022.5,2030.5,2050.5), y=c(inflate_2010,inflate_2022,inflate_2030,inflate_2050),xout=yeartime,rule=2)$y
  return(cost)



}

#' standing_charge_fun
#'
#' household standing charge (expectations & historical)
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return standing charge
#' @export
#'
#' @examples
standing_charge_fun <- function(sD,yeartime){

  cost_2015 <- sD %>% dplyr::filter(parameter=="standing_charge_2015") %>% dplyr::pull(value) #add more costs here if known
  cost_2021 <- sD %>% dplyr::filter(parameter=="standing_charge_2021") %>% dplyr::pull(value)

  cost_2022 <- sD %>% dplyr::filter(parameter=="standing_charge_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="standing_charge_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="standing_charge_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(2015.5,2021.5,2022.5,2030.5,2050.5), y=c(cost_2015,cost_2021,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)


}


#' finance_rate_fun
#'
#' historical and future solar PV finance rates. Allows for low interest load subvention.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return decimal rate (scalar)
#' @export
#'
#' @examples
finance_rate_fun <- function(sD,yeartime){

  fin_2015 <- 0.07 #add more costs here if known
  fin_2021 <- 0.07

  fin_2022 <- sD %>% dplyr::filter(parameter=="finance_rate_2022") %>% dplyr::pull(value)
  fin_2030 <- sD %>% dplyr::filter(parameter=="finance_rate_2030") %>% dplyr::pull(value)
  fin_2050 <- sD %>% dplyr::filter(parameter=="finance_rate_2050") %>% dplyr::pull(value)
  fin <- approx(x=c(2015.5,2021.5,2022.5,2030.5,2050.5), y=c(fin_2015,fin_2021,fin_2022,fin_2030,fin_2050),xout=yeartime,rule=2)$y
  return(fin)


}


#' ceg_fun
#'
#' clean export guarantee price euro/kWh to 2022-2050
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return euro/kWh
#' @export
#'
#' @examples
ceg_fun <- function(sD,yeartime){

  cost_2015 <- 0 #add more costs here if known
  cost_2021 <- 0

  cost_2022 <- sD %>% dplyr::filter(parameter=="ceg_2022") %>% dplyr::pull(value)
  cost_2030 <- sD %>% dplyr::filter(parameter=="ceg_2030") %>% dplyr::pull(value)
  cost_2050 <- sD %>% dplyr::filter(parameter=="ceg_2050") %>% dplyr::pull(value)
  cost <- approx(x=c(2015.5,2021.5,2022.5,2030.5,2050.5), y=c(cost_2015,cost_2021,cost_2022,cost_2030,cost_2050),xout=yeartime,rule=2)$y
  return(cost)
}


#' ceg_tax_threshold_fun
#'
#' MSS threshold in euros before tax is liable (tax disregard)
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return
#' @export
#'
#' @examples
ceg_tax_threshold_fun <- function(sD,yeartime){

  thres_2022 <- sD %>% dplyr::filter(parameter=="ceg_tax_threshold_2022") %>% dplyr::pull(value)
  thres_2030 <- sD %>% dplyr::filter(parameter=="ceg_tax_threshold_2030") %>% dplyr::pull(value)
  thres_2050 <- sD %>% dplyr::filter(parameter=="ceg_tax_threshold_2050") %>% dplyr::pull(value)
  thres <- approx(x=c(2022.5,2030.5,2050.5), y=c(thres_2022,thres_2030,thres_2050),xout=yeartime,rule=2)$y
  return(thres)
}

#' electricity_demand_factor_fun
#'
#' mean household demand compared to 2010 (year of CER dataset).
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return demand factor
#' @export
#'
#' @examples
electricity_demand_factor_fun <- function(sD,yeartime){

  seai_elec1 <- seai_elec %>% dplyr::filter(year <= 2020) #add more costs here if known
  seai_elec1 <- seai_elec1 %>% dplyr::mutate(factor=kWh/5302)

  fact_2021 <- sD %>% dplyr::filter(parameter=="electricity_demand_factor_2021") %>% dplyr::pull(value)
  fact_2030 <- sD %>% dplyr::filter(parameter=="electricity_demand_factor_2030") %>% dplyr::pull(value)
  fact_2050 <- sD %>% dplyr::filter(parameter=="electricity_demand_factor_2050") %>% dplyr::pull(value)
  fact <- approx(x=c(seai_elec1$year+0.5,2021.5,2030.5,2050.5), y=c(seai_elec1$factor,fact_2021,fact_2030,fact_2050),xout=yeartime,rule=2)$y
  return(fact)

}
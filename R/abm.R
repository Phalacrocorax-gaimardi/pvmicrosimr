

#' scenario_params_df
#'
#' builds the parameter set at yeartime from scenario sD
#'
#' @param sD scenario parameters e.g. scenario_0
#' @param yeartime decimal time
#'
#' @return long form dataframe containing parameter names and values
#' @export
#'
#' @examples
scenario_params_df <- function(sD,yeartime){
  #fast params
  scen <- tibble::tibble(parameter="yeartime", value=  yeartime)
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="battery_cost", value=  battery_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="battery_install_cost", value=  battery_install_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_cost", value=  pv_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_install_cost", value=  pv_install_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_price", value =  electricity_price_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_price_inflation", value =  electricity_price_inflation_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="standing_charge", value =  standing_charge_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ceg", value =  ceg_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ceg_tax_threshold", value =  ceg_tax_threshold_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="marginal_tax_rate", value =  dplyr::filter(sD, parameter=="marginal_tax_rate")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="finance_rate", value =  finance_rate_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="term_of_loan", value =  dplyr::filter(sD, parameter=="term_of_loan")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="discount_rate", value =  dplyr::filter(sD, parameter=="discount_rate")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="system_lifetime", value =  dplyr::filter(sD, parameter=="system_lifetime")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_demand_factor", value =  electricity_demand_factor_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_threshold", value =  dplyr::filter(sD, parameter=="sol_lower_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_upper_threshold", value =  dplyr::filter(sD, parameter=="sol_upper_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_grant", value =  dplyr::filter(sD, parameter=="sol_lower_grant")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_upper_grant", value =  dplyr::filter(sD, parameter=="sol_upper_grant")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_threshold", value =  dplyr::filter(sD, parameter=="sol_lower_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="grant_introduction_date", value =  dplyr::filter(sD, parameter=="grant_introduction_date")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="grant_removal_date", value =  dplyr::filter(sD, parameter=="grant_removal_date")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bat_threshold", value =  dplyr::filter(sD, parameter=="bat_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bat_grant", value =  dplyr::filter(sD, parameter=="bat_grant")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="self_sufficiency_effect", value =  self_sufficiency_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="kWp_per_m2", value =  kWp_per_m2_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="usable_roof_fraction", value =  dplyr::filter(sD, parameter=="usable_roof_fraction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="mean_shading_factor", value =  dplyr::filter(sD, parameter=="mean_shading_factor")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="acceleration_factor", value =  acceleration_fun(sD,yeartime)))

  return(scen %>% fast_params())
}

#' fast_params
#'
#' helper function to convert a long format dataframe to an environment object, used for fast access to scenario parameters
#'
#' @param params_long long format dataframe with columns "parameter" and "value"
#'
#' @return environment object
#' @export
#'
#' @examples
fast_params <- function(params_long){

  test <- as.list(params_long$value)
  names(test) <- params_long$parameter
  test <- list2env(test)
  return(test)
}


#params <- scenario_params_df(sD,2024.4)
#params <- fast_params(params)

#cer_systems <- cer_systems %>% filter(housecode %in% cer_survey$housecode)

#' initialise_agents
#'
#' creates the agent initial state including model weights, randomised available rooftop areas for solar PV and a mapping of survey agents to CER dataset. It is assumed that solar pv systems are absent at yeartime e.g. 2010
#'
#' @param agents the model dataframe
#' @param yeartime start year (default 2010)
#' @param lambda demand matching parameter. Large lambda is more strict.
#' @param clipping whether to clip weights to >=0
#'
#' @return a dataframe with columns
#' @export
#'
#' @examples
initialise_agents <- function(agents,yeartime,lambda=2,clipping=T){

  #initialise to 2010
  params <- scenario_params_df(scenario_wem,yeartime)
  test <- map_survey_to_cer(params,lambda)
  #owner occupier non-apartment
  #test <- test %>% dplyr::filter(q1 %in% 2:5,q3 %in% 1:2)
  test$ID <- 1:dim(test)[1]
  agents <- agents %>% dplyr::inner_join(test %>% dplyr::select(ID,housecode,qc1,region))

  #housing type
  agents <- agents  %>% dplyr::rowwise() %>% dplyr::mutate(house_type=pvmicrosimr::is_bungalow(qc1,region))
  agents <- agents %>% dplyr::select(-qc1,-region)
  #
  agents <- agents %>% dplyr::inner_join(cer_demand)
  agents <- agents  %>% dplyr::rowwise() %>% dplyr::mutate(area1=get_rooftop_solar_area(house_type,demand,usable_roof_fraction = params$usable_roof_fraction))
  agents <- agents %>% dplyr::mutate(area2 = area1 )
  #primitive shading model
  agents <- agents  %>% dplyr::rowwise() %>% dplyr::mutate(shading1=rbeta(1,params$mean_shading_factor/(1-params$mean_shading_factor),1), shading2 = rbeta(1,params$mean_shading_factor/(1-params$mean_shading_factor),1))
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(aspect=sample(c("South-North","SW-NE","SE-NW","East-West"),size=1))
  agents <- agents[,c(1,7:14,2:6)]
  agents$old_imports <- agents$demand
  agents$old_exports <- 0
  agents$old_solar1 <- 0
  agents$old_solar2 <- 0
  agents$old_battery <- 0
  agents$new_imports <- agents$demand
  agents$new_exports <- 0
  agents$new_solar1 <- 0
  agents$new_solar2 <- 0
  agents$new_battery <- 0
  #add initial rooftop capacity excluding apartments !
  #cer_survey1 <- cer_survey %>% dplyr::filter(housing_type != 1) %>% dplyr::rowwise() %>% dplyr::mutate(rooftop_capacity = get_rooftop_solar_potential(floor_area,housing_type,bedrooms))
  #missing rooftop capacity values replaced with mean for housing_type
  #roof <- cer_survey1 %>% dplyr::group_by(housing_type) %>% dplyr::mutate(rooftop_capacity = ifelse(is.na(rooftop_capacity), mean(rooftop_capacity,na.rm=T),rooftop_capacity))
  #roof <- roof %>% dplyr::ungroup() %>% dplyr::select(housecode,rooftop_capacity)
  #agents <- agents %>% dplyr::inner_join(roof)
  #set all social terms to zero
  agents$qsp21 <- 1
  #clipping
  if(clipping){
    x_min <- quantile(agents$w_q9_1,0.075)
    x_max <- quantile(agents$w_q9_1,0.925)
    agents <- agents %>% dplyr::mutate(w_q9_1=dplyr::case_when(w_q9_1<=x_min~x_min,w_q9_1>x_max~x_max, (w_q9_1> x_min) & (w_q9_1 < x_max)~w_q9_1))
    x_min <- quantile(agents$w_qsp21,0.06)
    x_max <- quantile(agents$w_qsp21,0.94)
    agents <- agents %>% dplyr::mutate(w_qsp21=dplyr::case_when(w_qsp21<=x_min~x_min,w_qsp21>x_max~x_max, (w_qsp21> x_min) & (w_qsp21 < x_max)~w_qsp21))
    x_min <- quantile(agents$w_q9_1,0.02)
    x_max <- quantile(agents$w_q9_1,0.98)
    agents <- agents %>% dplyr::mutate(w_theta=dplyr::case_when(w_theta<=x_min~x_min,w_theta>x_max~x_max, (w_theta> x_min) & (w_theta < x_max)~w_theta))
    }
  return(agents)
}

#agents_in <- initialise_agents(2010)
#agents_in$new_solar <- 0
#agents_in$new_battery <- 0

#' update_agents4
#'
#' Micro-simulation Updater
#'
#' The workhorse ABM function.Within a scenario, does a single month update of the agent characteristics. A random sample of agents evaluates their economic and social
#' utilities. If these exceed their individual threshold and EV is adopted.
#'
#'
#' @param sD  scenario dataframe
#' @param yeartime decimal time
#' @param agents_in input agent dataframe
#' @param social_network artifical social network
#' @param ignore_social option to ignore social effects. Default is FALSE.
#' @param empirical_u empirical utility model (see empirical_utils)
#' @param p. speed
#' @param lambda. bias
#'
#' @return new agent dataframe
#' @export
#' @importFrom magrittr %>%
#' @examples
update_agents4 <- function(sD,yeartime,agents_in, social_network,ignore_social=F, empirical_u = empirical_utils, p.,lambda.){

  #
  du_social <- dplyr::filter(empirical_u,code=="qsp21")$du_average
  theta <- dplyr::filter(empirical_u,code=="theta")$du_average
  #params at yeartime
  params <- scenario_params_df(sD,yeartime)
  #self-sufficiency
  averse <- c(0,0,0,aversion_4.,aversion_5.)*params$self_sufficiency_effect
  #parameters from scenario corresponding to yeartime
  kWpm2 <- params$kWp_per_m2

  a_s <- agents_in
  a_s$transaction <- F
  a_s <- dplyr::ungroup(a_s)

  a_s <- a_s %>% dplyr::mutate(old_imports=new_imports,old_exports=new_imports,old_solar1=new_solar1,old_solar2 = new_solar2,old_battery=new_battery)
  #this subsample of agents decide to look at rooftop pv
  b_s <- dplyr::slice_sample(a_s,n=round(dim(a_s)[1]*p.*params$acceleration_factor))
  b_s <- b_s %>% dplyr::mutate(transaction=T) %>% dplyr::select(ID,housecode,house_type,aspect,area1,area2,shading1,shading2,w_q9_1,w_qsp21,w_theta,q9_1,qsp21,old_imports, old_exports,old_solar1,old_solar2,old_battery)

  get_sys_optimal <- function(params, b_s){

    #pv rooftop capacity constrained finacial utilities corresponding to costs in params
    #find current (old) values of imports and exports
    cer_sys <- b_s %>% dplyr::left_join(dplyr::bind_rows(cer_systems1,cer_systems2,cer_systems3,cer_systems4))

    #cer_sys1 <- b_s %>% dplyr::left_join(cer_systems1)
    #cer_sys2 <- b_s %>% dplyr::left_join(cer_systems2)
    #cer_sys3 <- b_s %>% dplyr::left_join(cer_systems3)
    #cer_sys4 <- b_s %>% dplyr::left_join(cer_systems4)
    #cer_sys <- dplyr::bind_rows(cer_sys1,cer_sys2,cer_sys3,cer_sys4)


    cer_sys <- get_shaded_sys(cer_sys)
    #cer_sys <- b_s %>% dplyr::left_join(cer_systems)
    #new system is an enhancement
    cer_sys <- cer_sys %>% dplyr::filter(solar1 <= kWpm2*area1,solar1 >= old_solar1, solar2 <= kWpm2*area2, solar2 >= old_solar2, battery >= old_battery)
    #add shading factors in financial utility!
    cer_sys <- cer_sys %>% dplyr::mutate(du=get_sys_util_0(params,demand,old_imports,old_exports,old_solar1,old_solar2,old_battery,imports,exports,solar1-old_solar1,solar2-old_solar2,battery-old_battery))
    #optimal
    cer_sys_opt <- cer_sys %>% dplyr::group_by(housecode) %>% dplyr::filter(du==max(du))
    cer_sys_opt <- cer_sys_opt %>% dplyr::mutate(area1 = area1 - (solar1-old_solar1)/kWpm2, area2 = area2 - (solar2-old_solar2)/kWpm2)

    cer_sys_opt <- cer_sys_opt %>% dplyr::rename(new_solar1=solar1,new_solar2 = solar2,new_battery=battery,new_imports=imports,new_exports=exports)
    return(cer_sys_opt)
    #return(b_s %>% dplyr::inner_join(cer_sys_opt))
  }
  #financially optimal solar pv system
  b_s1 <- get_sys_optimal(params,b_s)
  #add in self-sufficiency/aversion effect
  # and scale factor from calibration
  #add parial utilities
  b_s1 <- b_s1 %>% dplyr::mutate(du_fin=beta.*w_q9_1*du-averse[q9_1])
  b_s1 <- b_s1 %>% dplyr::mutate(du_social = dplyr::case_when((old_solar1 > 0 | old_solar2 > 0)~0,(old_solar1==0 & old_solar2==0)~w_qsp21*du_social[qsp21]))
  #+lambda. or +w_theta*lambda.?
  b_s1 <- b_s1 %>% dplyr::mutate(du_theta = dplyr::case_when((old_solar1 > 0 | old_solar2 > 0)~0,(old_solar1==0 & old_solar2==0)~w_theta*theta+ lambda.))
  b_s1 <- b_s1 %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta)
  #some agents do not transact even when du_fin > 0
  b_s_transact <- b_s1 %>% dplyr::filter(du_tot > 0)
  b_s_notransact <- b_s1 %>% dplyr::filter(du_tot <= 0)

  b_s_transact$transaction <- T
  b_s_notransact$transaction <- F
  #this line reverses transactions that would occur of du were only term
  b_s_notransact <- b_s_notransact %>% dplyr::mutate(new_solar1=old_solar1,new_solar2=old_solar2,new_battery=old_battery)
  #b_s <- update_cars(b_s,params)
  #
  a_s <- dplyr::filter(a_s, !(ID %in% c(b_s_notransact$ID,b_s_transact$ID)))
  a_s <- dplyr::bind_rows(a_s,b_s_notransact,b_s_transact) %>% dplyr::arrange(as.numeric(ID))
  a_s <- a_s %>% dplyr::mutate(new_solar1 = tidyr::replace_na(new_solar1,0), new_solar2 = tidyr::replace_na(new_solar2,0),new_battery = tidyr::replace_na(new_battery,0))
  #recompute social variable
  ma <- igraph::get.adjacency(social_network)
  g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="ID")
  #social network conformity effect
  #
  #fossil_nodes  <- igraph::V(g)$fuel == "fossil"
  #adopter_nodes <- igraph::V(g)$old_solar == 0 & igraph::V(g)$new_solar > 0
  adopter_nodes <- igraph::V(g)$new_solar1 > 0 | igraph::V(g)$new_solar2 > 0
  a_s$qsp21 <- as.numeric(ma %*% adopter_nodes) #social reinforcement
  if(ignore_social) a_s$qsp21 <- 0 #no pvs assumed present in local network
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(qsp21 = min(qsp21+1,3)) #qsp21 encoding 1,2,3
  #agents_out <- a_s
  a_s <- a_s %>% dplyr::select(-du,-du_fin,-du_social,-du_theta,-du_tot)
  print(paste("time", round(yeartime,1), "PV system adopters",dim(a_s %>% dplyr::filter( (new_solar1 > 0 & old_solar1==0) | (new_solar2 > 0 & old_solar2==0)))[1]))
  print(paste("PV system augmentors",dim(a_s %>% dplyr::filter((old_solar1 > 0 & new_solar1 > old_solar1) | (old_solar2 > 0 & new_solar2 > old_solar2) | (old_solar1 > 0 & new_battery > old_battery) || (old_solar2 > 0 & new_battery > old_battery) ))[1]))
  return(dplyr::ungroup(a_s))
}

#p. <- 0.05/6
#beta. <- 0.136
#lambda. <-
#



#' runABM
#'
#' Runs pv system adoption simulation on artificial society of 759 agents.
#' Each run is performed on an independently generated social network with randomly assigned electricity demand time-series
#' derived from CER dataset (2010).
#'
#' Bi-montly timestep.
#'
#' @param sD scenario set-up dataframe, typically read with readxlxs(...,sheet=scenario)
#' @param Nrun integer, number runs
#' @param simulation_end the final year of simulation of early termination is required
#' @param resample_society if TRUE resample society with replacement to capture additional variability
#' @param n_unused_cores number of cores left unused in parallel/foreach. Recommended values 2 or 1.
#' @param use_parallel if TRUE uses multiple cores. Use FALSE for diagnostic runs
#' @param ignore_social if TRUE ignore social network effects. Default is FALSE
#'
#' @return a three component list - simulation output, scenario setup, meta-parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar%
#'
runABM <- function(sD, Nrun=1,simulation_end=end_year,resample_society=F,n_unused_cores=2, use_parallel=T,ignore_social=F){
  #
  year_zero <- 2010
  #calibration params:: MOVED TO SYSTDATA WHEN CALIBRATION COMPLETE
  p. <- sD %>% dplyr::filter(parameter=="p.") %>% dplyr::pull(value)
  lambda. <- sD %>% dplyr::filter(parameter=="lambda.") %>% dplyr::pull(value)
  #print(paste("p.=",p.,"lambda.=",lambda.))

  #bi-monthly runs
  Nt <- round((simulation_end-year_zero+1)*6)
  #annual runs
  #Nt <- round((simulation_end-year_zero+1))
  agents0 <- agents_init
  u_empirical <- empirical_utils
  #
  if(use_parallel){

    number_of_cores <- parallel::detectCores() - n_unused_cores
    doParallel::registerDoParallel(number_of_cores)

    abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_agents","update_agents4")) %dopar% {
    #abm <- foreach::foreach(j = 1:Nrun, .errorhandling = "pass",.export = c("initialise_agents","update_agents4")) %dopar% {

      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      if(!resample_society) social <- make_artificial_society(pv_society_oo,homophily,5)
      if(resample_society){
        agent_resample <- sample(1:dim(pv_society_oo)[1],replace=T)
        society_new <- society[agent_resample,]
        society_new$ID <- 1:dim(pv_society_oo)[1]
        social <- make_artificial_society(society_new,homophily,4.5)
      }
      #randomiise ICEV emissions assignment
      #choose segments
      agents_in <- initialise_agents(agents0,year_zero)
      #no transactions
      agents_in$transaction <- FALSE
      agent_ts<- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #bi-monthly
        yeartime <- year_zero+(t-1)/6
        #yeartime <- year_zero + (t-1)
        agent_ts[[t]] <- update_agents4(sD,yeartime,agent_ts[[t-1]],social_network=social,ignore_social, empirical_u = u_empirical,p.,lambda.) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #add vertex degree
      degrees <- tibble::tibble(ID=1:dim(pv_society_oo)[1],degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      agent_ts
    }

      meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","aversion_4.","aversion_5.","lambda.","p."),value=c(Nrun,simulation_end,beta.,aversion_4.,aversion_5.,lambda.,p.))
      return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }

  #don't use parallel
  #comment in next two lines for parallel
  if(!use_parallel){

    abm <- tibble::tibble()
    #number_of_cores <- parallel::detectCores() - n_unused_cores
    #doParallel::registerDoParallel(number_of_cores)
    #comment out next line for parallel
    for(j in 1:Nrun){
      #comment in next line for parallel
      #abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      if(!resample_society) social <- make_artificial_society(pv_society_oo,homophily,5)
      if(resample_society){
        agent_resample <- sample(1:dim(pv_society_oo)[1],replace=T)
        society_new <- pv_society_oo[agent_resample,]
        society_new$ID <- 1:dim(pv_society_oo)[1]
        social <- make_artificial_society(society_new,homophily,5)

      }
      #randomise ICEV emissions assignment
      #choose market segment for each agent
      agents_in <- initialise_agents(agents0,year_zero)
      #no transactions
      agents_in$transaction <- FALSE
      agent_ts <- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #
        #yeartime <- year_zero+(t-1)
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents4(sD,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,u_empirical,p.,lambda.) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #network degree
      degrees <- tibble::tibble(ID=1:dim(pv_society_oo)[1],degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      abm <- dplyr::bind_rows(abm,agent_ts)
      #comment in next line for parallel
      #agent_ts
    }
    meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","aversion_4.","aversion_5.","lambda.","p."),value=c(Nrun,simulation_end,beta.,aversion_4.,aversion_5.,lambda.,p.))
    return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }

}

# beta. <- 0.0247
# aversion_4. <- 0.004
#aversion_5. <- 0.007


#p. <- 0.008
#lambda. <- 0.097
#use_data(beta.,aversion_4.,aversion_5.,p.,lambda.,overwrite=T,internal=T,version=3)

#test[[1]] %>% group_by(t,simulation) %>% summarise(bat=sum(new_battery)) %>% ggplot(aes(2010+(t-1),bat,colour=factor(simulation))) + geom_line() + scale_y_continuous(trans="sqrt")
#test[[1]] %>% group_by(t,simulation) %>% summarise(pv=sum(new_solar)) %>% ggplot(aes(2010+(t-1),pv,colour=factor(simulation))) + geom_line() + scale_y_continuous(trans="sqrt")

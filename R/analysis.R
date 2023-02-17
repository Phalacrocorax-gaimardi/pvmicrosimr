#analysis functions for pvmicrosimr

#' get_capacities
#'
#' total (cumulative) installed capacities (solar and battery) by simulation, date and groupvars.
#'
#' @param abm output of runABM
#' @param groupvars grouping variables in addition to simulation number, date
#' @param house_stock assumed housing stock (currently static 1.1M)
#'
#' @return dataframe with installed capacities on roof1 and roof2
#' @export
#'
#' @examples
get_capacities <- function(abm,groupvars = NULL, house_stock = 1.1){

  groupvars <- c("simulation","date",groupvars)
  #abm0 <- abm[[1]] %>% dplyr::mutate(date=ymd("2010-02-01") %m+% months((t-1)*2)) %>% arrange(simulation,date)
  #Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)
  cap <- abm[[1]] %>% dplyr::group_by(across(all_of(groupvars)))  %>% dplyr::summarise(solar1=sum(new_solar1)/759*house_stock, solar2 = sum(new_solar2)/759*house_stock, battery=sum(new_battery)/759*house_stock)
  return(cap)

}

#' get_uptake
#'
#' the fraction of households that have adopted solar by date. Also returns that fraction of households that have adopted a solar+battery system and the storage attachement rate
#'
#' @param abm output of runABM
#' @param groupvars grouping variables (in addition to simulation and date)
#'
#' @return fraction of households that have installed solar, fraction that have installed solar & battery and storage attachment rate (ssar)
#' @export
#'
#' @examples
get_uptake <- function(abm,groupvars = NULL){

  groupvars <- c("simulation","date",groupvars)
  #abm0 <- abm[[1]] %>% dplyr::mutate(date=ymd("2010-02-01") %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date)
  #Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)
  attached <- abm[[1]] %>% dplyr::group_by(across(all_of(groupvars)))  %>% dplyr::filter(new_solar1+new_solar2 > 0 & new_battery >0 ) %>% dplyr::summarise(attached=n()/759)
  all <- abm[[1]] %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvars)))  %>% dplyr::filter(new_solar1+new_solar2 > 0) %>% dplyr::summarise(all=n()/759)
  dates <- abm[[1]]$date %>% unique() %>% sort()
  dates <- tidyr::expand_grid(simulation=1:Nrun,date=dates)
  all <- dates %>% dplyr::left_join(all) %>% dplyr::mutate(all=tidyr::replace_na(all,0))
  attached <- dates %>% dplyr::left_join(attached) %>% dplyr::mutate(attached=tidyr::replace_na(attached,0))
  all <- dplyr::inner_join(attached,all) #%>% mutate(all=replace_na(all,0),attached=replace_na(attached,0))
  all <- all %>% dplyr::mutate(sar=attached/all)
  return(all)

}

#' get_adopters
#'
#' returns the ensemble-averaged fraction of households that adopt during a particular interval.
#' also includes the fraction of households that augment an existing syste, share of battery adopters,
#' share of battery augmenters and share that augment their battery only.
#'
#' @param abm output of runABM
#'
#' @return
#' @export
#'
#' @examples
get_adopters <- function(abm){

  Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)
  pv_adopt <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter((new_solar1 > 0 | new_solar2 >0) & old_solar1==0 & old_solar2==0) %>% dplyr::summarise(pv_ad=n()/Nrun/759)
  #adopt <- adopt %>% group_by(year=year(date)) %>% summarise(n=mean(n)/759)
  pv_augment <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter((new_solar1 > old_solar1 | new_solar2 > old_solar2) & (old_solar1 > 0 | old_solar2 > 0)) %>% dplyr::summarise(pv_aug=n()/Nrun/759)
  bat_adopt <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter(new_battery > 0 & old_battery == 0) %>% dplyr::summarise(b_ad=n()/Nrun)
  bat_augment <- abm0 %>% dplyr::group_by(date) %>% dplyr::filter(new_battery > old_battery & old_battery > 0) %>% dplyr::summarise(b_aug=n()/Nrun/759)
  bat_augment_only <- abm0 %>% dplyr::group_by(date) %>% dplyr::filter(new_battery > old_battery & old_battery > 0 & new_solar1 == old_solar1 & new_solar2 == old_solar2) %>% dplyr::summarise(b_aug_o=n()/Nrun/759)

  dates <- abm0$date %>% unique() %>% sort()
  dates <- tidyr::expand_grid(date=dates)
  pv_adopt <- dates %>% dplyr::left_join(pv_adopt) %>% dplyr::mutate(pv_ad=tidyr::replace_na(pv_ad,0))
  pv_augment <- dates %>% dplyr::left_join(pv_augment) %>% dplyr::mutate(pv_aug=tidyr::replace_na(pv_aug,0))
  bat_adopt <- dates %>% dplyr::left_join(bat_adopt) %>% dplyr::mutate(b_ad=tidyr::replace_na(b_ad,0))
  bat_augment <- dates %>% dplyr::left_join(bat_augment) %>% dplyr::mutate(b_aug=tidyr::replace_na(b_aug,0))
  bat_augment_only <- dates %>% dplyr::left_join(bat_augment_only) %>% dplyr::mutate(b_aug_o=tidyr::replace_na(b_aug_o,0))

  all <- dplyr::inner_join(pv_adopt,pv_augment) %>% dplyr::inner_join(.,bat_adopt) %>% dplyr::inner_join(.,bat_augment) %>% dplyr::inner_join(.,bat_augment_only)
  return(all)

}



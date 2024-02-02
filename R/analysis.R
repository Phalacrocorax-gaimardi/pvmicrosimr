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

#' get_uptake2
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
get_uptake2 <- function(abm,groupvars = NULL){

  groupvars1 <- c("simulation","date",groupvars)
  groupvars2 <- c("date",groupvars)
  Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)

  #abm0 <- abm[[1]] %>% dplyr::mutate(date=ymd("2010-02-01") %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date)
  #Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)
  all <- abm[[1]] %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvars1)))  %>% dplyr::filter(new_solar1+new_solar2 > 0) %>% dplyr::summarise(n=dplyr::n())
  all0 <- abm[[1]]  %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvars1))) %>%  dplyr::summarise(ntot=dplyr::n())
  all <- all0 %>% dplyr::left_join(all) %>% dplyr::mutate(n=tidyr::replace_na(n,0)) %>% dplyr::mutate(all=n/ntot)  %>% dplyr::select(-n,-ntot)

  attached <- abm[[1]] %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvars1)))  %>% dplyr::filter(new_solar1+new_solar2 > 0 & new_battery >0 ) %>% dplyr::summarise(n=dplyr::n())
  attached <- all0 %>% dplyr::left_join(attached) %>% dplyr::mutate(n=tidyr::replace_na(n,0)) %>% dplyr::mutate(attached=n/ntot) %>% dplyr::select(-n,-ntot)

  #dates <- abm[[1]]$date %>% unique() %>% sort()
  #dates <- tidyr::expand_grid(simulation=1:Nrun,date=dates)
  #all <- dates %>% dplyr::left_join(all) %>% dplyr::mutate(all=tidyr::replace_na(all,0))
  #attached <- dates %>% dplyr::left_join(attached) %>% dplyr::mutate(attached=tidyr::replace_na(attached,0))
  all <- dplyr::inner_join(attached,all) #%>% mutate(all=replace_na(all,0),attached=replace_na(attached,0))
  all <- all %>% dplyr::mutate(sar=dplyr::case_when(all==0~0,all>0~attached/all))
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
  bat_adopt <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter(new_battery > 0 & old_battery == 0) %>% dplyr::summarise(b_ad=n()/Nrun/759)
  bat_augment <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter(new_battery > old_battery & old_battery > 0) %>% dplyr::summarise(b_aug=n()/Nrun/759)
  bat_augment_only <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter(new_battery > old_battery & old_battery > 0 & new_solar1 == old_solar1 & new_solar2 == old_solar2) %>% dplyr::summarise(b_aug_o=n()/Nrun/759)

  dates <- abm[[1]]$date %>% unique() %>% sort()
  dates <- tidyr::expand_grid(date=dates)
  pv_adopt <- dates %>% dplyr::left_join(pv_adopt) %>% dplyr::mutate(pv_ad=tidyr::replace_na(pv_ad,0))
  pv_augment <- dates %>% dplyr::left_join(pv_augment) %>% dplyr::mutate(pv_aug=tidyr::replace_na(pv_aug,0))
  bat_adopt <- dates %>% dplyr::left_join(bat_adopt) %>% dplyr::mutate(b_ad=tidyr::replace_na(b_ad,0))
  bat_augment <- dates %>% dplyr::left_join(bat_augment) %>% dplyr::mutate(b_aug=tidyr::replace_na(b_aug,0))
  bat_augment_only <- dates %>% dplyr::left_join(bat_augment_only) %>% dplyr::mutate(b_aug_o=tidyr::replace_na(b_aug_o,0))

  all <- dplyr::inner_join(pv_adopt,pv_augment) %>% dplyr::inner_join(.,bat_adopt) %>% dplyr::inner_join(.,bat_augment) %>% dplyr::inner_join(.,bat_augment_only)
  return(all)

}


#' get_energy
#'
#' annual total energetics (demand,solar,imports,exports in GWh) and self-sufficiency metrics
#'
#' @param abm list output of runABM
#' @param housing_stock owner-occupier housing stock in millions (default 1.1)
#'
#' @return dataframe with annual values
#' @export
#'
#' @examples
get_energy <- function(abm,housing_stock=1.15){
  #annual demand, imports and exports
  Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)
  power <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::summarise(imports=sum(new_imports,na.rm=T)/Nrun/759*housing_stock, exports=sum(new_exports)/Nrun/759*housing_stock,demand=sum(demand)/Nrun/759*housing_stock)
  #annual
  power <- power %>% dplyr::group_by(year=lubridate::year(date)) %>% summarise(imports=mean(imports), exports=mean(exports),demand=mean(demand))
  power <- power %>% dplyr::mutate(solar=demand+exports-imports)
  #power <- power %>% pivot_longer(-year,names_to="grid",values_to="GWh")
  power <- power %>% dplyr::mutate(self_consumption=1-exports/solar,self_sufficiency = 1-imports/demand)
  return(power)
}


#' get_installed
#'
#' median size of solar and battery capacities installed at each time step
#'
#' @param abm output of runABM
#' @param groupvars additional grouping variables (default NULL)
#'
#' @return
#' @export
#'
#' @examples
get_installed <- function(abm,groupvars=NULL){

  #get median size of new solar or battery installation at each time step
  #mean size of augmented solar or battery installation at each time step
  groupvars <- c("date",groupvars)
  Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)
  new_pv_size <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter((new_solar1 > 0 | new_solar2 >0) & old_solar1==0 & old_solar2==0) %>% dplyr::summarise(pv_new1=mean(new_solar1), pv_new2=mean(new_solar2))
  new_bat_size <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter((new_solar1 > 0 | new_solar2 >0) & old_solar1==0 & old_solar2==0 & new_battery>0) %>% dplyr::summarise(bat_new=mean(new_battery))
  #mean battery size excluding installations without batteries
  #adopt <- adopt %>% group_by(year=year(date)) %>% summarise(n=mean(n)/759)
  aug_pv_size <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter((new_solar1 > old_solar1 | new_solar2 > old_solar2) & (old_solar1 > 0 | old_solar2 > 0)) %>% dplyr::summarise(pv_aug1=median(new_solar1), pv_aug2=median(new_solar2))
  aug_bat_size <- abm[[1]] %>% dplyr::group_by(date) %>% dplyr::filter((new_solar1 > old_solar1 | new_solar2 > old_solar2) & (old_solar1 > 0 | old_solar2 > 0) & new_battery > old_battery) %>% dplyr::summarise(bat_aug=median(new_battery))

  dates <- abm[[1]]$date %>% unique() %>% sort()
  dates <- tidyr::expand_grid(date=dates)
  new_pv_size <- dates %>% dplyr::left_join(new_pv_size) %>% dplyr::mutate(pv_new1=replace_na(pv_new1,0),pv_new2=replace_na(pv_new2,0))
  new_bat_size <- dates %>% dplyr::left_join(new_bat_size) %>% dplyr::mutate(bat_new=replace_na(bat_new,0))
  aug_pv_size <- dates %>% dplyr::left_join(aug_pv_size) %>% dplyr::mutate(pv_aug1=replace_na(pv_aug1,0),pv_aug2=replace_na(pv_aug2,0))
  aug_bat_size <- dates %>% dplyr::left_join(aug_bat_size) %>% dplyr::mutate(bat_aug=replace_na(bat_aug,0))

  comb <- dplyr::inner_join(new_pv_size,new_bat_size) %>% dplyr::inner_join(.,aug_pv_size) %>% dplyr::inner_join(.,aug_bat_size)
  return(comb)

}



#' get_available_area_remaining
#'
#' the remaining available roof area by aspect and date
#'
#' @param abm output of runABM
#'
#' @return available areas in km2
#' @export
#'
#' @examples
get_available_area_remaining <- function(abm){

  Nrun <- abm[[3]] %>% dplyr::filter(parameter=="Nrun") %>% dplyr::pull(value)
  areas <- abm[[1]] %>% dplyr::group_by(date,aspect) %>% dplyr::summarise(roof1=sum(area1)/Nrun/759*1.1,roof2=sum(area2)/Nrun/759*1.1)
  areas <- areas %>% tidyr::pivot_longer(c(-date,-aspect),names_to="roof",values_to="km2")
  areas <- areas %>% dplyr::rowwise() %>% dplyr::mutate(aspect2 = dplyr::case_when(roof=="roof1"~str_split(aspect,"-")[[1]][1],roof=="roof2"~str_split(aspect,"-")[[1]][2]))
  areas <- areas %>% dplyr::select(date,aspect2,km2) %>% dplyr::rename("aspect"=aspect2)
  return(areas)
}



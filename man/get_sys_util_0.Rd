% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finance.R
\name{get_sys_util_0}
\alias{get_sys_util_0}
\title{get_sys_util_0}
\usage{
get_sys_util_0(
  params,
  demand,
  old_imports,
  old_exports,
  old_solar1,
  old_solar2,
  old_battery,
  new_imports,
  new_exports,
  d_solar1,
  d_solar2,
  d_battery,
  include_grant = T,
  customer = "domestic"
)
}
\arguments{
\item{params}{cost parameter long-format dataframe}

\item{demand}{annual household electrical energy demand (kWh) (from smart meter dataset).}

\item{old_imports}{annual energy (kWh) imports with existing system (old_solar1, old_solar2, old_battery)}

\item{old_exports}{annual energy exports (kWh) with existing system (old_solar1, old_solar2, old_battery)}

\item{old_solar1}{existing installed solar capacity on half roof1}

\item{old_solar2}{existing installed solar capacity on half roof2}

\item{old_battery}{currently installed battery}

\item{new_imports}{annual energy (kWh) import with new (proposed) system (old_solar1 + d_solar1, old_solar2 + d_solar2, old_battery+d_battery)}

\item{new_exports}{annual energy exports (kWh) with new (proposed) system (old_solar1 + d_solar1, old_solar2 + d_solar2, old_battery+d_battery)}

\item{d_solar1}{additional solar capacity on roof1}

\item{d_solar2}{additional solar capacity on roof2}

\item{d_battery}{kWh battery size increment in steps of 2.5 kWh}

\item{include_grant}{True or False}

\item{customer}{"domestic" is only option at the moment}
}
\value{
unscaled financial partial utility
}
\description{
the financial utility of investment in a household solar PV system. This is defined as the fractional reduction in net present value of all associated cashflows(NPV_0-NPV)/NPV_0 where NPV_0 is the net present value
of expected future bills (including inflation expectations, discounted out to a term) and NPV is the net present value of all future cashflows
when a solar PV investment is made (amortisation, feedin tariff revenues, reduced electricity bills, tax). The financial model assumes a 2-component roof with incremental pv investments
d_solar1 and d_solar2 and incremental battery installation d_battery.
This function calculates the financial partial utility for initial adoption (old_solar1=0 and old_solar2=0) or for augmentation of an existing installation.
It is assumed that no grant is available for augmenting an existing system.
}

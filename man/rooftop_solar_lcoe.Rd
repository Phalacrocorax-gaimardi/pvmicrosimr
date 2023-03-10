% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finance.R
\name{rooftop_solar_lcoe}
\alias{rooftop_solar_lcoe}
\title{rooftop_solar_lcoe}
\usage{
rooftop_solar_lcoe(
  params,
  capacity_factor,
  solar_capacity,
  finance_rate = NULL,
  include_battery = F,
  battery_capacity
)
}
\arguments{
\item{params}{current parameter set}

\item{capacity_factor}{capacity factor}

\item{solar_capacity}{installed pv capacity}

\item{finance_rate}{loan interest rate}

\item{include_battery}{default False}

\item{battery_capacity}{battery capacity kWh}
}
\value{
simple levelised system cost in euros per kWp
}
\description{
a levelised cost per kWh of a fully financed PV system (with battery but grants) with a lifetime of 20 years.
interest rate is ~ 5\%. opex is zero.
interest rate,installation and pv costs are from params
}

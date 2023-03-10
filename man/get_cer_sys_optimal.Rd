% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cer.R
\name{get_cer_sys_optimal}
\alias{get_cer_sys_optimal}
\title{get_cer_sys_optimal}
\usage{
get_cer_sys_optimal(params)
}
\arguments{
\item{params}{cost parameter environment object returned by fast_}
}
\value{
a dataframe
}
\description{
returns the optimally sized solar/battery system based on financial utility (NPV_0-NPV)/NPV_0 with constrained rooftop capacity (using get_rooftop_solar_potential()).
}

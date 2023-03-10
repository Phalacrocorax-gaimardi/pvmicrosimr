% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{get_capacities}
\alias{get_capacities}
\title{get_capacities}
\usage{
get_capacities(abm, groupvars = NULL, house_stock = 1.1)
}
\arguments{
\item{abm}{output of runABM}

\item{groupvars}{grouping variables in addition to simulation number, date}

\item{house_stock}{assumed housing stock (currently static 1.1M)}
}
\value{
dataframe with installed capacities on roof1 and roof2
}
\description{
total (cumulative) installed capacities (solar and battery) by simulation, date and groupvars.
}

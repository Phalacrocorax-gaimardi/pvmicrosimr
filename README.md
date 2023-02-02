
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pvmicrosimr

<!-- badges: start -->
<!-- badges: end -->

pvmicrosimr implements an agent-based-model that simulates the uptake of
rooftop solar PV + battery systems by Irish households.

## Installation

pvmicrosimr is installed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Phalacrocorax-gaimardi/pvmicrosimr")
```

## Example

It is simple to run the ABM. To run 10 simulations from a 2010 start
year to 2030:

``` r
library(pvmicrosimr)
## basic example code
runABM(scenario_wem,Nrun=10, end_year=2030)
```

The scenario dataset *scenario_wem* contains parameter-value pairs that
describe a scenario. This file enables cost and other parameters to be
determined at any time between 2010 and 2050.

``` r
head(pvmicrosimr::scenario_wem,3)
#>   category variable           parameter value unit comment
#> 1   policy    grant sol_lower_threshold     2   kW    <NA>
#> 2   policy    grant sol_upper_threshold     4   kW    <NA>
#> 3   policy    grant     sol_lower_grant   900    \200    <NA>
```

The scenario is typically read from an external excel workbook. For
example,

``` r
scenario_wem <- readxl:read_xlsx9("~/scenarioDesign.xlsx", sheet="scenario_WEM")
```

A scenario describes future policy, market and technical developments.
The scenario file also has historic parameter values.

## Datasets

The model calibration files *empirical_utils* (describing the empirical
utilities based on bi-monthly electricity bills q9_1,social term qsp21
and barrier theta terms) and *agents_init*.

``` r
pvmicrosimr::empirical_utils
#>    code answercode    du_average
#> 1  q9_1          1  0.0013590652
#> 2  q9_1          2  0.0014214514
#> 3  q9_1          3  0.0029869024
#> 4  q9_1          4 -0.0026563731
#> 5  q9_1          5 -0.0040996778
#> 6 qsp21          1 -0.0007532376
#> 7 qsp21          2  0.0009213721
#> 8 qsp21          3  0.0010910509
#> 9 theta         NA -0.1113034664
head(pvmicrosimr::agents_init,3)
#>   ID    w_q9_1   w_qsp21  w_theta q9_1 qsp21
#> 1  1 2.5285629 0.7017370 1.432500    4     2
#> 2  2 1.2123190 0.3140481 1.194532    4     1
#> 3  3 0.4929387 1.7286119 1.525450    3     1
```

The survey questions and answers are described in *pv_qanda* and the
relevant survey data is in *pv_survey_oo*.

A dataset derived from the half-hourly CER smart-meter trial demand data
are contained in *cer_systems*.

*cer_systems* provides annual demand, imported and exported energy for
155 households as a function of solar pv installed on roof 1 and roof 2
of a compound roof for 4 orientations (South-North, SW-NE, SE-NW,
East-West). This currently uses a single solar irradiance dataset from
Birr Co. Offaly. The roof pitch assumed is 30$\degree$. Solar1 and
solar2 increments 0.5kWp up to 12.5kWp then 1KWp thereafter up to 25kWp.
Therefore the max rooftop capacity is 50kWp.

## Model Details

Agents consists of 759 non-apartment own-occupiers. The model start year
is 2010 and the simulation runs at bi-monthly time steps. At each step a
sample of *p.* agents evaluate the utility for installing a solar PV +
battery system. The value of *p.* and the hypothetical bias adjustment
$\lambda.$ is dertmined from caibration to CSO and BER data on household
adoption of PV. An agents utility consists of three terms: weighted
financial partial utility, social partial utility and barrier
(disutility) terms. If this utility is positive the decision is made to
invest in rooftop solar pv.

The empirical partial utility dependence on q9_1 (i.e. demand) is
replaced by
$$\beta \frac{NPV_0-NPV}{NPV_0} + \mathrm{self sufficiency}$$ where
$NPV_0$ is the households estimated net present value of household
electricity bills assuming no investment in solar PV and $NPV$ includes
cashflows associated a decision to invest in solar PV.

At the initialisation of every model run, agents are assigned a
half-hourly demand time-series based on the 2010 CER survey dataset.
This assignment is made by loosely matching household annual demand
determined from the survey (bimonthly bill, 2018) to CER annual demand.
The data file *cer_systems* has a table of solar capacity, battery
capacity imports and exports for each house in the CER control dataset
(133) as a function of solar and battery capacity. The solar PV system
size is given in increments of 0.5kW (roughly one solar panel) while the
battery increment is 2.5 kWh. The NPV associated with each of these
systems can be calculated at a given time given a cost scenario. The
social terms is calculated by assuming that the agents live on a
homophilous social network of average degree 6.

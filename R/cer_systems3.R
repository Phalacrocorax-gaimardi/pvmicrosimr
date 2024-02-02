#' Calculated annual grid imports and exports as a function of installed solar and battery capacity for 155 households
#'
#' solar PV + battery system import and export table for 155 CER control group households. Used to find the financially optimal system.
#'
#' derived from CER 2010 dataset
#'
#' @format A data frame with 3,628,395 rows and 8 variables:
#' \describe{
#'   \item{housecode}{CER household code}
#'   \item{solar1}{installed solar capacity 0-25kW in increments of 0.5kW on first half-roof}
#'  \item{solar2}{installed solar capacity 0-25kW in increments of 0.5kW on second half-roof}
#'   \item{battery}{installed battery capacity 0-100kW in increments of 2.5KWh}
#'   \item{imports}{calculated total imports in kWh}
#'   \item{exports}{calculated total exports in kWh}
#'  \item{aspect}{orientation of house}
#'  \item{demand}{household annual demand (consumption) in kWh}
#'   #' }
#' @source CER, pvWatts (NREL)
"cer_systems3"

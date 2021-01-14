#' Simulated t-distributions to show use of q-q boxplots
#'
#' A dataset that contains simulated data to reproduce the simulated data
#' figures used in our manuscript
#'
#' @format A data frame with 4500 rows and 2 variables:
#' \describe{
#'   \item{y}{a value simulated from a distribution}
#'   \item{group}{a string specifying the distribution from which the y value
#'   is drawn}
#'   ...
#' }
#' @source simulations
"simulated_data"



#' World Bank indicator data for Labor Force participation rates
#'
#' A dataset that contains participation rates (%) for ages 15-24, separated
#' by gender, and measured in the years 2008, 2012, and 2017
#'
#' @format A data frame with 612 rows and 7 variables:
#' \describe{
#'   \item{Country Name}{name of country}
#'   \item{Country Code}{unique country identifier (string)}
#'   \item{Series Name}{Specifies male/female}
#'   \item{Series Code}{unique identifier for series}
#'   \item{year}{year for data}
#'   \item{indicator}{participation rate in percents}
#'   \item{log_indicator}{the log of the participation rate}
#'   ...
#' }
#' @source \url{https://datacatalog.worldbank.org/dataset/world-development-indicators}
"indicators"


#' Neuron spiking data for neural tuning orientation
#'
#' A dataset that contains the number of spikes for neurons across several
#' possible orientations of a grating
#'
#' @format A data frame with 12800 rows and 5 variables:
#' \describe{
#'   \item{orientation}{1 to 8, specifies the orientation of the grating}
#'   \item{nspikes}{number of spikes for a single trial of 1.28 seconds for a particular orientation}
#'   \item{region}{region of the brain where the neuron is located}
#'   ...
#' }
#' @source \url{https://CRCNS.org}
"indicators"


#' Neuron population firing data
#'
#' A dataset that contains populations of neurons from CA1 and LM and their
#' firing rates for three situations: base firing rate, dot motion, and
#' drifting gradient.  Each row represents a neuron
#'
#' @format A data frame with 13731 rows and 3 variables:
#' \describe{
#'   \item{ecephys_structure_acronym}{acronym for population location}
#'   \item{fr_type}{situation under which firing rate was recorded}
#'   \item{rate}{the firing rate}
#'   ...
#' }
#' @source \url{https://allensdk.readthedocs.io/en/latest/visual_coding_neuropixels.html}
"indicators"

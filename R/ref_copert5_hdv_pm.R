############################################
#' @title ref_copert5_hdv_pm
############################################
#'

############################################
# to think about...
# do we want to keep these as internal docs???
# if it goes to vein they'll disappear...
############################################

#' @name ref_copert5_hdv_pm
#' @description look-up table for COPERT5 PM emission function
#' average speed curves for heavy duty vehicles.
#'
#' @format A (3213x16) 'data.frame' object
#' \describe{
#'   \item{vehicle.type}{COPERT classification, primary vehicle description, e.g. bus}
#'   \item{size}{COPERT classification, vehicle size, specific to vehicle.type}
#'   \item{fuel}{COPERT classification, vehicle fuel}
#'   \item{euro.standard}{COPERT classification, vehicle EURO classification, options
#'   PRE, I, II, III, IV, V+EGR, V+SCR or V}
#'   \item{road.gradient}{COPERT classification, slope of road as a proportion, options
#'   -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06}
#'   \item{load}{COPERT classification, load, options 0, 0.5, 1}
#'   \item{a}{COPERT classification, constant}
#'   \item{b}{COPERT classification, constant}
#'   \item{c}{COPERT classification, constant}
#'   \item{d}{COPERT classification, constant}
#'   \item{e}{COPERT classification, constant}
#'   \item{f}{COPERT classification, constant}
#'   \item{g}{COPERT classification, constant}
#'   \item{min.speed.km.h.}{Minimum average curve can be applied to}
#'   \item{max.speed.km.h.}{Maximum average curve can be applied to}
#'   \item{fun}{The COPERT5 }
#' }
#' @source This look-up table is taken from:
#'
#' Dan Wakeling (Ricardo Energy & Environment).
#' Road vehicle emission factors for PM based on COPERT v5.0 (2017):
#' speed-emission factor equations. NAEI Ref:	ED62553001
#' https://naei.beis.gov.uk/data/ef-transport
"ref_copert5_hdv_pm"

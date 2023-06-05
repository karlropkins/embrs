############################################
#' @title bus objects
############################################

#' @name bus.objects
#' @aliases bus_ice bus_bev
#' @description Bus objects for use in __embrs__ models.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param eng.fuel (required character) the engine fuel used by the vehicle,
#' e.g. diesel.
#' @param euro.class (required character) vehicle EURO classification,
#' e.g. PRE, I, II, III, IV, V or VI.
#' @param ... other arguments, passed on to common vehicle build functions.
#' Arguments depend on the vehicle/engine/fuel combination. See embrs.vehicles
#' help for further details.
#' @param veh.type (character) type of vehicle, default bus.
#' @note Work in progress...
#' @returns These functions make bus models for __embrs__ emissions
#' models. The main models bus_ice and bus_bev are internal compression engine
#' and battery electric buses, respectively.
#' @references These functions are based on methods developed and reported by:
#'
#' Beddows, D.C. and Harrison, R.M., 2021. PM10 and PM2.5 emission factors for
#' non-exhaust particles from road vehicles: Dependence upon vehicle mass and
#' implications for battery electric vehicles. Atmospheric Environment, 244,
#' p.117886. \url{https://doi.org/10.1016/j.atmosenv.2020.117886}.
#'
#' And, extrapolated to speed and, break/tyre-wear related emission factors in:
#'
#' Tivey, J., Davies, H.C., Levine, J.G., Zietsman, J., Bartington, S.,
#' Ibarra-Espinosa, S. and Ropkins, K, 2023. Meta-Analysis as Early Evidence on
#' the Particulate Emissions Impact of EURO VI on Battery Electric Bus Fleet
#' Transitions. Sustainability 15, 1522. \url{https://doi.org/10.3390/su15021522}


#note these use local builds in embrs.local

#dropped _beddows, _embrs1, etc.
# now using
# method = "beddows", "embrs1", etc?


############################
#to think about
###########################

#could include a test route that should work for this vehicle?
#   running this would identify any issues before a big build

#make default embrs1 speed model? means all current routes would work

#add bus_hybrid and bus_bifuel



#think about following
################################

#bus_ice as base case
#bus_ice_embrs1 wrapper
#bus_beddows and bus_embrs2 modifications...?

#pass users args to .bus <- bus_ice(...)
#.bus <- embrs_vehicle(x=.bus[[1]][[1]], ****) my settings
#.bus <- embrs_vehicle(x=.bus[[1]][[1]], ...) user settings

#export embrs_vehicle???

#then with the users ... args

#drop bus_dice???


#################################
#buses build
#################################

#################################
#base models
#ice, bev
#################################

#splatted function
#' @rdname bus.objects
#' @export

bus_ice <-
  function(veh.wt = NULL, eng.fuel = NULL, euro.class = NULL, ...,
           veh.type = "bus"){
    if(is.null(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(is.null(eng.fuel) || is.null(euro.class)){
      stop("[embrs] bus_ice...() needs both eng.fuel and euro.class, see help?",
           call.=FALSE)
    }
    embrs_ice(veh.wt = veh.wt, eng.fuel = eng.fuel, euro.class = euro.class,
              ..., veh.type = veh.type)
  }

#splatted function
#' @rdname bus.objects
#' @export

bus_bev <-
  function(veh.wt = NULL, ..., veh.type = "bus"){
    if(is.null(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    embrs_bev(veh.wt = veh.wt, ..., veh.type = veh.type)
  }




###########################
#unexported


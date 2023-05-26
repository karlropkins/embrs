############################################
#' @title bus objects
############################################

#' @name bus.objects
#' @aliases bus_ice_beddows bus_ice bus_dice bus_bev_beddows bus_bev
#' bus_ice_embrs1 bus_bev_embrs1 bus_ice_embrs1 bus_bev_embrs1
#' @description Bus objects for use in __embrs__ models.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param eng.fuel (required character) the fuel used by the bus, currently
#' only diesel.
#' @param euro.class (required character) vehicle EURO classification,
#' can only be PRE, I, II, III, IV, V+EGR, V+SCR or VI.
#' @param n (numeric) number of vehicles, default 1.
#' @param name (character) name of the vehicle object.
#' @param ... other arguments, currently ignored
#' @note These may be moving to vein at some point...
#' @returns These functions make vehicle and fleet class __embrs__ objects.
#' @references These functions are based on methods developed and reported by:
#'
#' Beddows, D.C. and Harrison, R.M., 2021. PM10 and PM2. 5 emission factors for
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


#note embrs_vehicle unexported in embrs.local



#################################
#ice buses
#################################

#splatted function
#' @rdname bus.objects
#' @export
bus_ice_beddows <-
  function(veh.wt, eng.fuel = NULL, euro.class = NULL, n=1, name = NULL, ...){
    #basic build for diesel bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(missing(eng.fuel)){
      stop("[embrs] bus_ice...() needs eng.fuel, see help?",
           call.=FALSE)
    }
    if(missing(euro.class)){
      stop("[embrs] bus_ice...() needs euro.class, see help?",
           call.=FALSE)
    }

    obj <- list(
      args = list(
        n = n,
        veh.type = "bus",
        veh.wt = veh.wt,
        eng.type = "ice",
        eng.fuel = eng.fuel,
        euro.class = euro.class,
        brk.regen = FALSE
      ),
      funs = list(
        ef.exh.pm2.5 = ef_eea2019_exh_pm2.5,
        ef.brake.pm2.5 = ef_beddows_brake_pm2.5,
        ef.tyre.pm2.5 = ef_beddows_tyre_pm2.5,
        ef.road.pm2.5 = ef_beddows_road_pm2.5,
        ef.resusp.pm2.5 = ef_beddows_resusp_pm2.5,
        ef.exh.pm10 = ef_eea2019_exh_pm10,
        ef.brake.pm10 = ef_beddows_brake_pm10,
        ef.tyre.pm10 = ef_beddows_tyre_pm10,
        ef.road.pm10 = ef_beddows_road_pm10,
        ef.resusp.pm10 = ef_beddows_resusp_pm10
      )
    )
    ######################
    #failed!
    #trying to try the warnings
    #############################
    #last.warning <- last.warning[!duplicated(last.warning)]
    embrs_vehicle(name=name, x=obj, ...)
  }

#splatted function
#' @rdname bus.objects
#' @export
bus_ice <- function(...) bus_ice_beddows(...)

#splatted function
#' @rdname bus.objects
#' @export
bus_dice <-
  function(veh.wt, eng.fuel = "diesel", ...){
    #basic build for diesel bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    bus_ice_beddows(veh.wt = veh.wt, eng.fuel = eng.fuel, ...)
  }

#' @rdname bus.objects
#' @export
bus_ice_embrs1 <-
  function(veh.wt, ...){
    #basic build for embrs1 ice bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    x.args <- list(...)
    x.args$veh.wt <- veh.wt
    x.args <- embrs_ef.embrs1_reset(x.args)
    do.call(bus_ice_beddows, x.args)
  }

#' @rdname bus.objects
#' @export
bus_ice_embrs2 <-
  function(veh.wt, ...){
    #basic build for embrs1 ice bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    x.args <- list(...)
    x.args$veh.wt <- veh.wt
    x.args <- embrs_ef.embrs2_reset(x.args)
    do.call(bus_ice_beddows, x.args)
  }


#####################################
#bev buses
#####################################


#splatted function
#' @rdname bus.objects
#' @export

bus_bev_beddows <-
  function(veh.wt, n=1, name = NULL, ...){
    #basic build for battery electric bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    obj <- list(
      args = list(
        n = n,
        veh.type = "bus",
        veh.wt = veh.wt,
        eng.type = "electric",
        eng.fuel = "battery",
        brk.regen = FALSE
      ),
      funs = list(
        ef.brake.pm2.5 = ef_beddows_brake_pm2.5,
        ef.tyre.pm2.5 = ef_beddows_tyre_pm2.5,
        ef.road.pm2.5 = ef_beddows_road_pm2.5,
        ef.resusp.pm2.5 = ef_beddows_resusp_pm2.5,
        ef.brake.pm10 = ef_beddows_brake_pm10,
        ef.tyre.pm10 = ef_beddows_tyre_pm10,
        ef.road.pm10 = ef_beddows_road_pm10,
        ef.resusp.pm10 = ef_beddows_resusp_pm10
      )
    )
    embrs_vehicle(name=name, x=obj, ...)
  }


#splatted function
#' @rdname bus.objects
#' @export
bus_bev <- function(...) bus_bev_beddows(...)

#' @rdname bus.objects
#' @export
bus_bev_embrs1 <-
  function(veh.wt, ...){
    #basic build for embrs1 ice bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    x.args <- list(...)
    x.args$veh.wt <- veh.wt
    x.args <- embrs_ef.embrs1_reset(x.args)
    do.call(bus_bev_beddows, x.args)
  }

#' @rdname bus.objects
#' @export
bus_bev_embrs2 <-
  function(veh.wt, ...){
    #basic build for embrs1 ice bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    x.args <- list(...)
    x.args$veh.wt <- veh.wt
    x.args <- embrs_ef.embrs2_reset(x.args)
    do.call(bus_bev_beddows, x.args)
  }




##########################
#unexported
##########################

embrs_ef.embrs1_reset <- function(x.args){

  if(!"ef.brake.pm2.5" %in% names(x.args)){
    x.args$ef.brake.pm2.5 <- ef_embrs1_brake_pm2.5
  }
  if(!"ef.tyre.pm2.5" %in% names(x.args)){
    x.args$ef.tyre.pm2.5 <- ef_embrs1_tyre_pm2.5
  }
  if(!"ef.road.pm2.5" %in% names(x.args)){
    x.args$ef.road.pm2.5 <- ef_embrs1_road_pm2.5
  }
  if(!"ef.resusp.pm2.5" %in% names(x.args)){
    x.args$ef.resusp.pm2.5 <- ef_embrs1_resusp_pm2.5
  }
  if(!"ef.brake.pm10" %in% names(x.args)){
    x.args$ef.brake.pm10 <- ef_embrs1_brake_pm10
  }
  if(!"ef.tyre.pm10" %in% names(x.args)){
    x.args$ef.tyre.pm10 <- ef_embrs1_tyre_pm10
  }
  if(!"ef.road.pm10" %in% names(x.args)){
    x.args$ef.road.pm10 <- ef_embrs1_road_pm10
  }
  if(!"ef.resusp.pm10" %in% names(x.args)){
    x.args$ef.resusp.pm10 <- ef_embrs1_resusp_pm10
  }
  x.args
}

embrs_ef.embrs2_reset <- function(x.args){

  if(!"ef.brake.pm2.5" %in% names(x.args)){
    x.args$ef.brake.pm2.5 <- ef_embrs2_brake_pm2.5
  }
  if(!"ef.tyre.pm2.5" %in% names(x.args)){
    x.args$ef.tyre.pm2.5 <- ef_embrs2_tyre_pm2.5
  }
  if(!"ef.road.pm2.5" %in% names(x.args)){
    x.args$ef.road.pm2.5 <- ef_embrs1_road_pm2.5
  }
  if(!"ef.resusp.pm2.5" %in% names(x.args)){
    x.args$ef.resusp.pm2.5 <- ef_embrs1_resusp_pm2.5
  }
  if(!"ef.brake.pm10" %in% names(x.args)){
    x.args$ef.brake.pm10 <- ef_embrs2_brake_pm10
  }
  if(!"ef.tyre.pm10" %in% names(x.args)){
    x.args$ef.tyre.pm10 <- ef_embrs2_tyre_pm10
  }
  if(!"ef.road.pm10" %in% names(x.args)){
    x.args$ef.road.pm10 <- ef_embrs1_road_pm10
  }
  if(!"ef.resusp.pm10" %in% names(x.args)){
    x.args$ef.resusp.pm10 <- ef_embrs1_resusp_pm10
  }

  x.args
}

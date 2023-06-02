############################################
#' @title bus objects
############################################

#' @name bus.objects
#' @aliases bus_ice bus_bev bus_ice_beddows bus_bev_beddows
#' bus_ice_embrs1 bus_bev_embrs1 bus_ice_embrs1 bus_bev_embrs1
#' @description Bus objects for use in __embrs__ models.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param eng.fuel (required character) the fuel used by the bus, currently
#' only diesel.
#' @param euro.class (required character) vehicle EURO classification,
#' e.g. PRE, I, II, III, IV, V or VI.
#' @param n (numeric) number of vehicles, default 1.
#' @param name (character) name of the vehicle object.
#' @param ... other arguments, currently ignored
#' @note Work in progress...
#' @returns These functions make bus model for use in __embrs__ emissions
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


#note embrs_vehicle unexported in embrs.local

############################
#to think about
###########################

#could include a test route that should work for this vehicle?
#   running this would identify any issues before a big build

#make default embrs1 speed model? means all current routes would work

#add bus_hybrid and bus_bifuel
#would a model argument be easier
#base.model = "beddows", "embrs1", etc?


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
#ice buses
#################################


#splatted function
#' @rdname bus.objects
#' @export

bus_ice <-
  function(veh.wt = NULL, eng.fuel = NULL, euro.class = NULL, n = 1, name = NULL, ...){
    #default ice bus build for bus model
    #(bus_ice_embrs1)
    if(is.null(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(is.null(eng.fuel) || is.null(euro.class)){
      stop("[embrs] bus_ice...() needs both eng.fuel and euro.class, see help?",
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
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_embrs1_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs1_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_embrs1_brake_pm10,
        ef.tyre.pm10 = ef_embrs1_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    ######################
    #failed!
    # trying to reduce the number of
    # warnings when missing args trigger
    # a warning
    #############################
    #last.warning <- last.warning[!duplicated(last.warning)]
    embrs_vehicle(name=name, x=obj, ...)
  }

#splatted function
#' @rdname bus.objects
#' @export
bus_ice_beddows <- function(...){
  .bus <- bus_ice(...)
  .bus[[1]][[1]]$funs$ef.brake.pm2.5 <- ef_beddows_brake_pm2.5
  .bus[[1]][[1]]$funs$ef.tyre.pm2.5 <- ef_beddows_tyre_pm2.5
  .bus[[1]][[1]]$funs$ef.road.pm2.5 <- ef_beddows_road_pm2.5
  .bus[[1]][[1]]$funs$ef.resusp.pm2.5 <- ef_beddows_resusp_pm2.5
  .bus[[1]][[1]]$funs$ef.brake.pm10 <- ef_beddows_brake_pm10
  .bus[[1]][[1]]$funs$ef.tyre.pm10 <- ef_beddows_tyre_pm10
  .bus[[1]][[1]]$funs$ef.road.pm10 <- ef_beddows_road_pm10
  .bus[[1]][[1]]$funs$ef.resusp.pm10 <- ef_beddows_resusp_pm10
  embrs_vehicle(x=.bus[[1]][[1]], ...)
}


#splatted function
#' @rdname bus.objects
#' @export

bus_ice_embrs1 <- function(...){
  bus_ice(...)
}

#splatted function
#' @rdname bus.objects
#' @export
bus_ice_embrs2 <- function(...){
  .bus <- bus_ice(...)
  .bus[[1]][[1]]$funs$ef.brake.pm2.5 <- ef_embrs2_brake_pm2.5
  .bus[[1]][[1]]$funs$ef.brake.pm10 <- ef_embrs2_brake_pm10
  .bus[[1]][[1]]$funs$ef.tyre.pm2.5 <- ef_embrs2_tyre_pm2.5
  .bus[[1]][[1]]$funs$ef.tyre.pm10 <- ef_embrs2_tyre_pm10
  embrs_vehicle(x=.bus[[1]][[1]], ...)
}



bus_bev <-
  function(veh.wt = NULL, n = 1, name = NULL, ...){
    #default electric bus build for bus model
    #(bus_ice_embrs1) without the exhaust emissions

    if(is.null(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    ############################
    #to think about
    ############################
    #error if fuel not electric?
    #euro.class might be trickier?

    obj <- list(
      args = list(
        n = n,
        veh.type = "bus",
        veh.wt = veh.wt,
        eng.type = "electric",
        eng.fuel = "electric",
        brk.regen = FALSE
      ),
      funs = list(
        ef.brake.pm2.5 = ef_embrs1_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs1_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.brake.pm10 = ef_embrs1_brake_pm10,
        ef.tyre.pm10 = ef_embrs1_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    ######################
    #failed!
    # trying to reduce the number of
    # warnings when missing args trigger
    # a warning
    #############################
    #last.warning <- last.warning[!duplicated(last.warning)]
    embrs_vehicle(name=name, x=obj, ...)
  }

#splatted function
#' @rdname bus.objects
#' @export
bus_bev_beddows <- function(...){
  .bus <- bus_bev(...)
  .bus[[1]][[1]]$funs$ef.brake.pm2.5 <- ef_beddows_brake_pm2.5
  .bus[[1]][[1]]$funs$ef.tyre.pm2.5 <- ef_beddows_tyre_pm2.5
  .bus[[1]][[1]]$funs$ef.road.pm2.5 <- ef_beddows_road_pm2.5
  .bus[[1]][[1]]$funs$ef.resusp.pm2.5 <- ef_beddows_resusp_pm2.5
  .bus[[1]][[1]]$funs$ef.brake.pm10 <- ef_beddows_brake_pm10
  .bus[[1]][[1]]$funs$ef.tyre.pm10 <- ef_beddows_tyre_pm10
  .bus[[1]][[1]]$funs$ef.road.pm10 <- ef_beddows_road_pm10
  .bus[[1]][[1]]$funs$ef.resusp.pm10 <- ef_beddows_resusp_pm10
  embrs_vehicle(x=.bus[[1]][[1]], ...)
}


#splatted function
#' @rdname bus.objects
#' @export

bus_bev_embrs1 <- function(...){
  bus_bev(...)
}

#splatted function
#' @rdname bus.objects
#' @export
bus_bev_embrs2 <- function(...){
  .bus <- bus_bev(...)
  .bus[[1]][[1]]$funs$ef.brake.pm2.5 <- ef_embrs2_brake_pm2.5
  .bus[[1]][[1]]$funs$ef.brake.pm10 <- ef_embrs2_brake_pm10
  .bus[[1]][[1]]$funs$ef.tyre.pm2.5 <- ef_embrs2_tyre_pm2.5
  .bus[[1]][[1]]$funs$ef.tyre.pm10 <- ef_embrs2_tyre_pm10
  embrs_vehicle(x=.bus[[1]][[1]], ...)
}


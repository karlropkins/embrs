############################################
#' @title bus objects
############################################

#' @name bus.objects
#' @aliases bus_ice bus_dice bus_bev bus_ice_beddows bus_bev_beddows
#' bus_ice_embrs1 bus_bev_embrs1 bus_ice_embrs1 bus_bev_embrs1
#' @description Bus objects for use in __embrs__ models.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param eng.fuel (required character) the fuel used by the bus, currently
#' only diesel.
#' @param euro.class (required character) vehicle EURO classification,
#' can only be PRE, I, II, III, IV, V+EGR, V+SCR or VI.
#' @param n (numeric) number of vehicles, defualt 1.
#' @param name (character) name of the vehicle object.
#' @param ... other arguments, currently ignored
#' @param x (for \code{embrs_vehicle}) a list of args to
#' using when building an __embrs__ vehicle/fleet object.
## #' @returns These functions make vehicle and fleet class __embrs__ objects.
#' @note These may be moving to vein at some point...
#' @references [add embrs and Beddows references...]

###############################
#testing embrs_vehicle



################################
#embrs_vehicle
#workhorse function for making
#vehicle/fleet objects
################################

embrs_vehicle <-
  function(name = NULL, x = NULL, ...){

    .obj <- list(...)
#print(.obj)
    if(is.null(x)){
      stop("nothing to work with!")
    }
    obj <- list(args=list(),
                funs=list()
    ) # thinking of adding docs as extra case
    #separate all functions start ef.
    test <- grep("^ef[.]", tolower(names(.obj)))
    .ef <- if(length(test)>0) {
      .obj[test]
    } else {
      list()
    }
    test <- which(!1:length(.obj) %in% test)
    .obj <- .obj[test]
    #x args and funs to output
    if(is.list(x$args)){
      obj$args <- modifyList(obj$args, x$args)
    }
    if(is.list(x$funs)){
      obj$funs <- modifyList(obj$funs, x$funs)
    }
    #... args and .ef to output
    if(is.list(.obj)){
      obj$args <- modifyList(obj$args, .obj)
    }
    if(is.list(.ef)){
      obj$funs <- modifyList(obj$funs, .ef)
    }

    name <- if(is.null(name)){
      paste(obj$args$eng.fuel,  obj$args$eng.type, obj$args$veh.type, sep=".")
    } else { as.character(name) }
    obj$args$name <- name

    out <- obj
    class(out) <- "embrs_vehicle"
    out <- list(out)
    names(out)[1] <- out[[1]]$args$name
    out <- list(fleet=out)
    class(out) <- c("embrs", "fleet")
    out
  }


#splatted function
#' @rdname bus.objects
#' @export
bus_bev <- function(...) bus_bev_beddows(...)

#splatted function
#' @rdname bus.objects
#' @export
bus_ice <- function(...) bus_ice_beddows(...)

#splatted function
#' @rdname bus.objects
#' @export
bus_dice <-
  function(veh.wt, eng.fuel = NULL, ...){
    #basic build for diesel bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(missing(eng.fuel)){
      eng.fuel <- "diesel"
    }
    bus_ice_beddows(veh.wt = veh.wt, eng.fuel = eng.fuel, ...)
}

#################################
#beddows buses
#################################


#splatted function
#' @rdname bus.objects
#' @export
bus_ice_beddows <-
  function(veh.wt, euro.class = NULL, eng.fuel = NULL, n=1, name = NULL, ...){
    #basic build for diesel bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(missing(euro.class)){
      stop("[embrs] bus_ice...() needs euro.class, see help?",
           call.=FALSE)
    }
    if(missing(eng.fuel)){
      stop("[embrs] bus_ice...() needs eng.fuel, see help?",
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
    embrs_vehicle(name=name, x=obj, ...)
  }


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



#################################
#embrs1 buses
#################################

#' @rdname bus.objects
#' @export
bus_ice_embrs1 <-
  function(veh.wt, euro.class = NULL, eng.fuel = NULL, n=1, name = NULL, ...){
    #basic build for battery electric bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(missing(euro.class)){
      stop("[embrs] bus_ice...() needs euro.class, see help?",
           call.=FALSE)
    }
    if(missing(eng.fuel)){
      stop("[embrs] bus_ice...() needs eng.fuel, see help?",
           call.=FALSE)
    }

    obj <- list(
      args = list(
        n = n,
        veh.type = "bus",
        veh.wt = veh.wt,
        eng.type = "ice",
        eng.fuel = "diesel",
        euro.class = euro.class,
        brk.regen = FALSE
      ),
      funs = list(
        ef.exh.pm2.5 = ef_eea2019_exh_pm2.5,
        ef.brake.pm2.5 = ef_embrs1_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs1_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.exh.pm10 = ef_eea2019_exh_pm10,
        ef.brake.pm10 = ef_embrs1_brake_pm10,
        ef.tyre.pm10 = ef_embrs1_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    embrs_vehicle(name=name, x=obj, ...)
  }

#' @rdname bus.objects
#' @export
bus_bev_embrs1 <-
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
        #no exhaust electric
        #ef.exh.pm2.5 = ef_emep_exh_pm2.5,
        ef.brake.pm2.5 = ef_embrs1_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs1_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        #ef.exh.pm10 = ef_emep_exh_pm10,
        ef.brake.pm10 = ef_embrs1_brake_pm10,
        ef.tyre.pm10 = ef_embrs1_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    embrs_vehicle(name=name, x=obj, ...)
  }


#' @rdname bus.objects
#' @export
bus_ice_embrs2 <-
  function(veh.wt, euro.class = NULL, eng.fuel = NULL, n=1, name = NULL, ...){
    #basic build for battery electric bus model
    if(missing(veh.wt)){
      stop("[embrs] bus...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(missing(euro.class)){
      stop("[embrs] bus_ice...() needs euro.class, see help?",
           call.=FALSE)
    }
    if(missing(eng.fuel)){
      stop("[embrs] bus_ice...() needs eng.fuel, see help?",
           call.=FALSE)
    }

    obj <- list(
      args = list(
        n = n,
        veh.type = "bus",
        veh.wt = veh.wt,
        eng.type = "ice",
        eng.fuel = "diesel",
        euro.class = euro.class,
        brk.regen = FALSE
      ),
      funs = list(
        #ef.exh.pm2.5 = ef_copert5_exh_pm2.5,
        ef.exh.pm2.5 = ef_eea2019_exh_pm2.5,
        ef.brake.pm2.5 = ef_embrs2_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs2_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        #ef.exh.pm10 = ef_copert5_exh_pm10,
        ef.exh.pm10 = ef_eea2019_exh_pm10,
        ef.brake.pm10 = ef_embrs2_brake_pm10,
        ef.tyre.pm10 = ef_embrs2_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    embrs_vehicle(name=name, x=obj, ...)
  }

#' @rdname bus.objects
#' @export
bus_bev_embrs2 <-
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
        #no exhaust electric
        #ef.exh.pm2.5 = ef_emep_exh_pm2.5,
        ef.brake.pm2.5 = ef_embrs2_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs2_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        #ef.exh.pm10 = ef_emep_exh_pm10,
        ef.brake.pm10 = ef_embrs2_brake_pm10,
        ef.tyre.pm10 = ef_embrs2_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    embrs_vehicle(name=name, x=obj, ...)
  }

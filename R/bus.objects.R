############################################
#' @title bus objects
############################################

#' @name bus.objets
#' @aliases bus_ice bus_bev
#' @description Bus objects for use in __embrs__ models.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param n (numeric) number of vehicles, defualt 1.
#' @param name (character) name of the vehicle object.
#' @param ... other arguments, currently ignored
#' @param x (for workhorse only) a list of args to using when building
#' an __embrs__ object.
## #' @returns These functions make vehicle and fleet class __embrs__ objects.
#' @note These may be moving to vein at some point...
#' @references add embrs and Beddows references...

###############################
#test vehicle_workshore

#next jobs
#use vein bus EMEP/EEA method to add
#ef.exh.pm function ef_emep_exh_pm (pm=pm10=pm2.5)
#dplyr::filter(vein:::sysdata$eea, Category == "BUS" & Pollutant=="PM" & Fuel=="D" & RoadSlope==0 & EuroStandard=="VI" & Load ==0.5, Segment=="Urban Buses Midi <=15 t")
#vein::ef_hdv_speed(v = "Ubus", eu="V", p= "PM", t="Midi", g="<=15")(30)
#vein::ef_hdv_speed(v = "Ubus", eu="VI", p= "PM", t="Midi", g="<=15")(30)
#last errors!!!

#speed lookup table
#naei_route2spd (ricardo method)
#have this as excel


#splatted function
#' @rdname bus.objects
#' @export
bus_ice <-
  function(veh.wt, n=1, name = NULL, ...){
    #basic build for battery electric bus model
    if(missing(veh.wt)){
      stop("need veh.wt")
    }
    obj <- list(
      args = list(
        n = n,
        veh.type = "bus",
        veh.wt = veh.wt,
        eng.type = "ice",
        eng.fuel = "diesel",
        brk.regen = FALSE
      ),
      funs = list(
        ef.exh.pm2.5 = ef_emep_exh_pm2.5,
        ef.brake.pm2.5 = ef_beddows_brake_pm2.5,
        ef.tyre.pm2.5 = ef_beddows_tyre_pm2.5,
        ef.road.pm2.5 = ef_beddows_road_pm2.5,
        ef.resusp.pm2.5 = ef_beddows_resusp_pm2.5,
        ef.exh.pm10 = ef_emep_exh_pm10,
        ef.brake.pm10 = ef_beddows_brake_pm10,
        ef.tyre.pm10 = ef_beddows_tyre_pm10,
        ef.road.pm10 = ef_beddows_road_pm10,
        ef.resusp.pm10 = ef_beddows_resusp_pm10
      )
    )
    vehicle_workhorse(name=name, x=obj, ...)
  }


#splatted function
#' @rdname bus.objects
#' @export

bus_bev <-
  function(veh.wt, n=1, name = NULL, ...){
    #basic build for battery electric bus model
    if(missing(veh.wt)){
      stop("need veh.wt")
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
    vehicle_workhorse(name=name, x=obj, ...)
  }


#splatted function
#' @rdname bus.objects
#' @export

vehicle_workhorse <-
  function(name = NULL, x = NULL, ...){

    .obj <- list(...)
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




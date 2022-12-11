############################################
#' @title bus objects
############################################

#' @name bus.objets
#' @aliases bus_ice
#' @description Bus objects for use in __embrs__ models.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param n (numeric) number of vehicles, defualt 1.
#' @param name (character) name of the vehicle object.
#' @param ... other arguments, currently ignored
## #' @returns These functions make vehicle and fleet class embrs objects.
#' @note These may be moving to vein at some point...
#' @references add embrs and Beddows references...

###############################
#if workhorses work for routes
#think about same here????
#

#splatted function
#' @rdname bus.objects
#' @export
bus_ice <-
function(veh.wt, n=1, name = NULL, ...){
  #basic build for on internal combustion engine bus model
  if(missing(veh.wt)){
    stop("need veh.wt")
  }
  .obj <- list(...)
  obj <- list(
    args = list(
      n = n,
      veh.type = "bus",
      veh.wt = veh.wt,
      eng.type = "ice",
      eng.fuel = "diesel"
    ),
    funs = list(
      ef.brake.pm2.5 = ef_beddows_brake_pm2.5,
      ef.tyre.pm2.5 = ef_beddows_tyre_pm2.5,
      ef.road.pm2.5 = ef_beddows_road_pm2.5,
      ef.resusp.pm2.5 = ef_beddows_resusp_pm2.5 #,
      #ef.brake.pm10 = ef_beddows_brake_pm10,
      #ef.tyre.pm10 = ef_beddows_tyre_pm10,
      #ef.road.pm10 = ef_beddows_road_pm10,
      #ef.resusp.pm10 = ef_beddows_resusp_pm10
    )
    #doc to think about
  )
  #add name to object
  #might not be right way to do this,,,
  name <- if(is.null(name)){
    paste(obj$args$eng.fuel,  obj$args$eng.type, obj$args$veh.type, sep=".")
  } else { as.character(name) }
  obj$args$name <- name
  out <- modifyList(obj, .obj)
  class(out) <- "embrs_vehicle"
  out <- list(out)
  names(out)[1] <- out[[1]]$args$name
  out <- list(fleet=out)
  class(out) <- c("embrs", "fleet")
  out
}


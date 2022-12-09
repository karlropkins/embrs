############################################
#' @title vehicle class
############################################

#' @name vehicle.class
#' @aliases bus_ice bus_bev
#' @description Functions to build vehicle objects for use in embrs emission
#' models.
#' @param veh.wt (required numeric) vehicle weight in kg.
#' @param veh.wt (numeric) number of vehicles, by default 1.
#' @returns These functions make vehicle class objects, which .
#' @note These may be moving to vein at some point...
#' @references add embrs and Beddows references...

#quick build function
#' @rdname vehicle.class
#' @export
bus_ice <- function(veh.wt, n=1, ...){
  #basic build for on internal combustion engine bus model
  if(missing(veh.wt)){
    stop("need veh.wt")
  }
  .obj <- list(...)
  obj <- list(
    n = n,
    veh.type = "bus",
    veh.wt = veh.wt,
    eng.type = "ice",
    eng.fuel = "diesel",
    ef.brake.pm = ef_beddows_brake_pm,
    ef.tyre.pm = ef_beddows_tyre_pm,
    ef.road.pm = ef_beddows_road_pm,
    ef.resusp.pm = ef_beddows_resusp_pm
    #doc to think about
  )
  out <- modifyList(obj, .obj)
  class(out) <- "embrs_veh"
  out <- list(out)
  class(out) <- "embrs_fleet"
  out
}



#' @rdname vehicle.class
#' @method print embrs_veh
#' @export
print.embrs_veh <-
  function(x, ...){
    ans <- paste("embrs: vehicle\n",
                 "       ", x$n, " x ", x$veh.type, sep="")
    cat(ans, "\n")
    invisible(x)
  }


#' @rdname vehicle.class
#' @method print embrs_fleet
#' @export
print.embrs_fleet <-
  function(x, ...){
    cat("embrs: fleet\n")
    for(i in 1:length(x)){
      cat("       ", x[[i]]$n, " x ", x[[1]]$veh.type, "\n", sep="")
    }
    invisible(x)
  }






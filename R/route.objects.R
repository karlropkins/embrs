############################################
#' @title route.objects
############################################

#' @name route.objects
#' @aliases route_veh_spd route_naei_rural route_naei_urban
#' route_naei_motorway route_naei_urm
#' @description Route objects for use in embrs models.
#' @param veh.spd (numeric or function) the vehicle speed in km/hr.
#' @param name (character) optional object name, by default the route source and type.
#' @param route.def (character) required route description.
#' @param route.source (character) required source of route model.
#' @param route.dist (numeric) route or journey distance in km, default 1.
#' @param route.slope (numeric) route or journey slope, default 0.
#' @param ... other arguments, currently ignored.
## #' @returns These functions make route class embrs objects.
#' @note trying to streamline these, so likely to be subject to change
## #' @references [to do] add embrs and naei references...


########################
#to think about
########################

#think about naming
#if declared in call not handled nicely when summing...
#   local fix or
#   make a names function for embrs objects???

# simple example to do???

#splatted function
#' @rdname route.objects
#' @export
route_veh_spd <- function(veh.spd=NULL, name=NULL, route.def=NULL,
                          route.source=NULL, ...){
  if(is.null(veh.spd)){
    stop("[embrs] route_veh_spd() needs vehicle speed (veh.spd), see help?",
         call.=FALSE)
  }
  if(is.null(name)){
    name <- paste("veh.spd:", veh.spd, "km/hr", sep="")
  }
  if(is.null(route.def)){
    route.def <- paste("spd.", veh.spd, sep="")
  }
  if(is.null(route.source)){
    route.source <- "[user]"
  }
  embrs_route(name=name, route.def=route.def, route.source = route.source,
              veh.spd=veh.spd, ...)
}


#splatted function
#' @rdname route.objects
#' @export
route_naei_rural <- function(name=NULL, route.def="rural", route.source="uk naei",
                             veh.spd=naei_route2spd, ...){
  embrs_route(name=name, route.def=route.def, route.source = route.source,
                  veh.spd=veh.spd, ...)
}

#splatted function
#' @rdname route.objects
#' @export
route_naei_urban <- function(name=NULL, route.def="urban", route.source="uk naei",
                             veh.spd=naei_route2spd, ...){
  embrs_route(name=name, route.def=route.def, route.source = route.source,
                  veh.spd=veh.spd,...)
}

#splatted function
#' @rdname route.objects
#' @export
route_naei_motorway <- function(name=NULL, route.def="motorway", route.source="uk naei",
                                veh.spd=naei_route2spd, ...){
  embrs_route(name=name, route.def=route.def, route.source = route.source,
                  veh.spd=veh.spd, ...)
}


#splatted function
#' @rdname route.objects
#' @export
route_naei_urm <-
  function(...){
    route_naei_urban(...) + route_naei_rural(...) + route_naei_motorway(...)
  }

###############
#this can't handle name nicely...
#move name up into formals???


#splatted function
#' @rdname route.objects
#' @export
route_ukbc_outerlondon <- function(name=NULL, route.def="outer london",
                             route.source="ukbc", veh.spd=ukbc_route2spd,
                             brk_b=ukbc_route2brk_b,
                             tyr_t=ukbc_route2tyr_t,
                             ...){
  embrs_route(name=name, route.def=route.def, route.source = route.source,
              veh.spd=veh.spd, brk_b=brk_b, tyr_t=tyr_t, ...)
}

#splatted function
#' @rdname route.objects
#' @export
route_ukbc_innerlondon <- function(name=NULL, route.def="inner london",
                                   route.source="ukbc", veh.spd=ukbc_route2spd,
                                   brk_b=ukbc_route2brk_b,
                                   tyr_t=ukbc_route2tyr_t,
                                   ...){
  embrs_route(name=name, route.def=route.def, route.source = route.source,
              veh.spd=veh.spd, brk_b=brk_b, tyr_t=tyr_t, ...)
}

#splatted function
#' @rdname route.objects
#' @export
route_ukbc_rural <- function(name=NULL, route.def="rural", route.source="ukbc",
                             veh.spd=ukbc_route2spd,
                             brk_b=ukbc_route2brk_b,
                             tyr_t=ukbc_route2tyr_t,
                             ...){
  embrs_route(name=name, route.def=route.def, route.source = route.source,
              veh.spd=veh.spd, brk_b=brk_b, tyr_t=tyr_t, ...)
}


#splatted function
#' @rdname route.objects
#' @export
route_ukbc_oir <-
  function(...){
    route_ukbc_outerlondon(...) + route_ukbc_innerlondon(...) + route_ukbc_rural(...)
  }


######################
#see route_naei_urm
#  about naming



################################
#unexported functions
###############################

#embrs_route
#workhorse for route objects

embrs_route <-
  function(name = NULL, route.def = NULL, route.source = NULL,
           route.dist = 1, route.slope = 0, ...){
    if(is.null(route.def) || is.null(route.source)){
      stop("[embrs] route_...() needs route.def and route.source [see help]",
           call. = FALSE)
    }
    obj <- list(args=list(name=name,
                          route.def=route.def,
                          route.source=route.source,
                          route.dist=route.dist,
                          route.slope=route.slope,
                          ...))
    if(is.null(obj$args$name)){
      obj$args$name <- paste(obj$args$route.source,  obj$args$route.def, sep=".")
    }
    class(obj) <- "embrs_route"
    out <- list(obj)
    names(out)[1] <- out[[1]]$args$name
    out <- list(routes=out)
    class(out) <- c("embrs", "routes")
    out
  }

############################################
#' @title route.objects
############################################

#' @name route.objects
#' @aliases route_naei_rural route_naei_urban route_naei_motorway
#' route_naei_urm
#' @description Route objects for use in embrs models.
#' @param name (character) optional object name, by default the route source and type.
#' @param route.def (character) required route description.
#' @param route.source (character) require source of route model.
#' @param route.dist (numeric) the route distance in km, by default 1.
#' @param ... other arguments, currently ignored.
## #' @returns These functions make vehicle and fleet class embrs objects.
#' @note trying to streamline these, so likely to be be subject to change
## #' @references [to do] add embrs and naei references...

#############################
#testing route_workhorse
#and route functions built using that...
#############################

route_workhorse <-
  function(name = NULL, route.def = NULL, route.source = NULL,
           route.dist = 1, ...){
    if(is.null(route.def) || is.null(route.source)){
      stop("route_... functions needs route.def and route.source [see help]")
    }
    obj <- list(args=list(name=name,
                         route.def=route.def,
                         route.source=route.source,
                         route.dist=route.dist,
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

#splatted function
#' @rdname route.objects
#' @export
route_naei_rural <- function(name=NULL, route.def="rural", route.source="uk naei",
                       ...){
  route_workhorse(name=name, route.def=route.def, route.source = route.source, ...)
}

#splatted function
#' @rdname route.objects
#' @export
route_naei_urban <- function(name=NULL, route.def="urban", route.source="uk naei",
                             ...){
  route_workhorse(name=name, route.def=route.def, route.source = route.source, ...)
}

#splatted function
#' @rdname route.objects
#' @export
route_naei_motorway <- function(name=NULL, route.def="motorway", route.source="uk naei",
                             ...){
  route_workhorse(name=name, route.def=route.def, route.source = route.source, ...)
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


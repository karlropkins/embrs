############################################
#' @title route.objects
############################################

#' @name route.objects
#' @aliases route_naei_rural route_naei_urban route_naei_motorway
#' route_naei_rum
#' @description Route objects for use in embrs models.
#' @param ... additional arguments, currently ignored.
#' @param ... other arguments, currently ignored
## #' @returns These functions make vehicle and fleet class embrs objects.
#' @note These may be moving to vein at some point...
#' @references add embrs and naei references...

###########################
#all routes are the same expect name and args
#make workhorse
#and route functions via that...
#############################


#splatted function
#' @rdname route.objects
#' @export
route_naei_rural <-
function(name = NULL, ...){
  #basic build for on internal combustion engine bus model
  .obj <- list(...)
  obj <- list(
    args = list(
      route.def = "rural",
      route.source = "uk naei"
    )
    #doc to think about
  )
  name <- if(is.null(name)){
    paste(obj$args$route.source,  obj$args$route.def, sep=".")
  } else { as.character(name) }
  name <- if(is.null(name)){
    paste(obj$args$route.source,  obj$args$route.def, sep=".")
  } else { as.character(name) }
  obj$args$name <- name
  out <- modifyList(obj, .obj)
  class(out) <- "embrs_route"
  out <- list(out)
  names(out)[1] <- out[[1]]$args$name
  out <- list(routes=out)
  class(out) <- c("embrs", "routes")
  out
}


#splatted function
#' @rdname route.objects
#' @export
route_naei_urban <-
  function(name = NULL, ...){
    #basic build for on internal combustion engine bus model
    .obj <- list(...)
    obj <- list(
      args = list(
        route.def = "urban",
        route.source = "uk naei"
      )
      #doc to think about
    )
    name <- if(is.null(name)){
      paste(obj$args$route.source,  obj$args$route.def, sep=".")
    } else { as.character(name) }
    name <- if(is.null(name)){
      paste(obj$args$route.source,  obj$args$route.def, sep=".")
    } else { as.character(name) }
    obj$args$name <- name
    out <- modifyList(obj, .obj)
    class(out) <- "embrs_route"
    out <- list(out)
    names(out)[1] <- out[[1]]$args$name
    out <- list(routes=out)
    class(out) <- c("embrs", "routes")
    out
  }


#' @rdname route.objects
#' @export
route_naei_motorway <-
  function(name = NULL, ...){
    #basic build for on internal combustion engine bus model
    .obj <- list(...)
    obj <- list(
      args = list(
        route.def = "motorway",
        route.source = "uk naei"
      )
      #doc to think about
    )
    name <- if(is.null(name)){
      paste(obj$args$route.source,  obj$args$route.def, sep=".")
    } else { as.character(name) }
    obj$args$name <- name
    out <- modifyList(obj, .obj)
    class(out) <- "embrs_route"
    out <- list(out)
    names(out)[1] <- out[[1]]$args$name
    out <- list(routes=out)
    class(out) <- c("embrs", "routes")
    out
  }


#splatted function
#' @rdname route.objects
#' @export
route_naei_rum <-
  function(){
    route_naei_rural() + route_naei_urban() + route_naei_motorway()
  }

###############################
#currently unexported functions
###############################

###############################
#embrs_vehicle
##############################

#workhorse function for making
#vehicle/fleet objects

####################
#to do
####################

#doc args

#to think about
######################
#embrs_vehicle to handle embrs vehicle fleets and vehicles
#embrs_vehicle(name="hi", bus_bev(veh.wt=10000)[[1]][[1]], ef.brake.pm10 = NULL)[[1]][[1]]$fun


embrs_vehicle <-
  function(name = NULL, x = NULL, ...){

    .obj <- list(...)
    #print(.obj)
    if(is.null(x)){
      stop("nothing to work with!")
    }
    #could work up to handle x = embrs object?
    ## print(names(x))
    ## x = embrs_fleet_vehicle[[1]][[1]]
    obj <- list(args=list(),
                funs=list()
    )
    #was thinking of adding docs as extra case
    #BUT ending up doing it via ef_...(verbose=TRUE)
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
      if(obj$args$eng.fuel==obj$args$eng.type){
        paste(obj$args$eng.fuel, obj$args$veh.type, sep=".")
      } else {
        paste(obj$args$eng.fuel,  obj$args$eng.type, obj$args$veh.type, sep=".")
      }
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


#############################
#route_route
#############################

#used to build route_objects

####################
#to do
####################

#doc args

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

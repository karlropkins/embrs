############################################
#' @title embrs
############################################

#' @name embrs.main
#' @aliases build_inventory
#' @description Main functions for embrs emission model anslysis.
#' @param x (required embrs object) the __embrs__ model to analyse.
#' @returns These functions return different __embrs__ outputs.
#' @note These may be moving to vein at some point...
#' @references add embrs and Beddows references...

#############################
#could see us associating variables with inventories
#e.g. veh.wt or modifying variable

##############################
#issues
############################
#inventory picking up messy row names
#with ef_embrs related builds
#(but numbers/df shapes look sensible)




#' @rdname embrs.main
#' @export
build_inventory <-
  function(x, ...){
    #need a nice check for is this an embrs object
    if(class(x)[1] != "embrs" & length(class(x)) != "2"){
      stop("[embrs] this function is only intented for use with embrs objects",
           call. = FALSE)
    }
    if(class(x)[2] != "model"){
      stop("[embrs] this is a partially built embrs model, see docs",
           call. = FALSE)
    }
    x
    ##########################
    #can tidy the below code a lot once this is
    #up and running
    ##########################
    ans <- lapply(1:length(x), function(n){
      #for each model
      .mod <- x[[n]]
      ans <- lapply(names(.mod), function(.veh){
        vehicle <- .mod[[.veh]]
        #names(vehicle)
        routes <- vehicle$routes
        names(routes)
        ans <- lapply(names(routes), function(.rou){
          route <- routes[[.rou]]
          #route
          #run any route$args that are functions
          for(i in 1:length(route$args)){
            if(is.function(route$args[[i]])){
              #this is messy because it sends a function to itself
              route$args[[i]] <- do.call(route$args[[i]],
                                         modifyList(vehicle$args, route$args))
            }
          }
          #run through ef funs
          ans <- lapply(names(vehicle$funs), function(.fun){
            do.call(vehicle$funs[[.fun]], modifyList(vehicle$args, route$args))
          })
          ans <- dplyr::bind_rows(ans)
          ans <- data.frame(u.route=.rou, ans)
          ans
        })
        ans <- dplyr::bind_rows(ans)
        ans <- data.frame(vehicle=.veh, ans)
        ans
      })
      ans <- dplyr::bind_rows(ans)
      ans <- data.frame(level=n, ans)
      ans
    })
    dplyr::bind_rows(ans)
  }


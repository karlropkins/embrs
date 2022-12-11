############################################
#' @title embrs generics
############################################

#' @name embrs.generics
#' @aliases as.embrs_fleet.embrs_vehicle
#' @description Functions to build vehicle, fleet and route objects for use in
#' embrs emission models.
#' @param x,y function arguments
#' @returns These functions make vehicle and fleet class embrs objects.
#' @note These may be moving to vein at some point...
#' @references add embrs and Beddows references...



#' @rdname embrs.generics
#' @method + embrs
#' @export
`+.embrs` <-
  function(x, y, ...){
    if(class(y)[1] != "embrs"){
      stop("can't touch this")
    }
    if("fleet" %in% class(x)){
      if(!"fleet" %in% class(y)){
        stop("sorry can't add this to a fleet")
      }
      #print(class(x))
      #print(class(y))
      #return("here")
      temp <- make.unique(c(names(x$fleet), names(y$fleet)))
      x$fleet[(length(x$fleet)+1):(length(x$fleet)+length(y$fleet))] <- y$fleet
      names(x$fleet) <- temp
      return(x)
      #names(x$fleet) <- make.unique(names(x$fleet))
      #this trusts user
    }
    if("routes" %in% class(x)){
      if(!"routes" %in% class(y)){
        stop("sorry can't add this to a route")
      }
      #like
      temp <- make.unique(c(names(x$routes), names(y$routes)))
      x$routes[(length(x$routes)+1):(length(x$routes)+length(y$routes))] <- y$routes
      names(x$routes) <- temp
      return(x)
    }
    stop("bad embrs object?")

  }

#' @rdname embrs.generics
#' @method * embrs
#' @export
`*.embrs` <-
  function(x, y, ...){
    if(class(y)[1] != "embrs"){
      stop("can't touch this")
    }
    temp <- c(class(x)[2], class(y)[2])
    if("fleet" %in% temp & "routes" %in% temp){
      #we can multiple
      #must end with a return
      if(class(x)[2]=="fleet") {
        fleet <- x
        routes <- y
      } else {
        fleet <- y
        routes <- x
      }
      for(i in 1:length(fleet$fleet)){
        fleet$fleet[[i]]$routes <- routes$routes
        class(fleet$fleet) <- "embrs_model"
      }
      names(fleet) <- "model"
      class(fleet) <- c("embrs", "model")
      return(fleet)

    } else {
      stop("sorry can't multiply these")
    }
  }




#' @rdname embrs.generics
#' @method print embrs
#' @export
print.embrs <-
  function(x, ...){
    cat("embrs: ", class(x)[2], "(length = ", length(x[[1]]), ")\n", sep="")
    ##for(i in 1:length(x)){
    ##  cat("       ", x[[i]]$n, " x ", x[[1]]$veh.type, "\n", sep="")
    ##}
    invisible(x)
  }


#' @rdname embrs.generics
#' @method plot embrs
#' @export
plot.embrs <-
  function(x, ...){
    #quick test
    if(class(x)[2]!="model"){
      stop("partially built model? check docs")
    }
    .da <- build_inventory(x)
    ggplot2::ggplot(data=.da) +
      ggplot2::geom_col(ggplot2::aes(x=vehicle, y=ans,
                                      fill=em.source)) +
      ggplot2::facet_grid(~route, scales="free_y")
  }

##from paper
##ggplot(aes(ordered(veh), ans, fill=type)) +
##  geom_bar(stat="identity", position = "stack") +
##  facet_grid(ref~route, scales='free_y') +
##  labs(y = "Total PM Emissions [mg.veh<sup> -1</sup>km<sup> -1</sup>]", x="") +
##  scale_fill_discrete(name="PM type") +
##  theme_bw() +
##  theme(axis.title.y = element_markdown(),
##        legend.position = "top",
##        strip.text.y = ggtext::element_markdown(),
##        strip.background = element_rect(fill=NA))


#' @rdname embrs.generics
#' @method print embrs_vehicle
#' @export
print.embrs_vehicle <-
  function(x, ...){
    cat("embrs: ", class(x), " [", x$args$name, "]\n", sep="")
    invisible(x)
  }






######################
#not sure about this any more


as.embrs_fleet <-
  function(x, ...){
    UseMethod("as.embrs_fleet")
  }

as.embrs_fleet.default <-
  function(x, ...){
    stop("no method")
  }

as.embrs_fleet.embrs_vehicle <-
  function(x, ...){
    out <- list(.new = x)
    names(out) <- x$veh.type
    class(out) <- "embrs_fleet"
    out
  }



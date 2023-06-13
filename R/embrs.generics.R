############################################
#' @title embrs generics
############################################

#' @name embrs.generics
#' @aliases as.embrs_fleet.embrs_vehicle
#' @description Functions to build vehicle, fleet and route objects for use in
#' embrs emission models.
#' @param e1,e2 The objects to add or multiple when building an emission
#' inventory
#' @param x The object to print, plot, etc.
#' @param ... Additional arguments, sometimes ignored.
#' @param em.type (for plot only, character) the emissions to generate plot
#' for, options: 'all' for all emissions types, or 'just.pm' for just
#' particulates. See Note.
#' @param plot.type (for plot only, character) the type of plot to generate,
#' options: 'by.em' standard plot; 'by.vehicle'.
#' See Note.
#' \code{NULL} to disable/show all emissions types calculated.
#' @returns These functions make vehicle and fleet class __embrs__ objects.
#' @note These functions are in development, so please be aware that options
#' outputs may change significantly.
#' plot.type = 'by.vehicle' and em.type = 'just.pm' produces plots like those
#' used in Tivey el al (2023) to compare particulate emissions from different
#' buses types.
#' @references
#' Tivey, J., Davies, H.C., Levine, J.G., Zietsman, J., Bartington, S.,
#' Ibarra-Espinosa, S. and Ropkins, K, 2023. Meta-Analysis as Early Evidence on
#' the Particulate Emissions Impact of EURO VI on Battery Electric Bus Fleet
#' Transitions. Sustainability 15, 1522. \url{https://doi.org/10.3390/su15021522}



#notes
###########################
#error formatting

#stop("[embrs] bus...() needs veh.wt, see help?",
#     call.=FALSE)


#' @rdname embrs.generics
#' @method + embrs
#' @export
`+.embrs` <-
  function(e1, e2, ...){
    if(class(e2)[1] != "embrs"){
      stop("[embrs] can't add a ", paste(class(e2), collapse="_"),
           " to a [embrs] object!",
           call. = FALSE)
    }
    if("fleet" %in% class(e1)){
      if(!"fleet" %in% class(e2)){
        stop("[embrs] can't add a ", paste(class(e2), collapse="_"),
             " to a [embrs_fleet] object!",
             call. = FALSE)
      }
      #print(class(x))
      #print(class(y))
      #return("here")
      temp <- make.unique(c(names(e1$fleet), names(e2$fleet)))
      e1$fleet[(length(e1$fleet)+1):(length(e1$fleet)+length(e2$fleet))] <- e2$fleet
      names(e1$fleet) <- temp
      return(e1)
      #names(x$fleet) <- make.unique(names(x$fleet))
      #this trusts user
    }
    if("routes" %in% class(e1)){
      if(!"routes" %in% class(e2)){
        stop("[embrs] can't add a ", paste(class(e2), collapse="_"),
             " to a [embrs_routes] object!",
             call. = FALSE)
      }
      #like
      temp <- make.unique(c(names(e1$routes), names(e2$routes)))
      e1$routes[(length(e1$routes)+1):(length(e1$routes)+length(e2$routes))] <- e2$routes
      names(e1$routes) <- temp
      return(e1)
    }
    if("model" %in% class(e1)){
      if(!"model" %in% class(e2)){
        stop("[embrs] can't add a ", paste(class(e2), collapse="_"),
             " to a [embrs_model] object!",
             call. = FALSE)
      }
      #like
      temp <- make.unique(c(names(e1$model), names(e2$model)))
      e1$model[(length(e1$model)+1):(length(e1$model)+length(e2$model))] <- e2$model
      names(e1$model) <- temp
      return(e1)
    }

    stop("[embrs] sorry, unexpected [embrs] combination! maybe bad object?",
         call. = FALSE)
  }


#' @rdname embrs.generics
#' @method * embrs
#' @export
`*.embrs` <-
  function(e1, e2, ...){
    if(class(e2)[1] != "embrs"){

    }
    temp <- c(class(e1)[2], class(e2)[2])
    if("fleet" %in% temp & "routes" %in% temp){
      #we can multiple
      #must end with a return
      if(class(e1)[2]=="fleet") {
        fleet <- e1
        routes <- e2
      } else {
        fleet <- e2
        routes <- e1
      }
      for(i in 1:length(fleet$fleet)){
        fleet$fleet[[i]]$routes <- routes$routes
        class(fleet$fleet[[i]]) <- "embrs_model"
      }
      names(fleet) <- "model"
      class(fleet) <- c("embrs", "model")
      return(fleet)
    } else {
      stop("[embrs] sorry can't multiply these",
           call. = FALSE)
    }
  }




#' @rdname embrs.generics
#' @method print embrs
#' @export
print.embrs <-
  function(x, ...){
    cat("embrs: ", class(x)[2], "(count = ", length(x[[1]]), ")\n", sep="")
    for(i in 1:length(x[[1]])){
      cat("  > ", gsub("embrs_", "", class(x[[1]][[i]])), " ",
          names(x[[1]][i]), "\n", sep="")
      if(class(x)[2]=="model"){
        for(j in 1:length(x[[1]][[i]]$routes)){
          cat("    > ", gsub("embrs_", "", class(x[[1]][[i]]$routes[[j]])), " ",
              names(x[[1]][[i]]$routes[j]), "\n", sep="")
        }
      }
    }
  }


#' @rdname embrs.generics
#' @method plot embrs
#' @import ggplot2
#' @export

#could also add ggtext to imports for the formatting
#could add options to set x and y labels, etc.
# ... but + xlab("whatever") works nicely

#plot.type and em.type feels messy
#    might want to rethink this

plot.embrs <-
  function(x, plot.type = "by.em", em.type = "all", ...){
    #quick test
    if(class(x)[2]!="model"){
      stop("[embrs] partially built model? check help?",
           call.=FALSE)
    }
#this about this...
#will be a better way...
    .ylab <- if(em.type=="just.pm"){
      "Total Particulate Emissions [mg/km]"
    } else {
      "Total Emissions [mg/km]"
    }
    .da <- build_inventory(x, em.type = em.type, ...)
    #############################
    #these force it to plot in order of
    #occurrence in the build_inventory data.frame
    #############################
    #might want to be able to turn off
    ###############################
    .da$vehicle <- factor(.da$vehicle, levels = unique(.da$vehicle))
    .da$route <- factor(.da$route, levels = unique(.da$route))
    .da$em.source <- factor(.da$em.source, levels = rev(unique(.da$em.source)))
    #rev() last because first a bottom of stack seems more sensible...

    if(plot.type=="by.vehicle"){
      #embrs paper plot
      if(length(levels(.da$route)) <= 1){
        ##############################
        #including this in case only one or no routes
        #might drop or remove strip
        #############################
        ggplot(data=.da) +
          geom_col(aes(x=vehicle, y=ans,
                       fill=em.source)) +
          labs(y = .ylab, x="") +
          facet_grid(route~em.type, scales="free_y") +
          #scales not needed???
          theme_bw()

      } else {
        ggplot(data=.da) +
          geom_col(aes(x=vehicle, y=ans, fill=em.source)) +
          labs(y = .ylab, x="") +
          ###############################
        #ylab will need better handling if distance enabled
        #maybe use plot.type argument and have different
        #types of plot
        #################################
        facet_grid(em.type~route, scales="free_y") +
          #scales not needed???
          theme_bw()
      }
    } else {
      #default plot
      ggplot(data=.da) +
        geom_col(aes(x=em.type, y=ans, fill=em.source)) +
        labs(y = .ylab, x="") +
        ###############################
      #ylab will need better handling if distance enabled
      #maybe use plot.type argument and have different
      #types of plot
      #################################
      facet_grid(vehicle~route, scales="free_y") +
        #scales not needed???
        theme_bw()
    }
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
    #invisible(x)
  }

#' @rdname embrs.generics
#' @method print embrs_route
#' @export
print.embrs_route <-
  function(x, ...){
    cat("embrs: ", class(x), " [", x$args$name, "]\n", sep="")
    #invisible(x)
  }

#' @rdname embrs.generics
#' @method print embrs_model
#' @export
print.embrs_model <-
  function(x, ...){
    cat("embrs: ", class(x), " [", x$args$name, "]\n", sep="")
    #invisible(x)
  }



######################
#not sure about this any more


as.embrs_fleet <-
  function(x, ...){
    UseMethod("as.embrs_fleet")
  }

as.embrs_fleet.default <-
  function(x, ...){
    stop("[embrs] sorry, no embrs method defined",
         call. = FALSE)
  }

as.embrs_fleet.embrs_vehicle <-
  function(x, ...){
    out <- list(.new = x)
    names(out) <- x$veh.type
    class(out) <- "embrs_fleet"
    out
  }




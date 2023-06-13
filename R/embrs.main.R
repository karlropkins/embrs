############################################
#' @title embrs
############################################

#' @name embrs.main
#' @aliases build_inventory
#' @description Main functions for embrs emission model anslysis.
#' @param x (required embrs object) the __embrs__ model to analyse.
#' @param em.type (character) the emissions types to calculate emission
#' factors for, options: \code{NULL} to disable and show all calculated
#' emissions; just.pm' (default) for just PM emissions.
#' @param verbose (logical) If TRUE, include methods details
#' when reporting EF predictions.
#' @param ... other arguments, passed on
#' @returns These functions return different __embrs__ outputs.
#' @note These may be moving to vein at some point...
#' @references These functions are based on methods used in Tivey et al (2023):
#'
#' Tivey, J., Davies, H.C., Levine, J.G., Zietsman, J., Bartington, S.,
#' Ibarra-Espinosa, S. and Ropkins, K, 2023. Meta-Analysis as Early Evidence on
#' the Particulate Emissions Impact of EURO VI on Battery Electric Bus Fleet
#' Transitions. Sustainability 15, 1522. \url{https://doi.org/10.3390/su15021522}


#######################
#general
#######################

#check
################################

#last time I checked one warning on devtools::check()

#this is because I used a vein::: fix in code

#   need to drop that when vein goes to cran

#check fixing getting a little messy
############################

#we have a global variable declarations in several places
#could also have several overlapping @importFrom arguments

#   we move these all to a common embrs.misc.r or zzz.r script

#that could also be a better place for these general notes

#BUT that would need some quiet time...





##############################
#issues
############################
#inventory picking up messy row names
#with ef_embrs related builds
#(but numbers/df shapes look sensible)

#temp fix at end of build_inventory
#BUT rather have this fixed properly


###########################
#notes
###########################

#error messaging
#currently aiming for....

#stop("[embrs] bus...() needs veh.wt, see help?",
#     call.=FALSE)


#need to think about veh.spd
#if supplied to the vehicle, version in route supersedes
#and version in route is often there as default...

#    does this need a warning or handling??

#' @rdname embrs.main
#' @export
build_inventory <-
  function(x, em.type = NULL, verbose = FALSE, ...){
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
          ###################################
          #rather remove unwanted functions here
          #faster plus can't fall over on any this user does not want run
          ####################################
          .funs <- names(vehicle$funs)
          #######################
          #temp fix
          #this will need tidying
          #########################
          if(!is.null(em.type)){
            if(em.type == "just.pm"){
              #######################
              #watch this
              #works as longer as ".pm" is unique to pm ef function naming
              #      in the embrs_vehicle objects
              .funs <- .funs[grepl("[.]pm", .funs)]
            }
          }
          ###
          #end temp fix
          #########################

          ans <- lapply(.funs, function(.fun){
            #pass verbose = TRUE to all vehicle emission functions
            temp <- modifyList(vehicle$args, route$args)
            if(verbose){
              temp$verbose <- TRUE
            }
            do.call(vehicle$funs[[.fun]], temp)
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
    v.out <- dplyr::bind_rows(ans)
    #############################
    #temp fix for mess row names
    #############################
    row.names(v.out) <- 1:nrow(v.out)
    v.out
  }


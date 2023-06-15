############################################
#' @title coach objects
############################################

#' @name coach.objects
#' @aliases coach_ice coach_bev
#' @description Coach objects for use in __embrs__ models.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param eng.fuel (required character) the engine fuel used by the vehicle,
#' e.g. diesel.
#' @param euro.class (required character) vehicle EURO classification,
#' e.g. PRE, I, II, III, IV, V or VI.
#' @param ... other arguments, passed on to common vehicle build functions.
#' Arguments depend on the vehicle/engine/fuel combination. See embrs.vehicles
#' help for further details.
#' @param veh.type (character) type of vehicle, default coach.
#' @note Work in progress...
#' @returns These functions make coach models for __embrs__ emissions
#' models. The main models coach_ice and coach_bev are internal compression engine
#' and battery electric coaches, respectively. These functions extend bus method
#' used in Tivey et al (2023) to coaches.
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

#########################
#coach builds

#code currently based on bus objects...

#for beddows, embrs1 and embrs 2
#      most lookups naei spd, ukbc spd, ukbc brk, ukbc tyr a coach is just a bus
#for eea
#      coach is the category == BUS, grep("coach", tolower(segment))

############################
#to think about
###########################

#could include a test route that should work for this vehicle?
#   running this would identify any issues before a big build

#default is currently embrs1 speed model? means all current routes would work

#

#think about following
################################

#################################
#coach builds
#################################

#################################
#base models
#ice, bev
#################################

#splatted function
#' @rdname coach.objects
#' @export

coach_ice <-
  function(veh.wt = NULL, eng.fuel = NULL, euro.class = NULL, ...,
           veh.type = "coach"){
    if(is.null(veh.wt)){
      stop("[embrs] coach...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(is.null(eng.fuel) || is.null(euro.class)){
      stop("[embrs] coach_ice...() needs both eng.fuel and euro.class, see help?",
           call.=FALSE)
    }
    embrs_ice(veh.wt = veh.wt, eng.fuel = eng.fuel, euro.class = euro.class,
              ..., veh.type = veh.type)
  }



## #splatted function
## #' @rdname coach.objects
## #' @export

# on hold until hybrid bus is sorted

coach_hybrid <-
  function(veh.wt = NULL, eng.fuel = NULL, euro.class = NULL, ...,
           veh.type = "coach"){
    if(is.null(veh.wt)){
      stop("[embrs] coach...() needs veh.wt, see help?",
           call.=FALSE)
    }
    if(is.null(eng.fuel) || is.null(euro.class)){
      stop("[embrs] coach_hybrid...() needs both eng.fuel and euro.class, see help?",
           call.=FALSE)
    }
    embrs_hybrid(veh.wt = veh.wt, eng.fuel = eng.fuel, euro.class = euro.class,
              ..., veh.type = veh.type)
  }


#splatted function
#' @rdname coach.objects
#' @export

coach_bev <-
  function(veh.wt = NULL, ...,
           veh.type = "coach"){
    if(is.null(veh.wt)){
      stop("[embrs] coach...() needs veh.wt, see help?",
           call.=FALSE)
    }
    embrs_bev(veh.wt = veh.wt, ..., veh.type = veh.type)
  }




###########################
#unexported


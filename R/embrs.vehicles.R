############################################
#' @title common vehicle objects
############################################

#' @name embrs.vehicles
#' @aliases embrs_ice embrs_bev
#' @description Common builds for __embrs__ vehicle objects.
#' @param veh.wt (required numeric) weight of vehicle in kg.
#' @param veh.type (required character) type of vehicle, e.g bus.
#' @param eng.fuel (required character) the engine fuel used by the vehicle,
#' currently only diesel.
#' @param euro.class (required character) vehicle EURO classification,
#' e.g. PRE, I, II, III, IV, V or VI.
#' @param n (numeric) number of vehicles, default 1.
#' @param method (character) vehicle model, currently one of: beddows,
#' embrs1 (default) or embrs2.
#' @param name (character) name of the vehicle object.
#' @param ... other arguments, currently passed on to emission factor (ef)
#' models. In some cases, these may request additional inputs, e.g. when
#' ef_vein_eea has more than one ef model for a given
#' vehicle/engine/fuel/euro.class combination exhaust emission model it may
#' also request the exhaust technology (exh.tech), see ?ef_vein_eea_exhaust.
#' @note Work in progress; these may change...
#' @returns These functions make common vehicle objects for use in __embrs__
#' emissions models. The main models ice_build and bev_build are for internal
#' compression engine and battery electric vehicles, respectively.
#' @references These functions are based on methods developed and reported by:
#'
#' Beddows, D.C. and Harrison, R.M., 2021. PM10 and PM2.5 emission factors for
#' non-exhaust particles from road vehicles: Dependence upon vehicle mass and
#' implications for battery electric vehicles. Atmospheric Environment, 244,
#' p.117886. \url{https://doi.org/10.1016/j.atmosenv.2020.117886}.
#'
#' And, extrapolated to speed (embrs1) and, break/tyre-wear (embrs2)
#' related emission factors in:
#'
#' Tivey, J., Davies, H.C., Levine, J.G., Zietsman, J., Bartington, S.,
#' Ibarra-Espinosa, S. and Ropkins, K, 2023. Meta-Analysis as Early Evidence on
#' the Particulate Emissions Impact of EURO VI on Battery Electric Bus Fleet
#' Transitions. Sustainability 15, 1522. \url{https://doi.org/10.3390/su15021522}

########################
#notes
#######################

#moved dropped ..._beddows(), ..._embrs1(), ..._embrs2() functions by
#      moving coding/option into parent function as ...(model = "beddows")
#      [this reduces the number of vehicle objects four-fold]


##############################################
#doing
##############################################

#generalising code for roll out by
#vehicle type, engine type and emissions type

#currently intending all vehicle objects to
#be just wrappers for these with veh.type set/forced???
#   BUT might be special cases that kill that idea


#veh types
##############################
#done: bus
#doing: coach
#      added option to veh.type handling in ef_vein_eea_exhaust
#            check remarks there about testing subseting
#            (not all options/combinations allowed)
#      THIS NEEDS handling for
#             look-ups for naei spd and ukbc parameters (maybe as bus?)
#             fuel correction (maybe as bus?)
#                  handling of both keeps documenting
#
#proposing: truck (hgv), van (n1 to n3), car (various sizes),
#       motorcycle (various types),
#but large job to get right...

#eng type
#############################
#done: ice, bev,
#doing/testing: hybrid
#         unexported object added embrs_hybrid below
#             will need exporting and documenting when ready to go...
#         unexported object bus_hybrid added to bus.objects
#             will have to do same for all vehicle types
#             will need to export and document when ready to go...
#         added hybrid as eng.type option in ef_vein_eea_exhaust
#             will also need considering for any other ef..exhaust functions

#   STOP:
#         hybrid looks like big bus
#             check with Sergio
#             see if in example report from early bus work

#proposing: bifuel?

#em.type
#############################
#done: exh pm (10 & 2.5), brake pm (10 & 2.5), tyre pm (10 & 2.5)
#      road pm (10 & 2.5), resusp pm (10 & 2.5)
#doing/testing: nox
#       wrote ef_vein_eea_exhaust_nox wrapper in ef_vein_eea_exhaust.R.
#              wrote, exported and documented
#       added ef_vein_eea_exhaust_nox to funs in all three method options
#              in embrs_ice
#              [will also need it in any embrs_[eng.type] model with exhaust
#              emissions]
#
#       NB: handling in build_inventory and plot.embrs using additional
#             argument em.type, allows calculate/plot all or just.pm emissions
#
#proposing: gaseous species, co, hc; others in eea???


##################
#to think about
####################

#maybe move embrs_route to route.objects?

#maybe rename embrs.local embrs.vehicles

#gas phase species handling if we add gaseous species
#    in plots currently assume pm only...
#    in embrs_ice, etc, e.g. add all ef_... here
#        but ignore at build_inventory or select here...
#    (consider both above together?)

#how to do no2 and no?
#   eea has nox

#how to do HC
#   eea has CH4 and NMHC... could add???

#add cng and biodiesel fuels
#   eea has these for buses
#      not for coaches

#how to handle euro.class?
#    1,2,3 etc for cars, I, II, III for larger vehicles

#how to handle fuel correction
#    currently only coded for diesel bus

#could added method = "eea"
#    do NEEs as well as exhaust emissions by eea
#        sergio?

#splatted function
#' @rdname embrs.vehicles
#' @export

embrs_ice <-
  function(veh.wt = NULL, veh.type = NULL, eng.fuel = NULL, euro.class = NULL,
           n = 1, method = "embrs1", name = NULL, ...){
    #default ice vehicle build for general use model
    #(ice_embrs1)

    #a lot here needs to replicated below in embrs_bev
    #don't think we can generalise at the moment
    #because of euro.class + eng.fuel

    if(is.null(veh.type) || is.null(veh.wt)){
      stop("[embrs] all vehicles need both veh.type and veh.wt, see help?",
           call.=FALSE)
    }
    if(is.null(eng.fuel) || is.null(euro.class)){
      stop("[embrs] all ice vehicles...() need both eng.fuel and euro.class, see help?",
           call.=FALSE)
    }
    if(!tolower(method) %in% c("beddows", "embrs1", "embrs2")){
      stop("[embrs] unknown vehicle emission model, see help?",
           call.=FALSE)
    }

    .veh <- list(
      args = list(
        n = n,
        veh.type = veh.type,
        veh.wt = veh.wt,
        eng.type = "ice",
        eng.fuel = eng.fuel,
        euro.class = euro.class,
        brk.regen = FALSE
      ),
      #embrs1 method
      funs = list(
        ef.exh.co = ef_vein_eea_exhaust_co,
        ef.exh.nox = ef_vein_eea_exhaust_nox,
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_embrs1_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs1_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_embrs1_brake_pm10,
        ef.tyre.pm10 = ef_embrs1_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    if(tolower(method)=="beddows"){
      .veh$funs <- list(
        ef.exh.co = ef_vein_eea_exhaust_co,
        ef.exh.nox = ef_vein_eea_exhaust_nox,
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_beddows_brake_pm2.5,
        ef.tyre.pm2.5 = ef_beddows_tyre_pm2.5,
        ef.road.pm2.5 = ef_beddows_road_pm2.5,
        ef.resusp.pm2.5 = ef_beddows_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_beddows_brake_pm10,
        ef.tyre.pm10 = ef_beddows_tyre_pm10,
        ef.road.pm10 = ef_beddows_road_pm10,
        ef.resusp.pm10 = ef_beddows_resusp_pm10
      )
    }
    if(tolower(method)=="embrs2"){
      .veh$funs <- list(
        ef.exh.co = ef_vein_eea_exhaust_co,
        ef.exh.nox = ef_vein_eea_exhaust_nox,
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_embrs2_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs2_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_embrs2_brake_pm10,
        ef.tyre.pm10 = ef_embrs2_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    }

    ######################
    #failed!
    # trying to reduce the number of
    # warnings when missing args trigger
    # a warning
    #############################
    #last.warning <- last.warning[!duplicated(last.warning)]
    embrs_vehicle(name=name, x=.veh, ...)
  }


## #splatted function
## #' @rdname embrs.vehicles
## #' @export

#not exporting or documenting this yet...

embrs_hybrid <-
  function(veh.wt = NULL, veh.type = NULL, eng.fuel = NULL, euro.class = NULL,
           n = 1, method = "embrs1", name = NULL, ...){
    #default ice vehicle build for general use model
    #(ice_embrs1)

    #a lot here needs to replicated from in embrs_ice
    #don't think we can generalise at the moment
    #because might be differences later

    if(is.null(veh.type) || is.null(veh.wt)){
      stop("[embrs] all vehicles need both veh.type and veh.wt, see help?",
           call.=FALSE)
    }
    if(is.null(eng.fuel) || is.null(euro.class)){
      stop("[embrs] all ice vehicles...() need both eng.fuel and euro.class, see help?",
           call.=FALSE)
    }
    ###############################
    #could add extra checks here
    #etc does this look like a hybrid
    #     e.g. is fuel a mix??
    #     e.g. diesel+electric has +
    ################################

    if(!tolower(method) %in% c("beddows", "embrs1", "embrs2")){
      stop("[embrs] unknown vehicle emission model, see help?",
           call.=FALSE)
    }

    .veh <- list(
      args = list(
        n = n,
        veh.type = veh.type,
        veh.wt = veh.wt,
        eng.type = "hybrid",
        eng.fuel = eng.fuel,
        euro.class = euro.class,
        brk.regen = FALSE
      ),
      #embrs1 method
      funs = list(
        ef.exh.nox = ef_vein_eea_exhaust_nox,
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_embrs1_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs1_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_embrs1_brake_pm10,
        ef.tyre.pm10 = ef_embrs1_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    if(tolower(method)=="beddows"){
      .veh$funs <- list(
        ef.exh.nox = ef_vein_eea_exhaust_nox,
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_beddows_brake_pm2.5,
        ef.tyre.pm2.5 = ef_beddows_tyre_pm2.5,
        ef.road.pm2.5 = ef_beddows_road_pm2.5,
        ef.resusp.pm2.5 = ef_beddows_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_beddows_brake_pm10,
        ef.tyre.pm10 = ef_beddows_tyre_pm10,
        ef.road.pm10 = ef_beddows_road_pm10,
        ef.resusp.pm10 = ef_beddows_resusp_pm10
      )
    }
    if(tolower(method)=="embrs2"){
      .veh$funs <- list(
        ef.exh.nox = ef_vein_eea_exhaust_nox,
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_embrs2_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs2_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_embrs2_brake_pm10,
        ef.tyre.pm10 = ef_embrs2_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    }

    embrs_vehicle(name=name, x=.veh, ...)
  }



#splatted function
#' @rdname embrs.local
#' @export

embrs_bev <-
  function(veh.wt = NULL, veh.type= NULL,
           n = 1, method = "embrs1", name = NULL, ...){
    #default electric vehicle build
    #ice without the exhaust emissions

    if(is.null(veh.type) || is.null(veh.wt)){
      stop("[embrs] all vehicles need both veh.type and veh.wt, see help?",
           call.=FALSE)
    }
    if(!tolower(method) %in% c("beddows", "embrs1", "embrs2")){
      stop("[embrs] unknown vehicle emission model, see help?",
           call.=FALSE)
    }
    ############################
    #to think about
    ############################
    #error if fuel not electric?
    #euro.class might be trickier?

    .veh <- list(
      args = list(
        n = n,
        veh.type = "bus",
        veh.wt = veh.wt,
        eng.type = "electric",
        eng.fuel = "electric",
        brk.regen = FALSE
      ),
      #embrs1 method
      funs = list(
        ef.brake.pm2.5 = ef_embrs1_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs1_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.brake.pm10 = ef_embrs1_brake_pm10,
        ef.tyre.pm10 = ef_embrs1_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    )
    if(tolower(method)=="beddows"){
      .veh$funs <- list(
        ef.brake.pm2.5 = ef_beddows_brake_pm2.5,
        ef.tyre.pm2.5 = ef_beddows_tyre_pm2.5,
        ef.road.pm2.5 = ef_beddows_road_pm2.5,
        ef.resusp.pm2.5 = ef_beddows_resusp_pm2.5,
        ef.brake.pm10 = ef_beddows_brake_pm10,
        ef.tyre.pm10 = ef_beddows_tyre_pm10,
        ef.road.pm10 = ef_beddows_road_pm10,
        ef.resusp.pm10 = ef_beddows_resusp_pm10
      )
    }
    if(tolower(method)=="embrs2"){
      .veh$funs <- list(
        ef.exh.pm2.5 = ef_vein_eea_exhaust_pm2.5,
        ef.brake.pm2.5 = ef_embrs2_brake_pm2.5,
        ef.tyre.pm2.5 = ef_embrs2_tyre_pm2.5,
        ef.road.pm2.5 = ef_embrs1_road_pm2.5,
        ef.resusp.pm2.5 = ef_embrs1_resusp_pm2.5,
        ef.exh.pm10 = ef_vein_eea_exhaust_pm10,
        ef.brake.pm10 = ef_embrs2_brake_pm10,
        ef.tyre.pm10 = ef_embrs2_tyre_pm10,
        ef.road.pm10 = ef_embrs1_road_pm10,
        ef.resusp.pm10 = ef_embrs1_resusp_pm10
      )
    }
    ######################
    #failed!
    # trying to reduce the number of
    # warnings when missing args trigger
    # a warning
    #############################
    #last.warning <- last.warning[!duplicated(last.warning)]
    embrs_vehicle(name=name, x=.veh, ...)
  }











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


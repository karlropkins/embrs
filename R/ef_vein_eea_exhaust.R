############################################
#' @title ef_vein_eea_exhaust
############################################

#' @name ef_vein_eea_exhaust
#' @aliases ef_vein_eea_exhaust ef_vein_eea_exhaust_pm2.5
#' ef_vein_eea_exhaust_pm10
#' @description Functions to calculate vehicle speed-based hot exhaust
#' emission factors using on EMEP/EEA guidelines 2019 methods.
#' @param veh.spd average vehicle speed, in km/h.
#' @param veh.type vehicle type, currently only coded for buses, see Note.
#' @param veh.wt vehicle weight, in km.
#' @param veh.segment EEA vehicle sub-classification, specific to vehicle type.
#' For buses, embrs assigns this using veh.type and veh.wt, so it can often be
#' ignored.
#' @param em.type type of emission, currently PM, PM2.5 or PM10, see Note.
#' @param route.def route description, optional for ef_vein_eea() functions.
#' @param eng.type engine type: ice, etc.
#' @param eng.fuel fuel used by the vehicle: diesel, biodiesel, cng, etc.
#' @param fuel.corr (logical) apply fuel correction, see Note.
#' @param em.source emission source: currently, exhaust only.
#' @param euro.class EURO classification: PRE, I, II, III, IV,
#' V or VI only.
#' @param exh.tech exhaust technology: for euro.class IV SCR or EGR,
#' for V  SCR or EGR, and for VI DPF+SCR only
#' @param eng.load engine load, as proportion: 0. 0.5 or 1 only.
#' @param route.slope route slope, as proportion: -0.06, -0.04, -0.02, 0,
#' 0.02, 0.04 or 0.06 only.
#' @param route.mode route driving/journey mode: Urban Peak, Urban Off Peak,
#' Rural, Motorway, or NA (not defined).
#' @param verbose (logical) If TRUE, include methods details
#' when reporting EF predictions.
#' @param ... other arguments, currently ignored
#' @note The embrs::ef_vein_eea functions are currently only coded for
#' buses, \code{veh.type="bus"}.
#'
#' Strictly, EEA 2019 PM exhaust emission factors are PM, but because
#' vehicle exhaust PM is fine it is typically assumed that:
#'
#' PM10 = PM2.5 = PM for exhaust emissions from most conventional diesel and
#' gasoline vehicles.
#'
#' Fuel Corrections are applied to account for improvements in emissions
#' associated with improvements in fuel quality,
#'
#' @source
#' Average speed curves are based on European Environment Agency 2019 EMEP/EEA
#' methods as documented in:
#'
#' European Environment Agency, 2019. EMEP/EEA air pollutant
#' emission inventory guidebook.
#' <https://www.eea.europa.eu/publications/emep-eea-guidebook-2019>
#'
#' And as implemented in R package vein:
#'
#' Ibarra-Espinosa, S., Ynoue, R., O'Sullivan, S., Pebesma, E., Andrade, M. D. F.,
#' and Osses, M.: VEIN v0.2.2: an R package for bottom-up vehicular emissions
#' inventories, Geosci. Model Dev., 11, 2209-2229,
#' <https://doi.org/10.5194/gmd-11-2209-2018>, 2018.
#'
#' Ibarra-Espinosa S (2022). _vein: Vehicular Emissions Inventories_. R package
#' version 0.9.9, <https://CRAN.R-project.org/package=vein>.
#'
#' Fuel corrections are based on:
#'
#' TRL Emission Factors - scaling factors for mileage and improvements in fuel quality
#'  https://www.gov.uk/government/publications/road-vehicle-emission-factors-2009
#'


###########################
#notes
###########################

#this NEEDS R:vein
#now added as imports

#standardise error messaging in function
#see note or format in embrs.local as used in e.g. embrs_ice


#issue
########################
#method is not tracking slope, mode and load when set...
#     this might be sorted now

#note
###############################
#this code is still very messy...
#

#need to think about
##############################
#fuel
#exh.tech














##########################
#latest version
##########################

# this needs something separate ice, bifuel and hybrid???
# euro.class still has work to do

# .eea source needs to be sorted before this goes to CRAN...
#       if it does???

# think about electric vehicle


#splatted function
#' @rdname ef_vein_eea_exhaust
#' @export
ef_vein_eea_exhaust <-
  function(veh.spd = NULL, veh.type=NULL, veh.wt = NULL,
           veh.segment = NULL, em.type = NULL, route.def = NULL,
           eng.type= NULL, eng.fuel = NULL, fuel.corr = TRUE,
           em.source = "exhaust", euro.class = NULL,
           exh.tech = NULL, eng.load = NULL,
           route.slope = NULL, route.mode = NULL,
           verbose = FALSE, ...)
  {

    #need something for route.def
    if(is.null(route.def)){
      route.def <- "[unknown]"
    }

    #only exhaust emissions
    if(is.null(em.source) || tolower(em.source) != "exhaust"){
      stop("[embrs>] ef_vein_eea_exhaust(): ONLY for exhaust emissions",
           call. = FALSE
      )
    }

    #veh.spd, veh.type, eng.fuel, em.type...
    #might also need euro.class??
    if(is.null(veh.spd) || is.null(veh.type) || is.null(eng.fuel) ||
       is.null(em.type)|| is.null(euro.class)){
      stop(paste("[embrs>] ef_vein_eea_exhaust(): EEA needs at least:\n",
                 "\t veh.spd, veh.type, eng.fuel, em.type, euro.class",
                 sep=""),
           call. = FALSE
      )
    }

    #if exported from vein
    ## .eea <- vein::get_ef_ref("eea")
    #and delete local version of function
    .eea <- data.table::setDT(vein:::sysdata[["eea"]])
    ##.eea <- vein::get_ef_ref("eea")

    .category <- .fuel <- .pollutant <- .eurostandard <- NULL
    .mode <- .roadslope <- .load <- NA

    #################
    #.category
    #bus, coach
    if(veh.type %in% c("bus", "coach")){
      #lookup for eea bus
      .category <- "BUS"
      .eea <- .eea[.eea$Category==.category,]
      if(veh.type=="bus"){
        .eea <- .eea[!grepl("coach", tolower(.eea$Segment)),]
      } else {
        .eea <- .eea[grepl("coach", tolower(.eea$Segment)),]
      }
    } else {
      stop("[embrs>] ef_vein_eea_exhaust(): unknown vehicle type (veh.type);",
           "\n\tmaybe one of: bus, coach",
           call. = FALSE
      )
    }

    ####################
    #.fuel
    #so far only allowing diesel, biodiesel and cng
    #so this will not work with an electric vehicle
    if(!is.null(eng.type) && eng.type %in% c("ice")){
      if(eng.type == "ice"){
        d1 <- c("diesel", "biodiesel", "cng")
        d2 <- c("D", "BIO D", "CNG")
      }
    } else {
      stop("[embrs>] ef_vein_eea_exhaust(): Unknown engine type (eng.type);",
           "\n\tmaybe one of: ice",
           call. = FALSE
      )
    }

    .fuel <- if(tolower(eng.fuel) %in% d1){
      #print(d1 %in% eng.fuel)
      d2[d1 %in% eng.fuel]
    } else {
      "missing"
    }
    ref <- unique(.eea$Fuel)
    ref2 <- d1[d2 %in% ref]
    if(!.fuel %in% ref){
      stop("in embrs:ef_vein_eea_exhaust()",
           "\n    Unknown vehicle, engine and fuel combination ",
           "\n    ", veh.type, "; ", eng.type, "; ", eng.fuel,
           "\n    maybe try one of: ", paste(ref2, collapse = ", "),
           call. = FALSE
      )
    }
    .eea <- .eea[.eea$Fuel==.fuel,]

    #################
    #.segment
    #this has to be case by case because
    #all segments as different
    #so there will need to be some careful handling
    if(!is.null(veh.segment)){
      .segment <- veh.segment
    } else {
      if(is.null(veh.wt)){
        stop("[embrs] ef_vein_eea_exhaust...(): Need one of veh.wt or veh.segment",
             call. = FALSE
        )
      } else {
        if(length(unique(.eea$Segment))==1){
          #only one!
          .segment <- unique(.eea$Segment)
        } else {
          if(tolower(veh.type)=="bus"){
            #bus
            if(veh.wt <= 15000){
              .segment <- "Urban Buses Midi <=15 t"
            }
            if(veh.wt > 15000 & veh.wt <= 18000){
              .segment <- "Urban Buses Standard 15 - 18 t"
            }
            if(veh.wt >18000){
              .segment <- "Urban Buses Articulated >18 t"
            }
          }
          #coach
          if(tolower(veh.type)=="coach"){
            if(veh.wt <= 18000){
              .segment <- "Coaches Standard <=18 t"
            }
            if(veh.wt >18000){
              .segment <- "Urban Buses Articulated >18 t"
            }
          }
        }
      }
    }
    ref <- unique(.eea$Segment)
    if(!.segment %in% ref){
      stop("[embrs>] ef_vein_eea_exhaust(): Unknown vehicle segment (veh.segment)",
           "\n\tmaybe one of: ", paste(ref, collapse = ", "),
           call. = FALSE
      )
    }
    .eea <- .eea[.eea$Segment==.segment,]

    ####################
    #.pollutant
    d1 <- c("pm", "pm10", "pm2.5", "nox", "co", "nmhc", "ec", "ch4",
            "nh3", "n2o")
    d2 <- c("PM", "PM", "PM", "NOx", "CO", "NMHC", "EC", "CH4",
            "NH3", "N2O")
    .pollutant <- if(tolower(em.type) %in% d1){
      d2[d1 %in% em.type]
    } else {
      "missing"
    }
    ref <- unique(.eea$Pollutant)
    if(!.pollutant %in% ref){
      stop("[embrs>] ef_vein_eea_exhaust(): Unknown emission type (em.type)",
           "\n\tmaybe one of: ", paste(ref, collapse = ", "),
           call. = FALSE
      )
    }
    .eea <- .eea[.eea$Pollutant==.pollutant,]

    ######################
    #.eurostandard
    #so far only allowing main euro classes
    #see unique(test_vein_eea()$EuroStandard) for full list

    d1 <- c("pre", "i", "ii", "iii", "iv", "v", "vi")
    d2 <- c("PRE", "I", "II", "III", "IV", "V", "VI")

    .eurostandard <- if(tolower(euro.class) %in% d1){
      d2[d1 %in% tolower(euro.class)]
    } else {
      "missing"
    }
    ref <- unique(.eea$EuroStandard)
    ref <- ref[ref %in% d2]
    if(!.eurostandard %in% ref){
      print(.eurostandard)
      stop("[embrs>] ef_vein_eea_exhaust(): UNknown EURO classification (euro.class)",
           "\n\tmaybe one of: ", paste(ref, collapse = ", "),
           call. = FALSE
      )
    }
    .eea <- .eea[.eea$EuroStandard==.eurostandard,]

    #################
    #.technology
    if(is.null(exh.tech)){
      if(length(unique(.eea$Technology))==1){
        .technology <- unique(.eea$Technology)
      } else {
        stop("[embrs>] ef_vein_eea_exhaust(): Unknown exhaust technology (exh.tech)",
             "\n\tmaybe one of: ", paste(unique(.eea$Technology), collapse = ", "),
             call. = FALSE
        )
      }
    } else {
      ref <- unique(.eea$Technology)
      if(tolower(exh.tech) %in% tolower(ref)){
        .technology <- ref[tolower(exh.tech) == tolower(ref)]
      } else {
        stop("[embrs>] ef_vein_eea_exhaust(): Unknown exhaust technology (exh.tech)",
             "\n\tmaybe one of: ", paste(unique(.eea$Technology), collapse = ", "),
             call. = FALSE
        )
      }
    }
    if(is.na(.technology)){
      .eea <- .eea[is.na(.eea$Technology),]
    } else {
      .eea <- .eea[.eea$Technology == .technology,]
    }


    ##############
    #next three sections need work
    #################

    #.mode
    #mode curretly uses NA...
    if(is.null(route.mode)){
      route.mode <- NA
    }
    ref <- unique(.eea$Mode)
    if(route.mode %in% ref){
      .mode <- route.mode
    } else {
      stop("[embrs>] ef_vein_eea_exhaust(): Unknown route mode (route.mode)",
           "\n\tmaybe one of: ", paste(unique(.eea$Mode), collapse = ", "),
           call. = FALSE
      )
    }
    if(is.na(.mode)){
      .eea <- .eea[is.na(.eea$Mode),]
    } else {
      .eea <- .eea[.eea$Mode == .mode,]
    }

    #################
    #roadslope
    #default 0 or NA
    if(is.null(route.slope)){
      route.slope <- if(0 %in% unique(.eea$RoadSlope)){
        0
      } else {
        NA
      }
    }
    ref <- unique(.eea$RoadSlope)
    if(route.slope %in% ref){
      .roadslope <- route.slope
    } else {
      #special case
      if(any(is.na(ref))){
        .roadslope <- NA
      } else {
        stop("[embrs>] ef_vein_eea_exhaust(): Unknown route slope (route.slope)",
             "\n\tmaybe one of: ", paste(unique(.eea$RoadSlope), collapse = ", "),
             call. = FALSE
        )
      }
    }
    if(is.na(.roadslope)){
      .eea <- .eea[is.na(.eea$RoadSlope),]
    } else {
      .eea <- .eea[.eea$RoadSlope == .roadslope,]
    }

    #############
    #load
    #default 0.5 or NA...
    if(is.null(eng.load)){
      eng.load <- if(0.5 %in% unique(.eea$Load)){
        0.5
      } else {
        NA
      }
    }
    ref <- unique(.eea$Load)

    if(eng.load %in% ref){
      .load <- eng.load
    } else {
      #special case
      if(any(is.na(ref))){
        .load <- NA
      } else {
        stop("[embrs>] ef_vein_eea_exhaust(): Unknown engine load (eng.load)",
             "\n\tmaybe one of: ", paste(unique(.eea$Load), collapse = ", "),
             call. = FALSE
        )
      }
    }
    if(is.na(.load)){
      .eea <- .eea[is.na(.eea$Load),]
    } else {
      .eea <- .eea[.eea$Load == .load,]
    }

    out <- vein::ef_eea(category = .category,
                        fuel = .fuel,
                        segment = .segment,
                        euro = .eurostandard,
                        tech = .technology,
                        pol = .pollutant,
                        mode = .mode,
                        slope = ifelse(is.na(.roadslope), 0, .roadslope),
                        load = ifelse(is.na(.load), 0, .load),
                        speed = veh.spd)
    out <- as.numeric(out) #kill units for now

    #fuel correction
    #########################
    #currently only coded for buses PM, NOx, CO and HC

    #PM   pre 0.94; I 0.94; II 0.94; III 0.96; IV 1.00; V (erg/scr) 1.00; 1.00
    #NOx  pre 0.99; I 0.99; II 0.99; III 0.99; IV 1.00; V (erg/scr) 1.00; VI 1.00
    #CO   pre 1.03; I 1.03; II 1.03; III 1.04; IV 1.00; V (erg/scr) 1.00; VI 1.00
    #HC   pre 1.07; I 1.07; II 1.07; III 1.04; IV 1.00; V (erg/scr) 1.00; VI 1.00

    #######################
    #could move this to
    #new functions???
    #######################

    if(fuel.corr){
      if(tolower(veh.type)=="bus"){
        if(.pollutant=="PM"){
          sc <- 1
          if(tolower(euro.class) %in% c("pre", "i", "ii")) {
            sc <- 0.94
          }
          if(tolower(euro.class) %in% c("iii")) {
            sc <- 0.96
          }
          out <- out * sc
        }
        if(.pollutant=="NOx"){
          sc <- 1
          if(tolower(euro.class) %in% c("pre", "i", "ii", "iii")) {
            sc <- 0.99
          }
          out <- out * sc
        }
        if(.pollutant=="CO"){
          sc <- 1
          if(tolower(euro.class) %in% c("pre", "i", "ii")) {
            sc <- 1.03
          }
          if(tolower(euro.class) %in% c("iii")) {
            sc <- 1.04
          }
          out <- out * sc
        }
        if(.pollutant=="HC"){
          sc <- 1
          if(tolower(euro.class) %in% c("pre", "i", "ii")) {
            sc <- 1.07
          }
          if(tolower(euro.class) %in% c("iii")) {
            sc <- 1.04
          }
          out <- out * sc
        }
      }
    }
    #units g/km to mg/km
    out <- out * 1000
    #might want to output
    #should we track eng.load, route.slope? route.mode? etc, if they are being used?
    #should method description track the eea pollutant not em.type???
    #       not the same, e.g. (beddows) pm10 = (beddows) pm2.5 = (eea) pm...
    v.out <- data.frame(em.type, em.source, euro.class, eng.fuel,
                        exh.tech = .technology, route.def,
                        veh.spd = veh.spd, veh.wt, ans = out)
    if(verbose){
      v.out$method.name <- "European Environment Agency (2019)"
      v.out$method.descr <- paste("EMEP/EEA air pollutant emission inventory guidebook ",
                                  "speed based ", v.out$em.type, " ",
                                  v.out$em.source,  " emissions",
                                  ", for ", v.out$eng.fuel, ", ", .segment,
                                  " segment ", v.out$veh.type,
                                  sep="")
      v.out$method.ref <- "https://www.eea.europa.eu/publications/emep-eea-guidebook-2019"
      return(v.out)
    } else {
      return(v.out)
    }

  }

## #splatted function
## #' @rdname ef_vein_eea_exhaust
## #' @export
ef_vein_eea_exhaust_nox <- function(em.type = "nox", ...){
  #testing this
  #so we need to send vein
  ef_vein_eea_exhaust(em.type = "nox", ...)
}


#splatted function
#' @rdname ef_vein_eea_exhaust
#' @export
ef_vein_eea_exhaust_pm2.5 <- function(em.type = "pm2.5", ...){
  #note for this pm10 = pm2.5 = pm
  #so we need to send vein PM and reset it to PM10
  ef_vein_eea_exhaust(em.type = "pm2.5", ...)
}

#splatted function
#' @rdname ef_vein_eea_exhaust
#' @export
ef_vein_eea_exhaust_pm10 <- function(em.type = "pm10", ...){
  #note for this pm10=pm2.5=pm
  #so we need to send vein PM and reset it to PM10
  ef_vein_eea_exhaust(em.type = "pm10", ...)
}




######################
#unexported code

#now redundant???

embrs_eea2019_prep <- function(x, opts, x.name){
  #x - user input
  #opts - known cases
  #x.name
  if(is.character(x)){
    if(x %in% opts){
      return(x)
    } else {
      stop(paste("[embrs] ef_eea2019...() unknown ", x.name,
                 " Needs to be one of: ",
                 paste(opts, collapse = ", ") , sep=""),
           call.= FALSE)
    }
  }
  if(is.numeric(x)){
    if(x > max(opts, na.rm=TRUE) | x < min(opts, na.rm=TRUE)){
      stop(paste("[embrs] ef_eea2019...() ", x.name,
                 " outside modelled range\n",
                 " set-points: ",
                 paste(opts, collapse = ", ") , sep=""),
           call.= FALSE)
    } else {
      if(x %in% opts){
        return(x)
      } else {
        new.x <- opts[which.min(abs(x-opts))[1]]
        warning(paste("[embrs] ef_eea2019...() ", x.name,
                      " not a known set-point\n",
                      " (known set-points: ", paste(opts, collapse = ", "),
                      ")\n",
                      " reseting ", x, " to ", new.x, sep=""),
                call.= FALSE)
        return(new.x)
      }
    }
  }
  #if unknown
  stop(paste("[embrs] ef_eea2019...() does not know ", x.name,
             " option, ", x, ", see help?", sep=""),
       call.= FALSE)
}

########
#for vein...
#if sergio ok with this
###########

#get_ef_ref <- function(ref){
  #but without the vein::
  #data.table::setDT(vein:::sysdata[[ref]])
#}


#can't use this in anything going to CRAN...

test_vein_eea <- function(category=NULL,
                          fuel=NULL,
                          segment=NULL,
                          eurostandard=NULL,
                          technology=NULL,
                          pollutant=NULL,
                          mode=NULL,
                          roadslope=NULL,
                          load=NULL,
                          case=NULL,
                          ...){

  #drop this?
  .xargs <- list(...)

  temp <- vein:::sysdata$eea
  #temp <- vein::get_ef_ref("eea")

  if(!is.null(category)){
    ref <- unique(temp$Category)
    if(!tolower(category) %in% tolower(ref)){
      stop(paste("EEA2019: unknown vehicle category \n",
                 "pick one of; ",
                 paste(ref, collapse =", "),
                 sep=""),
           call. = FALSE
      )
    }
    temp <- dplyr::filter(temp, tolower(Category)==tolower(category))
  }
  if(!is.null(pollutant)){
    ref <- unique(temp$Pollutant)
    if(!tolower(pollutant) %in% tolower(ref)){
      stop(paste("EEA2019: unknown Pollutant\n",
                 "pick one of; ",
                 paste(ref, collapse =", "),
                 sep=""),
           call. = FALSE
      )
    }
    temp <- dplyr::filter(temp, tolower(Pollutant)==tolower(pollutant))
  }
  if(!is.null(fuel)){
    ref <- unique(temp$Fuel)
    if(!tolower(fuel) %in% tolower(ref)){
      stop(paste("EEA2019: unknown Fuel\n",
                 "pick one of; ",
                 paste(ref, collapse =", "),
                 sep=""),
           call. = FALSE
      )
    }
    temp <- dplyr::filter(temp, tolower(Fuel)==tolower(fuel))
  }
  if(!is.null(eurostandard)){
    ref <- unique(temp$EuroStandard)
    if(!tolower(eurostandard) %in% tolower(ref)){
      stop(paste("EEA2019: unknown EuroStandard \n",
                 "pick one of; ",
                 paste(ref, collapse =", "),
                 sep=""),
           call. = FALSE
      )
    }
    temp <- dplyr::filter(temp, tolower(EuroStandard)==tolower(eurostandard))
  }
  if(!is.null(segment)){
    ref <- unique(temp$Segment)
    if(!tolower(segment) %in% tolower(ref)){
      stop(paste("EEA2019: unknown Segment \n",
                 "pick one of; ",
                 paste(ref, collapse =", "),
                 sep=""),
           call. = FALSE
      )
    }
    temp <- dplyr::filter(temp, tolower(Segment)==tolower(segment))
  }

  ###########################
  #case handling
  ###########################
  if(!is.null(case)){
    if(!tolower(case) %in% tolower(names(temp))){
      stop(paste("EEA2019: unknown EEA2019 name \n",
                 "pick one of; ",
                 paste(tolower(names(temp)), collapse =", "),
                 sep=""),
           call. = FALSE
      )
    } else {
      ##################
      #output for case
      ##################
      names(temp) <- tolower(names(temp))
      return(unique(temp[[case]]))
    }
  }
  ###############
  #output default
  ###############
  temp
}


############################################
#' @title ef_eea2019
############################################

#' @name ef_eea2019
#' @aliases ef_eea2019_exhaust_pm ef_eea2019_exhaust_pm2.5
#' ef_eea2019_exhaust_pm10
#' @description Functions to calculate vehicle speed-based hot exhaust
#' PM2.5 and PM10 emission factors based on EEA guidelines 2019 methods.
#' @param veh.spd average vehicle speed, in km/h.
#' @param veh.type vehicle type, currently only coded for urban buses, see Note.
#' @param veh.wt vehicle weight, in km.
#' @param em.type type of emission, currently PM, PM2.5 or PM10, see Note.
#' @param route.def route description, optional for ef_copert5 functions.
#' @param fuel.type fuel used by the vehicle, diesel, etc.
#' @param fuel.corr (logical) apply fuel correction, see Note.
#' @param em.source emission source: currently, exhaust only.
#' @param euro.class EURO classification: PRE, I, II, III, IV, V,
#' V or VI only.
#' @param exh.tech exhaust technology: for euro.class IV SCR or EGR,
#' for V  SCR or EGR, and for VI DPF+SCR only
#' @param eng.load engine load, as proportion: 0. 0.5 or 1 only.
#' @param route.slope route slope, as proportion: -0.06, -0.04, -0.02, 0,
#' 0.02, 0.04 or 0.06 only.
#' @param ... other arguments, currently ignored
#' @note The embrs::ef_eea2019 functions are currently only coded for urban
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
#' https://www.eea.europa.eu/publications/emep-eea-guidebook-2019
#'
#' As implemented in R package vein:
#'
#' Ibarra-Espinosa, S., Ynoue, R., O'Sullivan, S., Pebesma, E., Andrade, M. D. F.,
#' and Osses, M.: VEIN v0.2.2: an R package for bottom-up vehicular emissions
#' inventories, Geosci. Model Dev., 11, 2209-2229,
#' https://doi.org/10.5194/gmd-11-2209-2018, 2018.
#'
#' Ibarra-Espinosa S (2022). _vein: Vehicular Emissions Inventories_. R package
#' version 0.9.9, <https://CRAN.R-project.org/package=vein>.
#'
#' Fuel corrections are based on:
#'
#' TRL Emission Factors - scaling factors for mileage and improvements in fuel quality
#'  https://www.gov.uk/government/publications/road-vehicle-emission-factors-2009
#'


#this NEEDS vein

#splatted function
#' @rdname ef_eea2019
#' @export
ef_eea2019_exh_pm <-
function(veh.spd = NULL, veh.type=NULL, veh.wt = NULL,
                                  em.type = "pm", route.def = NULL,
                                  fuel.type = NULL, fuel.corr = TRUE,
                                  em.source = "exhaust", euro.class = NULL,
                                  exh.tech = NULL, eng.load = NULL,
                                  route.slope = NULL, ...)
{

  ###################
  #to do
  #####################
  #fuel
  #   currently ignored because diesel is only option for urban bus
  #correction factors
  #   coded (again urban bus only) but like something better
  ######################

  if(is.null(veh.wt)){
    stop("ef...(): embrs needs veh.wt (vehicle weight in kg), see help",
         call. = FALSE
    )
  }
  if(is.null(veh.spd)){
    stop("ef_eea2019_exhaust...(): needs veh.spd (vehicle speed in km/hr), see help",
         call. = FALSE
    )
  }
  if(is.null(route.def)){
    route.def <- "[unnamed]"
  }
  if(is.null(euro.class)){
    stop("ef_eea2019...(): needs euro.class, see help",
         call. = FALSE
    )
  }
  if(is.null(exh.tech)){
    #works for euro.class pre, I, II and III
    #see later for others
    exh.tech <- NA
  }


  if(is.null(route.slope)) {
    route.slope <- 0
    #copert default
  }
  if(is.null(eng.load)) {
    eng.load <- 0.5
    #copert default
  }

  ###################
  #currently urban bus only
  # could very quickly do coach and truck
  # from same lookup but they would need
  # only veh.type section because options
  # (e.g. veh.wt ranges are different)
  # so would need to modify the else below...
  ####################

  if(veh.type == c("bus")){

    #######################
    #this uses code in vein

    ####################
    #load can only be 0, 0.5 or 1
    #road.gradients can only be -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06
    eng.load <- embrs_eea2019_prep(eng.load, c(0, 0.5, 1), "eng.load")
    route.slope <- embrs_eea2019_prep(route.slope, c(-0.06, -0.04, -0.02,
                                               0, 0.02, 0.04, 0.06),
                                   "route.slope")

    ##################
    #bus euro can only be PRE, I, II, III, IV, V, V, VI
    ##################
    euro.class <- embrs_eea2019_prep(euro.class, c("PRE", "I", "II", "III",
                                                   "IV", "V",
                                                   "VI"),
                                      "euro.class")
    #exh.tech can be NA for pre euro to III
    #         can be SCR or EGR for euro IV to V
    #         can be DPF+SCR for euro VI
    if(euro.class %in% c("PRE", "I", "II", "III")){
      if(!is.na(exh.tech)){
        #should be NA
        warning("ef_eea2019_exhaust...(): for pre EURO IV bus, exh.tech not option, ignoring",
                call. = FALSE
        )
        exh.tech <- NA
      }
    }
    if(euro.class %in% c("IV", "V")){
      if(!exh.tech %in% c("SCR", "EGR")){
        #should be SCR or EGR
        stop("ef_eea2019...(): for EURO IV or V bus, exh.tech SCR or EGR",
             call. = FALSE
        )
      }
    }
    if(euro.class %in% c("VI")){
      if(!exh.tech %in% c("DPF+SCR")){
        #should be DPF+SCR
        warning("ef_eea2019...(): for EURO VI bus, exh.tech only DPF+SCR, resetting",
             call. = FALSE
        )
        exh.tech <- "DPF+SCR"
      }
    }

    if(veh.wt <= 15000){
      size <- "Urban Buses Midi <=15 t"
    }
    if(veh.wt > 15000 & veh.wt <= 18000){
      size <- "Urban Buses Standard 15 - 18 t"
    }
    if(veh.wt >18000){
      size <- "Urban Buses Articulated >18 t"
    }

    out <- vein::ef_eea(category = "BUS",
                 fuel = "D",
                 segment = size,
                 euro = euro.class,
                 tech = exh.tech,
                 pol = "PM",
                 mode = NA,
                 slope = route.slope,
                 load = eng.load,
                 speed = veh.spd)
    out <- as.numeric(out) #kill units for now

####################
#can we warn if outside speed range
#see code in ef_copert5...
###################

    ########################
    #fuel scaling -
    #(will be better way to do this)
    #pre 0.94; I 0.94; II 0.94; III 0.96; IV 1.00; V (erg/scr) 1.00; 1.00
    ########################
    if(fuel.corr){
      sc <- 1
      if(euro.class %in% c("PRE", "I", "II")) {
        sc <- 0.94
      }
      if(euro.class %in% c("III")) {
        sc <- 0.96
      }
      out <- out * sc
    }
    #units g/km to mg/km
    out <- out * 1000
    #might want to output
    #euro class, eng.load and route.slope?
    #     maybe standard and verbose options for output???
    return(data.frame(em.type, em.source, euro.class, exh.tech, route.def,
                      veh.spd = veh.spd, veh.wt, ans = out))

  } else {
    stop("ef_eea2019...(): embrs version, currently only setup for urban buses",
         call. = FALSE
    )
  }

}


#splatted function
#' @rdname ef_eea2019
#' @export
ef_eea2019_exh_pm2.5 <- function(...){
  #note for this pm10 = pm2.5 = pm
  #so we need to send vein PM and reset it to PM10
  out <- ef_eea2019_exh_pm(...)
  out$em.type <- "pm2.5"
  out
}

#splatted function
#' @rdname ef_eea2019
#' @export
ef_eea2019_exh_pm10 <- function(...){
  #note for this pm10=pm2.5=pm
  #so we need to send vein PM and reset it to PM10
  out <- ef_eea2019_exh_pm(...)
  out$em.type <- "pm10"
  out
}



#unexported code

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

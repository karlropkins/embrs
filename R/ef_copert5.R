############################################
#' @title ef_copert5
############################################

#' @name ef_copert5
#' @aliases ef_copert5_exhaust_pm ef_copert5_exhaust_pm2.5
#' ef_copert5_exhaust_pm10
#' @description Functions to estimate vehicle speed-based exhaust
#' PM2.5 and PM10 emission factors based on COPERT5.0 speed curves.
#' @param veh.spd average vehicle speed, in km/h
#' @param veh.type vehicle type, currently only coded for urban buses
#' @param veh.wt vehicle weight, in km.
#' @param em.type type of emission, currently PM, PM2.5 or PM10, see Note.
#' @param route.def route description
#' @param fuel.type fuel used by the vehicle, diesel, etc
#' @param fuel.corr (logical) apply fuel correction, see Note
#' @param em.source emission source: currently, exhaust only
#' @param euro.class EURO classification: PRE, I, II, III, IV, V+ERG,
#' V+SCR or VI only
#' @param eng.load engine load, as proportion: 0. 0.5 or 1 only
#' @param route.slope route slope, as proportion: -0.06, -0.04, -0.02, 0,
#' 0.02, 0.04 or 0.06 only
#' @param ... other arguments, currently ignored
#' @note Strictly, COPERT PM exhaust emission factors are PM, but because
#' vehicle exhaust PM is fine it is typically assumed that:
#'
#' PM10 = PM2.5 = PM for exhaust emissions from most conventional diesel and
#' gasoline vehicles.
#'
#' Fuel Corrections are applied to account for improvements in emissions
#' associated with improvements in fuel quality,
#'
#' @source
#' Average speed curve are based on COPERT 5 methods as documented in
#'
#' Dan Wakeling (Ricardo Energy & Environment).
#' Road vehicle emission factors for PM based on COPERT v5.0 (2017):
#' speed-emission factor equations. NAEI Ref:	ED62553001
#' https://naei.beis.gov.uk/data/ef-transport
#'
#' Fuel corrections are based on:
#'
#' TRL Emission Factors - scaling factors for mileage and improvements in fuel quality
#'  https://www.gov.uk/government/publications/road-vehicle-emission-factors-2009
#'

#splatted function
#' @export
ef_copert5_exh_pm <-
function(veh.spd = NULL, veh.type=NULL, veh.wt = NULL,
                                  em.type = "pm", route.def = NULL,
                                  fuel.type = NULL, fuel.corr = TRUE,
                                  em.source = "exhaust", euro.class = NULL,
                                  eng.load = NULL, route.slope = NULL, ...)
{

  ###################
  #to do
  #####################
  #formula from data source
  #fuel
  #euro.class to output??
  #correction factors???
  ######################
  if(is.null(veh.wt)){
    stop("ef...(): embrs needs veh.wt (vehicle weight in kg), see help",
         call. = FALSE
    )
  }
  if(is.null(veh.spd)){
    stop("ef_copert5_exhaust...(): needs veh.spd (vehicle speed in km/hr), see help",
         call. = FALSE
    )
  }
  if(is.null(route.def)){
    route.def <- "[unnamed]"
  }
  if(is.null(euro.class)){
    stop("ef_copert5...(): needs euro.class, see help",
         call. = FALSE
    )
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
  #currently bus only
  # could add coach and truck
  # from same lookup
  ####################

  if(veh.type == c("bus")){

    ######################
    #replacing previous ref
    #with larger data sent
    # document source
    #######################
    ref <- subset(ref_copert5_hdv_pm, vehicle.type=="bus")

    ####################
    #load can only be 0, 0.5 or 1
    #road.gradients can only be -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06
    ###################
    ref <- subset(ref, load==eng.load & road.gradient==route.slope)
    ##################
    #bus euro can only be PRE, I, II, III, IV, V+EGR, V+SCR, VI
    ##################
    ref <- subset(ref, euro.standard==euro.class)
    if(veh.wt <= 15000){
      ref <- subset(ref, size=="urban bus midi <=15 t")
    }
    if(veh.wt > 15000 & veh.wt <= 18000){
      ref <- subset(ref, size=="urban bus standard 15 - 18 t")
    }
    if(veh.wt >18000){
      ref <- subset(ref, size=="urban bus articulated >18 t")
    }
    if(nrow(ref)!=1){
      stop("ef_copert5...(): missing or wrong bus settings, check help",
           call. = FALSE
      )
    }
    if(veh.spd>ref$max.speed.km.h.){
      warning("ef_copert5...(): veh.spd out of range, resetting",
           call. = FALSE
      )
      veh.spd <- ref$max.speed.km.h.
    }
    if(veh.spd<ref$min.speed.km.h.){
      warning("ef_copert5...(): veh.spd out of range, resetting",
           call. = FALSE
      )
      veh.spd <- ref$min.speed.km.h.
    }

    #replacing equation with ref$fun
    #out <- (ref$a + (ref$b * veh.spd) + (ref$c * (veh.spd^2)) + ref$d/veh.spd)/(ref$e +
    #    (ref$f * veh.spd) + (ref$g * (veh.spd^2)))

    out <- eval(parse(text=ref$fun))

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
    return(data.frame(em.type, em.source, euro.class, route.def, veh.spd = veh.spd,
        veh.wt, ans = out))

  } else {
    stop("ef_copert5...(): embrs version, currently only setup for urban buses",
         call. = FALSE
    )
  }

}


#splatted function
#' @rdname ef_copert5
#' @export
ef_copert5_exh_pm2.5 <- function(...){
  #note for this pm10 = pm2.5 = pm
  #so we need to send vein PM and reset it to PM10
  out <- ef_copert5_exh_pm(...)
  out$em.type <- "pm2.5"
  out
}

#splatted function
#' @rdname ef_copert5
#' @export
ef_copert5_exh_pm10 <- function(...){
  #note for this pm10=pm2.5=pm
  #so we need to send vein PM and reset it to PM10
  out <- ef_copert5_exh_pm(...)
  out$em.type <- "pm10"
  out
}


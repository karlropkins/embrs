############################################
#' @title EMEP Emissions Factors
############################################

################################
#note sure if this should be called EMEP or COPERT
################################
#sergio or suggestions??
############################

#' @name ef_emep
#' @aliases ef_emep_exh_pm2.5 ef_emep_exh_pm10
#' @description Functions to estimate vehicle speed-based emission factors
#' based on EMEP handbook methods.
#'
#' IMPORTANT: THESE CURRENTLY ONLY WORK FOR EURO VI BUSES...
#' @param veh.spd (numeric, required) vehicle speed (in km/hr).
#' @param veh.wt (numeric, required) vehicle wt (in kg).
#' @param em.type (character) type of emissions to predict, just PM at the moment.
#' @param route.def (character) route description or identifier.
#' @param em.source (character) emission source, just exhaust at the moment.
#' @param euro.class (numeric at moment) the emission control classification,
#' currently as number 0 (pre euro) to euro 6.
#' @param ... other arguments, often passed on.
#' @returns These functions build data.frames of vehicle speed based emission
#' factors.
#' @note These may be moving to vein at some point...
#' @references These functions are based on methods developed and reported by:
#'
#' [handbook]



#in development....
#needs work...




#next jobs
#use vein bus EMEP/EEA method rather than my own cutdown code...

#ef.exh.pm function ef_emep_exh_pm (pm=pm10=pm2.5)
#dplyr::filter(vein:::sysdata$eea, Category == "BUS" & Pollutant=="PM" & Fuel=="D" & RoadSlope==0 & EuroStandard=="VI" & Load ==0.5, Segment=="Urban Buses Midi <=15 t")
#vein::ef_hdv_speed(v = "Ubus", eu="V", p= "PM", t="Midi", g="<=15")(30)
#vein::ef_hdv_speed(v = "Ubus", eu="VI", p= "PM", t="Midi", g="<=15")(30)
#last errors!!!


#I have .xlxs if it is any help

#require(readxl)
#excel_sheets("rtp_Copert5_PMEFs_final.xlsx")
#temp <- read_excel("rtp_Copert5_PMEFs_final.xlsx",
#           "HGVs & Buses", skip=7,
#           col_types = c("text"))
#filtered excel for just buses, size, e6
#temp <- read.delim("clipboard")
#there is one of these rows for
#each road.grad (-6, -4 -2, 0, 2,4,6)
#and each load (0, 50 100)...
#not everything that look like zero is zero in source...
#    watch the rounding...

#route.slope=0 is now route object default

#need to think about load handling
#vehicle object veh.load or eng.load=0.5 default?...

#fuel and emission tehc to track...


ef_emep_exh <- function(veh.spd=NULL, veh.wt=NULL,
                        em.type = "pm2.5", route.def=NULL,
                        em.source = "exhaust", euro.class=NULL,
                        ...){
  #######################
  #this is currently cut-down version from initial analysis
  ########################
  #this only work for EURO 6 BUS
  #so hard-coded
  ########################
  #replacing with vein functions

  #.args <- list(...)
  ##########################
  #ans wants to be the emep ef estimate
  #when this is done...

  ##############################
  #need to add route.slope to route defaults
  #0 in workhorse?
  #############################
  #need to add eng.load to vehicle defaults
  #50%?
  ##############################
  #this will need to know about exh.control for
  #some euro classes, e.g. BUS EURO 5
  ###############################
  #also need to decide how to handle
  #6/VI assignment

  #copert exhaust - cut down
  #single case
  #from previous analysis
  exh.lu <- data.frame(
    veh = "Buses",
    size = "Urban Buses Standard 15 - 18 t",
    fuel = "Diesel",
    euro= "HD Euro VI",
    road.grad = 0,
    veh.50 = 50,
    a = -0.0009729201,
    b= -0.0000579382,
    c= 0.0000011438,
    d= 0.0849296694,
    e = 1.00,
    f= -0.0401183801,
    g= 0.000485111,
    min.spd = 10,
    max.spd = 85,
    spd = 82,
    ref = 0.0031 #,
    #out = "~(a+b*spd+c*spd^2+d/spd)/(e+f*spd+g*spd^2)"
  )

  spd <- veh.spd
  ref <- exh.lu
  #based on formula that I had to manually paste from
  #excel where ref is above...
  out <- (ref$a+(ref$b*spd)+(ref$c*(spd^2))+ref$d/spd) /
    (ref$e+(ref$f*spd)+(ref$g*(spd^2)))
  #would also be a correction factor as of barlow...
  #but that is *1 for E6
  out <- out*1000 #so output is mg

  #out is ef as mg/veh/km
  #######################################

  data.frame(em.type, em.source, route.def, veh.spd = veh.spd, veh.wt,
             ans=out)
}


#splatted function
#' @rdname ef_emep
#' @export
ef_emep_exh_pm2.5 <- function(...){
  #note for this pm10 = pm2.5 = pm
  #so we need to send vein PM and reset it to PM10
  out <- ef_emep_exh(...)
  out$em.type <- "pm2.5"
  out
}

#splatted function
#' @rdname ef_emep
#' @export
ef_emep_exh_pm10 <- function(...){
  #note for this pm10=pm2.5=pm
  #so we need to send vein PM and reset it to PM10
  out <- ef_emep_exh(...)
  out$em.type <- "pm10"
  out
}









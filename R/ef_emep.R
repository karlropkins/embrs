############################################
#' @title EMEP Emissions Factors
############################################

#' @name ef_emep
#' @aliases ef_emep_exh_pm2.5 ef_emep_exh_pm10
#' @description Functions to estimate vehicle speed-based emission factors
#' based on EMEP handbook methods.
#' @param veh.spd (numeric, required) vehicle speed (in km/hr).
#' @param em.type (character) type of emissions to predict, just PM at the moment.
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

ef_emep_exh <- function(veh.spd=NULL, veh.wt=NULL,
                        em.type = "pm2.5", route.def=NULL,
                        em.source = "exhaust", euro.class=NULL,
                        ...){
  #######################
  #just to test....

  .args <- list(...)
  ##########################
  #ans wants to be the emep ef estimate
  #when this is done...

  data.frame(em.type, em.source, route.def, avg.spd = veh.spd, veh.wt,
             ans=veh.spd)
}


#splatted function
#' @rdname ef_emep
#' @export
ef_emep_exh_pm2.5 <- function(...){
  #note for this pm10=pm2.5=pm
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









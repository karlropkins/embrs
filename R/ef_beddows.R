############################################
#' @title Beddows Non-Exhaust Emissions Factors
############################################

#' @name ef_beddows
#' @aliases ef_beddows_nee_pm
#' ef_beddows_brake_pm2.5 ef_beddows_tyre_pm2.5 ef_beddows_road_pm2.5
#' ef_beddows_resusp_pm2.5
#' ef_beddows_brake_pm10 ef_beddows_tyre_pm10 ef_beddows_road_pm10
#' ef_beddows_resusp_pm10
#' @description Functions to estimated vehicle weight-based non-exhaust
#' PM2.5 and PM10 emission factors based on methods of Beddows.
#' @param veh.wt (numeric, required) vehicle weight (in kg).
#' @param em.type (character) type of emissions to predict, by default PM2.5
#' and PM10.
#' @param em.source (character) emission source, by default brake, tyre, road
#' and resuspension, but can be any combination.
#' @param brk.regen vehicle regenerative braking, default \code{FALSE} means
#' not installed/used or a numeric (0-1) for efficiency if installed/used,
#' e.g. 0.25 = 25% efficient (equivalent to a 75% of a conventional
#' non-regenerative brake contribution).
#' @param route.def (character) route definitions, by default urban, rural and motorway.
#' @param route.source (character) route sources, must be UK NAEI.
#' @param ... other arguments, often passed on.
#' @returns These functions build data.frames of urban, rural and motorway emission
#' factors for non-exhaust PM2.5 and PM10. \code{ef_beddows_nee_pm} is the main
#' function, and others are wrappers for single soucre and type emissions factors.
#' @note These may be moving to vein at some point...
#' @references These functions are based on methods developed and reported by:
#'
#' Beddows, D.C. and Harrison, R.M., 2021. PM10 and PM2. 5 emission factors for
#' non-exhaust particles from road vehicles: Dependence upon vehicle mass and
#' implications for battery electric vehicles. Atmospheric Environment, 244,
#' p.117886. \url{https://doi.org/10.1016/j.atmosenv.2020.117886}

########################
#how to do subscript in this markdown pm10 and pm2.5 in @returns
#i think dois need special handling??

########################
#could make one ef_beddows_nepm function
#with extra args em.type and source


#need to rename these....

ef_beddows_nee_pm <- function(veh.wt, em.type = c("pm2.5", "pm10"),
                              em.source = c("brake", "tyre", "road",
                                            "resusp"), brk.regen=FALSE,
                              route.def=c("urban", "rural", "motorway"),
                              route.source = "uk naei",
                              verbose = FALSE, ...){
  if(tolower(route.source) != "uk naei"){
    stop("[embrs]: ef_beddows...() only intended for use with naei route definitions.",
            call.=FALSE)
  }
  type <- rep(c(rep("pm2.5", 3), rep("pm10", 3)),4)
  route <- c("urban", "rural", "motorway")
  source <- rep(c("brake", "tyre", "road", "resusp"), each = 6)
  #constants from beddows & harrison
  #need checking
  b <- c(4.2, 1.8, 0.4, 11, 4.5, 1,
         5.8, 4.5, 3.8, 8.2, 6.4, 5.5,
         rep(2.8, 3), rep(5.1, 3),
         rep(2.0, 3), rep(8.2, 3))
  eb <- c(1.1, 0.9, 0.4, 2.7, 2.4, 1,
          0.5, 0.3, 0.3, 0.6, 0.5, 0.4,
          rep(0.5, 3), rep(0.9, 3),
          rep(0.8, 3), rep(3.2, 3))
  c <- c(1.9, 1.5, 1.3, 1.9, 1.5, 1.3,
         2.3, 2.3, 2.3, 2.3, 2.3, 2.3,
         rep(1.5, 3), rep(1.5, 3),
         rep(1.1, 3), rep(1.1, 3))
  ec <- c(0.2, 0.3, 0.4, 0.2, 0.3, 0.4,
          0.4, 0.4, 0.4, 0.4, 0.4, 0.4,
          rep(0.1, 3), rep(0.1, 3),
          rep(0.4, 3), rep(0.4, 3))
  ans <- b*((veh.wt/1000)^(1/c))
  ans.hi <- (b+eb)*((veh.wt/1000)^(1/(c-ec)))
  ans.low <- (b-eb)*((veh.wt/1000)^(1/(c+ec)))
  #regenerative braking
  brk.regen <- as.numeric(brk.regen)
  if(brk.regen){
    temp <- 1 - brk.regen
    ans[1:6] <- ans[1:6] * temp
    ans.low[1:6] <- ans.low[1:6] * temp
    ans.hi[1:6] <- ans.hi[1:6] * temp
  }
  v.out <- data.frame(em.type = type, em.source = source, route.def = route,
                      veh.wt, brk.regen, b, eb, c, ec, ans, ans.low, ans.hi)
  v.out <- v.out[v.out$em.type %in% tolower(em.type),]
  v.out <- v.out[v.out$em.source %in% tolower(em.source),]
  v.out <- v.out[v.out$route.def %in% tolower(route.def),]
  if(verbose) {
    return(v.out)
  } else {
    return(v.out[!names(v.out) %in% c("b", "eb", "c", "ec")])
  }
}



#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_brake_pm2.5 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm2.5", em.source = "brake",
                      ...)
  }

#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_brake_pm10 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm10", em.source = "brake",
                      ...)
  }



#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_tyre_pm2.5 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm2.5", em.source = "tyre",
                      ...)
  }

#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_tyre_pm10 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm10", em.source = "tyre",
                      ...)
  }


#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_road_pm2.5 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm2.5", em.source = "road",
                      ...)
  }

#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_road_pm10 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm10", em.source = "road",
                      ...)
  }

#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_resusp_pm2.5 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm2.5", em.source = "resusp",
                      ...)
  }

#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_resusp_pm10 <-
  function(em.type, em.source, ...){
    ef_beddows_nee_pm(em.type="pm10", em.source = "resusp",
                      ...)
  }





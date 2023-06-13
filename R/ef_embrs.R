############################################
#' @title embrs Non-Exhaust Emissions Factors
############################################

#' @name ef_embrs
#' @aliases ef_embrs1_nee_pm
#' ef_embrs1_brake_pm2.5 ef_embrs1_tyre_pm2.5 ef_embrs1_road_pm2.5
#' ef_embrs1_resusp_pm2.5
#' ef_embrs1_brake_pm10 ef_embrs1_tyre_pm10 ef_embrs_road1_pm10
#' ef_embrs1_resusp_pm10
#' ef_embrs2_brake_pm2.5 ef_embrs2_tyre_pm2.5 ef_embrs2_brake_pm10
#' ef_embrs1_tyre_pm10
#' @description Functions to estimated vehicle weight, speed and brake-work-based
#' non-exhaust PM2.5 and PM10 emission factors based the extension of Beddows
#' proposed in embrs paper.
#' @param veh.wt (numeric, required) vehicle weight (in kg).
#' @param veh.spd (numeric, required) vehicle speed (in km/hr).
#' @param veh.type (character) type of vehicle, e.g bus.
#' @param em.type (character) type of emissions to predict, by default PM2.5
#' and PM10.
#' @param em.source (character) emission source, by default brake, tyre, road
#' and resuspension, but can be any combination.
#' @param brk.regen vehicle regenerative braking, default \code{FALSE} means
#' not installed/used or a numeric (0-1) for efficiency if installed/used,
#' e.g. 0.25 = 25% efficient (equivalent to a 75% of a conventional
#' non-regenerative brake contribution).
#' @param route.def (character) route definition(s).
#' @param route.source (character) route source(s).
#' @param verbose (logical) If TRUE, include methods details
#' when reporting EF predictions.
#' @param ... other arguments, often passed on.
#' @param brk_b (numeric; ef_embrs2_brake... only) brake work parameter
#' proposed in Tivey et al (2023).
#' @param tyr_t (numeric; ef_embrs2_brake... only) tyre work parameter
#' proposed in Tivey et al (2023).
#' @returns These functions build data.frames of vehicle weight and speed,
#' (and brake and tyre work) based emission factors for non-exhaust PM2.5
#' and PM10. \code{ef_embrs1_nee_pm} is the main (weight and speed)
#' function, other \code{ef_embrs1...} functions are wrappers for this for
#' single source and type emissions factors, and \code{ef_embrs2...} functions
#' are brake and tyre work based functions proposed in Tivey et al (2023).
#' @note These may be moving to vein at some point...
#' @references These functions are based on methods developed and reported by:
#'
#' Beddows, D.C. and Harrison, R.M., 2021. PM10 and PM2.5 emission factors for
#' non-exhaust particles from road vehicles: Dependence upon vehicle mass and
#' implications for battery electric vehicles. Atmospheric Environment, 244,
#' p.117886. \url{https://doi.org/10.1016/j.atmosenv.2020.117886}.
#'
#' And, extrapolated to speed, brake and tyre-wear related emission factors in:
#'
#' Tivey, J., Davies, H.C., Levine, J.G., Zietsman, J., Bartington, S.,
#' Ibarra-Espinosa, S. and Ropkins, K, 2023. Meta-Analysis as Early Evidence on
#' the Particulate Emissions Impact of EURO VI on Battery Electric Bus Fleet
#' Transitions. Sustainability 15, 1522. \url{https://doi.org/10.3390/su15021522}

########################
#these are the embrs speed, etc functions from the embrs paper
#I am hoping these can be tidied a bit...

#avg.spd model for nee (embrs model)


#notes
###########################
#error formatting

#stop("[embrs] bus...() needs veh.wt, see help?",
#     call.=FALSE)


#do I want to import from dplyr??

#splatted function
#' @rdname ef_embrs
#' @importFrom stats lm predict
#' @importFrom utils write.table
#' @export
ef_embrs1_nee_pm <- function(veh.wt, veh.spd=NULL, veh.type = NULL,
                              em.type = c("pm2.5", "pm10"),
                              em.source = c("brake", "tyre", "road",
                                            "resusp"), brk.regen=FALSE,
                              route.def=c("urban", "rural", "motorway"),
                              route.source = "uk naei",
                              verbose = FALSE, ...){

  if(is.null(veh.spd)){
    stop("[embrs] ef_embrs1...() needs veh.spd, vehicle speed in km/hr",
         call.=FALSE)
  }
  ans <- ef_beddows_nee_pm(veh.wt=veh.wt, em.type=em.type,
                         em.source=em.source[1],
                         brk.regen=brk.regen,
                         route.def=c("urban", "rural", "motorway"),
                         route.source = "uk naei",
                         verbose = FALSE, ...)

  ######################################
  #this needs thinking about
  #em.source[1] in case them send more
  #can only do one at go at moment....
  #must force the last ones
  #####################################
  #maybe stick the lot in a lapply
  #and bind if more than one em.source
  #####################################

  ans$veh.spd <- naei_route2spd("veh.spd", veh.type=veh.type,
                         route.source = "uk naei",
                         route.def=c("urban", "rural", "motorway"))

  ####################
  #get above from local function

  mod1 <- lm(ans~poly(veh.spd, 1), ans)
  mod1.low <- lm(ans.low~poly(veh.spd, 1), ans)
  mod1.hi <- lm(ans.hi~poly(veh.spd, 1), ans)

  #############################
  #ignoring for now
  #function terms
  #only need mod1 outputs
  #############################
  #guess rep length is length of supplied veh.spd
  #################

  #l1 <- c(rep(mod1$coefficients[1], 3),
  #        rep(mod2$coefficients[1], 3))
  #el1 <- abs(c(rep((mod1.hi$coefficients[1]-mod1.low$coefficients[1])/2, 3),
  #             rep((mod2.hi$coefficients[1]-mod2.low$coefficients[1])/2, 3)))

  #l2 <- c(rep(mod1$coefficients[2], 3),
  #        rep(mod2$coefficients[2], 3))
  #el2 <- abs(c(rep((mod1.hi$coefficients[2]-mod1.low$coefficients[2])/2, 3),
  #             rep((mod2.hi$coefficients[2]-mod2.low$coefficients[2])/2, 3)))

  ans <- c(predict(mod1, newdata=data.frame(
    veh.spd=veh.spd)))

  ans.low <- c(predict(mod1.low, newdata=data.frame(
    veh.spd=veh.spd)))

  ans.hi <- c(predict(mod1.hi, newdata=data.frame(
    veh.spd=veh.spd)))

  ########################################
  #check for stupid mod outputs -values???
  ########################################

  v.out <- data.frame(em.type = em.type, em.source = em.source,
                      route.def = route.def, veh.wt, veh.spd,
                      brk.regen = as.numeric(brk.regen),
                      #l1, el1, l2, el2,
                      ans, ans.low, ans.hi)
  #############################
  #look at beddows function here onwards...
  #for verbose handling
  if(verbose){
    v.out$method.name <- "Tivey et al (2023)"
    v.out$method.descr <- paste("Weight and speed based model of vehicle ", v.out$em.type, " ",
                                v.out$em.source,  " emissions",
                                ", based on extrapolation of Beddows & Harrison (2021)",
                                sep="")
    v.out$method.ref <- "https://doi.org/10.3390/su15021522"
    v.out
  } else {
    v.out
  }
}



#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_brake_pm2.5 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm2.5", em.source = "brake",
                      ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_tyre_pm2.5 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm2.5", em.source = "tyre",
                     ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_road_pm2.5 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm2.5", em.source = "road",
                     ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_resusp_pm2.5 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm2.5", em.source = "resusp",
                     ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_brake_pm10 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm10", em.source = "brake",
                     ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_tyre_pm10 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm10", em.source = "tyre",
                     ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_road_pm10 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm10", em.source = "road",
                     ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs1_resusp_pm10 <-
  function(em.type, em.source, ...){
    ef_embrs1_nee_pm(em.type="pm10", em.source = "resusp",
                     ...)
  }


#########################
#ef_embrs2
#########################

# only brake and tyre models for embrs2

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs2_brake_pm <- function(veh.wt, veh.spd=NULL, veh.type = NULL,
                               brk_b = NULL,
                             em.type = c("pm2.5", "pm10"),
                             em.source = c("brake"), brk.regen=FALSE,
                             route.def=c("urban", "rural", "motorway"),
                             route.source = "uk naei",
                             verbose = FALSE, ...){
  if(is.null(veh.spd)){
    stop("[embrs] ef_embrs2_brake...() needs veh.spd, vehicle speed in km/hr",
         call.=FALSE)
  }
  if(is.null(brk_b)){
    stop("[embrs] ef_embrs2_brake...() needs brk_b",
         call.=FALSE)
  }

  ans <- ef_beddows_nee_pm(veh.wt=veh.wt, em.type=em.type,
                           em.source=em.source[1],
                           brk.regen=brk.regen,
                           route.def=c("urban", "rural", "motorway"),
                           route.source = "uk naei",
                           verbose = FALSE, ...)
  #see ef_embrs1... about em.source[1]

  temp <- embrs_ukbc_lookup()

  #########################
  #this can be simplified
  #########################
  ans$veh.spd <- temp$veh.spd_km.h[4:6]
  ans$acc <- as.numeric(temp$acc[4:6])
  ans$prop.acc <- as.numeric(temp$acc.pc[4:6])/100
  ans$dec <- as.numeric(temp$dec[4:6])
  ans$prop.dec <- as.numeric(temp$dec.pc[4:6])/100
  ans$brk_b <- (-ans$dec * ans$prop.dec)/ans$veh.spd
  ans$tyr_t <- ((-ans$dec * ans$prop.dec)+(ans$acc * ans$prop.acc))/ans$veh.spd
  ############################


  mod1 <- lm(ans~poly(brk_b, 1), ans)
  mod1.low <- lm(ans.low~poly(brk_b, 1), ans)
  mod1.hi <- lm(ans.hi~poly(brk_b, 1), ans)

  ##for later, see embrs1 handling of this and verbose...
  ##l1 <- c(rep(mod1$coefficients[1], 3),
  ##        rep(mod2$coefficients[1], 3))
  ##el1 <- abs(c(rep((mod1.hi$coefficients[1]-mod1.low$coefficients[1])/2, 3),
  ##             rep((mod2.hi$coefficients[1]-mod2.low$coefficients[1])/2, 3)))
  ##
  ##l2 <- c(rep(mod1$coefficients[2], 3),
  ##        rep(mod2$coefficients[2], 3))
  ##el2 <- abs(c(rep((mod1.hi$coefficients[2]-mod1.low$coefficients[2])/2, 3),
  ##             rep((mod2.hi$coefficients[2]-mod2.low$coefficients[2])/2, 3)))


  ##ukbc <- data.frame(
  ##  avg.spd=c(16.9, 10.0, 31.3),
  ##  acc = c(0.442, 0.4181, 0.2877),
  ##  prop.acc = c(0.4188, 0.3574, 0.4038),
  ##  dec = c(-0.619, -0.647, -0.5912),
  ##  prop.dec =c(0.2384, 0.2154, 0.2201)
  ##)

  #see previous
  ##ukbc$brk <- (-ukbc$dec * ukbc$prop.dec)/ukbc$avg.spd
  ##ukbc$tyr <- ((-ukbc$dec * ukbc$prop.dec)+(ukbc$acc * ukbc$prop.acc))/ukbc$avg.spd

  #return(list(ans,ukbc))

  ans <- predict(mod1, newdata=data.frame(
    brk_b=brk_b))
  ans.low <- predict(mod1.low, newdata=data.frame(
    brk_b=brk_b))
  ans.hi <- predict(mod1.hi, newdata=data.frame(
    brk_b=brk_b))

  v.out <- data.frame(em.type = em.type, em.source = em.source,
                      route.def = route.def, veh.wt, veh.spd,
                      brk_b,
                      brk.regen = as.numeric(brk.regen),
                      #l1, el1, l2, el2,
                      ans, ans.low, ans.hi)
  if(verbose){
    v.out$method.name <- "Tivey et al (2023)"
    v.out$method.descr <- paste("Weight and braking function ",
                                "based model of vehicle ", v.out$em.type, " ",
                                v.out$em.source,  " emissions",
                                ", based on extrapolation of Beddows & Harrison (2021)",
                                sep="")
    v.out$method.ref <- "https://doi.org/10.3390/su15021522"
    v.out
  } else {
    v.out
  }
}

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs2_brake_pm2.5 <-
  function(em.type, em.source, ...){
    ef_embrs2_brake_pm(em.type="pm2.5", em.source = "brake",
                     ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs2_brake_pm10 <-
  function(em.type, em.source, ...){
    ef_embrs2_brake_pm(em.type="pm10", em.source = "brake",
                       ...)
  }



#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs2_tyre_pm <- function(veh.wt, veh.spd=NULL, veh.type = NULL,
                               tyr_t = NULL,
                               em.type = c("pm2.5", "pm10"),
                               em.source = c("tyre"), brk.regen=FALSE,
                               route.def=c("urban", "rural", "motorway"),
                               route.source = "uk naei",
                               verbose = FALSE, ...){
  if(is.null(veh.spd)){
    stop("[embrs] ef_embrs2_tyre...() needs veh.spd, vehicle speed in km/hr",
         call.=FALSE)
  }
  if(is.null(tyr_t)){
    stop("[embrs] ef_embrs2_tyre...() needs tyr_t, tyre constant",
         call.=FALSE)
  }

  ans <- ef_beddows_nee_pm(veh.wt=veh.wt, em.type=em.type,
                           em.source=em.source[1],
                           brk.regen=brk.regen,
                           route.def=c("urban", "rural", "motorway"),
                           route.source = "uk naei",
                           verbose = FALSE, ...)
  #see ef_embrs1... about em.source[1]
  temp <- embrs_ukbc_lookup()

  #########################
  #this can be simplified
  #########################
  ans$veh.spd <- temp$veh.spd_km.h[4:6]
  ans$acc <- as.numeric(temp$acc[4:6])
  ans$prop.acc <- as.numeric(temp$acc.pc[4:6])/100
  ans$dec <- as.numeric(temp$dec[4:6])
  ans$prop.dec <- as.numeric(temp$dec.pc[4:6])/100
  ans$brk_b <- (-ans$dec * ans$prop.dec)/ans$veh.spd
  ans$tyr_t <- ((-ans$dec * ans$prop.dec)+(ans$acc * ans$prop.acc))/ans$veh.spd
  ############################

  mod1 <- lm(ans~poly(tyr_t, 1), ans)
  mod1.low <- lm(ans.low~poly(tyr_t, 1), ans)
  mod1.hi <- lm(ans.hi~poly(tyr_t, 1), ans)

  ##for later, see embrs1 handling of this and verbose...
  ##l1 <- c(rep(mod1$coefficients[1], 3),
  ##        rep(mod2$coefficients[1], 3))
  ##el1 <- abs(c(rep((mod1.hi$coefficients[1]-mod1.low$coefficients[1])/2, 3),
  ##             rep((mod2.hi$coefficients[1]-mod2.low$coefficients[1])/2, 3)))
  ##
  ##l2 <- c(rep(mod1$coefficients[2], 3),
  ##        rep(mod2$coefficients[2], 3))
  ##el2 <- abs(c(rep((mod1.hi$coefficients[2]-mod1.low$coefficients[2])/2, 3),
  ##             rep((mod2.hi$coefficients[2]-mod2.low$coefficients[2])/2, 3)))


  ##ukbc <- data.frame(
  ##  avg.spd=c(16.9, 10.0, 31.3),
  ##  acc = c(0.442, 0.4181, 0.2877),
  ##  prop.acc = c(0.4188, 0.3574, 0.4038),
  ##  dec = c(-0.619, -0.647, -0.5912),
  ##  prop.dec =c(0.2384, 0.2154, 0.2201)
  ##)

  #see previous
  ##ukbc$brk <- (-ukbc$dec * ukbc$prop.dec)/ukbc$avg.spd
  ##ukbc$tyr <- ((-ukbc$dec * ukbc$prop.dec)+(ukbc$acc * ukbc$prop.acc))/ukbc$avg.spd

  #return(list(ans,ukbc))

  ans <- predict(mod1, newdata=data.frame(
    tyr_t=tyr_t))
  ans.low <- predict(mod1.low, newdata=data.frame(
    tyr_t=tyr_t))
  ans.hi <- predict(mod1.hi, newdata=data.frame(
    tyr_t=tyr_t))

  v.out <- data.frame(em.type = em.type, em.source = em.source,
                      route.def = route.def, veh.wt, veh.spd,
                      tyr_t,
                      brk.regen = as.numeric(brk.regen),
                      #l1, el1, l2, el2,
                      ans, ans.low, ans.hi)
  if(verbose){
    v.out$method.name <- "Tivey et al (2023)"
    v.out$method.descr <- paste("Weight and accel/dec function ",
                                "based model of vehicle ", v.out$em.type, " ",
                                v.out$em.source,  " emissions",
                                ", based on extrapolation of Beddows & Harrison (2021)",
                                sep="")
    v.out$method.ref <- "https://doi.org/10.3390/su15021522"
    v.out
  } else {
    v.out
  }

}

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs2_tyre_pm2.5 <-
  function(em.type, em.source, ...){
    ef_embrs2_tyre_pm(em.type="pm2.5", em.source = "tyre",
                       ...)
  }

#splatted function
#' @rdname ef_embrs
#' @export
ef_embrs2_tyre_pm10 <-
  function(em.type, em.source, ...){
    ef_embrs2_tyre_pm(em.type="pm10", em.source = "tyre",
                       ...)
  }














































########################################
#working




#speed for brake
brake.embr.avgspd <- function(wt, output="df"){
  require(dplyr)
  ans <- brake.beddows(wt)
  ans$avg.spd <- c(32, 62, 82)
  mod1 <- lm(ans~poly(avg.spd, 1),
             data=dplyr::filter(ans, ref=="pm2.5"))
  mod1.low <- lm(ans.low~poly(avg.spd, 1),
                 data=dplyr::filter(ans, ref=="pm2.5"))
  mod1.hi <- lm(ans.hi~poly(avg.spd, 1),
                data=dplyr::filter(ans, ref=="pm2.5"))

  mod2 <- lm(ans~poly(avg.spd, 1),
             data=dplyr::filter(ans, ref=="pm10"))
  mod2.low <- lm(ans.low~poly(avg.spd, 1),
                 data=dplyr::filter(ans, ref=="pm10"))
  mod2.hi <- lm(ans.hi~poly(avg.spd, 1),
                data=dplyr::filter(ans, ref=="pm10"))

  ref <- c(rep("pm2.5", 3), rep("pm10", 3))
  route <- c("Outer London", "inner London",
             "Rural")
  l1 <- c(rep(mod1$coefficients[1], 3),
          rep(mod2$coefficients[1], 3))
  el1 <- abs(c(rep((mod1.hi$coefficients[1]-mod1.low$coefficients[1])/2, 3),
               rep((mod2.hi$coefficients[1]-mod2.low$coefficients[1])/2, 3)))

  l2 <- c(rep(mod1$coefficients[2], 3),
          rep(mod2$coefficients[2], 3))
  el2 <- abs(c(rep((mod1.hi$coefficients[2]-mod1.low$coefficients[2])/2, 3),
               rep((mod2.hi$coefficients[2]-mod2.low$coefficients[2])/2, 3)))


  ans <- c(predict(mod1, newdata=data.frame(
    avg.spd=c(16.9, 10.0, 31.3))),
    predict(mod2, newdata=data.frame(
      avg.spd=c(16.9, 10.0, 31.3))))

  ans.low <- c(predict(mod1.low, newdata=data.frame(
    avg.spd=c(16.9, 10.0, 31.3))),
    predict(mod2.low, newdata=data.frame(
      avg.spd=c(16.9, 10.0, 31.3))))

  ans.hi <- c(predict(mod1.hi, newdata=data.frame(
    avg.spd=c(16.9, 10.0, 31.3))),
    predict(mod2.hi, newdata=data.frame(
      avg.spd=c(16.9, 10.0, 31.3))))

  out <- data.frame(ref, route, l1, el1, l2, el2,
                    ans, ans.low, ans.hi)

  if(output=="latex"){
    #to do
    #write.table(a, "clipboard", row.names = F, col.names = F, quote=F)
    ref <- gsub("pm2.5", "$PM_{2.5}$", ref)
    ref <- gsub("pm10", "$PM_{10}$", ref)
    out2 <- paste(paste(route, ref, sep= " "),
                  " & ", signif(l1, 3), " $\\pm$ ", signif(el1, 3), " & ", signif(l2, 3), " $\\pm$ ", signif(el2, 3), " & ",
                  signif(ans, 2),
                  " (", signif(ans.low, 2), " - ",
                  signif(ans.hi, 2), ") \\\\", sep="")
    write.table(out2, "clipboard", row.names = F, col.names = F, quote=F)
  }
  return(out)
}

#proxies for brake
##########################
#need alternative for yyy
##########################
##yyy <- readRDS("yyy.rds")

brake.embr.brkwrk <- function(wt, output="df"){
  ans <- brake.beddows(wt)
  ans$avg.spd <- c(32, 62, 82)
  ans$acc <- as.numeric(yyy$pos[4:6])
  ans$prop.acc <- as.numeric(yyy$pos.pc[4:6])/100
  ans$dec <- as.numeric(yyy$neg[4:6])
  ans$prop.dec <- as.numeric(yyy$neg.pc[4:6])/100
  ans$brk <- (-ans$dec * ans$prop.dec)/ans$avg.spd
  ans$tyr <- ((-ans$dec * ans$prop.dec)+(ans$acc * ans$prop.acc))/ans$avg.spd

  #return(ans)

  mod1 <- lm(ans~poly(brk, 1),
             data=dplyr::filter(ans, ref=="pm2.5"))

  mod1.low <- lm(ans.low~poly(brk, 1),
                 data=dplyr::filter(ans, ref=="pm2.5"))
  mod1.hi <- lm(ans.hi~poly(brk, 1),
                data=dplyr::filter(ans, ref=="pm2.5"))

  mod2 <- lm(ans~poly(brk, 1),
             data=dplyr::filter(ans, ref=="pm10"))
  mod2.low <- lm(ans.low~poly(brk, 1),
                 data=dplyr::filter(ans, ref=="pm10"))
  mod2.hi <- lm(ans.hi~poly(brk, 1),
                data=dplyr::filter(ans, ref=="pm10"))

  ref <- c(rep("pm2.5", 3), rep("pm10", 3))
  route <- c("Outer London", "inner London",
             "Rural")

  l1 <- c(rep(mod1$coefficients[1], 3),
          rep(mod2$coefficients[1], 3))
  el1 <- abs(c(rep((mod1.hi$coefficients[1]-mod1.low$coefficients[1])/2, 3),
               rep((mod2.hi$coefficients[1]-mod2.low$coefficients[1])/2, 3)))

  l2 <- c(rep(mod1$coefficients[2], 3),
          rep(mod2$coefficients[2], 3))
  el2 <- abs(c(rep((mod1.hi$coefficients[2]-mod1.low$coefficients[2])/2, 3),
               rep((mod2.hi$coefficients[2]-mod2.low$coefficients[2])/2, 3)))


  ukbc <- data.frame(
    avg.spd=c(16.9, 10.0, 31.3),
    acc = c(0.442, 0.4181, 0.2877),
    prop.acc = c(0.4188, 0.3574, 0.4038),
    dec = c(-0.619, -0.647, -0.5912),
    prop.dec =c(0.2384, 0.2154, 0.2201)
  )

  ukbc$brk <- (-ukbc$dec * ukbc$prop.dec)/ukbc$avg.spd
  ukbc$tyr <- ((-ukbc$dec * ukbc$prop.dec)+(ukbc$acc * ukbc$prop.acc))/ukbc$avg.spd

  #return(list(ans,ukbc))

  ans <- c(predict(mod1, newdata=data.frame(
    brk=ukbc$brk)),
    predict(mod2, newdata=data.frame(
      brk=ukbc$brk)))

  ans.low <- c(predict(mod1.low, newdata=data.frame(
    brk=ukbc$brk)),
    predict(mod2.low, newdata=data.frame(
      brk=ukbc$brk)))

  ans.hi <- c(predict(mod1.hi, newdata=data.frame(
    brk=ukbc$brk)),
    predict(mod2.hi, newdata=data.frame(
      brk=ukbc$brk)))

  #print(ukbc$brk)

  out <- data.frame(ref, route, l1, el1, l2, el2,
                    ans, ans.low, ans.hi)

  if(output=="latex"){
    #to do
    #write.table(a, "clipboard", row.names = F, col.names = F, quote=F)
    ref <- gsub("pm2.5", "$PM_{2.5}$", ref)
    ref <- gsub("pm10", "$PM_{10}$", ref)
    out2 <- paste(paste(route, ref, sep= " "),
                  " & ", signif(l1, 3), " $\\pm$ ", signif(el1, 3), " & ", signif(l2, 3), " $\\pm$ ", signif(el2, 3), " & ",
                  signif(ans, 2),
                  " (", signif(ans.low, 2), " - ",
                  signif(ans.hi, 2), ") \\\\", sep="")
    write.table(out2, "clipboard", row.names = F, col.names = F, quote=F)
  }
  return(out)
}



tyre.embr.brkwrk <- function(wt, output="df"){
  ans <- tyre.beddows(wt)
  ans$avg.spd <- c(32, 62, 82)
  ans$acc <- as.numeric(yyy$pos[4:6])
  ans$prop.acc <- as.numeric(yyy$pos.pc[4:6])/100
  ans$dec <- as.numeric(yyy$neg[4:6])
  ans$prop.dec <- as.numeric(yyy$neg.pc[4:6])/100
  ans$brk <- (-ans$dec * ans$prop.dec)/ans$avg.spd
  ans$tyr <- ((-ans$dec * ans$prop.dec)+(ans$acc * ans$prop.acc))/ans$avg.spd

  #return(ans)

  mod1 <- lm(ans~poly(tyr, 1),
             data=dplyr::filter(ans, ref=="pm2.5"))

  mod1.low <- lm(ans.low~poly(tyr, 1),
                 data=dplyr::filter(ans, ref=="pm2.5"))
  mod1.hi <- lm(ans.hi~poly(tyr, 1),
                data=dplyr::filter(ans, ref=="pm2.5"))

  mod2 <- lm(ans~poly(tyr, 1),
             data=dplyr::filter(ans, ref=="pm10"))
  mod2.low <- lm(ans.low~poly(tyr, 1),
                 data=dplyr::filter(ans, ref=="pm10"))
  mod2.hi <- lm(ans.hi~poly(tyr, 1),
                data=dplyr::filter(ans, ref=="pm10"))

  ref <- c(rep("pm2.5", 3), rep("pm10", 3))
  route <- c("Outer London", "inner London",
             "Rural")

  l1 <- c(rep(mod1$coefficients[1], 3),
          rep(mod2$coefficients[1], 3))
  el1 <- abs(c(rep((mod1.hi$coefficients[1]-mod1.low$coefficients[1])/2, 3),
               rep((mod2.hi$coefficients[1]-mod2.low$coefficients[1])/2, 3)))

  l2 <- c(rep(mod1$coefficients[2], 3),
          rep(mod2$coefficients[2], 3))
  el2 <- abs(c(rep((mod1.hi$coefficients[2]-mod1.low$coefficients[2])/2, 3),
               rep((mod2.hi$coefficients[2]-mod2.low$coefficients[2])/2, 3)))

  ukbc <- data.frame(
    avg.spd=c(16.9, 10.0, 31.3),
    acc = c(0.442, 0.4181, 0.2877),
    prop.acc = c(0.4188, 0.3574, 0.4038),
    dec = c(-0.619, -0.647, -0.5912),
    prop.dec =c(0.2384, 0.2154, 0.2201)
  )

  ukbc$brk <- (-ukbc$dec * ukbc$prop.dec)/ukbc$avg.spd
  ukbc$tyr <- ((-ukbc$dec * ukbc$prop.dec)+(ukbc$acc * ukbc$prop.acc))/ukbc$avg.spd

  #return(list(ans,ukbc))

  ans <- c(predict(mod1, newdata=data.frame(
    tyr=ukbc$tyr)),
    predict(mod2, newdata=data.frame(
      tyr=ukbc$tyr)))

  ans.low <- c(predict(mod1.low, newdata=data.frame(
    tyr=ukbc$tyr)),
    predict(mod2.low, newdata=data.frame(
      tyr=ukbc$tyr)))

  ans.hi <- c(predict(mod1.hi, newdata=data.frame(
    tyr=ukbc$tyr)),
    predict(mod2.hi, newdata=data.frame(
      tyr=ukbc$tyr)))

  out <- data.frame(ref, route, l1, el1, l2, el2,
                    ans, ans.low, ans.hi)

  if(output=="latex"){
    #to do
    #write.table(a, "clipboard", row.names = F, col.names = F, quote=F)
    ref <- gsub("pm2.5", "$PM_{2.5}$", ref)
    ref <- gsub("pm10", "$PM_{10}$", ref)
    out2 <- paste(paste(route, ref, sep= " "),
                  " & ", signif(l1, 3), " $\\pm$ ", signif(el1, 3), " & ", signif(l2, 3), " $\\pm$ ", signif(el2, 3), " & ",
                  signif(ans, 2),
                  " (", signif(ans.low, 2), " - ",
                  signif(ans.hi, 2), ") \\\\", sep="")
    write.table(out2, "clipboard", row.names = F, col.names = F, quote=F)
  }
  return(out)
}





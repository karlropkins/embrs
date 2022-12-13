############################################
#' @title Beddows Non-Exhaust Emissions Factors
############################################

#' @name ef_beddows
#' @aliases ef_beddows_brake_pm2.5 ef_beddows_tyre_pm2.5 ef_beddows_road_pm2.5
#' ef_beddows_resusp_pm2.5
#' @description Functions to estimated vehicle weight-based non-exhaust
#' PM2.5 and PM10 emission factors based on methods of Beddows.
#' @param veh.wt Vehicle weight (in kg).
#' @param brk.regen vehicle regenerative braking, default \code{FALSE} means
#' not installed/used or a numeric (0-1) for efficiency if installed/used,
#' e.g. 0.25 = 25% efficient (equivalent to a 75% of a conventional
#' non-regenerative brake contribution).
#' @param output ignore for now... (built the tables in the embrs paper but
#' most likely revising for reporter element of package...)
#' @returns These functions data.frame of Urban, Rural and Motorway emission
#' factors for brake, tyre, road and resuspension PM2.5 and PM10.
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


#splatted function
#' @rdname ef_beddows
#' @export
ef_beddows_brake_pm2.5 <-
  function(veh.wt, route.def, brk.regen, ...){
    out <- ef_beddows_brake_pm(veh.wt)
    #error out if any route.def not in function output
####################################
#replace this with vein equivalent
#then drop ef_beddows_brake_pm
####################################
    out <- out[tolower(out$ref)=="pm2.5",]
    out <- out[tolower(out$route) %in% route.def,]
    out <- out[c("ans", "ans.low", "ans.hi")]
    if(brk.regen){
      out <- out * (1-brk.regen)
    }
#hi and low need fixing at source
#for all these functions
    #build output as data frame
    #route, em.source, em.type, ans, ans.low, ans.hi
    ans <- data.frame(route=route.def, em.source="brake", em.type="pm2.5", out)
    ans
  }

#' @rdname ef_beddows
#' @export
ef_beddows_tyre_pm2.5 <-
  function(veh.wt, route.def,...){
    out <- ef_beddows_tyre_pm(veh.wt)
    #error out if any route.def not in function output
####################################
#replace this with vein equivalent
#then drop ef_beddows_brake_pm
####################################
    out <- out[tolower(out$ref)=="pm2.5",]
    out <- out[tolower(out$route) %in% route.def,]
    out <- out[c("ans", "ans.low", "ans.hi")]
    #build output as data frame
    #route, em.source, em.type, ans, ans.low, ans.hi
    ans <- data.frame(route=route.def, em.source="tyre", em.type="pm2.5", out)
    ans
  }

#' @rdname ef_beddows
#' @export
ef_beddows_road_pm2.5 <-
  function(veh.wt, route.def,...){
    out <- ef_beddows_road_pm(veh.wt)
    #error out if any route.def not in function output
    ####################################
    #replace this with vein equivalent
    #then drop ef_beddows_brake_pm
    ####################################
    out <- out[tolower(out$ref)=="pm2.5",]
    out <- out[tolower(out$route) %in% route.def,]
    out <- out[c("ans", "ans.low", "ans.hi")]
    #build output as data frame
    #route, em.source, em.type, ans, ans.low, ans.hi
    ans <- data.frame(route=route.def, em.source="road", em.type="pm2.5", out)
    ans
  }

#' @rdname ef_beddows
#' @export
ef_beddows_resusp_pm2.5 <-
  function(veh.wt, route.def,...){
    out <- ef_beddows_resusp_pm(veh.wt)
    #error out if any route.def not in function output
    ####################################
    #replace this with vein equivalent
    #then drop ef_beddows_brake_pm
    ####################################
    out <- out[tolower(out$ref)=="pm2.5",]
    out <- out[tolower(out$route) %in% route.def,]
    out <- out[c("ans", "ans.low", "ans.hi")]
    #build output as data frame
    #route, em.source, em.type, ans, ans.low, ans.hi
    ans <- data.frame(route=route.def, em.source="resusp", em.type="pm2.5", out)
    ans
  }


#############################
#no longer exported

#splatted function
ef_beddows_brake_pm <-
function(wt, output="df"){
    ref <- c(rep("pm2.5", 3), rep("pm10", 3))
    route <- c("Urban", "Rural", "Motorway")
    b <- c(4.2, 1.8, 0.4, 11, 4.5, 1)
    eb <- c(1.1, 0.9, 0.4, 2.7, 2.4, 1)
    c <- c(1.9, 1.5, 1.3, 1.9, 1.5, 1.3)
    ec <- c(0.2, 0.3, 0.4, 0.2, 0.3, 0.4)
    ans <- b*((wt/1000)^(1/c))
    ans.hi <- (b+eb)*((wt/1000)^(1/(c-ec)))
    ans.low <- (b-eb)*((wt/1000)^(1/(c+ec)))
    out <- data.frame(ref, route, b, eb, c, ec, ans, ans.low, ans.hi)
    if(output=="latex"){
        #to do
        #write.table(a, "clipboard", row.names = F, col.names = F, quote=F)
        ref <- gsub("pm2.5", "$PM_{2.5}$", ref)
        ref <- gsub("pm10", "$PM_{10}$", ref)
        out2 <- paste(paste(route, ref, sep= " "),
                      " & ", b, " $\\pm$ ", eb, " & ",
                      c, " $\\pm$ ", ec, " & ", signif(ans, 2),
                      " (", signif(ans.low, 2), " - ",
                      signif(ans.hi, 2), ") \\\\", sep="")
        write.table(out2, "clipboard", row.names = F, col.names = F, quote=F)
    }
    return(out)
}

ef_beddows_tyre_pm <-
function(wt, output="df"){
    ref <- c(rep("pm2.5", 3), rep("pm10", 3))
    route <- c("Urban", "Rural", "Motorway")
    b <- c(5.8, 4.5, 3.8, 8.2, 6.4, 5.5)
    eb <- c(0.5, 0.3, 0.3, 0.6, 0.5, 0.4)
    c <- c(2.3, 2.3, 2.3, 2.3, 2.3, 2.3)
    ec <- c(0.4, 0.4, 0.4, 0.4, 0.4, 0.4)
    ans <- b*((wt/1000)^(1/c))
    ans.hi <- (b+eb)*((wt/1000)^(1/(c-ec)))
    ans.low <- (b-eb)*((wt/1000)^(1/(c+ec)))
    out <- data.frame(ref, route, b, eb, c, ec, ans, ans.low, ans.hi)
    if(output=="latex"){
      #to do
      #write.table(a, "clipboard", row.names = F, col.names = F, quote=F)
      ref <- gsub("pm2.5", "$PM_{2.5}$", ref)
      ref <- gsub("pm10", "$PM_{10}$", ref)
      out2 <- paste(paste(route, ref, sep= " "),
                    " & ", b, " $\\pm$ ", eb, " & ",
                    c, " $\\pm$ ", ec, " & ", signif(ans, 2),
                    " (", signif(ans.low, 2), " - ",
                    signif(ans.hi, 2), ") \\\\", sep="")
      write.table(out2, "clipboard", row.names = F, col.names = F, quote=F)
    }
    return(out)
}


ef_beddows_road_pm <-
  function(wt, output="df"){
    ref <- c(rep("pm2.5", 3), rep("pm10", 3))
    route <- c("Urban", "Rural", "Motorway")
    b <- c(rep(2.8, 3), rep(5.1, 3))
    eb <- c(rep(0.5, 3), rep(0.9, 3))
    c <- c(rep(1.5, 3), rep(1.5, 3))
    ec <- c(rep(0.1, 3), rep(0.1, 3))
    ans <- b*((wt/1000)^(1/c))
    ans.hi <- (b+eb)*((wt/1000)^(1/(c-ec)))
    ans.low <- (b-eb)*((wt/1000)^(1/(c+ec)))
    out <- data.frame(ref, route, b, eb, c, ec, ans, ans.low, ans.hi)
    if(output=="latex"){
      #to do
      #write.table(a, "clipboard", row.names = F, col.names = F, quote=F)
      ref <- gsub("pm2.5", "$PM_{2.5}$", ref)
      ref <- gsub("pm10", "$PM_{10}$", ref)
      out2 <- paste(paste(route, ref, sep= " "),
                    " & ", b, " $\\pm$ ", eb, " & ",
                    c, " $\\pm$ ", ec, " & ", signif(ans, 2),
                    " (", signif(ans.low, 2), " - ",
                    signif(ans.hi, 2), ") \\\\", sep="")
      write.table(out2, "clipboard", row.names = F, col.names = F, quote=F)
    }
    return(out)
  }

ef_beddows_resusp_pm <-
  function(wt, output="df"){
    ref <- c(rep("pm2.5", 3), rep("pm10", 3))
    route <- c("Urban", "Rural", "Motorway")
    b <- c(rep(2.0, 3), rep(8.2, 3))
    eb <- c(rep(0.8, 3), rep(3.2, 3))
    c <- c(rep(1.1, 3), rep(1.1, 3))
    ec <- c(rep(0.4, 3), rep(0.4, 3))
    ans <- b*((wt/1000)^(1/c))
    ans.hi <- (b+eb)*((wt/1000)^(1/(c-ec)))
    ans.low <- (b-eb)*((wt/1000)^(1/(c+ec)))
    out <- data.frame(ref, route, b, eb, c, ec, ans, ans.low, ans.hi)
    if(output=="latex"){
      #to do
      #write.table(a, "clipboard", row.names = F, col.names = F, quote=F)
      ref <- gsub("pm2.5", "$PM_{2.5}$", ref)
      ref <- gsub("pm10", "$PM_{10}$", ref)
      out2 <- paste(paste(route, ref, sep= " "),
                    " & ", b, " $\\pm$ ", eb, " & ",
                    c, " $\\pm$ ", ec, " & ", signif(ans, 2),
                    " (", signif(ans.low, 2), " - ",
                    signif(ans.hi, 2), ") \\\\", sep="")
      write.table(out2, "clipboard", row.names = F, col.names = F, quote=F)
    }
    return(out)
  }



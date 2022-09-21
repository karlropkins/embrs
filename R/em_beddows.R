############################################
#' @title Beddows Non-Exhaust Emissions Factors
############################################

#' @name ef_beddows
#' @aliases ef_pm_brake_beddows ef_beddows_tyre_pm ef_beddows_road_pm
#' ef_beddows_resusp_pm
#' @description Functions to estimated vehicle weight-based non-exhaust
#' PM2.5 and PM10 emission factors based on methods of Beddows.
#' @param wt Vehicle weight (in kg).
#' @param output ignore for now... (built the tables in the embrs paper but
#' most likely revising for reporter element of package...)
#' @returns These functions data.frame of Urban, Rural and Motorway emission
#' factors for brake, tyre, road and resuspension PM2.5 and PM10.
#' @note These may be moving to vein at some point...
#' @references add Beddows and Harrison ref...

#splatted function
#' @rdname ef_beddows
#' @export
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

#' @rdname ef_beddows
#' @export
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


#' @rdname ef_beddows
#' @export
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

#' @rdname ef_beddows
#' @export
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



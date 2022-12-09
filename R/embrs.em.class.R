############################################
#' @title em.class
############################################

#' @name em.class
#' @aliases em_test
#' @description Functions to build emissions models using embrs objects.
# #' @param no inputs at moment
#' @returns These functions make em class objects, which...
#' @note These may be moving to vein at some point...
#' @references add embrs and Beddows references...


#' @rdname em.class
#' @export
em_test <- function(...){
  out <- list(
    veh = list(bus_ice()),
    act = list(act_naei_urban())
  )
  class(out) <- "embrs_em"
}



#' @rdname em.class
#' @method + embrs_act
#' @export
`+.embrs_act` <- function(x, y, ...){
  if("embrs_veh" %in% class(y)){
    class(x) <- "list"
    class(y) <- "list"
    out <- list(
      vehs = list(y),
      act = list(x)
    )
    class(out) <- "embrs_em"
    return(out)
  }
  #can't add these
  stop("can't multiple these")
}

#' @rdname em.class
#' @method * embrs_acts
#' @export
`*.embrs_acts` <- function(x, y, ...){
  if("embrs_veh" %in% class(y)){
    out <- list(
      vehs = list(y),
      act = x
    )
    class(out) <- "embrs_em"
    return(out)
  }
  #can't add these
  stop("can't multiple these")
}


#' @rdname em.class
#' @method * embrs_veh
#' @export
`*.embrs_veh` <- function(x, y, ...){
  if("embrs_act" %in% class(y)){
    out <- list(
      vehs = list(x),
      act = list(y)
    )
    class(out) <- "embrs_em"
    return(out)
  }
  if("embrs_acts" %in% class(y)){
    out <- list(
      vehs = list(x),
      act = y
    )
    class(out) <- "embrs_em"
    return(out)
  }
  #can't add these
  stop("can't multiple these")
}








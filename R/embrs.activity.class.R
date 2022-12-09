############################################
#' @title activity.class
############################################

#' @name activity.class
#' @aliases act_naei_rural
#' @description Functions to build activity objects for use in embrs emission
#' models.
# #' @param no inputs at moment
#' @returns These functions make activity class objects, which .
#' @note These may be moving to vein at some point...
#' @references add embrs and Beddows references...

#quick build function
#' @rdname activity.class
#' @export
act_naei_rural <- function(...){
  #basic build for on internal combustion engine bus model
  .obj <- list(...)
  obj <- list(
    act.def = "rural",
    act.source = "uk naei"
    #doc to think about
  )
  out <- modifyList(obj, .obj)
  class(out) <- "embrs_act"
  out
}

#' @rdname activity.class
#' @export
act_naei_urban <- function(...){
  #basic build for on internal combustion engine bus model
  .obj <- list(...)
  obj <- list(
    act.def = "urban",
    act.source = "uk naei"
    #doc to think about
  )
  out <- modifyList(obj, .obj)
  class(out) <- "embrs_act"
  out
}

#quick build function
#' @rdname activity.class
#' @export
act_naei_motorway <- function(...){
  #basic build for on internal combustion engine bus model
  .obj <- list(...)
  obj <- list(
    act.def = "motorway",
    act.source = "uk naei"
    #doc to think about
  )
  out <- modifyList(obj, .obj)
  class(out) <- "embrs_act"
  out
}


#quick build function
#' @rdname activity.class
#' @export
act_naei_rum <- function(...){
  #basic build for on internal combustion engine bus model
  .obj <- list(...)
  obj <- list(
      rural = act_naei_rural(),
      urban = act_naei_urban(),
      motorway = act_naei_motorway()
  )
  out <- modifyList(obj, .obj)
  class(out) <- "embrs_acts"
  out
}

#' @rdname vehicle.class
#' @method print embrs_act
#' @export
print.embrs_act <- function(x, ...){
    ans <- paste("embrs: activity\n",
                 "       ", x$act.source, " ", x$act.def, sep="")
    cat(ans, "\n")
    invisible(x)
  }


#' @rdname vehicle.class
#' @method print embrs_acts
#' @export
print.embrs_acts <- function(x, ...){

  act.source <- if("act.source" %in% names(x)){
    x$act.source
  } else {
    temp <- lapply(x, function(y){
      temp <- y$act.source
    })
    do.call(c, temp)
  }
  act.def <- if("act.def" %in% names(x)){
    x$act.def
  } else {
    temp <- lapply(x, function(y){
      temp <- y$act.def
    })
    do.call(c, temp)
  }
  rep <- paste("       ", act.source, " ", act.def, sep="", collapse = "\n")

  ans <- paste("embrs: activity\n",
               rep, sep="")
  cat(ans)
  invisible(x)
}

#' @rdname vehicle.class
#' @method + embrs_act
#' @export
`+.embrs_act` <- function(x, y, ...){
  if("embrs_act" %in% class(y)){
    out <- list(
      x, y
    )
    names(out) <- make.unique(c(x$act.def, y$act.def))
    class(out) <- "embrs_acts"
    return(out)
  }
  if("embrs_acts" %in% class(y)){
    out <- y
    out$.new <- x
    out <- out[unique(c(".new", names(out)))]
    names(out)[1] <- x$act.def
    class(out) <- "embrs_acts"
    names(out) <- make.unique(names(out))
    return(out)
  }
  #can't add these
  stop("can't add these")
}


#' @rdname vehicle.class
#' @method + embrs_acts
#' @export
`+.embrs_acts` <- function(x, y, ...){
  if("embrs_act" %in% class(y)){
    out <- x
    out$.new <- y
    names(out)[length(out)] <- y$act.def
    names(out) <- make.unique(names(out))
    class(out) <- "embrs_acts"
    return(out)
  }
  if("embrs_acts" %in% class(y)){
    class(x) <- "list"
    class(y) <- "list"
    out <- c(x, y)
    names(out) <- make.unique(names(out))
    class(out) <- "embrs_acts"
    return(out)
  }
  #can't add these
  stop("can't add these")
}


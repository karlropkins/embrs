############################################
#' @title misc modifiers
############################################

#' @name misc.mod
#' @aliases naei_route2spd
#' @description functions that currently don't have better homes...
#' @param veh.type (character) vehicle type e.g. bus
#' @param veh.def (character) route definition, for NAEI urban, rural or
#' motorway
#' @param route.source (character) route source, for NAEI
#' @param ... other arguments, often passed on.
#' @returns These estimated veh speed for the supplied route.
#' @note These are currently the estimates used by __embrs__ models...
#' @references naei_route2spd is based on speed estimates in:
#'
#' Brown, P., Wakeling, D., Pang, Y. and Murrells, T. Methodology for the UK's
#' road transport emissions inventory: Version for the 2016 National Atmospheric
#' Emissions Inventory. Report for the Department for Business, Energy &
#' Industrial Strategy. 2018.
#' https://uk-air.defra.gov.uk/assets/documents/reports/cat07/1804121004_Road_transport_emissions_methodology_report_2018_v1.1.pdf
#'

#naei_route2spd (ricardo method)
#have this as excel in working folder


########################
#this needs a lot of tidying
#unexported lookup embrs_spd_ref
#and exported naei_route2spd

#currently only works on one combination at a time

#notes
###########################
#error formatting

#stop("[embrs] bus...() needs veh.wt, see help?",
#     call.=FALSE)


#splatted function
#' @rdname misc.mod
#' @export

naei_route2spd <- function(veh.type = NULL, route.def = NULL,
                           route.source = NULL, ...){

  if(any(is.null(veh.type), is.null(route.def),
         is.null(route.source))){
    stop("[embrs] naei_route2spd() needs veh.type, route.def and route.source!",
         call. = FALSE)
  }
  ############################
  #this needs a lot of tidying
  ############################
  ref <- embrs_spd_ref()
  ref$route.type <- tolower(gsub(" ", "", ref$route.type))
  ref$route.type.2 <- tolower(gsub(" ", "", ref$route.type.2))
  ref$veh.type <- tolower(gsub(" ", "", ref$veh.type))

  #################
  #this currently is all or nothing!
  #think about changing to col.name %in% formal
     ref <- ref[ref$veh.type %in% tolower(gsub(" ", "", veh.type)),]
     ref <- ref[ref$route.type %in% tolower(gsub(" ", "", route.def)),]
     ref <- ref[ref$route.type.2 %in% tolower(gsub(" ", "", route.source)),]
     if(nrow(ref)==0){
       stop("[embrs] naei_route2spd() no default veh.spd for this combination!",
            call. = FALSE)
     }
     #other options to return NULL or NA???
     #or data frame???
     ref$veh.spd_km.h

}


#not exporting at moment

embrs_spd_ref <- function(...){
  ref <- data.frame(
    comment = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                "", "", "NAEI BUS", "", "", "", "", "NAEI BUS", "", "", "",
                "", "NAEI BUS", "", "embrs estimate", "embrs estimate",
                "embrs estimate"),
    route.type = c("Cental London", "Cental London", "Cental London",
                   "Inner London", "Inner London", "Inner London",
                   "Outer London", "Outer London", "Outer London",
                   "Outer London", "Conurbation ", "Conurbation ",
                   "Conurbation ", "Conurbation ",
                   "Urban", "Urban", "Urban", "Urban", "Rural", "Rural",
                   "Rural", "Rural", "Motorway", "Motorway", "Motorway",
                   "Motorway", "Cental London", "Cental London",
                   "Cental London", "Inner London", "Inner London",
                   "Inner London","Outer Lonodn", "Outer Lonodn",
                   "Outer Lonodn", "Outer Lonodn", "Conurbation ",
                   "Conurbation ", "Conurbation ", "Conurbation ",
                   "Urban", "Urban", "Urban", "Urban", "Rural", "Rural",
                   "Rural", "Rural", "Motorway", "Motorway", "Motorway",
                   "Motorway", "Cental London", "Cental London", "Cental London",
                   "Inner London", "Inner London", "Inner London", "Outer Lonodn",
                   "Outer Lonodn", "Outer Lonodn", "Outer Lonodn", "Conurbation ",
                   "Conurbation ", "Conurbation ", "Conurbation ", "Urban",
                   "Urban", "Urban", "Urban", "Rural", "Rural", "Rural", "Rural",
                   "Motorway", "Motorway", "Motorway", "Motorway", "Cental London",
                   "Cental London", "Cental London", "Inner London", "Inner London",
                   "Inner London", "Outer Lonodn", "Outer Lonodn", "Outer Lonodn",
                   "Outer Lonodn", "Conurbation ",  "Conurbation ",
                   "Conurbation ", "Conurbation ", "Urban", "Urban",
                   "Urban", "Urban", "Rural", "Rural", "Rural", "Rural", "Motorway",
                   "Motorway", "Motorway", "Motorway", "Urban ", "Rural", "Motorway"),
    route.type.2 = c("Central London Major principal roads", "Major trunk roads",
                     "Minor roads", "Inner London Major principal roads", "Major trunk roads",
                     "Minor roads", "Outer London Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Urban Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Rural single carriageway Major roads",
                     "Minor roads", "Rural dual carriageway", "Rural motorway", "Outer London Motorways",
                     "Conurbation Motorways", "Urban Motorways", "Rural motorway",
                     "Central London Major principal roads", "Major trunk roads",
                     "Minor roads", "Inner London Major principal roads", "Major trunk roads",
                     "Minor roads", "Outer London Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Urban Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Rural single carriageway Major roads",
                     "Minor roads", "Rural dual carriageway", "Rural motorway", "Outer London Motorways",
                     "Conurbation Motorways", "Urban Motorways", "Rural motorway",
                     "Central London Major principal roads", "Major trunk roads",
                     "Minor roads", "Inner London Major principal roads", "Major trunk roads",
                     "Minor roads", "Outer London Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Urban Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Rural single carriageway Major roads",
                     "Minor roads", "Rural dual carriageway", "Rural motorway", "Outer London Motorways",
                     "Conurbation Motorways", "Urban Motorways", "Rural motorway",
                     "Central London Major principal roads", "Major trunk roads",                       "Minor roads", "Inner London Major principal roads", "Major trunk roads",
                     "Minor roads", "Outer London Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Urban Major principal roads", "Major trunk roads",
                     "Minor roads", "Motorways", "Rural single carriageway Major roads",
                     "Minor roads", "Rural dual carriageway", "Rural motorway", "Outer London Motorways",
                     "Conurbation Motorways", "Urban Motorways", "Rural motorway",
                     "UK NAEI", "UK NAEI", "UK NAEI"),
    veh.type = c("car", "car", "car", "car", "car", "car", "car", "car", "car", "car", "car", "car", "car",
                 "car", "car", "car", "car", "car", "car", "car", "car", "car",
                 "car", "car", "car", "car", "lgv", "lgv", "lgv", "lgv", "lgv",
                 "lgv", "lgv", "lgv", "lgv", "lgv", "lgv", "lgv", "lgv", "lgv",
                 "lgv", "lgv", "lgv", "lgv", "lgv", "lgv", "lgv", "lgv", "lgv",
                 "lgv", "lgv", "lgv", "hgv", "hgv", "hgv", "hgv", "hgv", "hgv",
                 "hgv", "hgv", "hgv", "hgv", "hgv", "hgv", "hgv", "hgv", "hgv",
                 "hgv", "hgv", "hgv", "hgv", "hgv", "hgv", "hgv", "hgv", "hgv",
                 "hgv", "hgv", "bus", "bus", "bus", "bus", "bus", "bus", "bus",
                 "bus", "bus", "bus", "bus", "bus", "bus", "bus", "bus", "bus",
                 "bus", "bus", "bus", "bus", "bus", "bus", "bus", "bus", "bus",
                 "bus", "bus", "bus", "bus"),
    veh.spd_km.h = c(16, 24, 16, 21L, 32L, 20L, 31L, 46L, 29L, 108L, 31L, 38L, 30L, 97L, 36L,
                     53L, 35L, 97L, 77L, 61L, 111L, 113L, 108L, 97L, 97L, 113L, 16L,
                     24L, 16L, 21L, 32L, 20L, 31L, 46L, 29L, 108L, 31L, 38L, 30L,
                     97L, 36L, 53L, 35L, 97L, 77L, 61L, 111L, 113L, 108L, 97L, 97L,
                     113L, 16L, 24L, 16L, 21L, 32L, 20L, 31L, 46L, 29L, 87L, 31L,
                     37L, 30L, 82L, 36L, 52L, 34L, 82L, 72L, 62L, 90L, 90L, 87L, 82L,
                     82L, 90L, 16L, 16L, 16L, 24L, 24L, 20L, 32L, 32L, 29L, 87L, 24L,
                     24L, 30L, 82L, 32L, 32L, 29L, 82L, 71L, 62L, 93L, 95L, 87L, 82L,
                     82L, 95L, 32L, 62L, 82L))
  ref
}


#not exporting at the moment

embrs_ukbc_lookup <- function(...){
  ref <- data.frame(
             veh.type = "bus",
             veh.spd_km.h = c(16.9, 10.0, 31.3,
                     32, 62, 82),
             route.source = c(rep("ukbc", 3), rep("uk naei", 3)),
             route.def = c("Outer London", "Inner London", "Rural",
                     "Urban", "Rural", "Motorway"),

             #from new cycle from tim barlow and nemo??
             #acc = c(0.442, 0.4181, 0.2877),
             #prop.acc = c(0.4188, 0.3574, 0.4038),
             #dec = c(-0.619, -0.647, -0.5912),
             #prop.dec =c(0.2384, 0.2154, 0.2201)

             dec = c(-0.619, -0.647, -0.5912,
                     -0.468739424707642,
                     -0.514481209154443, -0.466105499442562),
             dec.pc = c(23.84, 21.54, 22.01,
                        33.3898305084746,
                        29.8901098901099, 30.6501547987616),

             acc = c(0.442, 0.4181, 0.2877,
                     0.52993295019581,
                     0.508129084971385, 0.387746415773711),
             acc.pc = c(41.88, 35.74, 40.38,
                        29.4915254237288,
                        29.8901098901099, 38.390092879257)
             )
             ref$embrs.brk_b <- (-ref$dec * (ref$dec.pc/100))/ref$veh.spd
             ref$embrs.tyr_t <- ((-ref$dec * (ref$dec.pc/100))+
                               (ref$acc * (ref$acc.pc/100)))/ref$veh.spd

  ref
}


## should be able to rationalise these functions


ukbc_route2spd <- function(veh.type = NULL, route.def = NULL,
                           route.source = NULL, ...){

  if(any(is.null(veh.type), is.null(route.def),
         is.null(route.source))){
    stop("[embrs] ukbc_route2spd() needs veh.type, route.def and route.source!",
         call. = FALSE)
  }
  ############################
  #this needs a lot of tidying
  ############################
  ref <- embrs_ukbc_lookup()
  #return(ref)
  ref$route.def <- tolower(gsub(" ", "", ref$route.def))
  ref$route.source <- tolower(gsub(" ", "", ref$route.source))
  ref$veh.type <- tolower(gsub(" ", "", ref$veh.type))

  #return(ref)

  #################
  #this currently is all or nothing!
  #think about changing to col.name %in% formal
  ref <- ref[ref$veh.type %in% tolower(gsub(" ", "", veh.type)),]
  ref <- ref[ref$route.def %in% tolower(gsub(" ", "", route.def)),]
  ref <- ref[ref$route.source %in% tolower(gsub(" ", "", route.source)),]
  if(nrow(ref)==0){
    stop("[embrs] ukbc_route2spd() no default veh.spd for this combination!",
         call. = FALSE)
  }
  #other options to return NULL or NA???
  #or data frame???
  ref$veh.spd_km.h

}


ukbc_route2brk_b <- function(veh.type = NULL, route.def = NULL,
                           route.source = NULL, ...){

  if(any(is.null(veh.type), is.null(route.def),
         is.null(route.source))){
    stop("[embrs] ukbc_route2brk_b() needs veh.type, route.def and route.source!",
         call. = FALSE)
  }
  ############################
  #this needs a lot of tidying
  ############################
  ref <- embrs_ukbc_lookup()
  #return(ref)
  ref$route.def <- tolower(gsub(" ", "", ref$route.def))
  ref$route.source <- tolower(gsub(" ", "", ref$route.source))
  ref$veh.type <- tolower(gsub(" ", "", ref$veh.type))

  #return(ref)

  #################
  #this currently is all or nothing!
  #think about changing to col.name %in% formal
  ref <- ref[ref$veh.type %in% tolower(gsub(" ", "", veh.type)),]
  ref <- ref[ref$route.def %in% tolower(gsub(" ", "", route.def)),]
  ref <- ref[ref$route.source %in% tolower(gsub(" ", "", route.source)),]
  if(nrow(ref)==0){
    stop("[embrs] ukbc_route2brk_b() no default brk_b for this combination!",
         call. = FALSE)
  }
  #other options to return NULL or NA???
  #or data frame???
  ref$embrs.brk_b

}


ukbc_route2tyr_t <- function(veh.type = NULL, route.def = NULL,
                             route.source = NULL, ...){

  if(any(is.null(veh.type), is.null(route.def),
         is.null(route.source))){
    stop("[embrs] ukbc_route2tyr_t() needs veh.type, route.def and route.source!",
         call. = FALSE)
  }
  ############################
  #this needs a lot of tidying
  ############################
  ref <- embrs_ukbc_lookup()
  #return(ref)
  ref$route.def <- tolower(gsub(" ", "", ref$route.def))
  ref$route.source <- tolower(gsub(" ", "", ref$route.source))
  ref$veh.type <- tolower(gsub(" ", "", ref$veh.type))

  #return(ref)

  #################
  #this currently is all or nothing!
  #think about changing to col.name %in% formal
  ref <- ref[ref$veh.type %in% tolower(gsub(" ", "", veh.type)),]
  ref <- ref[ref$route.def %in% tolower(gsub(" ", "", route.def)),]
  ref <- ref[ref$route.source %in% tolower(gsub(" ", "", route.source)),]
  if(nrow(ref)==0){
    stop("[embrs] ukbc_route2tyr_t() no default tyr_t for this combination!",
         call. = FALSE)
  }
  #other options to return NULL or NA???
  #or data frame???
  ref$embrs.tyr_t

}

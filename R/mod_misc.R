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

########################
#this needs a lot of tidying
#unexported lookup
#embrs_spd_ref and exported naei_route2spd

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


#splatted function
#' @rdname misc.mod
#' @export

naei_route2spd <- function(veh.type = NULL, route.def = NULL,
                           route.source = NULL, ...){

  if(any(is.null(veh.type), is.null(route.def),
         is.null(route.source))){
    stop("need them all!")
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
     ref <- ref[ref$veh.type == tolower(gsub(" ", "", veh.type)),]
     ref <- ref[ref$route.type == tolower(gsub(" ", "", route.def)),]
     ref <- ref[ref$route.type.2 == tolower(gsub(" ", "", route.source)),]
     if(nrow(ref)==0){
       stop("no match!")
     }
     #other options to return NULL or NA???
     #or data frame???
     ref$veh.spd_km.h

}






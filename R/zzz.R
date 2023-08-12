############################################
# general code
############################################

#undefined globals
utils::globalVariables(c("Category", "Pollutant", "Fuel", "EuroStandard",
                         "Segment", "vehicle", "ans", "em.source"))
#(possibly from)
#Category, Pollutant, Fuel, EuroStandard, Segment
#    ef_vein_eea (vein:ef_eea())
#vehicle, ans, em.source
#    embrs.generics (plot.embrs)

#(need to check for others...)


#build note
###########################

#currently builds with one warning
#associated with use of ::: in ef_vein_eea

#can drop this and related function once
#    my vein code goes into cran version


#to think about
#############################

#could also be a better place for general notes
#could use this as common place for import and importFrom
#could be useful place for common unexported code?
#need quite time to think about how useful any of this is???


##############################
#issues
############################

#build_inventory
############################

#inventory picking up messy row names
#with ef_embrs related builds
#(but numbers/df shapes look sensible)
#temp fix at end of build_inventory
#BUT rather have this fixed properly


#veh.spd
##################################

#if supplied to the vehicle object, version in route supersedes
#and version in route is often there as default...
#    does this need a warning or handling??



###########################
#other notes
###########################

#error messaging
##########################
#currently aiming for....

#stop("[embrs] bus...() needs veh.wt, see help?",
#     call.=FALSE)


########################
#adding veh.types
########################

#...

########################
#adding eng.fuel
########################

#...

########################
#adding euro.class
########################

#affects
#     ef_vein_eea
#     (vehicle objects)

########################
#possible examples
########################

#plot((bus_ice(15925, euro.class = "vi", eng.fuel = "diesel")) *
#   (route_veh_spd(10)+route_veh_spd(20)+route_veh_spd(30)+
#   (route_veh_spd(40)+route_veh_spd(50)+route_veh_spd(60)+route_veh_spd(70))))

#expand on this for avg.spd emissions curves plot

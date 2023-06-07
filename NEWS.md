# Version 0.0 - Release Notes

* [0.0.8]
    * added ef_vein_eea_exhaust_nox; added em.type option to build_inventory 
    and plot; plot defaults to just shows pm emissions rather than all 
    emissions [2023-06-07]   
    * started restructuring code for non-pm emissions and other vehicle 
    types [2023-06-60] 
* [0.0.7]
    * replaced separate beddows, embrs1 and embrs2 vehicle models with one 
    model with extra argument method = "beddows"; in embrs_ice, etc. 
    [2023-06-05] 
    * replaced ef_eea2019 with ef_vein_eea [2023-06-02]
    * added verbose argument to all ef functions and build_inventory; 
    added method documentation to EF output [2023-05-26]
    * standardised formal arguments in ef functions [2023-05-26]
    * removed ef_copert and ef_emep related code; ef_eea2019 functions 
    now supersede all ef_copert and ef_emep equivalents [2023-05-26]
    * added bus_dice() as diesel bus shortcut [2023-05-23]
    * added ef_eea2019 like ef_copert5 but using vein::ef_eea() [2023-02-07]
    * now imports vein [2023-02-07] 
* [0.0.6]
    * ef_copert5 update (force setting to set-points) [2023-01-24]
    * archive and web update (news, readme, index) [2023-01-19] 
    * added ef_copert5 functions (still needs work; likely to replace 
      ef_emep) [2023-01-19]
* [0.0.5]
    * standardised errors messages (still needs work) [2022-12-30]
    * added draft favicon [2022-12-30]
* [0.0.4]
    * started embrs emission functions as ef_embrs... [2022-12-27]
    * added naei_route2spd lookup table [2022-12-23]
    * started exhaust emission functions as ef_emep... [2022-12-23]
    * added vehicle/fleet object workforce function [2022-12-21]
* [0.0.3]
    * website update, coc links (and minor fixes) [2022-12-14]
    * website update, news update [2022-12-14]
    * generalised ef_beddows... functions and extended to pm10 [2022-12-14] 
    * website update, updated description (web links) [2022-12-13]
    * website update, added coc and contribution [2022-12-13]
* [0.0.2]
    * object tidying (print.embrs output) [2022-12-13]
    * object handling revision [2022-12-13]
    * website skeleton build [2022-12-12]
* [0.0.1] 
    * first draft embrs object docs [2022-12-12]
    * revised embrs object classes (ops handling) [2022-12-11]
    * first draft embrs object classes [2022-12-09]
    * built basic package using grey.area [2022-09-21]
    * added ef_beddows functions (basic methods from beddows paper) [2022-09-21]
    * added ef_embrs functions (basic methods from bus paper) [2022-09-21]

    


# Created by Esteban
# Wed Feb 25, 2015

heat <- function(x, ...) UseMethod("heat")

#TODO: Proof source

#' @title heat 
#'
#' @description
#' Perform a monthly heat balance and annual energy demand of buildings
#' @source
#' DIN V 18599
#'
#' @param building (optional, default = FALSE) use a user define building
#' data file. 
#' @param climate (optional, default = Hamburg) use a specific climate file.
#' @param fp (optional, default = Gas) primary energy factor and CO2-eq
#' emissions factor. 
#' @param general_name (optional, default = "No Name") if this value is not
#' specifically define the function will load the values from the building data
#' file.
#' @param general_ploto (optional, default = FALSE) 
#' @param general_plotn (optional, default="No Name")
#' @param building_uwb (optional, default = FALSE) 
#' @param building_windows (optional, default = FALSE)
#' @param building_uvalw (optional, default = FALSE)
#' @param building_uvalr (optional, default = FALSE)
#' @param building_uvalwindow (optional, default = FALSE)                   
#' @param building_dim (optional, default = c(FALSE,FALSE))
#' @param building_h (optional, default = FALSE)
#' @param user_aircrate (optional, default = FALSE)
#' @param user_ti (optional, default = FALSE)
#' @param user_qi (optional, default = FALSE)
#' @param building_orientation (optional, default = FALSE)
#' @param building_storagecapacity (optional, default = FALSE)
#' @param building_roofslope (optional, default = FALSE)
#' @param output_type (optional, default = "Year")
#' @examples
#' # Use the default values to compute the heat demand of a building
#' climate_month_names <- c("January","February","March","April",
#'                          "May","June","July","August",
#'                          "September","October","November","December")
#' 
#' model <- heat(building_orientation = 0,
#'               output_type = "Month")
#' Qhm <- model$Qhm
#' barplot(Qhm,
#'         names.arg=climate_month_names,
#'         main = "Monthly Heat Demand",
#'         ylab =  "head Demand [kWh]")
#' 
#' # This example show the heat demand variation for different
#' # U values combinations
#' Buildings.Number <- 9
#' 
#' U.Values <- matrix(c(
#'     1.3, 1.0, 3.0,   # 01
#'     1.2, 0.9, 2.7,   # 02
#'     1.1, 0.8, 2.7,   # 03
#'     1.0, 0.7, 2.7,   # 04
#'     0.9, 0.6, 2.4,   # 05
#'     0.8, 0.5, 2.1,   # 06
#'     0.6, 0.4, 1.9,   # 07
#'     0.5, 0.3, 1.6,   # 08
#'     0.4, 0.2, 1.6),  # 09
#'     3,Buildings.Number)
#' 
#' Heat.Demand <- rep(0,Buildings.Number)
#' for (i in 1:Buildings.Number){
#'     UvalW <- U.Values[1,i]
#'     UvalR <- U.Values[2,i]
#'     UvalWindow <- U.Values[3,i]
#'     temp.2 <- heat(building_uvalw = UvalW, 
#'                     building_uvalr = UvalR, 
#'                     building_uvalwindow = UvalWindow)
#'     Heat.Demand[i] <- temp.2$Qhs
#' }
#' 
#' # bar plot
#' barplot(Heat.Demand,
#'         names.arg = seq(1910,2010,12),
#'         main = "Heat demand for a set of buildings",
#'         ylab = "Heat demand [kWh/m²a]",
#'         xlab = "Building age")
#' 
#' # This example show the heat demand variation for different
#' # user influenced parameters
#' Buildings.Number <- 5
#' 
#' Param.Values <- matrix(c(
#'     7, 22, 0.7,   # 01
#'     6, 21, 0.6,   # 02
#'     5, 20, 0.5,   # 03
#'     4, 19, 0.4,   # 04
#'     3, 18, 0.3),  # 05
#'     3,Buildings.Number)
#' 
#' UvalW <- 0.4
#' UvalR <- 0.2
#' UvalWindow <- 1.6
#' 
#' Heat.Demand <- rep(0,Buildings.Number)
#' for (i in 1:Buildings.Number){
#'     AirCRate <- Param.Values[1,i]
#'     Ti <- Param.Values[2,i]
#'     qi <- Param.Values[3,i]
#'     temp.2 <- heat(user_aircrate = AirCRate, 
#'                    user_ti = Ti, 
#'                    user_qi = qi,
#'                    building_uvalw = UvalW, 
#'                    building_uvalr = UvalR, 
#'                    building_uvalwindow = UvalWindow)
#'     Heat.Demand[i] <- temp.2$Qhs
#' }
#' 
#' # bar plot
#' barplot(Heat.Demand,
#'         main = "Heat demand for a set of buildings",
#'         ylab = "Heat demand [kWh/m²a]",
#'         xlab = "Building age")
#' 
#' # This example show the heat demand variation for different
#' # Building orientations, it used the ggplot2 library
#' library(ggplot2)
#'
#' iter <- seq(0,360,1)
#' Heat.Demand = rep(0,length(iter))
#' Heat.Gains.Solar = rep(0,length(iter))
#' Irradiation.Sum = rep(0,length(iter))
#' 
#' building_dim <- c(12,6) 
#' for (i in 1:length(iter)){
#'     BO <- iter[i]
#'     temp.2 <- heat(building_orientation = BO,
#'                    building_dim = building_dim)
#'     Heat.Demand[i] <- temp.2$Qhs
#'     Heat.Gains.Solar[i] <- temp.2$Ss
#'     Irradiation.Sum[i] <- temp.2$Ti
#' }
#' 
#' # Polar plot + line plot
#' result <- data.frame(heat.demand = Heat.Demand,
#'                      orientation = iter)
#' doh <- ggplot(result, aes(orientation, heat.demand))
#' # Line plot
#' doh + geom_line(colour = "red", size = 1)  + 
#'     coord_polar(direction = -1, start = -pi/2) +
#'     labs(title = "Heat demand for all possible building orientations") + 
#'     scale_x_continuous(breaks=seq(0, 360, 15))
#' 
#' @author M. Esteban Munoz H.
#TODO: change case in examples
#TODO: import examples 
#TODO: proof data input
heat.default <- function(
        building = FALSE,
        climate = "Hamburg",
        fp = "Gas",
        general_name = "No Name", 
        general_ploto = FALSE, 
        general_plotn = "No Name",
        building_uwb = FALSE, 
        building_windows = FALSE,                           
        building_uvalw = FALSE, 
        building_uvalr = FALSE, 
        building_uvalwindow = FALSE,                           
        building_dim = c(FALSE,FALSE), 
        building_h = FALSE, 
        user_aircrate = FALSE, 
        user_ti = FALSE, 
        user_qi = FALSE, 
        building_orientation = FALSE, 
        building_storagecapacity = FALSE, 
        building_roofslope = FALSE,
        output_type = "Year")
    {

    model <- heatest(
        building = building,
        climate = climate,
        fp = fp,
        general_name = general_name, 
        general_ploto = general_ploto, 
        general_plotn = general_plotn,
        building_uwb = building_uwb, 
        building_windows = building_windows, 
        building_uvalw = building_uvalw, 
        building_uvalr = building_uvalr, 
        building_uvalwindow = building_uvalwindow,                     
        building_dim = building_dim, 
        building_h = building_h, 
        user_aircrate = user_aircrate, 
        user_ti = user_ti, 
        user_qi = user_qi, 
        building_orientation = building_orientation, 
        building_storagecapacity = building_storagecapacity, 
        building_roofslope = building_roofslope,
        output_type = output_type)
    
    class(model) <- "heat"

    model}


heatest <- function(
    building             = FALSE,
    climate              = FALSE,
    fp                   = FALSE,
    general_name         = "No Name", 
    general_ploto        = FALSE, 
    general_plotn        = "No Name",
    building_uwb         = FALSE, 
    building_windows     = FALSE,                           
    building_uvalw       = FALSE, 
    building_uvalr       = FALSE, 
    building_uvalwindow  = FALSE,                           
    building_dim         = c(FALSE,FALSE), 
    building_h           = FALSE, 
    user_aircrate        = FALSE, 
    user_ti              = FALSE, 
    user_qi              = FALSE, 
    building_orientation = FALSE, 
    building_storagecapacity = FALSE, 
    building_roofslope   = FALSE,
    output_type          = "Year")
    {
    ## input data:
    
    ##################
    # (1) as function of
    ##################
    # Separate module for building data see "Data/GeometryModule.r"
    # (A) General
    #------------------------------------------------------------
    # Name              Char          Name of the typ + F
    # PlotOption        Boolean       Plot figure?
    # PlotName          Name          Name to save plot
    # (B) Building data:
    #------------------------------------------------------------------------------
    # Uwb               Double        Correction value for thermal bridges
    # Windows           Double        Percentage of facade with windows      
    # UvalW             Double        U-value for walls                 
    # UvalR             Double        U-value for Roof                    
    # UvalWindow        Double        U-value for Window                   
    # Dim               1x2 Double    Building dimensions L x B 
    # h                 Double        Height of Building            
    # AirCRate          Double        Air change rate [h-1]         
    # Ti                1x12 Double   Ti(Theta i) internal temperature 
    # qi                Double        Internal heat emissions.      
    # Ob                Double        Building orientation               
    # RoofSlope         Double        Slope of building roof              
    # StorageCapacity   Double        Storage capacity of Building      
    if (building){
        load(building)
    }else{
        #TODO: use internal data
        #load("./Data/BuildingData")
        data(building)
    }
    
    # uncomment this line for de-bug
    #load("./Data/BuildingDataDBug")
    #building_orientation <- 1
    
    ## Check input data
    if (general_name             == "No Name") {general_name        <- T.general_name} 
    if (general_ploto            == FALSE)     {general_ploto       <- T.general_ploto} 
    if (general_plotn            == "No Name") {general_plotn       <- T.general_plotn}
    if (building_uwb             == FALSE)     {building_uwb        <- T.building_uwb}
    if (building_windows         == FALSE)     {building_windows    <- T.building_windows}                           
    if (building_uvalw           == FALSE)     {building_uvalw      <- T.building_uvalw}
    if (building_uvalr           == FALSE)     {building_uvalr      <- T.building_uvalr}
    if (building_uvalwindow      == FALSE)     {building_uvalwindow <- T.building_uvalwindow}                           
    if (building_dim[1]          == FALSE)     {building_dim        <- T.building_dim}
    if (building_h               == FALSE)     {building_h          <- T.building_h}
    if (user_aircrate            == FALSE)     {user_aircrate       <- T.user_aircrate} 
    if (user_ti                  == FALSE)     {user_ti             <- T.user_ti}
    if (user_qi                  == FALSE)     {user_qi             <- T.user_qi}
    if (building_orientation     == FALSE){building_orientation     <- T.building_orientation} 
    if (building_storagecapacity == FALSE){building_storagecapacity <- T.building_storagecapacity}
    if (building_roofslope       == FALSE){building_roofslope       <- T.building_roofslope}
    
    ##################
    # (2) Imported Data <load>
    ##################
    ##Climate:
    # Separate module for climate calculation see "Data/ClimateModule.r"
    data_list <- try(unlist(data(package="heat"))) 
    data_list <- data_list[data_list != ""]
    data_list <- data_list[data_list != "heat"]
    data_list <- data_list[data_list != "Data sets"]
    data_list <- data_list[data_list != "/usr/lib/R/library"]
    if (paste("Climate.I (",climate,")", sep="") %in% data_list){
        data(list=climate, package="heat")
    } else {
        load(climate)
    }
    ## Primary and CO2-eq factors
    if ("Fp" %in% data_list){
        data(list="Fp", package="heat")
    } else {
        load(fp)
    }
    factor.fp  <- Fp[[fp]][1]
    factor.CO2 <- Fp[[fp]][2]
    # I           4x12 Double   Isolation level orientation x months    
    # Month       1x12 Double   Month number                
    # Te          1x12 Double   outside temperature         
    # t           1x12 Double   days of the month       
    
    ## output data:
    # Qhs  --> Specific heat demand
    # Hts  --> Specific transmission losses

    # Get the radiation levels for this building
    Climate.I <- getSolarRadiation(building_orientation)

    ##################
    ## Heat gains Qg
    ##################
    
    ##internal gains Si
    # heated area An
    Building.An <- building_dim[1]*building_dim[2]*building_h/3
    Heat.gains.Si <- user_qi * Building.An
    
    ##monthly solar heat flows Ss
    # Ss(M) average monthly solar heat flow [W]
    # I(M,j) average solar intensity of radiation [W/m²]
    # A(s,j) actual collector surface [m²]
    # J orientation (direction and down-grade to vertical)
    Heat.gains.Ss <-
    Climate.I[,"north"]*(building_dim[1]*building_h*building_windows) +
    Climate.I[,"west"] *(building_dim[2]*building_h*building_windows) +
    Climate.I[,"south"]*(building_dim[1]*building_h*building_windows) +
    Climate.I[,"east"] *(building_dim[2]*building_h*building_windows)
    
    ##Heat gains Qg
    #0,024 kWh = 1 Wd
    #Ss(M) average monthly solar heat flow [W]
    #Si(M) heat flow by internal heat sources [W]
    #t(M) number of the days in a particular month [d/M]
    Heat.gains.Qg <- 0.024 * (Heat.gains.Ss + Heat.gains.Si) * Climate.t
    
    ##################
    ## Heat losses Ql
    ##################
    
    ##ventilation loss Hv !! Only for natural ventilation
    # AirCRate = air change rate [h-1]
    # V volume of air in heated building (according to EnEV it obtains V = 0.8* Ve)
    # pL* CPL heat storage capacity of air = 0.34 [Wh/(m²K)]
    Building.Ve <- building_dim[1]*building_dim[2]*building_h
    Building.V <- 0.8 * Building.Ve
    Constant.plCpl = 0.34
    Heat.loss.Hv <- user_aircrate * Building.V * Constant.plCpl;
    
    ##thermal bridge addition
    # Uwb correction value for thermal bridges [W/m²K]
    # A total heat transmitting building envelope [m²]
    Building.A.Roof  <- 2*(building_dim[2]*((building_dim[1]/2)/cos(building_roofslope*pi/180)))
    Building.A.Wall.1 <- building_dim[1]*building_h
    Building.A.Wall.2 <- building_dim[2]*building_h
    Building.A.Window <- Building.A.Wall.1 * building_windows * 2 + Building.A.Wall.2 * building_windows * 2
    # Building.A.Slab = building_dim[1] * building_dim[2]
    Building.A <- 2*Building.A.Wall.1 + 2*Building.A.Wall.2 + Building.A.Roof
    Heat.loss.Hwb <- building_uwb * Building.A
    
    ##transmission losses Ht
    # Temperature correction factor Fx
    # Ti(Theta i) internal temperature [K]
    # Tu(M) Temperature in unheated space [K]
    # Te(M) Ambient (=external) temperature [K]
    
    # Fx(M) = (Ti-Tu(M))/(Ti-Te(M)) integrated in Area Calculator module
    # Heat losses referring to heat loss surface Hu
    # U(i) Heat transfer coefficient [W/(m²K)]
    # A(i) Analogical to building part surface [m²]
    # Fx(i) Temperature correction factor
    
    # Hu(i) = Fx * U * (integrated in Htu)
    Heat.loss.Hu <- 0
    
    # Htfh Not consider in these calculation.
    Heat.loss.Htfh <- 0
    
    # Ls Not consider in these calculation. Integrated in Htu
    Heat.loss.Ls <- 0
    
    ##transmission losses Ht
    # U(i) Heat transfer coefficient incountercurrent with ambient air[W/(sqm K)]
    # A(i) Analogical to building part surface [sqm]
    # Hu Heat losses referring to heat loss surface [W/K]
    # Ls thermal guide value for ground bordering surfaces [W/K]
    # Hwb thermal bridges addition [W/K]
    # Htfh Specific transmission heat loss by building parts with integrated panel heating [W/K]
    # Ht = sum (U) * sum (A) + Hu + Ls + Hwb + Htfh
    Heat.loss.Htu <- (
        (2*Building.A.Wall.1*(1-building_windows) + 
         2*Building.A.Wall.2*(1-building_windows)
        ) * building_uvalw) + 
        (Building.A.Roof * building_uvalr) + 
        (Building.A.Window * building_uvalwindow)
    Heat.loss.Ht <- Heat.loss.Htu + Heat.loss.Hu + Heat.loss.Ls + Heat.loss.Hwb + Heat.loss.Htfh
    
    #specific total heat loss (transmission and ventilation heat losses;HT+HV) [W/K] H
    Heat.loss.H <- Heat.loss.Ht + Heat.loss.Hv
    
    ##Heat losses Ql
    # 0.024 kWh = 1 Wd
    # H(M) specific total heat loss (transmission and ventilation heat losses;HT+HV) [W/K]
    # Ti-Te(M) difference between internal and ambient temperature [K]
    # t(M) number of the days in a particular month [d/M]
    Heat.loss.Ql <- 0.024 * Heat.loss.H * (user_ti - Climate.Te) * Climate.t
    
    ##################
    ## Monthly heat demand Qh
    ##################
    
    # Ql(M) sum of monthly heat loss due to transmission and ventilation [kWh/M]
    # Qg(M) sum of monthly heat gains [kWh/M]
    # n(M) monthly utilization factor for the gains [-]
    Heat.demand.Thermal = building_storagecapacity * Building.Ve / Heat.loss.H
    Heat.demand.a = 1 + Heat.demand.Thermal/16
    Heat.demand.y <- Heat.gains.Qg/Heat.loss.Ql
    
    Heat.demand.Qhm <- rep(0,12)
    for (m in 1:12) {
        if (Heat.demand.y[m] == 1 ){
                Heat.demand.n <- (Heat.demand.a / (Heat.demand.a +1))
        } else {
                Heat.demand.n <- (1-Heat.demand.y[m]^Heat.demand.a)/(1-Heat.demand.y[m]^(Heat.demand.a+1))
        }
        Heat.demand.Qhm[m] <- Heat.loss.Ql[m] - Heat.demand.n * Heat.gains.Qg[m]
    }
    
    # subtraction of negative heat demand
    Heat.demand.Qhm[Heat.demand.Qhm < 0] <- 0
    Heat.demand.Qh <- sum(Heat.demand.Qhm)
    
    ## Specific heat demand [kWh/sqm a] Qhs 
    # and specific transmission losses Hts [W/sqm K]
    Heat.demand.Qhs = Heat.demand.Qh / Building.An
    Heat.demand.Hts = Heat.loss.Ht / Building.An

    ## Sum of solar heat gains
    Heat.gains.Ss.sum <- sum(Heat.gains.Ss)
    ## Total Irradiation
    Total.Irradiation <- sum(Climate.I)

    ## Primary energy demand
    Heat.demand.Qpm <- Heat.demand.Qhm * factor.fp #month
    Heat.demand.Qp  <- Heat.demand.Qh  * factor.fp #year

    ## CO2 emissions
    CO2m <- Heat.demand.Qpm * factor.CO2 * 3.6e-9 #month
    CO2  <- Heat.demand.Qp  * factor.CO2 * 3.6e-9 #year
    
    ## Result
    #TODO: format output as list
    if (output_type == "Year"){
        result = data.frame(
            Ti  = Total.Irradiation,
            Ss  = Heat.gains.Ss.sum, 
            Qhs = Heat.demand.Qhs, 
            Hts = Heat.demand.Hts,
            Qp  = Heat.demand.Qp,
            CO2 = CO2
            )
    } else if (output_type == "Month"){
        result = data.frame(
            Qhm = Heat.demand.Qhm,
            Qpm = Heat.demand.Qpm,
            CO2 = CO2m
            )
    }
    return(result)
    }

#' @title getSolarRadiation 
#'
#' @description
#' Computes the solar radiation of the building based on its orientation
#'
#' @param building_orientation orientation of the building in degrees
#' @return Climate.I radiation levels 
#' @author M. Estebna Munoz H.
#TODO: make example 
getSolarRadiation <- function(building_orientation){
  ## Solar radiation matrix
  # it is easier to change the radiation values that to rotate the building :)
  if (
    building_orientation == 0 || 
    building_orientation == 180 || 
    building_orientation == 360
    ){
    new.Orientation <- c("north", "west", "south","east")
    temp.rectangular <- TRUE
  } else if (
    building_orientation == 90 ||
    building_orientation == 270
    ){
    new.Orientation <- c("west", "south","east","north")
    temp.rectangular <- TRUE
  } else if (
    building_orientation > 0 &&
    building_orientation < 90
    ){
    temp.b <- (building_orientation-0)/90
    new.Orientation <- data.frame(A=c("north","west","south","east"), 
                                  B=c("west","south","east","north" ))
    temp.rectangular <- FALSE
  } else if (
    building_orientation > 90 &&
    building_orientation < 180
    ){
    temp.b <- (building_orientation-90)/90
    new.Orientation <- data.frame(A=c("west","south","east","north"), 
                                  B=c("south","east","north","west"))
    temp.rectangular <- FALSE
  } else if (
    building_orientation > 180 &&
    building_orientation < 270
    ){
    temp.b <- (building_orientation-180)/90
    new.Orientation <- data.frame(A=c("south","east","north","west"), 
                                  B=c("east","north","west","south"))
    temp.rectangular <- FALSE
  } else if (
    building_orientation > 270 &&
    building_orientation < 360
    ){
    temp.b <- (building_orientation-270)/90
    new.Orientation <- data.frame(A=c("east","north","west","south"), 
                                  B=c("north","west","south","east"))
    temp.rectangular <- FALSE
  }

  #                        (1)N
  #                        ^              7
  #                        |             /
  #                        |            /
  #                       90°          /
  #                        |          /
  #                        |         /
  #                        |        /
  #                        |       /
  #                        |      /   <
  #     (II)    (     x1---|-----x     )   (I)
  #              >    |    |    /4
  #                   |    |   / |
  #                   |    |  /  |    
  #                   |    | /   |     
  #                   |    |/    |
  # (1)W <--180°------|----+-----|--------------0°---> (1)E
  #                   |    |     |
  #                   2    |     |
  #     (III)         x----|----3x         (IV)
  #                        |   
  #                        |   1) building_orientation = 0 || 180 || 360
  #                        |      N-W-S-E
  #                        |   2) building_orientation = 90 || 270
  #                        |      W-S-E-N
  #                        |   3) building_orientation = 1   - 89  (I  )
  #                      270°     NW-SW-SE-NE
  #                        |   4) building_orientation = 91  - 179 (II )
  #                        v      SW-SE-NE-NW
  #                     (1)S   5) building_orientation = 181 - 269 (III)
  #                               SE-NE-NW-SW
  #                            6) building_orientation = 271 - 359 (IV )
  #                               NE-NW-SW-SE
  
  if (temp.rectangular == TRUE){
    colnames(Climate.I) <- new.Orientation
  } else {
    temp.val <- matrix(rep(0,48),nrow=12,ncol=4)
    for (i in 1:4) {
      temp.Pos <- grep(paste("^",new.Orientation$A[i],"$",sep=""), colnames(Climate.I))
      temp.A <- Climate.I[temp.Pos]
      temp.Pos <- grep(paste("^",new.Orientation$B[i],"$",sep=""), colnames(Climate.I))
      temp.B <- Climate.I[temp.Pos]
      temp.val[,i] = unlist(temp.A*(1-temp.b) + temp.B*temp.b)
    }
    Climate.I <- as.data.frame(temp.val)
    colnames(Climate.I) <- c("north", "west", "south","east")
  }
  return(Climate.I)
}

#TODO: make print function
#TODO: make summary function
#TODO: make plot function

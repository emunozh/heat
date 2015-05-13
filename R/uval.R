# Created by Esteban
# Wed Feb 25, 2015

#' @title getLayers
#'
#' @description
#' Function to generate layers based on an external data source
#'
#' @param material, material names
#' @param thickness, layer thickness
#' @param materialsData, (optional default = False).
#' @return Layers
#' @examples
#' Materials <- c(
#'     "Mineralischer Edelputz",
#'     "Porensinterbeton mit Quarzsand_900",
#'     "EPS_040.30",
#'     "Holzwolle-Leichtbauplatten_Heraklith-Platten (Magnesia)_390",
#'     "Mineralischer Armierungsputz"
#'     )
#' 
#' Thicknes = c(0.02,0.24,0.04,0.025,0.015)
#' Layers <- getLayers(Materials, Thicknes)
#' @author M. Estebna Munoz H.
getLayers <- function(material, thickness,
                      materialsData=FALSE, verbose=FALSE){
    # Load an external data file
    if(class(materialsData) == "data.frame"){
        materials <- materialsData
    }else{
        data(materials)
    }

    # Create empty vectors
    Conductivity <- vector(length=length(material))
    Diffusion <- vector(length=length(material))

    # Search layers in external data source 
    for(m in 1:length(material)){
        ind <- which(materials$name == material[m])
        if(length(ind) > 1){
            cat("WARNING: found more than one matching material, I will use only the first match\n")
            ind <- ind[1]
        }

        if(verbose){
            cat(length(ind)==0, "\t", m, "\t",
                ind, "\t", materials$ConducV[ind], "\n")
        }
        if(length(ind) == 0){
            Conductivity[m] <- NaN
            Diffusion[m] <- NaN
        }else{
            Conductivity[m] <- materials$ConducV[ind]
            Diffusion[m] <- materials$DiffV[ind]
        }
    }

    # Layers from Outside to Inside Layer
    Layers <- data.frame(
        Layer = material,
        Thickness = thickness,
        Conductivity = Conductivity,
        Diffusion = Diffusion
        )
    return(Layers)
}

#TODO: convert to class function plot
PlotT <- function(
        Layers, yvalues, plotname,
        equivalent=FALSE, press=FALSE, condensate=FALSE){
    if (equivalent){
        filename <- paste(plotname, "pressure", sep="")
        add.plot = 0.1
        yy <- c(0, 0, 3000, 3000)
        Xlayer = Layers$Sd
        HashLayer = Layers$Diffusion
        HashDensity = 0.001
        p.ylim = c(0,2500)
        p.ylab = "Pressure in Pascal"
        p.xlab = "Diffusion equivalent thickness"
        p.main = "Pressure profile of building component\n"
    } else {
        filename <- paste(plotname, "temperature", sep="")
        add.plot = 0.01
        yy <- c(-15, -15, 25, 25)
        Xlayer = Layers$Thicknes
        HashLayer = Layers$Conductivity
        HashDensity = 10
        p.ylim = c(-12,22)
        p.ylab = "Temperature in degree Celsius"
        p.xlab = "Thickness in cm"
        p.main = "Temperature profile of building component\n"
    }


    if (!(file.exists("FIGURES"))) dir.create("FIGURES")
    
    pdf(file=paste("FIGURES/", filename,".pdf", sep=""))
    
    Thicknes <- c(0, add.plot)
    for(a in 1:length(Layers$Thicknes)){
        Thicknes[a+2] <- Thicknes[a+1] + Xlayer[a]
    }
    Thicknes[length(Thicknes)+1] <- Thicknes[length(Thicknes)] + add.plot
    
    pos <- Thicknes[2:(length(Thicknes)-1)]
    
    pos.lab <- c()
    for(j in 1:(length(Thicknes)-1)){
        pos.lab[j] <- Thicknes[j]+(Thicknes[j+1] - Thicknes[j])/2
    }
    
    lab <- c("outdoors")
    for(l in 1:length(Layers$Layer)){
        lab[l+1] <- as.character(Layers$Layer[l])
    }
    lab[length(lab)+1] <- "indoors"
    
    # create variable for plot hash
    for(c in 1:length(Layers$Conductivity)){
        if (is.nan(HashLayer[c])){
        Layers$PlotH[c] <- 0
        } else {
        Layers$PlotH[c] <- HashLayer[c] * HashDensity
        }
    }

    plot(yvalues~Thicknes, type="n",
        ylab=p.ylab, xlab=p.xlab, main=p.main, ylim=p.ylim,
        par(adj=0.5))
    axis(3, at=pos.lab, labels=lab, col.axis="red")
    abline(v = pos, lty = 2)
    for(p in 1:(length(pos)-1)){
        xx <- c(pos[p], pos[p+1], pos[p+1], pos[p])
        polygon(xx, yy, col = "grey", density = Layers$PlotH[p])
    }
    lines(yvalues~Thicknes, type="l",col="red",lwd=3,lty=1)
    if (equivalent){
        lines(press~Thicknes, type="l",col="red",lwd=3,lty=2)
        legend("bottomright", c("P","P_sat"), 
            col=c("red","red"), lwd=3, lty=c(1,2),
            bty = "n", bg="white")
        for(c in 1:length(Thicknes)){
        if(condensate[c]){
            text(yvalues[c]~Thicknes[c], labels="condensate", offset=0.9, pos=1)
            points(yvalues[c]~Thicknes[c],pch=25,bg="blue",cex=2)
        }
        }
    }
    dev.off()
}

# Saturation point
getPressSat <- function(t){
  if(t >= 0){
    Psat = 610.5 * exp((17.269*t)/(237.3+t))
  } else {
    Psat = 610.5 * exp((21.875*t)/(265.5+t))
  }
  return(Psat)
}

#TODO: Prof source
#' @title calculateUval
#'
#' @description
#' U-value calculator
#' @source
#' DIN 4108-3
#'
#' @param Layers, building elements layers
#' @param k.t_e (optional, default = -10) External air temperature [C]
#' @param k.t_i (optional, default = 20) Internal air temperature [C]
#' @param k.h_e (optional, default = 25) External heat transfer coefficient
#' [m2K/W]
#' @param k.h_i (optional, default = 8) Internal heat transfer coefficient
#' [m2K/W]
#' @param k.rf_e (optional, default = 80) External air moisture
#' @param k.rf_i (optional, default  = 50) Internal air moisture
#' @param k.p_e (optional, default = 1168) External pressure [Pa]
#' @param k.p_i (optional, default = 321) Internal pressure [Pa]
#' @param plottemp (optional, default = FALSE) Store a temperature profile plot
#' under "./FIGURES/[name]_pressure.pdf" and
#' "./FIGURES/[name]_temperature.pdf"
#' @param plotpress (optional, default = FALSE) Store a pressure profile plot
#' under "./FIGURES"
#' @param writetab (optional, default = FALSE) Store three tables for pressure,
#' temperature and material properties as "./TABLES/[name]_PressProfile.csv", 
#' "./TABLES/[name]_TempProfile.csv" and "./TABLES/[name]_Layers.csv"
#' @param name (optional = "")
#' @return Uval component U-value
#' @examples
#' # Reproduces the example 4 from DIN 4108-3
#' MaterialsToget <- c(
#'   "Mineralischer Edelputz",
#'   "Porensinterbeton mit Quarzsand_900",
#'   "EPS_040.30",
#'   "Holzwolle-Leichtbauplatten_Heraklith-Platten (Magnesia)_390",
#'   "Mineralischer Armierungsputz"
#'   )
#' 
#' Thicknes = c(0.02,0.24,0.04,0.025,0.015)
#' 
#' # Generate the Layers
#' Layers <- getLayers(MaterialsToget, Thicknes)
#' 
#' uval <- calculateUval(Layers, k.t_e=-5,
#'                       plottemp=TRUE, plotpress=TRUE,
#'                       name="ExternalData_", writetab=TRUE)
#'
#' # from DIN 4108-3
#' # B.5 Beispiel 4: Außenwand mit nachträglicher raumseitiger Wärmedämmung
#' # Konstruktion: Außenwand mit nachträglicher Innendämmung
#' Layers <- data.frame(
#'     Layer = c(
#'         "Außenputz",
#'         "Mauerwerk",
#'         "EPS-Dämmstoff",
#'         "HWL-Platte",
#'         "Innenputz"),
#'     Thicknes = c(0.02,0.24,0.04,0.025,0.015),
#'     Conductivity = c(1,0.4,0.04,0.08,0.7),
#'     Diffusion = c(40,8,20,4,15))
#' 
#' uval <- calculateUval(Layers, k.t_e=-5,
#'                       plottemp=TRUE, plotpress=TRUE,
#'                       name="Innendaemmung_", writetab=TRUE)
#'
#' # from DIN 4108-3
#' # Konstruktion: Leichte Außenwand mit hinterlüfteter Vorsatzschale
#' Layers <- data.frame(
#'     Layer = c(
#'         "Vorgehängte Außenschale",
#'         "Belüftete Luftschicht",
#'         "Spanplatte V100",
#'         "Mineralwolle",
#'         "Diffusionshemmende Schicht",
#'         "Spanplatte V20"),
#'     Thicknes = c(0.02, 0.03, 0.019, 0.16, 0.00005, 0.019),
#'     Conductivity = c(NaN, NaN, 0.127, 0.04, NaN, 0.127),
#'     Diffusion = c(NaN, NaN, 100, 1, 40000, 50))
#'     
#' uval <- calculateUval(Layers, k.t_e=-5,
#'                       plottemp=TRUE, plotpress=TRUE,
#'                       name="leichteAussenwand_", writetab=TRUE)
#'
#' # from Keller, B., & Rutz, S. (2010).
#' # Pinpoint: Key facts + figures for sustainable buildings. Basel: Birkhauser.
#' Layers <- data.frame(
#'     Layer = c("External Plaster", "Expanded Polystyrene", 
#'               "Reinforced Concrete", "Internal Stuco"),
#'     Thicknes =     c(0.01, 0.18 , 0.25, 0.01),
#'     Conductivity = c(0.87, 0.038, 1.8,  0.7),
#'     Diffusion =    c(15, 30, 80, 6))
#' 
#' uval <- calculateUval(Layers, k.t_e=-5,
#'                       plottemp=TRUE, plotpress=TRUE,
#'                       name="ReinforcedConcrete_", writetab=TRUE)
#'
#' @author M. Estebna Munoz H.
calculateUval <- function(
        Layers,
        k.t_e =  -10, 
        k.t_i =   20, # Internal and External temperature [°c]
        k.h_e =   25, 
        k.h_i =    8, # Internal heat transfer coefficients [m2K/W]
        k.rf_e =  80,
        k.rf_i =  50, # Internal air moisture [%]
        k.p_e = 1168,
        k.p_i =  321, # Internal pressure [Pa]
        plottemp = FALSE,
        plotpress = FALSE,
        writetab = FALSE,
        name=""
    ){
    # Compute the u-value for the given component.
    # Implemented for wall elements.

    # We calculate the transmission [W/mK]
    Layers$H <- Layers$Thicknes / Layers$Conductivity
    # And the corresponding r values [m2/ K/W]
    R_tot = 1/k.h_i + sum(Layers$H, na.rm = TRUE) + 1/k.h_e
    Uval = 1/R_tot
    
    # We calculate the equivalent watter diffusion thickness [m]
    Layers$Sd <- Layers$Thicknes * Layers$Diffusion
    Layers$Sd[is.nan(Layers$Sd)] <- 0
    # And the corresponding sdt value [m]
    Sd_tot = sum(Layers$Sd, na.rm = TRUE)

    # We calculate the temperature difference between layers
    q = Uval * abs(k.t_i-k.t_e) # [W/m2K]
    temp.delta <- c(1/k.h_e*q)
    for(i in 1:length(Layers$Thicknes)){temp.delta[i+1] <- Layers$H[i]*q}
    temp.delta[length(temp.delta)+1] <- c(1/k.h_i*q)
    # And the temperature profile
    temp <- c(k.t_e)
    for(j in 1:length(temp.delta)){
        if (is.nan(temp.delta[j])){
            temp[j+1] <- temp[j]
        } else {
            temp[j+1] <- temp[j] + temp.delta[j]
        }
    }
    
    # The saturation pressure
    press_sat <- c()
    for(m in 1:length(temp)){
        press_sat[m] <- getPressSat(temp[m])
    }

    # We calculate the pressure difference between layers
    # And the pressure profile
    gdo = abs(k.p_i-k.p_e)/sum(Layers$Sd)
    sum.sd = 0
    CondensatePoints <- c()
    press <- c()
    press[length(temp)] = k.p_e
    press[length(temp)-1] = k.p_e
    press[1] = k.p_i
    CondensatePoints[length(temp)] <- FALSE
    CondensatePoints[length(temp)-1] <- FALSE
    CondensatePoints[1] <- FALSE
    for(l in seq(length(temp)-2,2)){
        this.sd = Layers$Sd[l-1]
        press[l] = press[l+1]-this.sd*gdo
        if (press[l] > press_sat[l]){
            press[l] = press_sat[l]
            gdo = (press[l+1]-k.p_e)/(sum(Layers$Sd)-sum.sd)
            CondensatePoints[l] <- TRUE 
        } else {
            CondensatePoints[l] <- FALSE
        }
        sum.sd = sum.sd + Layers$Sd[l-1]
    }
    
    if(plottemp){PlotT(Layers, temp, name)}
    if(plotpress){PlotT(
        Layers, press, name, equivalent=TRUE,
        press = press_sat, condensate=CondensatePoints)}
    
    if(writetab){
        press_prof <- data.frame(
            Layer_Pressure=press,
            Saturation_Pressure=press_sat
        )
        temp_prof <- data.frame(
            Layer_Temperature=temp
        )
        if (!(file.exists("TABLES"))) dir.create("TABLES")
        write.csv(Layers, file = paste("TABLES/", name,"Layers.csv", sep=""))
        write.csv(temp_prof, file = paste("TABLES/", name,"TempProfile.csv", sep=""))
        write.csv(press_prof, file = paste("TABLES/", name,"PressProfile.csv", sep=""))
    }
    return(Uval)
}

#TODO: make print function
#TODO: make summary function
#TODO: make plot function

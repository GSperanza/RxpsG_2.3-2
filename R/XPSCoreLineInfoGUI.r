#XPSFitParamInfo function display coreline fit information and component parameters

#' @title XPSFitParamInfo to describe a Core-Line best fit
#' @description XPSFitParamInfo provides the list of components and 
#    the relative fitting parameters for a selected XPSCoreLine.
#' @examples
#' \dontrun{
#' 	XPSFitParamInfo()
#' }
#' @export
#'


XPSFitParamInfo <- function() {

   updateObj <- function(h,...){
      SelectedFName <- svalue(InfoObj1)
      FName <- get(SelectedFName,envir=.GlobalEnv) #carico in SampID il relativo XPSSAmple
      SpectList <- XPSSpectList(SelectedFName)
      delete(Infoframe2,InfoObj2)
      InfoObj2  <<- gcombobox(SpectList, selected=-1, editable=FALSE, spacing=10, handler=function(h,...){
                          activeSpectName <- svalue(InfoObj2)
                          Indx <- svalue(InfoObj2, index=TRUE)
                          SetSpectrum()
                          FName <- get(activeFName, envir=.GlobalEnv) #load the active spectrum
                          txt=paste("                             ", FName@Filename, ":  ",activeSpectName, "  Fit Parameters                                    ")
                          delete(InfoGroup3,SpectInfo1)
                          delete(InfoGroup3,SpectInfo2)
                          delete(InfoGroup3,SpectInfo3)
                          delete(InfoGroup3,SpectInfo4)
                          fitrng <- round(range(FName[[Indx]]@RegionToFit$x),2)
                          txt <- paste("                              Extension of the fit region:  ", fitrng[1], "-", fitrng[2])
                          SpectInfo1 <<- glabel(text=txt, container=InfoGroup3) #label to long to extend window dimensions
                          fitrng <- round(range(FName[[Indx]]@RegionToFit$y),2)
                          txt <- paste("                              Intensity of the fit region:  ",fitrng[1],"-", fitrng[2])
                          SpectInfo2 <<- glabel(text=txt, container=InfoGroup3)
                          BLtype <- FName[[Indx]]@Baseline$type
                          txt <- paste("                              Base Line Type:  ",BLtype)
                          SpectInfo3 <<- glabel(text=txt, container=InfoGroup3)
                          txt=paste("                             ", FName@Filename, ":  ",activeSpectName, "  FIT PARAMETERS                                    ")
                          SpectInfo4 <<- glabel(text=txt, container=InfoGroup3)

                          if (length(FName[[Indx]]@Components) == 0){  #no information if Baseline not present
                             InfoTable[] <- ""
                             ShowParam[] <- ""
                             message <- paste("No Fit found for", activeSpectName, sep=" ")
                             gmessage(msg=message, title = "CORE LINE INFO",  icon = "warning")
                          } else {
                             InfoTable[] <- SetDataFrame()
                             ShowParam[] <- ""
                          }
                       }, container=Infoframe2)
      add(Infoframe2,InfoObj2)
      InfoTable[] <- "Select a CoreLine"   #update the Table info
      ShowParam[] <- ""
      plot(FName)
   }


   SetSpectrum <- function(h,...){
      activeFName <- svalue(InfoObj1)
      activeSpectName <- svalue(InfoObj2)
      Indx <- svalue(InfoObj2, index=TRUE)
      assign("activeFName", activeFName,envir=.GlobalEnv) #loas activeXPSSample
      assign("activeSpectName", activeSpectName,envir=.GlobalEnv)
      assign("activeSpectIndx", Indx, envir=.GlobalEnv)
      FName <- get(activeFName, envir=.GlobalEnv)
      plot(FName[[Indx]])
   }


   SetDataFrame <- function() {
      activeFName <- get("activeFName", envir=.GlobalEnv)
      FName <- get(activeFName, envir=.GlobalEnv)
      Indx <- get("activeSpectIndx", envir=.GlobalEnv)
      N_Comp <- length(FName[[Indx]]@Components)
      CompNames <- names(FName[[Indx]]@Components)

      fnName <- sapply(FName[[Indx]]@Components, function(x)  x@funcName) #was VBtop analysis performed on coreline?
      if ("VBtop" %in% fnName){
         for(jj in 1:N_Comp){ #jj runs on the fit components
            FitFnctn <- rbind(FitFnctn, "//")
            Area <- rbind(Area, "//")
            FWHM <- rbind(FWHM, "//")
            RSF <- rbind(RSF, "//")
            if (FName[[Indx]]@Components[[jj]]@funcName=="VBtop"){
                BE <- rbind(BE, as.character(round(FName[[Indx]]@Components[[jj]]@param[1,1], 3))) #Component BE
                CompNames[jj] <- paste(CompNames[jj], "VBtop", sep=" ")
            } else {
                BE <- rbind(BE, "//")
            }
            Conc  <- rbind(Conc, "//")
         }
         CompNames <- encodeString(CompNames, width=10, justify="centre")
         FitFnctn <- encodeString(FitFnctn, width=10, justify="centre")
         Area <- encodeString(Area, width=10, justify="right")
         FWHM <- encodeString(FWHM, width=10, justify="right")
         RSF <- encodeString(RSF, width=10, justify="right")
         BE <- encodeString(BE, width=10, justify="right")
         Conc <- encodeString(Conc, width=10, justify="right")
         fitParam <- data.frame(CompNames, FitFnctn, Area, FWHM, RSF, BE, Conc, stringsAsFactors=FALSE)

      } else {
#--- Fit Quantification sul FIT ---
         RSF <- FName[[Indx]]@RSF
         if (RSF==0) RSF <- 1
         sumCoreLine <- sum(FName[[Indx]]@Fit$y)/RSF #Fit contribution
         for(jj in 1:N_Comp){    #jj runs on the fit components
            RSF <- FName[[Indx]]@Components[[jj]]@rsf
            if (RSF==0) { #RSF not defined(es. Auger, VB spectra...): cannot normalize
               sumComp[jj] <- sum(FName[[Indx]]@Components[[jj]]@ycoor-FName[[Indx]]@Baseline$y)
            } else {
               sumComp[jj] <- sum(FName[[Indx]]@Components[[jj]]@ycoor-FName[[Indx]]@Baseline$y)/RSF  #contributo della singola componente
            }
         }
#---Set DataFrame Table
         RSF <- NULL
         for(jj in 1:N_Comp){ #jj runs on the FitComponents
#           CompNames <- rbind(CompNames, paste("C", jj, sep=""))
            FitFnctn <- rbind(FitFnctn, unlist(FName[[Indx]]@Components[[jj]]@funcName))
            Area <- rbind(Area,round(sumComp[jj], 3))
            FWHM <- rbind(FWHM,round(FName[[Indx]]@Components[[jj]]@param[3,1], 2)) #FWHM component jj
            RSF <- rbind(RSF,unlist(FName[[Indx]]@Components[[jj]]@rsf)) #RSF component jj
            BE <- rbind(BE,round(FName[[Indx]]@Components[[jj]]@param["mu","start"], 2)) #BE component jj
            Conc <- rbind(Conc,round(100*sumComp[jj]/sumCoreLine, 2))  #Concentration component jj
         }
         CompNames <- encodeString(CompNames, width=10, justify="centre")
         FitFnctn <- encodeString(FitFnctn, width=10, justify="centre")
         Area <- encodeString(Area, width=10, justify="right")
         FWHM <- encodeString(FWHM, width=10, justify="right")
         RSF <- encodeString(RSF, width=10, justify="right")
         BE <- encodeString(BE, width=10, justify="right")
         Conc <- encodeString(Conc, width=10, justify="right")
         fitParam <- data.frame(CompNames, FitFnctn, Area, FWHM, RSF, BE, Conc, stringsAsFactors=FALSE)
      }
      return(fitParam)
   }


#----- variabili -----

   FName <- NULL
   FName <- get(activeFName, envir = .GlobalEnv)
   activeFName <- get("activeFName", envir = .GlobalEnv)
   Indx <- activeSpectIndx
   N_comp=length(FName[[Indx]]@Components)
   SpectList <- XPSSpectList(activeFName)
   sumCoreLine <- 0
   sumComp <- array(0,dim=N_comp)  #define a dummy vector of zeros
   CompNames <- NULL
   FitFnctn <- NULL
   Area <- NULL
   FWHM <- NULL
   BE <- NULL
   RSF <- NULL
   Conc <- NULL
   fitParam <- NULL


#--- CTRL on Fit
#   if (length(FName[[Indx]]@Components) == 0){  #no information se il Baseline non presente
#      message <- paste("No Fit found for", activeFName," - ", activeSpectName, sep=" ")
#      gmessage(msg=message, title = "CORE LINE INFO",  icon = "warning")
#      return()
#   }

#---Set Data.Frame of fit parameters on active XPS Sample

   InfoFwin <- gwindow(title="Core Line Fit Info.", parent=c(50, 10), visible=FALSE) #open a second window for the fit parameters
   size(InfoFwin) <- c(550,450)
   InfoGroup1 <- ggroup(horizontal=FALSE, container=InfoFwin)

# --- Spect-Selection ---
   InfoGroup2 <- ggroup(horizontal=TRUE, container=InfoGroup1)
   glabel("      ", container = InfoGroup2) #glabel as separator
   Infoframe1 <- gframe(" Select XPSsample ", spacing=5, container=InfoGroup2)
   glabel("      ", container = InfoGroup2) #glabel as separator
   Infoframe2 <- gframe(" Select CoreLine ", spacing=5, container=InfoGroup2)
   FNameList <- XPSFNameList()

   InfoObj1 <- gcombobox(FNameList, selected=-1, editable=FALSE, spacing=10, handler=function(h,...){
                          SetSpectrum()
                          updateObj()
                        }  , container = Infoframe1)
   svalue(InfoObj1) <- activeFName

   InfoObj2 <- gcombobox(SpectList, selected=Indx, editable=FALSE, spacing=10, handler=function(h,...){
                          activeSpectName <<- svalue(InfoObj2)
                          Indx <<- svalue(InfoObj2, index=TRUE)
                          SetSpectrum()
                          txt=paste("                             ", FName@Filename, ":  ",activeSpectName, "  Fit Parameters                                    ")
                          delete(InfoGroup3,SpectInfo1)
                          delete(InfoGroup3,SpectInfo2)
                          delete(InfoGroup3,SpectInfo3)
                          delete(InfoGroup3,SpectInfo4)
                          fitrng <- round(range(FName[[Indx]]@RegionToFit$x),2)
                          txt <- paste("                              Extension of the fit region:  ", fitrng[1], "-", fitrng[2])
                          SpectInfo1 <<- glabel(text=txt, container=InfoGroup3)
                          fitrng <- round(range(FName[[Indx]]@RegionToFit$y),2)
                          txt <- paste("                              Intensity of the fitted spectrum:  ",fitrng[1],"-", fitrng[2])
                          SpectInfo2 <<- glabel(text=txt, container=InfoGroup3)
                          BLtype <- FName[[Indx]]@Baseline$type
                          txt <- paste("                              Base Line Type:  ",BLtype)
                          SpectInfo3 <<- glabel(text=txt, container=InfoGroup3)
                          txt <- paste("                             ", FName@Filename, ":  ",activeSpectName, "  FIT PARAMETERS                                    ")
                          SpectInfo4 <<- glabel(text=txt, container=InfoGroup3)

                          if (length(FName[[Indx]]@Components) == 0){  #no information if Baseline not defined
                             InfoTable[] <<- ""
                             ShowParam[] <<- ""
                             message <- paste("No Fit found for", activeSpectName, sep=" ")
                             gmessage(msg=message, title = "CORE LINE INFO",  icon = "warning")
                          } else {
                             InfoTable[] <<- SetDataFrame()
                             insert(ShowParam, "  ")
                          }
                       }, container=Infoframe2)


   InfoGroup3 <- ggroup(horizontal=FALSE, spacing=2, container=InfoGroup1)

   fitrng <- round(range(FName[[Indx]]@RegionToFit$x),2)
   txt <- paste("                              Extension of the fit region:  ", fitrng[1], "-", fitrng[2], sep="")
   SpectInfo1 <- glabel(text=txt, container=InfoGroup3)
   fitrng <- round(range(FName[[Indx]]@RegionToFit$y),2)
   txt <- paste("                              Intensity of the fit region:  ",fitrng[1],"-", fitrng[2], sep="")
   SpectInfo2 <- glabel(text=txt, container=InfoGroup3)
   BLtype <- FName[[Indx]]@Baseline$type
   txt <- paste("                              Base Line Type:  ",BLtype, sep="")
   SpectInfo3 <- glabel(text=txt, container=InfoGroup3)
   txt <- paste("                              ", FName@Filename, ":  ",activeSpectName, "  FIT PARAMETERS", sep="")
   SpectInfo4 <- glabel(text=txt, container=InfoGroup3)
   txt <- paste("                              Comments:  ", FName@Comments, sep="")
   SpectInfo5 <- glabel(text=txt, container=InfoGroup3)

   InfoGroup4 <- ggroup(horizontal=FALSE, container=InfoGroup1)
   if (length(FName[[Indx]]@Components) > 0){
      fitParam <- SetDataFrame()
   } else {
      txt <- paste("No Fit found for", activeSpectName, sep=" ")
      gmessage(msg=txt, title = "CORE LINE INFO",  icon = "warning")
   }

   InfoTable <- gtable(items=fitParam, container=InfoGroup4)       
   size(InfoTable) <- c(600,200)

   SpectInfo6 <- glabel(text="Component Line Shape: ", container=InfoGroup4)

   addHandlerDoubleclick(InfoTable, handler=function(h,...){ #addHandlerDoubleclick returns the index of selected component
                          fnName <- sapply(FName[[Indx]]@Components, function(x)  x@funcName) #was VBtop analysis performed on coreline?
                          if ("VBtop" %in% fnName){
                             gmessage(msg="No additional information for VBtop analysis", title="INFO", icon="info")
                             return()
                          }
                          CompIndx <- svalue(InfoTable, index=TRUE)    #reuse fitparam to load component fitting parameters
                          activeFName <- get("activeFName", envir=.GlobalEnv) #set the activeSpectrum be equal to the last loaded file
                          FName <- get(activeFName, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                          Indx <- get("activeSpectIndx", envir=.GlobalEnv)
                          FunctName <- FName[[Indx]]@Components[[CompIndx]]@funcName
                          txt <- paste("Component Line Shape: ", FunctName, sep="")
                          svalue(SpectInfo6) <- txt
                          dispose(ShowParam) #cancel the SwhoParam table
                          fitParam <- NULL
                          FP <- FName[[Indx]]@Components[[CompIndx]]@param
                          FP <- round(FP, 3)
                          VarNames <- rownames(FName[[Indx]]@Components[[CompIndx]]@param)
                          VarNames <- encodeString(VarNames, width=13, justify="right")
                          options(stringsAsFactors=FALSE) #Without this option the class(fitParam$VarNames)== FACTOR e non CHARACTER
                          fitParam <- c(fitParam, paste("    Parameter", "        Start", "          Min", "          Max\n", sep=""))
                          LL <- length(FP[[1]])
                          for (ii in 1:LL){
                               fitParam <- c(fitParam, paste(encodeString(VarNames[ii], width=13, justify="right"),
                                                             encodeString(FP$start[ii], width=13, justify="right"),
                                                             encodeString(FP$min[ii], width=13, justify="right"),
                                                             encodeString(FP$max[ii], width=13, justify="right"), sep=""))
                          }

                          insert(ShowParam, fitParam)
                         })

    ShowParam <- gtext("", container = InfoGroup4)
    size(ShowParam) <- c(600,150)

    CLobj2 <- gbutton(" EXIT ", handler=function(h,...){
                         dispose(InfoFwin)
                   }, container = InfoGroup4)
    visible(InfoFwin) <- TRUE
}

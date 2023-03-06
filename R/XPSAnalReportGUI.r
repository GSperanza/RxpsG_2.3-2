#XPSAnalReport() function to make a report containing the list of fitting components 
#and relative abundance for the selected coreline

#' @title XPSAnalReport provides information about a Core Line fit
#' @description XPSAnalReport() makes the computation of the integral intensity of
#'  each of the fitting components of a given coreline. The sum of the component 
#'  integral intensities =100% (it is the best fit integral intensity).
#' @examples
#' \dontrun{
#'	  XPSReport()
#' }
#' @export
#'


XPSAnalReport <- function(){


   RetrivePE <- function(CoreLine){  #Retrieve the PE value from the CL coreline information slot
      info <- CoreLine@Info[1]   #retrieve info containing PE value
      xxx <- strsplit(info, "Pass energy")  #extract PE value
      PE <- strsplit(xxx[[1]][2], "Iris") #PE value
      PE <- as.integer(PE[[1]][1])
      return(PE)
   }

   FindFittedCL <- function(){
      Fidx <- which(sapply(XPSSample, function(x) hasBaseline(x))==TRUE)  #extract all CoreLines with BaseLine
      SpecialComp <- c("VBtop", "VBFermi", "Derivative", "FitProfile",
                   "HillSigmoid", "HillSigmoid.KE", "Sigmoid",
                   "PowerDecay", "ExpDecay", "Linear")
#now eliminate from FittedCL list the SpecialComponents
      NN <- length(Fidx)
      CL <- Fidx  #CL temporary vector with indexes of FittedCL
      for(ii in NN:1){
          LL <- length(XPSSample[[CL[ii]]]@Components)
          if (LL > 0){  #Fit components must be present not only a baseline
              for(jj in 1:LL){
                  funcName <- XPSSample[[CL[ii]]]@Components[[jj]]@funcName
                  if (funcName %in% SpecialComp) {
                      Fidx <- Fidx[-ii]
                      break
                  }
              }
          }
      }
      return(Fidx)
   }

   ReportSelection <- function(){
      SelectedCL2 <<- SelectedCL1 <<- NULL
      TabTxt <<- NULL
      dispose(tabWin) #clears tabWin window
      font(tabWin) <- list(weight="light", family=Font, style=FStyle, size=FSize)
      TabTxt <<- ""
      TabTxt <<- c(TabTxt, paste ("===> File Name:", XPSSample@Filename), "\n\n")  #Filename

#----Call STANDARD Report
      RprtOpt <- svalue(StdrdRprt) #Make Quantification Report?
      if (RprtOpt == TRUE){
         jj <- 1
         for(ii in 1:length(ChkCL2)){
              if(svalue(ChkCL2[[ii]])){
                 SelectedCL2[jj] <<- NonFittedCL[ii]
                 SelectedCL2[jj] <<- unlist(strsplit(SelectedCL2[jj], "\\."))[1] #extract index from the SelectedCL1 name
                 jj <- jj+1
              }
          }
          SelectedCL2 <<- as.integer(SelectedCL2)
          if(length(SelectedCL2) == 0){
             gmessage(msg="Standard Report requires selection of OTHER Core-Lines ", title="ERROR", icon="error")
             return()
          }
          TabTxt <<- c(TabTxt, paste("STANDARD REPORT \n\n"))
          MakeStdrdReport()
          TabTxt <<- c(TabTxt, "\n")
      }
#----Call FIT Report
      RprtOpt <- svalue(FitRprt)   #Make Fit Report?
      if (RprtOpt == TRUE){
          jj <- 1
          for(ii in 1:length(ChkCL1)){
              if(svalue(ChkCL1[[ii]])){
                 SelectedCL1[jj] <<- FittedCL[ii]
                 SelectedCL1[jj] <<- unlist(strsplit(SelectedCL1[jj], "\\."))[1] #extract index from the SelectedCL1 name
                 jj <- jj+1
              }
          }
          if(length(SelectedCL1) == 0){
             gmessage(msg="Fit Report requires selection of FITTED Core-Lines", title="ERROR", icon="error")
             return()
          }
          SelectedCL1 <<- as.integer(SelectedCL1)
          TabTxt <<- c(TabTxt, paste("FIT REPORT \n\n"))
          for(ii in SelectedCL1){  #Make fit Report for the selected CoreLines
              MakeFitReport(XPSSample[[ii]])
          }
          TabTxt <<- c(TabTxt, "\n")
      }
#----call QUANTIFICATION Report
      RprtOpt <- svalue(QuantRprt) #Make Quantification Report?
      if (RprtOpt == TRUE){
          jj <- 1
          for(ii in 1:length(ChkCL1)){
              if(svalue(ChkCL1[[ii]])){
                 SelectedCL1[jj] <<- FittedCL[ii]
                 SelectedCL1[jj] <<- unlist(strsplit(SelectedCL1[jj], "\\."))[1] #extract index from the SelectedCL1 name
                 jj <- jj+1
              }
          }
          if(length(SelectedCL1) == 0){
             gmessage(msg="Quantification Report requires selection of FITTED Core-Lines", title="ERROR", icon="error")
             return()
          }
          SelectedCL1 <<- as.integer(SelectedCL1)
          if(length(SelectedCL1) == 0){
             gmessage(msg="Quantification Report requires selection of FITTED Core-Lines", title="ERROR", icon="error")
             return()
          }
          TabTxt <<- c(TabTxt, "QUANTIFICATION REPORT \n\n")
          MakeQuantReport()
          TabTxt <<- c(TabTxt, "\n")
      }
      if(svalue(QuantRprt) == FALSE && svalue(FitRprt) == FALSE && svalue(StdrdRprt) == FALSE){
         gmessage(msg="Please Select the Reporting Model", title="ERROR", icon="error")
         return()
      }
      if(length(SelectedCL1) == 0 && length(SelectedCL2) == 0){
         gmessage(msg="Please Select the Core-Lines to Report", title="ERROR", icon="error")
         return()
      }
      TabTxt <<- paste(TabTxt, collapse="")
      insert(tabWin, TabTxt) #write report in tabWin
      cat(as.character(TabTxt))
   }


##====MAKE Reports
##FIT Report
   MakeFitReport <- function(CoreLine){
      CompNames <- names(CoreLine@Components)
      sumCoreLine <- 0
      N_comp <- length(CoreLine@Components) #this is the number of fit components
      sumComp <- array(0,dim=N_comp)  #array if zerros
      RSF <- CoreLine@RSF
      E_stp <- round(abs(CoreLine@.Data[[1]][2]-CoreLine@.Data[[1]][1]), 2) #energy step

      if (length(CoreLine@Baseline)==0) {
#--- No Baseline No Fit ---
         TabTxt <<- c(TabTxt, paste("*** ", CoreLine@Symbol, ": no fit present", sep=" "), "\n")
         PE <- RetrivePE(CoreLine)
         TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "Energy Step: ",E_stp, sep=""), "\n")
      } else if (length(CoreLine@Baseline) > 0 && N_comp==0) {
#--- Baseline only No Fit ---
         TabTxt <<- c(TabTxt, paste("*** ", CoreLine@Symbol, "Coreline only Baseline ", CoreLine@Baseline$type[1], " present: ", sep=" "), "\n")
         PE <- RetrivePE(CoreLine)
         TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "Energy Step: ",E_stp, sep=""), "\n")
         if (RSF==0){
            sumCoreLine <- sum(CoreLine@RegionToFit$y-CoreLine@Baseline$y*E_stp) #Spectral area of the core-line
         } else {
            sumCoreLine <- sum(CoreLine@RegionToFit$y-CoreLine@Baseline$y)*E_stp/RSF #normalized spectral area of the core-line
         }

         area <- sprintf("%1.2f",sumCoreLine)  #converto
         txt <- paste("  ", CoreLine@Symbol,"peak area: ", area, sep=" ")
         CellLength <- nchar(txt)
         TabTxt <<- c(TabTxt,printCell("label",txt,cellB,CellLength,"left"), "\n")
         TabTxt <<- c(TabTxt, "\n")

      } else if (N_comp > 0) {
#--- Baseline + Fit ---
         #VBtop analysis
         fnName <- sapply(CoreLine@Components, function(x)  x@funcName) #was VBtop analysis performed on coreline?
         if ("VBtop" %in% fnName){
             TabTxt <<- c(TabTxt, paste("***  ", CoreLine@Symbol, " Core-Line Fit Info: ", sep=""), "\n")
             PE <- RetrivePE(CoreLine)
             TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "Energy Step: ",E_stp, sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             TabTxt <<- c(TabTxt, paste("     BaseLine applied: ",CoreLine@Baseline$type[1], sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             CellLength <- c(10, 12, 8)
             cellB <- " "
             txt <- c("Components", "Fit Funct.", "Position")
             TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n")
             for(jj in 1:N_comp){ #jj runs on CoreLines, jj runs on Fit components
                 Function <- sprintf("%s",CoreLine@Components[[jj]]@funcName) #Fit Funct name
                 BE <- "//"
                 if (Function=="VBtop"){
                     BE <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[1,1]) #Component BE
                 }
                 txt <- c(CompNames[jj], Function, BE) #make string to print
                 TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n") #print in mode TABLEROW
             }
         } else {
             sumCoreLine <- 0
             TotArea <- 0
             for(jj in 1:N_comp){    #jj runs on the core-line fit components
                RSF <- CoreLine@Components[[jj]]@rsf
                if (RSF==0) { #RSF not defined
                   sumComp[jj] <- sum(CoreLine@Components[[jj]]@ycoor-CoreLine@Baseline$y)*E_stp
                } else {
                   sumComp[jj] <- sum(CoreLine@Components[[jj]]@ycoor-CoreLine@Baseline$y)*E_stp/RSF  #controbution of the single FITcomponent
                }
                sumCoreLine <- sumCoreLine + sumComp[jj]
             }
             TotArea <- TotArea + sum(CoreLine@Fit$y)/RSF #Contributo del Fit

# width of colum<ns("Components", "FitFunct.", "Area(cps)", "Intensity", "FWHM", "BE(eV)", "TOT.(%)")
             TabTxt <<- c(TabTxt, paste("***  ", CoreLine@Symbol, " Core-Line Fit Info: ", sep=""), "\n")
             PE <- RetrivePE(CoreLine)
             TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "Energy Step: ",E_stp, sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             TabTxt <<- c(TabTxt, paste("     BaseLine applied: ",CoreLine@Baseline$type[1], sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             CellLength <- c(10, 10, 15, 10, 6, 8, 12)
             cellB <- " "

#Columns names
             txt <- c("Components", "Fit Funct.", "Area (cps)", "Intensity", "FWHM", "Position", "Weight(%)")
             TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n")


#Rows describing Fit Components
             for(jj in 1:N_comp){ #jj runs on CoreLines, jj runs on Fit components
                Function <- sprintf("%s",CoreLine@Components[[jj]]@funcName) #Fit Funct name
                if (length(grep("DoniachSunjic", Function))){
                    Function <- gsub("DoniachSunjic", "D.S.",Function) #extract the additional part after DoniachSunjic
                }
                Area <- sprintf("%1.2f",sumComp[jj]) #area componente jj linea di core ii
                Intensity <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[1,1]) #Component Intensity componente
                FWHM <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[3,1]) #Component FWHM
                BE <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[2,1]) #Component BE
                Conc <- sprintf("%1.2f",100*sumComp[jj]/sumCoreLine)  #Core-Line Relative Component Concentrations: Sum(components%)=100
                txt <- c(CompNames[jj], Function, Area, Intensity, FWHM, BE, Conc) #make string to print
                TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n") #print in mode TABLEROW
             }
         }
         TabTxt <<- c(TabTxt, "\n")
      }
   }

##STANDARD Report
   MakeStdrdReport <- function() {
      for(ii in SelectedCL2){  #Make fit Report for the selected CoreLines
          LL <- length(XPSSample[[ii]]@.Data[[1]])
          Bnd1 <- XPSSample[[ii]]@.Data[[1]][1]
          Bnd2 <- XPSSample[[ii]]@.Data[[1]][LL]
          TabTxt <<- c(TabTxt, paste("*** ", XPSSample[[ii]]@Symbol, ": no fit present", sep=" "), "\n")
          PE <- RetrivePE(XPSSample[[ii]])
          E_stp <- round(abs(XPSSample[[ii]]@.Data[[1]][2] - XPSSample[[ii]]@.Data[[1]][1]), 2)
          TabTxt <<- c(TabTxt, paste("Core Line  : ",slot(XPSSample[[ii]],"Symbol"), sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("E-range    : ",round(Bnd1, 2)," - ", round(Bnd2, 2), sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("N. data    : ",length(XPSSample[[ii]]@.Data[[1]])), "\n")
          TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("Energy Step: ",E_stp, sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("Baseline   : ",ifelse(hasBaseline(XPSSample[[ii]]),XPSSample[[ii]]@Baseline$type[1], "NONE"), sep=""), "\n")
          TabTxt <<- c(TabTxt, "Info: \n")
          TabTxt <<- c(TabTxt, paste(XPSSample[[ii]]@Info, "\n", sep=""))

### this block NOT NECESSARY all information in ...@Info
#          if(XPSSample[[ii]]@Symbol == "VBf"){
#             idx <- sapply(XPSSample[[ii]]@Components, function(x) which(x@funcName == "VBtop"))
#             idx <- which(idx > 0) #index of the component having 'VBtop' name
#             TabTxt <<- c(TabTxt, paste("VBFermi:", XPSSample[[ii]]@Components[[idx]]@param["mu", "start"], sep=""), "\n")
#          }
#          if("\U0394." %in% XPSSample[[ii]]@Symbol){ #MaxMinDistance
#             idx <- sapply(XPSSample[[ii]]@Components, function(x) which(x@funcName == "Derivative"))
#             idx <- which(idx > 0) #index of the component having 'VBtop' name
#             MaxMinD <- abs(XPSSample[[ii]]@Components[[idx]]@param["mu", "min"] -
#                            XPSSample[[ii]]@Components[[idx]]@param["mu", "max"])
#             TabTxt <<- c(TabTxt, paste("MaxMin Dist.:", MaxMinD, sep=""), "\n")
#          }
          TabTxt <<- c(TabTxt, "\n")
      }
   }

##QUANTIFICATION Report
   MakeQuantReport <- function(){
      sumCoreLine <- 0
      TotArea <- 0
      CellLength <- c(10, 15, 5, 8, 8, 9)
      cellB <- " "
      #Columns names

      AreaComp <- list()
      NormAreaComp <- list()
      for(ii in SelectedCL1){  #SelectedCL1 contains the analyzed CoreLines excluding VB Auger spectra
          AreaComp[[ii]] <- vector()
          NormAreaComp[[ii]] <- vector()
          N_Comp <- length(XPSSample[[ii]]@Components)
          E_stp <- abs(XPSSample[[ii]]@.Data[[1]][2]-XPSSample[[ii]]@.Data[[1]][1]) #energy step
          if(N_Comp == 0){  #Contribution of NON-fitted BKG-subtracted Core-Lines
             RSF <- XPSSample[[ii]]@RSF
             AreaComp[[ii]][1] <- sum(XPSSample[[ii]]@RegionToFit$y-XPSSample[[ii]]@Baseline$y)*E_stp
             NormAreaComp[[ii]][1] <- AreaComp[[ii]][1]/RSF
          } else {
             for(jj in 1:N_Comp){    #ii runs on the Core-Lines, jj runs on core-line-fit-components
                 RSF <- XPSSample[[ii]]@Components[[jj]]@rsf
                 AreaComp[[ii]][jj] <- sum(XPSSample[[ii]]@Components[[jj]]@ycoor-XPSSample[[ii]]@Baseline$y)*E_stp  #single FITcomponent contribution
                 if (RSF==0) { #RSF not defined
                     NormAreaComp[[ii]][jj] <- AreaComp[[ii]][jj]
                 } else {
                     NormAreaComp[[ii]][jj] <- AreaComp[[ii]][jj]/RSF
                 }
             }
          }
          TotArea <- TotArea + sum(NormAreaComp[[ii]]) #Contributo del Fit
      }

      for(ii in SelectedCL1){
          if (length(grep(":::", XPSSample[[ii]]@Info) > 0)){
              TabTxt <<- c(TabTxt, XPSSample[[ii]]@Info, "\n")
          }
          #First Table Row
          CellLength <- c(12, 15, 5, 5, 8, 10)
          txt <- c("Components", "Area(cps)", "FWHM", "RSF", "BE(eV)", "TOT.(%)")
          TabTxt <<- c(TabTxt,printCell("tableRow", txt, CellB=" ", CellLength, "center"), "\n")
          N_Comp <- length(XPSSample[[ii]]@Components)
          CompNames <- names(XPSSample[[ii]]@Components)

          #Concentration of CoreLine as a whole
          RSF <- XPSSample[[ii]]@RSF
          Area <- sprintf("%1.2f", sum(AreaComp[[ii]]))  #round to 2 decimals and transform to string
          Conc <- sprintf("%1.2f",100*sum(NormAreaComp[[ii]])/TotArea)
          txt <- c(XPSSample[[ii]]@Symbol, Area, RSF, " ", " ", Conc )  #core-line concentrations + FitFunct, FWHM and BE
          TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n")

          #Rows describing  FitComponent concentration
          if(N_Comp > 0){  #Contribution of Fit Components
             for(jj in 1:N_Comp){ #jj runs on CoreLines, jj runs on Fit components
                 Area <- sprintf("%1.2f",AreaComp[[ii]][jj]) #area componente jj linea di core ii
                 FWHM <- sprintf("%1.2f",XPSSample[[ii]]@Components[[jj]]@param[3,1]) #Component FWHM
                 RSF <- sprintf("%1.3f",XPSSample[[ii]]@Components[[jj]]@rsf) #Component RSF
                 BE <- sprintf("%1.2f",XPSSample[[ii]]@Components[[jj]]@param[2,1]) #Component BE
                 Conc <- sprintf("%1.2f",100*NormAreaComp[[ii]][jj]/TotArea)  #Concentration of componente
                 txt <- c(CompNames[jj], Area, FWHM, RSF, BE, Conc) #make string to print
                 TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n") #print in mode TABLEROW
             }
          }
          TabTxt <<- c(TabTxt, " \n")
      }
   }

   ResetVars <- function(){
      SelectedCL1 <<- NULL
      SelectedCL2 <<- NULL
      TabTxt <<- NULL
      dispose(tabWin) #clears tabWin window

   }

#----- variabili -----

   XPSSample <- get(activeFName, envir = .GlobalEnv)
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   Font <- XPSSettings$General[1]
   FStyle <- XPSSettings$General[2]
   FSize <- XPSSettings$General[3]
   NCorelines <- length(XPSSample)
   ChkCL1 <- list()
   ChkCL2 <- list()
   SelectedCL1 <- NULL
   SelectedCL2 <- NULL
   FNameList <- XPSFNameList()  #list of the XPSSample loaded in the Global Env
   FittedIdx <- FindFittedCL()
   if(length(FittedIdx) == 0){ 
      FittedIdx <- 0 
      FittedCL <- NULL
      NonFittedCL <- XPSSpectList(activeFName)
   } else {
      FittedCL <- XPSSpectList(activeFName)[FittedIdx]  #names of the analyzed CoreLines
      NonFittedCL <- XPSSpectList(activeFName)[-FittedIdx]   #names of the NON fitted Core-Lines
   }
   TabTxt <- NULL


#---Widget
   FontAttr <- list(weight="normal", style="normal", family="normal", size="small")
   txtWin <- gwindow(title="XPS Sample Report", parent=c(50, 10), visible=FALSE)
   size(txtWin) <- c(780, 700)
   RGroup1 <- ggroup(horizontal=FALSE, spacing=3, container=txtWin)

   RFrame1 <- gframe(" SELECT XPSsample ", spacing=3, container=RGroup1)
   ChkXSamp <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h, ...){
                         activeFName <<- svalue(ChkXSamp)
                         XPSSample <<- get(activeFName, envir = .GlobalEnv)
                         LL <- length(FittedCL)    #delete checkbox for Fitted and NON Fitted CoreLines
                         if(LL > 0){
                            for(ii in 1:LL){ delete(FitComplyt1, ChkCL1[[ii]]) }
                         }
                         LL <- length(NonFittedCL)
                         if(LL > 0){
                            for(ii in 1:LL){ delete(FitComplyt2, ChkCL2[[ii]]) }
                         }
                         FittedCL <<- NonFittedCL <<- NULL
                         FittedIdx <<- FindFittedCL()
                         LL <- length(FittedCL)    #N. Fitted CL in the new XPSSample
                         if(length(FittedIdx) == 0){
                            FittedIdx <<- 0
                            FittedCL <<- NULL
                            NonFittedCL <<- XPSSpectList(activeFName)
                         } else {
                            FittedCL <<- XPSSpectList(activeFName)[FittedIdx]  #names of the analyzed CoreLines
                            NonFittedCL <<- XPSSpectList(activeFName)[-FittedIdx]   #names of the NON fitted Core-Lines
                         }
                         LL <- length(FittedCL)
                         if (LL > 0 && LL <= 7){
                             for(ii in 1:LL){
                                 FitComplyt1[1,ii] <<- ChkCL1[[ii]] <<- gcheckbox(FittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt1)
                             }
                         } else if (LL > 7){
                             for(ii in 1:7){
                                 FitComplyt1[1,ii] <<- ChkCL1[[ii]] <<- gcheckbox(FittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt1)
                             }
                             for(ii in 8:LL){
                                 FitComplyt1[2,(ii-7)] <<- ChkCL1[[ii]] <<- gcheckbox(FittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt1)
                             }
                         }
                         LL <- length(NonFittedCL)
                         if (LL > 0 && LL <= 7){
                             for(ii in 1:LL){
                                 FitComplyt2[1,ii] <<- ChkCL2[[ii]] <<- gcheckbox(NonFittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt2)
                             }
                         } else if (LL > 7){
                             for(ii in 1:7){
                                 FitComplyt2[1,ii] <<- ChkCL2[[ii]] <<- gcheckbox(NonFittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt2)
                             }
                             for(ii in 8:LL){
                                 FitComplyt2[2,(ii-7)] <<- ChkCL2[[ii]] <<- gcheckbox(NonFittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt2)
                             }
                         }
                         plot(XPSSample)
                     }, container = RFrame1)
   svalue(ChkXSamp) <- activeFName

   RFrame2 <- gframe("Select Core-Lines and the Report Format", spacing=1, horizontal=FALSE, container=RGroup1)
   RGroup2 <- ggroup(horizontal=TRUE, spacing=1, container=RFrame2)
   glabel("Fitted: ", container=RGroup2)
   FitComplyt1 <- glayout(spacing=5, horizontal=TRUE, container=RGroup2)
   LL <- length(FittedCL)
   if (LL > 0 && LL <= 7){
       for(ii in 1:LL){
           FitComplyt1[1,ii] <- ChkCL1[[ii]] <- gcheckbox(FittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt1)
       }
   } else if (LL > 7){
       for(ii in 1:7){
           FitComplyt1[1,ii] <- ChkCL1[[ii]] <- gcheckbox(FittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt1)
       }
       for(ii in 8:LL){
           FitComplyt1[2,(ii-7)] <- ChkCL1[[ii]] <- gcheckbox(FittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt1)
       }
   }
   RGroup3 <- ggroup(horizontal=TRUE, spacing=1, container=RFrame2)
   FitRprt <- gcheckbox("Fit report", checked=FALSE, spacing=1, container=RGroup3)
   font(FitRprt) <- list(family = "sans", size=8, weight="bold")
   QuantRprt <- gcheckbox("Quantification report", checked=FALSE, spacing=1, container=RGroup3)
   font(QuantRprt) <- list(family = "sans", size="8", weight="bold")
   gbutton(" MAKE REPORT ", spacing=3, handler=function(h, ...){
                         ReportSelection()
                     }, container=RGroup3)
   gbutton(" RESET ", spacing=3, handler=function(h, ...){
                         LL <- length(ChkCL1)
                         for(ii in 1:LL){
                             svalue(ChkCL1[[ii]]) <<- FALSE
                         }
                         svalue(FitRprt) <- FALSE
                         svalue(QuantRprt) <- FALSE
                         ResetVars()
                     }, container=RGroup3)

   RGroup4 <- ggroup(horizontal=TRUE, spacing=1, container=RFrame2)
   glabel("Other: ", container=RGroup4)
   FitComplyt2 <- glayout(spacing=5, horizontal=TRUE, container=RGroup4)
   LL <- length(NonFittedCL)
   if (LL > 0 && LL <= 7){
       for(ii in 1:LL){
           FitComplyt2[1,ii] <- ChkCL2[[ii]] <- gcheckbox(NonFittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt2)
       }
   } else if (LL > 7){
       for(ii in 1:7){
           FitComplyt2[1,ii] <- ChkCL2[[ii]] <- gcheckbox(NonFittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt2)
       }
       for(ii in 8:LL){
           FitComplyt2[2,(ii-7)] <- ChkCL2[[ii]] <- gcheckbox(NonFittedCL[ii], checked=FALSE, spacing=1, container=FitComplyt2)
       }
   }
   RGroup5 <- ggroup(horizontal=TRUE, spacing=1, container=RFrame2)
   StdrdRprt <- gcheckbox("Standard report", checked=FALSE, spacing=5, container=RGroup5)
   font(StdrdRprt) <- list(family = "sans", size=8, weight="bold")
   gbutton(" MAKE REPORT ", spacing=1, handler=function(h, ...){
                         ReportSelection()
                     }, container=RGroup5)
   gbutton(" RESET ", spacing=1, handler=function(h, ...){
                         LL <- length(ChkCL2)
                         for(ii in 1:LL){
                             svalue(ChkCL2[[ii]]) <<- FALSE
                         }
                         svalue(StdrdRprt) <- FALSE
                         ResetVars()
                     }, container=RGroup5)

   glabel("                                                                                         ",container=RGroup5) 
   
   gbutton(" SAVE TO FILE ", spacing=1, handler=function(h, ...){
                         TabTxt <- as.data.frame(TabTxt)
                         filename <- gfile(type="save", initial.filename=NULL, initial.dir=getwd() ) #select the filename
                         filename <- unlist(strsplit(filename, "\\."))
                         if( is.na(filename[2])) {        #if extension not given, .txt by default
                            filename[2] <- ".txt"
                         } else {
                            filename[2] <- paste(".", filename[2], sep="")
                         }
                         filename <- paste(filename[1], filename[2], sep="")
                         write.table(x=TabTxt, file = filename, sep=" ", eol="\n",
                                     dec=".", row.names=FALSE, col.names=FALSE)
                         XPSSaveRetrieveBkp("save")
                     }, container=RGroup5)

   gbutton(" EXIT ", spacing=1, handler=function(h, ...){
                        dispose(txtWin)
                        XPSSaveRetrieveBkp("save")
                     }, container=RGroup5)
                     
   tabWin <- gtext(" ", font.attr=FontAttr, container = RGroup1)
   font(tabWin) <- list(weight="light", family=Font, style=FStyle, size=FSize)
   size(tabWin) <- c(765, 480)
   visible(txtWin) <- TRUE

}

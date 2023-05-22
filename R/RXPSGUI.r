# xps() opens the main GUI of the RxpsG package to call the various
# functions to analyze XPS spectra
#'
#' @title xps is the main interface of the Package RxpsG
#' @description xps() activates the RxpsG front-end to use
#'   all the functions to load raw data perform the spectral
#'   analysis and plot raw and analyzed spectra
#' @examples
#'  \dontrun{
#'  xps()
#' }
#' @export
#'



xps <- function(){

options(warn = -1) #switch off warnings
options(guiToolkit = "tcltk")
#====Check correct Package installation========================================
Pkgs <- installed.packages(priority="NA")  #matrix of installed packages: names and additional  information
Pkgs <- unname(Pkgs[, 1])                  #retain only the pakage names
assign("Pkgs", Pkgs, envir=.GlobalEnv)     #save the list of installed Packages
if(is.na(match("RxpsG", Pkgs)) == TRUE ){  #check if the package 'RxpsG' is installed
    cat("\n ERROR Package 'RxpsG' not installed!")
    cat("\n Please control correct installation of required packages:")
    cat("\n digest
         \n gWidgets2
         \n gWidgets2tcltk
         \n latticeExtra
         \n minpack.lm
         \n signal ")
    return()
}


#=== IMPORTS: THIS PART MUST BE REMOVED WHEN COMPILING==========================

#Now imports base functions from required packages

options(guiToolkit="tcltk")
import::here(graphics, grconvertX, grconvertY, arrows, axTicks, axis, box, grid,
             layout, legend, lines, locator, matlines, matplot, mtext, par, plot.new,
             points, rect, segments, text)
import::here(grDevices, getGraphicsEvent, getGraphicsEventEnv, setGraphicsEventHandlers,
             X11, bmp, dev.cur, dev.print, graphics.off, jpeg, pdf, png, postscript,
             recordPlot, replayPlot, tiff)
import::here(methods, as, formalArgs, new, show, slot, "slot<-")
import::here(minpack.lm, nlsLM, nls.lm, nls.lm.control)

import::here(stats, as.formula, coef, convolve, fft, fitted, formula, getInitial, lm,
             model.weights, na.omit, nls.control, numericDeriv, predict, residuals,
             setNames, spline, window)
import::here(utils, capture.output, install.packages, read.table, write.csv, write.csv2, write.table)

#=== SUGGESTED==================================================================
#Now check installation of optional packages and import optional functions

### Following imports are placed in the macros where call to external functions are used

#if( is.na(match("baseline", Pkgs)) == FALSE ){  #check if the package 'baseline' is installed
# baseline is not loaded in the NAMESPACE cannot use isNamespaceLoaded
# cannot use import:: since it requires the library to be listed in the DESCRIPTION imports
# see also: https://import.rticulate.org                                           
#   baseline <- baseline::baseline
#   baseline.peakDetection <- baseline::baseline.peakDetection
#} 
#if( is.na(match("FME", Pkgs)) == FALSE ){  #check if the package 'FME' is installed   
#   modFit <- FME::modFit              #cannot use import::here because FME not listed in DESCRIPTION
#}

#if( is.na(match("rootSolve", Pkgs)) == FALSE ){ #check if the package 'rootSolve' is installed 
#   gradient <- rootSolve::gradient    #cannot use import::here because rootSolve not listed in DESCRIPTION
#}
#if( is.na(match("wavelets", Pkgs)) == FALSE ){   #check if the package 'wavelets' is installed 
#   mra <- wavelets::mra               #cannot use import::here because rootSolve not listed in DESCRIPTION
#   dwt <- wavelets::dwt               
#   wt.filter <- wavelets::wt.filter
#}
#===============================================================================



#===== Default variable settings ======

   tableXPS <- NULL

#===== GTable: active window INIT/UPDATE =====

setFileWin <- function(refresh) {
   if (refresh=="INIT") {         #GTable is created for the first time
      FNameList <- "              " #A temporary FName list is created to open a suitable GTableWin
      layoutXPS[1,1] <<- tableXPS <<- gtable(FNameList, container = layoutXPS)
      names(tableXPS) <<- "XPS Samples"
      size(tableXPS) <- c(200,260)
   }
   if (refresh=="UPDATE") {
      FNameList <- XPSFNameList()    #Update the FName list
      layoutXPS[1,1] <<- tableXPS <<- gtable(FNameList, container = layoutXPS)
      names(tableXPS) <<- "XPS Samples"
      size(tableXPS) <- c(200,260)
   }

   addHandlerDoubleclick(tableXPS,handler=function(h,...){   #The selected XPSSample name is read only with doubleclick
           if(FNameList[1]!="              "){
              FName <- svalue(tableXPS)
              activeFName <- FName
              SpectList <- unlist(XPSSpectList(FName))       #here the list of Core Lines is generated from the FName datafile
              FName <- get(FName,envir=.GlobalEnv)
              activeSpectName <- FName[[1]]@Symbol
              assign("activeFName", activeFName, envir=.GlobalEnv)
              assign("activeSpectIndx", 1, envir=.GlobalEnv) #select first spectrum as activeSpectIndx in GlobalEnv.
              assign("activeSpectName", activeSpectName, envir=.GlobalEnv)  #select name of the first spectrum as come activeSpectIndx in GlobalEnv.
#              Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
#              Gdev <- strsplit(Gdev, "title")
#              Gdev <- paste(Gdev[[1]][1], " title='",activeFName,"')", sep="")     #add the correct window title
#              graphics.off() #switch off the graphic window
#              eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
              plot(FName)  #plot selected XPSSample with all the corelines

              menulist <- list()
              LL <- length(SpectList) #Right mouse button activated the list of XPSSample Corelines
              for (ii in 1:LL){     #each of the gactions has the name of the correspondent Core Line
                  menulist[ii] <- list(gaction(label=SpectList[ii], action=ii, handler=function(h,...) {
                                XPSComponent <- SpectList[as.integer(h$action)]
                                XPSComponent <- unlist(strsplit(XPSComponent, "\\."))   #remove "NUMBER." at beginning of coreline name
                                indx <- as.integer(XPSComponent[1])
                                XPSComponent <- XPSComponent[2]
                                FName <- get(activeFName, envir=.GlobalEnv)
                    	           par(mfrow=c(1,1))   #reset plot to single panel (1 row 1 column)
                                plot(FName[[indx]]) #plot single spectrum
                                assign("activeFName", activeFName, envir=.GlobalEnv)  #save the active XPSSample i the .Global Env
                                assign("activeSpectIndx", indx, envir=.GlobalEnv)     #save the  index relative to the active Core Line in the .Global Env
                                assign("activeSpectName", FName[[indx]]@Symbol, envir=.GlobalEnv)  #save the name of the active Core Line in the .Global Env
                                XPSFitInfo()
                              })
                           )
              }
              popup <- addRightclickPopupMenu(tableXPS, menulist, ID=TRUE)
          }
      })
}

CtrlSurname <- function(FName){
      SpectList <- names(FName)
      LL <- length(indx <- grep("survey",SpectList))
      if (LL == 0){
          indx <- grep("Survey",SpectList)
      } else {
          for(ii in 1:LL){
              FName@names[indx[ii]] <- "Survey"
              FName[[indx[ii]]]@Symbol <- "Survey"
          }
      }
      if (length(indx)==0){  #names(FName) does not contain "survey" nor "Survey" but likely "Wide"
          LL <- length(indx <- grep("wide",SpectList))
          if (LL == 0){
             LL <- length(indx <- grep("Wide",SpectList))
          }
          if ( LL > 0){  #found name "wide" or "Wide"
              for(ii in 1:LL){
                  FName@names[indx[ii]] <- "Survey"
                  FName[[indx[ii]]]@Symbol <- "Survey"
              }
          }
      }
      return(FName)
}

#===== Menu FILE: actions definition =====

   FileActions <- list(

      gaction("= Load VMS, PXT Data",handler=function(h,...){
            PathFile <- gfile(text = "Select files ...", type = c("open"),
				                          filter = list("VMS, PXT files" = list(patterns=c( ".vms", ".pxt"))),
					                         multi = FALSE)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            activeFName <- FName
            FName <- XPSread(file=PathFile,Genplot=FALSE)
            assign(activeFName, FName, envir=.GlobalEnv)             #save FName class XPSSample  
            assign("activeFName", activeFName, envir=.GlobalEnv)     #selected FName is set the activeFName class character
            FName <- CtrlSurname(FName)

#---Controls on XPSSample
            FName <- XPSpkgCtrl(FName) #controls the "package" attributes "package" if it is  the old Rxps version
            FName@Sample <- PathFile   #save the file location
#------------
            XPSComponent <- unlist(strsplit(names(FName)[1], "\\."))   #Skip the "number." at beginning CoreLine name
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the CoreLine index as active index in .Global env
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  #save the CoreLine name as active name in the .Global env
            assign(activeFName, FName, envir=.GlobalEnv)  #Save the XPSSample name as active in the .Global env

            setFileWin("UPDATE")
            cat("\n")
            print(summary(FName))
#            Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
#            Gdev <- strsplit(Gdev, "title")
#            Gdev <- paste(Gdev[[1]][1], " title='",activeFName,"')", sep="")     #add the correct window title
#            graphics.off() #switch off the graphic window
#            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("   Load Old Scienta files",handler=function(h,...){
            gmessage(msg="Please select one of the files in the .../ANALYSIS/ folder", title="LOAD OLD SCIENTA", icon="warning")
            PathFile <- gfile(text = "Select files ...", type = c("open"),
				                  filter = list(),
					               multi = FALSE)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            assign("activeFName", FName, envir=.GlobalEnv)   #selected FName is set the activeFName class character
            FName <- XPSread(file=PathFile,Genplot=FALSE)
            FName@Sample <- DirName                          #agrees the real path with that saved in the file which could be copied from another place
            assign(activeFName, FName, envir=.GlobalEnv)     #save FName class XPSSample 
#---Controls on XPSSample
            FName <- CtrlSurname(FName)
            FName <- XPSpkgCtrl(FName) #control that the "package" attributes of FName : it should'nt be Rxps  (Canteri)
#------------
            FName@Sample <- PathFile   #save the file location
            XPSComponent <- unlist(strsplit(names(FName)[1], "\\."))   #skip the "NUMBER." at beginninc of Core Line Name
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the index of the first Core Line of XPSSample as active index
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  # save the name of the first Core Line of the loaded XPSSample as active Spectrum Name
            assign(activeFName, FName, envir=.GlobalEnv)    #save the loaded XPSSample in the GlobalEnv.
            setFileWin("UPDATE")
            cat("\n")
            print(summary(FName))
#            Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
#            Gdev <- strsplit(Gdev, "title")
#            Gdev <- paste(Gdev[[1]][1], " title='",activeFName,"')", sep="")     #add the correct window title
#            graphics.off() #switch off the graphic window
#            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("   Load PXT+RPL Data",handler=function(h,...){
            PathFile <- gfile(text = "Select files ...", type = c("open"),
       	                      filter = list("PXT files" = list(patterns= ".pxt")),
                              multi = FALSE)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            command <- (paste(FName,"<-XPSread(file='",PathFile,"',Genplot=TRUE)", sep=""))
            eval(parse(text=command),envir=.GlobalEnv)           #here FName is a string
            activeFName <- FName
            assign("activeFName", activeFName, envir=.GlobalEnv) #set the active FName to the last read file
            FName <- get(activeFName,envir=.GlobalEnv)
#---Controls on XPSSample
            FName <- CtrlSurname(FName)
            FName <- XPSpkgCtrl(FName) #control on the package attributes
#------------
            FName@Sample <- PathFile   #save the file location
            XPSComponent <- unlist(strsplit(names(FName)[1], "\\."))
            assign("activeSpectIndx", 1, envir=.GlobalEnv)
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)
            assign(activeFName, FName, envir=.GlobalEnv)

            setFileWin("UPDATE")
            cat("\n")
            print(summary(FName))
#            Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
#            Gdev <- strsplit(Gdev, "title")
#            Gdev <- paste(Gdev[[1]][1], " title='",activeFName,"')", sep="")     #add the correct window title
#            graphics.off() #switch off the graphic window
#            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("= Load Analyzed Data",handler=function(h,...){
            PathFile <- gfile(text = "Select files ...", type = c("open"),
				                 filter = list("RData files" = list(patterns= c("*.RData", "*.RDF"))),
			               	  multi = FALSE)
				        if (length(PathFile)==0) {return()}       #when load-file-action aborted
            FName <- basename(PathFile)
            activeFName <- FName
            DirName <- dirname(PathFile)
            setwd(DirName)
            cat("\n New Working Directory: ", DirName)
            CheckName <- unlist(strsplit(FName, "\\." ))
            FNameList <- NULL
            if (CheckName[2]=="RData"){               #Load .Rdata files
               FNameList <- load(PathFile,envir=.GlobalEnv) #It could be that the dataFile is a group of XPSSample saved together. Then I need a FNameList
               LL <- length(FNameList)
               for(ii in 1:LL){                       #Update the FileName of the XPSSample changing the extension to .RData
                   FName <- get(FNameList[ii],envir=.GlobalEnv)  #load XPSSample data in FName
                   FName <- XPSpkgCtrl(FName)         #controls the attribute "package" of FName and set it to ".GlobalEnv"
                   FName@Sample <- paste(DirName, "/", FNameList[ii], sep="") #forces the @Sample to be equal to ActualDir+activeFName
                   if (activeFName != FNameList[1]){  #It happen that the XPSSample==FNameList[1] still contains the original name XXX.vms or XXX.pxt instead of XXX.RData
                      badFName <- FNameList[ii]
                      CheckName[1] <- unlist(strsplit(FNameList[ii], "\\." ))
                      FNameList[ii] <- paste(CheckName[1], ".RData", sep="") #build the correct FileName with extension .RData
                      FName@Filename <- FNameList[ii]        #Save the new FileName in the XPSSample
                      CheckName[1] <- paste(DirName, "/", FNameList[ii], sep="")
                      FName@Sample <- CheckName[1]           #Save the new FileName in the XPSSample
                      remove(list=badFName,pos=1,envir=.GlobalEnv)   #remove old FName from .GlobalEnv
                   }
                   assign(FNameList[ii], FName, envir=.GlobalEnv) #store CONTROLLED XPSSample in the GlobalEnv
               }
               FName <- get(FNameList[1],envir=.GlobalEnv)#the first element of the FNameList must be the active XPSSample
               activeFName <- FNameList[1]
               assign(activeFName, FName, envir=.GlobalEnv)         #save FName data  (class XPSSample)
               assign("activeFName", FNameList[1], envir=.GlobalEnv) #The first XPSSample_name loaded becomes the activeFName
            }

            if (CheckName[2]=="RDF"){                       #Load .RDF files
               FName <- readRDS(PathFile)
               FName <- XPSpkgCtrl(FName)           #controls the attribute "package" of FName and set it to ".GlobalEnv"
               CheckName <- paste(CheckName[1], ".RData", sep="") #build the correct FileName with extension .RData
               FName@Filename <- CheckName              #Save the new FileName in the XPSSample
               activeFName <- CheckName
               assign("activeFName", CheckName, envir=.GlobalEnv)     #selected FName is set the activeFName class character
               CheckName <- paste(DirName, "/", CheckName, sep="")
               FName@Sample <- CheckName                #Save the new FileName in the XPSSample
               assign(activeFName, FName, envir=.GlobalEnv)         #save FName data  (class XPSSample)
            }
#---Controls on XPSSample
            FName <- CtrlSurname(FName)
            FName <- XPSpkgCtrl(FName) #controls the "package" attributes "package" if it is  the old Rxps version
#---Assignments
            XPSComponent <- unlist(strsplit(names(FName)[1], "\\."))    #drop the "NUMBER." at beginninf of the coreline name
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the index corresponding to the active CoreLine in the .GlobalEnv.
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  #salvo l'INDICE del file caricato come ATTIVO nel GlobalEnv.
            assign(activeFName, FName, envir=.GlobalEnv)  #save the loaded XPSSample in the .GlobalEnv.
            setFileWin("UPDATE")
            cat("\n")
            print(summary(FName))
#            Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
#            Gdev <- strsplit(Gdev, "title")
#            Gdev <- paste(Gdev[[1]][1], " title='",activeFName,"')", sep="")     #add the correct window title
#            graphics.off() #switch off the graphic window
#            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(FName)
      }, container=groupMenu),

      gaction("= Save Analyzed Data", handler=function(h,...){
            XPSSaveData() #errmsg==1 XPSSaveData()executed regularly
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Import Ascii", handler=function(h,...){
            FName <- XPSImport.Ascii()
            if (is.na(FName)) {
               cat("\n import Ascii file Aborted")
            } else {
               setFileWin("UPDATE")
               plot(FName)
            }
      }, container=groupMenu),

      gaction("   Export Ascii", handler=function(h,...){
            FName<-get(activeFName, envir=.GlobalEnv)
            XPSExportAscii()
      }, container=groupMenu),

      gaction("   Split PXT Data File", handler=function(h,...){
            XPSSplit()
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Change Spectrum Name",handler=function(h,...){
            XPSSpectNameChange()
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Remove Current XPS-Sample", handler=function(h,...){
            answ <- gconfirm(msg="Removing the XPS Samples: all data/analyses will be lost. Proceed anyway?", title="Confirm Remove XPSSample", icon="warning")
            if (answ==TRUE){
               FName=get("activeFName",envir=.GlobalEnv)
               remove(list=FName,pos=1,envir=.GlobalEnv)
               FNameList <- XPSFNameList(warn=FALSE)
               LL <- length(FNameList)
               activeFName <- FNameList[1]
               activeSpectIndx <- 1
               assign("activeFName", activeFName, envir=.GlobalEnv)
               assign("activeSpectIndx", activeSpectIndx, envir=.GlobalEnv)

               setFileWin("UPDATE")
               if (LL > 0){
                  FName=get(activeFName,envir=.GlobalEnv)
                  plot(FName)
               } else {
#                  Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
#                  Gdev <- strsplit(Gdev, "title")
#                  Gdev <- paste(Gdev[[1]][1], " title='",activeFName,"')", sep="")     #add the correct window title
#                  graphics.off()                                   #reset graphic window
#                  eval(parse(text=Gdev),envir=.GlobalEnv)          #open graphic window
               }
            }
      }, container=groupMenu),

      gaction("   Remove All XPS-Samples", handler=function(h,...){
            answ <- gconfirm(msg="Removing all the XPS Samples: all data/analyses will be lost. Proceed anyway?", title="Confirm Remove XPSSample", icon="warning")
            if (answ==TRUE){
               FNameList <- XPSFNameList(warn=TRUE)
               LL=length(FNameList)
               for(ii in 1:LL){
                  FName <- FNameList[ii]
                  remove(list=FName,pos=1,envir=.GlobalEnv)
               }
               FNameList <- XPSFNameList(warn=FALSE)
               assign("activeFName", "", envir=.GlobalEnv)
               assign("activeSpectIndx", "", envir=.GlobalEnv)

               setFileWin("UPDATE")
               Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
               Gdev <- strsplit(Gdev, "title")
               Gdev <- paste(Gdev[[1]][1], " title=' ')", sep="")     #add the correct window title
               graphics.off() #switch off the graphic window
               eval(parse(text=Gdev),envir=.GlobalEnv)          #open graphic window
            } else {
              return()
            }
      }, container=groupMenu),

      gaction("   Set Working DIR", handler=function(h,...){
            XPSSetWD()
      }, container=groupMenu),

      gaction("   Preferences", handler=function(h,...){
            XPSPreferences()
      }, container=groupMenu),

      gaction("   Retrieve BKP-data", handler=function(h,...){
            XPSSaveRetrieveBkp("retrieve")
            setFileWin("UPDATE")
            FNameList <- XPSFNameList() #read the list of XPSSample loaded in the .GlobalEnv
            XPSSample <- get(FNameList[1], envir=.GlobalEnv)
            plot(XPSSample)
      }, container=groupMenu),

      gaction("   Refresh XPS Sample List",handler=function(h,...){
            setFileWin("UPDATE")
      }, container=groupMenu),

      gaction("   Quit", handler=function(h,...){
            ReturnVal <- tkmessageBox(message = "Do you want to save data before quitting?",
                                    icon = "question", type = "yesnocancel", default = "yes")
            answ <- tclvalue(ReturnVal)
            if (answ == "yes"){
               XPSSaveData()
            }
            else if (answ == "no"){
                dispose(winXPS)
            }
            else if (answ == "cancel"){ }
      }, container=groupMenu))

#===== Menu ANALYSIS: actions definition =====

   AnalysisActions <- list(
      gaction("Spectrum selection",handler=function(h,...){
             XPSSetFNameCLine()
      }, container=groupMenu),

      gaction("= Analyze",handler=function(h,...){
             XPSAnalysis()
      }, container=groupMenu),

      gaction("= Fit Constraints",handler=function(h,...){
             XPSConstraints()
      }, container=groupMenu),

      gaction("   FIT Lev.Marq.",handler=function(h,...){
             FName <- get(activeFName,envir=.GlobalEnv)
             indx <- get("activeSpectIndx",envir=.GlobalEnv)
             FName[[indx]] <- XPSFitLM(FName[[indx]], , plt=TRUE)
             assign(activeFName, FName, envir=.GlobalEnv)
      }, container=groupMenu),

      gaction("   FIT ModFit",handler=function(h,...){
             if( is.na(match("FME", Pkgs)) == TRUE){   #check if the package 'FME' is installed 
                txt <- "Package 'FME' not installed. \nOption 'ModFit' not available"
                gmessage(msg=txt, title="WARNING", icon="error")
                return()
             }

             FName <- get(activeFName,envir=.GlobalEnv)
             indx <- get("activeSpectIndx",envir=.GlobalEnv)
             FName[[indx]] <- XPSModFit(FName[[indx]])
             assign(activeFName, FName, envir=.GlobalEnv)
      }, container=groupMenu),

      gaction("= Move Components",handler=function(h,...){
             XPSMoveComp()
      }, container=groupMenu),

      gaction("= Quantify",handler=function(h,...){
             XPSQuant()
      }, container=groupMenu),

      gaction("= Energy Shift",handler=function(h,...){
             XPSEshift()
      }, container=groupMenu),

      gaction("   Process Coreline",handler=function(h,...){
             XPSProcessCoreLine()
      }, container=groupMenu),

      gaction("   Extract from survey",handler=function(h,...){
             SpectList <- XPSSpectList(activeFName)
             SpectName <- "Survey"
             indx <- grep(SpectName, SpectList, value=FALSE)
             if (length(indx) == 0){
                SpectName <- "survey"
                indx <- grep(SpectName, SpectList, value=FALSE)
                if (length(indx) > 0){ assign("activeSpectIndx", indx, envir=.GlobalEnv) }
             }
             if (length(indx) == 0){
                answ <- gconfirm(msg = "Sorry, no survey in this XPSsample. Proceed anyway?", title="SPECTRUM ERROR", icon = "warning")
                if (answ == FALSE) return
                XPSExtract()
             } else if (indx > 0){
                assign("activeSpectIndx", indx[1], envir=.GlobalEnv)
                XPSExtract()
             }
      }, container=groupMenu),

      gaction("   Reset Analysis",handler=function(h,...){
             XPSResetAnalysis()
      }, container=groupMenu),

      gaction("   Adjust Baseline",handler=function(h,...){
             XPSMoveBaseLine()
      }, container=groupMenu),

      gaction("   Depth Profile",handler=function(h,...){
             XPSDepthProfile()
      }, container=groupMenu),

      gaction("   Smoothing",handler=function(h,...){
             XPSFilter()
      }, container=groupMenu),

      gaction("   Differentiation",handler=function(h,...){
             XPSDiff()
      }, container=groupMenu),
      
      gaction("   Convolution-Deconvolution",handler=function(h,...){
             XPSCnvDcnv()
      }, container=groupMenu),

      gaction("   Interpolation-Decimation",handler=function(h,...){
             XPSInterpDecim()
      }, container=groupMenu),

      gaction("   VBtop estimation",handler=function(h,...){
             XPSVBTop()
      }, container=groupMenu),

      gaction("   Fermi edge estimation",handler=function(h,...){
             XPSVBFermi()
      }, container=groupMenu),

      gaction("   Data Sprucing   ",handler=function(h,...){
             XPSSprucingGUI()
      }, container=groupMenu),

      gaction("   Element Identification",handler=function(h,...){
             if( is.na(match("baseline", Pkgs)) == TRUE){   #check if the package 'baseline' is installed
                txt <- "Package 'baseline' not installed. \nOption 'Element identification' not available"
                gmessage(msg=txt, title="WARNING", icon="error")
                return()
             }

             XPSSurveyElementIdentify()
      }, container=groupMenu),

      gaction("   Corelines Auger Transition Tables",handler=function(h,...){
             XPSElemTab()
      }, container=groupMenu),

      gaction("   VMS Data Transmission Correction",handler=function(h,...){
             XPSVmsCorr()
      }, container=groupMenu))


#===== Menu PLOT: actions definition =====

   PlotActions <- list(
      gaction("Plot",handler=function(h,...){
             #Load the active XPSSample
             FName <- get(activeFName,envir=.GlobalEnv)
             plot(FName)
      }, container=groupMenu),

      gaction("Spectrum Selection",handler=function(h,...){
             XPSSetFNameCLine()
      }, container=groupMenu),

      gaction("Overlay Spectra",handler=function(h,...){
             XPSOverlay()
      }, container=groupMenu),

      gaction("Compare XPS-Samples",handler=function(h,...){
             XPSCompare()
      }, container=groupMenu),

      gaction("Custom Plot",handler=function(h,...){
             XPSCustomPlot()
      }, container=groupMenu),

      gaction("Two-Yscale Plot",handler=function(h,...){
             XPSTwoScalePlot()
      }, container=groupMenu),

      gaction("Annotate",handler=function(h,...){
             XPSAnnotate()
      }, container=groupMenu),

      gaction("Zoom & Cursor",handler=function(h,...){
             XPSZoomCur()
      }, container=groupMenu),

      gaction("Switch BE/KE scale",handler=function(h,...){
             XPSSwitch.BE.KE()
      }, container=groupMenu),

      gaction("Graphic Device Options",handler=function(h,...){
             XPSSetGraphDev()
      }, container=groupMenu),

      gaction("Set Analysis Window Size",handler=function(h,...){
          XPSSetWinSize()
      }, container=groupMenu))


#===== MENU infoFile: ACTIONS definition =====

   ListInfoActions <- list(
      gaction("XPS Sample Info",handler=function(h,...){
          XPSSampleInfo()
      }, container=groupMenu),

      gaction("Core Line Fit Info",handler=function(h,...){
          XPSFitParamInfo()
      }, container=groupMenu),
   
      gaction("Analysis Report",handler=function(h,...){
          XPSAnalReport()
      }, container=groupMenu),

      gaction("Help", handler = function(h,...){
          pth <- system.file("doc/RxpsGmanual.pdf", package="RxpsG", lib.loc=.libPaths())
          if (file.exists(pth)) {
            OS <- unname(tolower(Sys.info()["sysname"]))
            switch(OS,
                   "windows" = {
                     #since spaces (such as c:\Program Files\...) are interpreted by shell command
                     WD <- getwd()
                     #it is necessary to set the folder containing manual.pdf to open it
                     setwd(system.file("doc/", package = "RxpsG", lib.loc=.libPaths()))
                     shell("RxpsGmanual.pdf", wait = FALSE)
                     setwd(WD)     #restore previous WD
                   },
                   "linux" = {
                     CMD <- paste("evince ", pth, sep=" ")
                     system(CMD)
                   },
                   "darwin" = {
                     CMD <- paste("open ", pth, sep=" ")
                     system(CMD)
                   })
          }
          else {
            txt <- paste( "Oops... Manual not found! \nPlease check the folders doc/ if manual.pdf is present", sep = "")
            gmessage(txt, title = "WARNING: MANUAL NOT FOUND", icon = "warning")
          }
      }, container=groupMenu))

   MenuBarList <- list(File=FileActions, Analysis=AnalysisActions, Plot=PlotActions, Info_Help=ListInfoActions)

#==== Loading RxpsG picture=
   IMGpath <- system.file("extdata/xps.gif", package="RxpsG", lib.loc=.libPaths())
   if ( IMGpath == "" ) {
      cat("\n ==> ATTENTION: File xps.gif not found!")
      cat("\n ==> Check correct installation of RxpsG package")
      cat("\n ==> Check if ...R/Library/RxpsG/extdata directory is present")
      return(invisible(1))
   }
   if ( file.exists(IMGpath)==FALSE ) {
      cat("\n ==> ATTENTION: File xps.gif not found!")
      cat("\n ==> Check correct installation of RxpsG package")
      return(invisible(1))
   }
   

#===== XPS main Panel ======

   winXPS <- gwindow("RxpsG MAIN PANEL", parent=c(5,5), visible=FALSE)
   gmenu(MenuBarList, container=winXPS)
   groupXPS <- ggroup(horizontal=TRUE, container=winXPS)

   layoutXPS <- glayout(homogeneous=FALSE, spacing=3, container=groupXPS)
   FNameList <- "              " #A temporary FName list is created to open a suitable GTableWin
   layoutXPS[1,1] <-tableXPS <-gtable(FNameList, container = layoutXPS)
   names(tableXPS) <-"XPS Samples"
   size(tableXPS) <- c(200,260)
   layoutXPS[1,2] <- imageXPS <- gimage(filename = IMGpath, dirname ="", size=c(100,70), toolkit=guiToolkit(), container = layoutXPS)

   FNameList <- XPSFNameList(warn=FALSE)
   if (length(FNameList) > 0) {
      setFileWin("UPDATE")
   }
   visible(winXPS) <- TRUE

#Reading XPS settings which can be customized by users
   XPSSettings <- data.frame(stringsAsFactors=FALSE)
   FontPref <- list(Font="", Style="", Size="")
   
   Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG", lib.loc=.libPaths())
   if (file.exists(Ini.pthName)) {
     XPSSettings <- read.table(file = Ini.pthName, header=TRUE, sep="", stringsAsFactors = FALSE)
   } else {
     gmessage(msg="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
     return()
   }

# setting the proper graphic device
#--- get System info and apply correspondent XPS Settings ---
   OS <- unname(tolower(Sys.info()["sysname"]))
   switch (OS,
           "linux" =   {Gdev <- "X11(type='cairo', xpos=700, ypos=20, title=' ')" },
           "windows" = {Gdev <- "x11(xpos=700, ypos=20, title=' ')"},
           "darwin"  = {Gdev <- "quartz(xpos=700, ypos=20, title=' ')" })
   XPSSettings$General[6] <- Gdev
   graphics.off()                            #reset graphic window
   eval(parse(text=Gdev),envir=.GlobalEnv)
   if (length(XPSSettings$General[7]) == 0 || length(dir(XPSSettings$General[7])) == 0){
      gmessage("Working Dir NOT defined: please select your default Working Directory", title="SET THE WORKING DIR!", icon="error")
      XPSSetWD()
   } else {
      setwd(XPSSettings$General[7])
   }
   assign("XPSSettings", XPSSettings, envir=.GlobalEnv)

# Recover the R version used by Rstudio and save in .GlobalEnv
   RVersion <- R.Version()$version.string  #get the $version.string from the output list of R.Version()
   assign("RVersion", RVersion, envir=.GlobalEnv)
}


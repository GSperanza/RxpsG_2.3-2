# XPSEshift function to apply energy shifts to spectra to correct for charging effects

#' @title XPSEShift
#' @description XPSEshift function correct the energy scale of spectra
#'   affected by charging effects. Generally C1s from hydrocarbons at BE=285eV
#'   or Au4f 7/2 at BE=84eV  are chosen as reference peaks. Charges spectra are 
#'   shifted forcing the reference spectra to fall at their correct BE position.
#' @examples
#'  \dontrun{
#' 	XPSEshift()
#' }
#' @export
#'

XPSEshift <- function(){

  GetCurPos <- function(SingClick){
       coords <<- NULL
       enabled(Eframe3) <- FALSE   #prevent exiting Analysis if locatore active
       enabled(ZoomBtn) <- FALSE
       enabled(Eframe5) <- FALSE
       enabled(BtnGroup) <- FALSE
       EXIT <- FALSE
       LocPos <<- list(x=0, y=0)
       Nclk <- SingClick
       if (Nclk == FALSE) Nclk <- 1
       while(EXIT == FALSE){  #if pos1 not NULL a mouse butto was pressed
            LocPos <<- locator(n=Nclk, type="p", pch=3, cex=1.5, col="blue", lwd=2) #to modify the zoom limits
            if (is.null(LocPos)) {
                enabled(Eframe3) <- TRUE
                enabled(ZoomBtn) <- TRUE
                enabled(Eframe5) <- TRUE
                enabled(BtnGroup) <- TRUE
                EXIT <- TRUE
            } else {
                if (SingClick == 1){
                     enabled(Eframe3) <- TRUE
                     enabled(ZoomBtn) <- TRUE
                     enabled(Eframe5) <- TRUE
                     enabled(BtnGroup) <- TRUE
                     EXIT <- TRUE
                } else if (ZOOM==TRUE) {
                     if (SingClick == 2) {
                         Corners <<- LocPos  #plot the zoom area the first time
                         if (FName[[SpectIndx]]@Flags[1]) { #Binding energy set
                             Corners$x <<- sort(Corners$x, decreasing=TRUE) #pos$x in decrescent ordered => Corners$x[1]==Corners$x[2]
                         } else {
                             Corners$x <<- sort(Corners$x, decreasing=FALSE) #pos$x in ascending order
                         }
                         Corners$x <<- c(Corners$x[1], Corners$x[1], Corners$x[2], Corners$x[2])
                         Corners$y <<- c(sort(c(Corners$y[1], Corners$y[2]), decreasing=FALSE),
                                         sort(c(Corners$y[1], Corners$y[2]), decreasing=FALSE))
                         SingClick <- FALSE
                         Nclk <- 1
                     } else {            #modify the zoom area
                         FindNearest()
                     }
                     XYrange$x <<- c(Corners$x[1], Corners$x[3])
                     XYrange$y <<- c(Corners$y[1], Corners$y[2])
                     plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
                     rect(Corners$x[1], Corners$y[1], Corners$x[3], Corners$y[2])
                     points(Corners, type="p", pch=3, cex=1.5, col="blue", lwd=2)
                } else {
                     plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
                     points(LocPos, type="p", pch=3, cex=1.5, lwd=2, col="red")
                     LocPos$x <<- round(LocPos$x, digits=2)
                     svalue(Eobj5) <- LocPos$x
                     position <<- LocPos$x
                     tcl("update", "idletasks")  #force writing cursor position in the glabel
                }
            }
       }
  }

  FindNearest <- function(){
      D <- NULL
      Dmin <- ((LocPos$x-Corners$x[1])^2 + (LocPos$y-Corners$y[1])^2)^0.5  #init value
      for (ii in 1:4) {
          D[ii] <- ((LocPos$x-Corners$x[ii])^2 + (LocPos$y-Corners$y[ii])^2)^0.5  #dist P0 P1
          if(D[ii] <= Dmin){
             Dmin <- D[ii]
             idx=ii
          }
      }
      if (idx==1){
          Corners$x[1] <<- Corners$x[2] <<- LocPos$x
          Corners$y[1] <<- Corners$y[3] <<- LocPos$y
      } else if (idx==2){
          Corners$x[1] <<- Corners$x[2] <<- LocPos$x
          Corners$y[2] <<- Corners$y[4] <<- LocPos$y
      } else if (idx==3){
          Corners$x[3] <<- Corners$x[4] <<- LocPos$x
          Corners$y[1] <<- Corners$y[3] <<- LocPos$y
      } else if (idx==4){
          Corners$x[3] <<- Corners$x[4] <<- LocPos$x
          Corners$y[2] <<- Corners$y[4] <<- LocPos$y
      }
      return()
}

#--- Variables ---
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName,envir=.GlobalEnv)  #this is the XPS Sample
   ActiveFName <- get("activeFName",envir=.GlobalEnv)  #this is the name of the XPS Sample
   SpectIndx <- get("activeSpectIndx",envir=.GlobalEnv)
   SpectList <- XPSSpectList(ActiveFName)
   FNameList <- XPSFNameList()
   FNidx <- grep(ActiveFName, FNameList)
   FitCompList <- ""
   position <- NULL
   Eshift <- NULL
   XYrange <- list(x=NULL, y=NULL)
   LocPos <- list(x=NULL, y=NULL)
   Corners <- list(x=NULL, y=NULL)
   ZOOM <- FALSE                          

   Ewin <- gwindow("ENERGY SHIFT", visible=FALSE)
   size(Ewin) <- c(200, 400)
   addHandlerDestroy(Ewin, handler=function(h, ...){  #if MainWindow unproperly closed with X
                     EXIT <<- TRUE
                     XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                     assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                     plot(FName[[SpectIndx]])   #replot the CoreLine
                 })

   Egroup1 <- ggroup(horizontal=FALSE, container = Ewin)

   Eframe1 <- gframe(" Select the XPSSample and Coreline ", horizontal=TRUE, spacing=5, container=Egroup1)
   Eobj0 <- gcombobox(FNameList, selected=FNidx, editable=FALSE, handler=function(h,...){
                                  ActiveFName <<- svalue(Eobj0)
                                  FName <<- get(ActiveFName,envir=.GlobalEnv)
                                  SpectList <<- XPSSpectList(ActiveFName)
                                  FNameList <<- XPSFNameList()
                                  FNidx <<- grep(ActiveFName, FNameList)
                                  plot(FName)
                                  svalue(Eobj2) <<- ""
                                  delete(Eframe1, Eobj1)
                                  Eobj1 <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h, ...){
                                                    SpectName <- svalue(Eobj1)
                                                    SpectName <- unlist(strsplit(SpectName, "\\."))   #skip the number at beginning of string
                                                    SpectIndx <<- as.numeric(SpectName[1])
                                                    plot(FName[[SpectIndx]])
                                                    tmp <- names(FName[[SpectIndx]]@Components)
                                                    FitCompList <- c(FitCompList,tmp)
                                                    delete(Eframe2,Eobj2)
                                                    Eobj2 <<- gcombobox(FitCompList, selected=-1, editable=FALSE, index=TRUE, handler=function(h, ...){
                                                                       Component <- svalue(Eobj2)
                                                           	           FName <- get(ActiveFName, envir=.GlobalEnv)  #reload FName if a new EShift will be set
                                                                       OldValue <- FName[[SpectIndx]]@Components[[Component]]@param["mu","start"] # actual value of the fit component position
                                                                       OldValue <- round(as.numeric(OldValue), digits=2)
                                                                       svalue(Eobj5) <- OldValue   #display the actual value of the component position in GEdit
                                                                       if (length(FName[[SpectIndx]]@RegionToFit$x) > 0){
                                                                           XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                                                                           XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                                                                       } else {
                                                                           XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                                                                           XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                                                                       }
                                                                       if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
                                                                           XYrange$x <<- rev(XYrange$x)
                                                                       }
                                                              }, container=Eframe2)
                                                    enabled(Eobj2) <- TRUE
                                                    }, container=Eframe1)
                                  }, container=Eframe1)


   Eobj1 <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h, ...){
                                  SpectName <- svalue(Eobj1)
                                  SpectName <- unlist(strsplit(SpectName, "\\."))
                                  SpectIndx <<- as.numeric(SpectName[1])
                                  plot(FName[[SpectIndx]])
                                  tmp <- names(FName[[SpectIndx]]@Components)
                                  FitCompList <- c(FitCompList,tmp)
                                  delete(Eframe2,Eobj2)
                                  Eobj2 <<- gcombobox(FitCompList, selected=-1, editable=FALSE, index=TRUE, handler=function(h, ...){
                                                     Component <- svalue(Eobj2)
                                       	             FName <- get(ActiveFName, envir=.GlobalEnv)  #reload FName if a new EShift will be set
                                                     OldValue <- FName[[SpectIndx]]@Components[[Component]]@param["mu","start"] # actual value of the fit component position
                                                     OldValue <- round(as.numeric(OldValue), digits=2)
                                                     svalue(Eobj5) <- OldValue   #display the actual value of the component position in GEdit
                                                     if (length(FName[[SpectIndx]]@RegionToFit$x) > 0){
                                                         XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                                                         XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                                                     } else {
                                                         XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                                                         XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                                                     }
                                                     if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
                                                         XYrange$x <<- rev(XYrange$x)
                                                     }
                                            }, container=Eframe2)
                                  enabled(Eobj2) <- TRUE
                                  }, container=Eframe1)

   Eframe2 <- gframe(" Select Component ", horizontal=TRUE, spacing=5, container=Egroup1)
   Eobj2 <- gcombobox(FitCompList, selected=-1, editable=FALSE, index=TRUE, container=Eframe2)
   tkconfigure(Eobj2$widget, width=20)
   enabled(Eobj2) <- FALSE

   Eframe3 <- gframe("Apply Shift" , spacing=5, container=Egroup1)
   ShiftObj <- gradio(c("All Corelines", "Selected Coreline Only"), selected=1, horizontal=TRUE, container=Eframe3)

   Eframe4 <- gframe("Set Zoom Region" , horizontal=FALSE, spacing=5, container=Egroup1)
   glabel("Define the Zoom Area", container=Eframe4)
   ZoomBtn <- gbutton("Set the Zoom Area",  handler=function(h, ...){
                                  SpectName <- svalue(Eobj1)
                                  if(length(SpectName)==0) {
                                     gmessage(msg="WARNING: no coreline selected, zoom stopped", title = "WARNING",icon = "warning" )
                                     return()
                                  }
                                  txt <- " LEFT Mouse Button to Set the TWO Opposite Corners of the Zoom Area\n RIGHT Mouse Button to exit \n Click Near Markers to Modify The zoom area"
                                  gmessage(msg=txt , title = "WARNING",  icon = "warning")
                                  ZOOM <<- TRUE
                                  GetCurPos(SingClick=2)     #this to draw/modify the zooming area
                                  ZOOM <<- FALSE
                                  plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
                    }, container = Eframe4)


   Eframe5 <- gframe("Peak Position", horizontal=FALSE, spacing=5, container=Egroup1)
   CursorBtn <- gbutton("Cursor",  handler=function(h, ...){
                                  SpectName <- svalue(Eobj1)
                                  if(length(SpectName)==0) {
                                     gmessage(msg="WARNING: no coreline selected", title = "WARNING",icon = "warning" )
                                     return()
                                  }
                                  gmessage(msg="LEFT Mouse Button to Read Marker's Position; RIGHT Mouse Button to Exit" , title = "WARNING",  icon = "warning")
                                  GetCurPos(SingClick=FALSE)     #this to draw/modify the zooming area
                                  plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
                    }, container=Eframe5)

   glabel("Please Enter the New Energy Value:", spacing=1, container=Eframe5)
   Eobj5 <- gedit(initial.msg ="E?", handler=function(h,...){
                        NewE <- svalue(Eobj5)
                        Component <- svalue(Eobj2)
                        if (is.null(position) && length(Component)==0){
                            gmessage(msg="Cursor-Peak-Position or Fit Component lacking! Please select", title="LACKING REFERENCE POSITION", icon="warning")
                            svalue(Eobj5) <- "E?"
                            return()
                        }
                        if (NewE == "" ){
                           gmessage("New energy position NULL. Please give a correct value", title="ENERGY POSITION NULL", icon="warning")
                           svalue(Eobj5) <- "E?"
                           return()
                        }
                        if (NewE != "E?"){
                           NewE <- as.numeric(NewE)
                           Component <- svalue(Eobj2)
  	                        if (length(Component)==0){ #no fit present, no components, position read from cursor
    	                         Eshift <<- as.numeric(NewE-position)
  	                        } else {
                              Component <- as.numeric(substr(Component, 2,nchar(Component))) #exttract the component name
      	                       CompPos <- FName[[SpectIndx]]@Components[[Component]]@param["mu","start"]
    	                         Eshift <<- as.numeric(NewE-CompPos)
                           }
                           Escale <- FName[[SpectIndx]]@units[1]
                           if (svalue(ShiftObj)=="All Corelines") {
                              NCoreLines <- length(FName)
                              for (ii in 1:NCoreLines){
                                   if (Escale == FName[[ii]]@units[1]) { #Eshift calculated on a BE scale and CoreLine[[ii]] sa same Energy Units
    		                              FName[[ii]] <<- XPSapplyshift(FName[[ii]], Eshift)
                                   } else {
    		                              FName[[ii]] <<- XPSapplyshift(FName[[ii]], -Eshift)  # Eshift calculated on a BE scale while CoreLine[[ii]] is in Kinetic (or viceversa)
                                   }
                              }
                              plot(FName[[SpectIndx]])
                           } else if (svalue(ShiftObj)=="Selected Coreline Only"){ #Apply Eshift only on the selected coreline
                              if (Escale == FName[[SpectIndx]]@units[1]) {         #Eshift calculated on a BE scale and CoreLine[[ii]] sa same Energy Units
    		                         FName[[SpectIndx]] <<- XPSapplyshift(FName[[SpectIndx]], Eshift)
                               } else {
    		                         FName[[SpectIndx]] <<- XPSapplyshift(FName[[SpectIndx]],-Eshift)  # Eshift calculated on a BE scale while CoreLine[[ii]] is in Kinetic (or viceversa)
                               }
                               plot(FName[[SpectIndx]])
 		                        }
                        }
                        svalue(Eobj5) <- "E?"
                    }, container=Eframe5)

    BtnGroup <- ggroup(horizontal=TRUE, container = Egroup1)
    gbutton("       RESET       ", container = BtnGroup, handler = function(h, ...){
		                      FName <- XPSapplyshift(FName)
		                      svalue(Eobj5) <- "E?"
                        if (length(FName[[SpectIndx]]@RegionToFit$x) > 0){
                            XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                            XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                        } else {
                            XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                            XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                        }
                        plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)
    	                   assign(ActiveFName, FName, envir=.GlobalEnv)
                    })


   gbutton("        SAVE        ", container=BtnGroup, handler=function(...){
    	                   assign(ActiveFName, FName, envir=.GlobalEnv)
    	                   XPSSaveRetrieveBkp("save")
                    })


   gbutton("    SAVE & EXIT     ", container=BtnGroup, handler=function(...){
    	                   dispose(Ewin)
    	                   assign(ActiveFName, FName, envir=.GlobalEnv)
    	                   XPSSaveRetrieveBkp("save")
                    })
   visible(Ewin) <- TRUE
}

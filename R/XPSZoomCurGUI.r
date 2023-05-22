## zoom function


#' @title XPSZoomCur
#' @description XPSZoomCur for zooming a portion of a plotted spectrum.
#' @examples
#' \dontrun{
#' 	XPSZoomCur()
#' }
#' @export
#'



XPSZoomCur <- function(){

  GetCurPos <- function(SingClick){
       coords <<- NULL
       enabled(ZMframe1) <- FALSE   #prevent exiting Analysis if locatore active
       enabled(OKbutt) <- FALSE
       enabled(CPosButt) <- FALSE
       enabled(ExitButt) <- FALSE
       EXIT <- FALSE
       LocPos <<- list(x=0, y=0)
       Nclk <- SingClick
       if (Nclk == FALSE) Nclk <- 1
       while(EXIT == FALSE){  #if pos1 not NULL a mouse butto was pressed
            LocPos <<- locator(n=Nclk, type="p", pch=3, cex=1.5, col="blue", lwd=2) #to modify the zoom limits
            if (is.null(LocPos)) {
                enabled(ZMframe1) <- TRUE
                enabled(OKbutt) <- TRUE
                enabled(CPosButt) <- TRUE
                enabled(ExitButt) <- TRUE
                EXIT <- TRUE
            } else {
                if (SingClick == 1){
                     enabled(ZMframe1) <- TRUE
                     enabled(OKbutt) <- TRUE
                     enabled(CPosButt) <- TRUE
                     enabled(ExitButt) <- TRUE
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
                     ReDraw()  #refresh graph
                     rect(Corners$x[1], Corners$y[1], Corners$x[3], Corners$y[2])
                     points(Corners, type="p", pch=3, cex=1.5, col="blue", lwd=2)
                } else {
                     ReDraw() #refresh graph  to cancel previous cursor markers
                     points(LocPos, type="p", pch=3, cex=1.5, lwd=2, col="red")
                     LocPos <<- round(x=c(LocPos$x, LocPos$y), digits=2)
                     txt <- paste("X: ", as.character(LocPos[1]), ",   Y: ", as.character(LocPos[2]), sep="")
                     svalue(ZMobj4) <- txt
                     tcl("update", "idletasks")  #force writing cursor position in the glabel
                }
            }
       }
  }


ReDraw <- function(){   #redraw all spectrum with no restrictions to RegionToFit
   SampData <- as.matrix(FName[[SpectIndx]]@.Data) #Whole coreline in SampData
   plot(x=SampData[[1]], y=SampData[[2]], xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="black")
   SampData <- setAsMatrix(FName[[SpectIndx]], "matrix") # Regiontofit, Baseline, ecc in a matrix
   NC <- ncol(SampData)
   if (NC > 2) { #a Baseline is present
       BaseLine <- SampData[,3]
       matlines(x=SampData[,1], y=BaseLine, xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="sienna")
   }
   if (NC > 3){ #c'e' un fit
       FitComp <- SampData[,4:NC-1]  #first three column skipped
       SpectFit <- SampData[,NC]  #fit
       matlines(x=SampData[,1], y=FitComp, xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="blue")
       matlines(x=SampData[,1], y=SpectFit, xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="red")
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



#----- Variabili -----
   coord <- list()
   LocPos <- list()
   Corners <- list(x=NULL, y=NULL)
   ZOOM <- FALSE
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList() #list of XPSSamples
   FName <- get(activeFName,envir=.GlobalEnv)
   SpectList <- XPSSpectList(activeFName)
   SpectIndx <- get("activeSpectIndx",envir=.GlobalEnv)
   activeSpectName <- get("activeSpectName",envir=.GlobalEnv)
   XYrange <- list(x=range(FName[[SpectIndx]]@.Data[1]), y=range(FName[[SpectIndx]]@.Data[2]))
   if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
      XYrange$x <- rev(XYrange$x)
   }
   ResetXYrange <- XYrange

#===== Graphic Library Cntrl ===
   MatPlotMode <- get("MatPlotMode", envir=.GlobalEnv)
   if (MatPlotMode==FALSE){
      gmessage(msg="Overlay or CustomPlot active: DoubleClick on your XPSsample", title = "BAD GRAPHIC MODE",  icon = "error")
      return()
   }


#--- Redraw is used because plot is limited to the RegionToFit

   ReDraw()

#--- Zoom Cursor window ---

   ZMwin <- gwindow("ZOOM/CURSOR OPTION", parent=c(50,10) ,visible=FALSE)
   size(ZMwin) <- c(250, 270)
   addHandlerDestroy(ZMwin, handler=function(h, ...){  #if MainWindow unproperly closed with X
                     EXIT <<- TRUE
                     XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                     assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                     plot(FName[[SpectIndx]])   #replot the CoreLine
                 })

   ZMgroup <- ggroup(label="", horizontal=FALSE, container=ZMwin)

#   INFOframe <- gframe(text="INFO", horizontal=FALSE, spacing=1, container=ZMgroup)

   ZMframe0 <- gframe(text="XPS Sample and Core line Selection", horizontal=FALSE, spacing=5, container=ZMgroup)
   Fidx <- grep(activeFName, FNameList)
   XPS.Sample <- gcombobox(FNameList, selected=Fidx, editable=FALSE, handler=function(h,...){
                     activeFName <- svalue(XPS.Sample)
                     FName <<- get(activeFName, envir=.GlobalEnv)
                     SpectList <<- XPSSpectList(activeFName)
                     delete(ZMframe0, Core.Lines)
                     Core.Lines <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                                              SpectName <- svalue(Core.Lines)
                                              SpectName <- unlist(strsplit(SpectName, "\\."))   #drop the N. at beginning core-line name
                                              SpectIndx <<- as.integer(SpectName[1])
                                              XYrange <<- list(x=range(FName[[SpectIndx]]@.Data[1]), y=range(FName[[SpectIndx]]@.Data[2]))
                                              if (length(FName[[SpectIndx]]@RegionToFit) > 0) {   #reverse if BE scale
                                                  XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                                                  XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                                              } else {
                                                  XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                                                  XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                                              }
                                              if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
                                                  XYrange$x <<- rev(XYrange$x)
                                              }
                                              ResetXYrange <<- XYrange
                                              ReDraw()
                                    }, container=ZMframe0)
                 }, container = ZMframe0)
#   svalue(XPS.Sample) <- activeFName

   Core.Lines <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                     SpectName <- svalue(Core.Lines)
                     SpectName <- unlist(strsplit(SpectName, "\\."))   #drop the N. at beginning core-line name
                     SpectIndx <<- as.integer(SpectName[1])
                     XYrange <<- list(x=range(FName[[SpectIndx]]@.Data[1]), y=range(FName[[SpectIndx]]@.Data[2]))
                     if (length(FName[[SpectIndx]]@RegionToFit)>0) {   #reverse if BE scale
                        XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                        XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                     } else {
                        XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                        XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                     }
                     if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
                        XYrange$x <<- rev(XYrange$x)
                     }
                     ResetXYrange <<- XYrange
                     ReDraw()
                 }, container=ZMframe0)
   svalue(Core.Lines) <- paste(SpectIndx, ".",activeSpectName, sep="")

   ZMframe1 <- gframe(text="Define the Zoom Area", horizontal=FALSE, spacing=5, container=ZMgroup)

   ZMobj1 <- gbutton(" Set the Zoom Area ", handler=function(h,...){
                     txt <- " LEFT Mouse Button to Set the TWO Opposite Corners of the Zoom Area\n RIGHT Mouse Button to exit \n Click Near Markers to Modify The zoom area"
                     gmessage(msg=txt , title = "WARNING",  icon = "warning")
                     ZOOM <<- TRUE
                     GetCurPos(SingClick=2)
                     ZOOM <<- FALSE
                     plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y) #zoom
                 }, container = ZMframe1)

   ZMobj2 <- gbutton("   RESET PLOT    ", handler=function(h,...){
                     XYrange <<- ResetXYrange
                     ReDraw()
                 }, container = ZMframe1)

   ZMframe2 <- gframe(text="Manual zoom values", horizontal=FALSE, spacing=5, container=ZMgroup)
   glabel(text="Exact Range Values:", spacing=1, container=ZMframe2)
   ZMgroup1 <- ggroup(horizontal=TRUE, container=ZMframe2)
   x1 <- gedit("", width=15, initial.msg = "Xmin= ", container=ZMgroup1)
   x2 <- gedit("", width=15, initial.msg = "Xmax= ", container=ZMgroup1)
   ZMgroup2 <- ggroup(horizontal=TRUE, container=ZMframe2)
   y1 <- gedit("", width=15, initial.msg = "Ymin= ", container=ZMgroup2)
   y2 <- gedit("", width=15, initial.msg = "Ymax= ", container=ZMgroup2)
   tkconfigure(x1$widget, width=18)
   tkconfigure(x2$widget, width=18)
   tkconfigure(y1$widget, width=18)
   tkconfigure(y2$widget, width=18)
   OKbutt <- gbutton("  OK  ", handler=function(h,...){
                     x1 <- as.numeric(svalue(x1))
                     x2 <- as.numeric(svalue(x2))
                     y1 <- as.numeric(svalue(y1))
                     y2 <- as.numeric(svalue(y2))
                     if (FName[[SpectIndx]]@Flags) { #Binding energy set
                         XYrange$x <<- sort(c(x1, x2), decreasing=TRUE)
                         XYrange$y <<- sort(c(y1, y2))
                     } else {
                         XYrange$x <<- sort(c(x1, x2))
                         XYrange$y <<- sort(c(y1, y2))
                     }
                     ReDraw()
                 }, container = ZMframe2)


   ZMlabel <- glabel(text="    ", container=ZMgroup)
   ZMframe3 <- gframe(text="CURSOR POSITION", horizontal=FALSE, spacing=7, container=ZMgroup)

   CPosButt <- gbutton("Cursor Position", spacing=5, handler=function(h,...){
                     gmessage(msg="LEFT Mouse Button to Read Marker's Position; RIGHT Mouse Button to Exit" , title = "WARNING",  icon = "warning")
                     GetCurPos(SingClick=FALSE)
                     ReDraw()
                 }, container = ZMframe3)

   ZMobj4 <- glabel("Cursor position: ", container = ZMframe3)
   font(ZMobj4) <- list(family = "Sans", size="11")


   ExitButt <- gbutton("      EXIT      ", handler=function(h,...){
                    dispose(ZMwin)
                 }, container = ZMgroup)

   visible(ZMwin) <- TRUE
}

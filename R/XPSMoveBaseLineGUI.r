#Macro to change Baseline extremes

#'@title XPSMoveBaseLine
#'@description XPSMoveBaseLine modifies the BaseLine level and 
#'  limits of a given Coreline. New edge values are provided by
#   clicking with mouse on the CoreLine plot
#'@examples
#'\dontrun{
#' 	XPSMoveBaseLine()
#'}
#'@export
#'


XPSMoveBaseLine <- function(){

   ReDraw <- function(){

#--- Here the coreline and Baseline+Fit has to be displayed separately
       SampData <- as.matrix(XPSSample[[indx]]@.Data) #create spectrum data matrix for plot
       plot(x=SampData[[1]], y=SampData[[2]], xlim=Xrange1, ylim=Yrange1, type="l", lty="solid", lwd=1, col="black")
       SampData <- as(XPSSample[[indx]], "matrix") #create Baseline+Fit data matrix for plot
       NC <- ncol(SampData)
       if (NC > 2) { #there is a Baseline
          BaseLine <- SampData[,3]
          matlines(x=SampData[,1], y=BaseLine, xlim=Xrange1, type="l", lty="solid", lwd=1, col="sienna")
       }
       if (NC > 3){ #there is a fit
          FitComp <- SampData[,4:NC-1]  #Only components and fit
          SpectFit <- SampData[,NC]  #fit
          matlines(x=SampData[,1], y=FitComp, xlim=Xrange1, type="l", lty="solid", lwd=1, col="blue")
          matlines(x=SampData[,1], y=SpectFit, xlim=Xrange1, type="l", lty="solid", lwd=1, col="red")
       }
       if (SetZoom == TRUE){   #set zoom area corners
           points(Corners, type="p", col="blue", pch=3, cex=1.2, lwd=2.5)
           rect(Corners$x[1], Corners$y[1], Corners$x[4], Corners$y[4])
       }
   }



   updateObj <- function(h,...){
       SelectedXPSSample <- svalue(SourceFile)
       XPSSample <<- get(SelectedXPSSample,envir=.GlobalEnv)
       SpectList <<- XPSSpectList(SelectedXPSSample)
       delete(MBLFrame2,SourceCoreline)
       SourceCoreline <<- gcombobox(SpectList, selected=-1, handler=function(h, ...){
                                  SourceFile <- svalue(SourceFile)
                                  SourceCoreline <- svalue(SourceCoreline)
                                  SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #extract the spectrum idx
                                  indx <<- as.numeric(SourceCoreline[1])
                                  Xrange0 <<- range(XPSSample[[indx]]@.Data[[1]])
                                  Yrange0 <<- range(XPSSample[[indx]]@.Data[[2]])
                                  BLtype <- XPSSample[[indx]]@Baseline$type
                                  XPSSampleBkp <<- XPSSample #save for reset plot
                                  Xrange1 <<- range(XPSSample[[indx]]@.Data[[1]])
                                  if (XPSSample[[indx]]@Flags[1]) {   #reverse if BE scale
                                      Xrange1 <<- rev(Xrange1)
                                  }
                                  Yrange1 <<- range(XPSSample[[indx]]@.Data[[2]])
                                  enabled(MBLbutton) <- TRUE
                                  enabled(ADBLbutton) <- TRUE
                                  enabled(SZAbutton) <- TRUE
                                  enabled(RSTZMbutton) <- TRUE
                                  enabled(RSTbutton) <- TRUE
                                  enabled(SAVbutton) <- TRUE
                                  enabled(SAVEXbutton) <- TRUE
                                  ReDraw()
                             }, editable=FALSE, container=MBLFrame2)
      add(MBLFrame2,SourceCoreline)
      plot(XPSSample)
      enabled(SourceCoreline) <- TRUE #enable the selection of the coreline
   }

   MakeBaseLine <- function(){
        deg <- NULL
        Wgt <- NULL
        BLinfo <- XPSSample[[indx]]@Baseline$type
        BasLinType <- BLinfo[1]
        BasLinType <<- tolower(BasLinType)
        if (BasLinType == "linear" || BasLinType == "shirley" || BasLinType == "2p.shirley" || BasLinType == "2p.tougaard" || BasLinType == "3p.tougaard") {
           XPSSample[[indx]] <<- XPSsetRegionToFit(XPSSample[[indx]])
           XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
           XPSSample[[indx]] <<- XPSsetRSF(XPSSample[[indx]])
        } else if (BasLinType == "polynomial") {
           deg <- as.numeric(BLinfo[2])
           XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
        } else if (BasLinType == "spline") {
            # Now make BaseLine
            decr <- FALSE #Kinetic energy set
            if (XPSSample[[indx]]@Flags[1] == TRUE) { decr <- TRUE }
            idx <- order(splinePoints$x, decreasing=decr)
            splinePoints$x <- splinePoints$x[idx] #splinePoints$x in ascending order
            splinePoints$y <- splinePoints$y[idx] #following same order select the correspondent splinePoints$y
            LL <- length(splinePoints$x)
            XPSSample[[indx]]@Boundaries$x <<- c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
            XPSSample[[indx]]@Boundaries$y <<- c(splinePoints$y[1],splinePoints$y[LL])
            XPSSample[[indx]] <<- XPSsetRegionToFit(XPSSample[[indx]])
            XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
            XPSSample[[indx]] <<- XPSsetRSF(XPSSample[[indx]])
        } else if (BasLinType == "3p.shirley" || BasLinType == "lp.shirley" || BasLinType == "4p.tougaard") {
            Wgt <- as.numeric(BLinfo[2])
            XPSSample[[indx]] <<- XPSsetRegionToFit(XPSSample[[indx]])
            XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
            XPSSample[[indx]] <<- XPSsetRSF(XPSSample[[indx]])
        }
        LL <- length(XPSSample[[indx]]@Components)
        if (LL > 0) {
           for(ii in 1:LL){
              XPSSample[[indx]]@Components[[ii]] <<- Ycomponent(XPSSample[[indx]]@Components[[ii]], x=XPSSample[[indx]]@RegionToFit$x, y=XPSSample[[indx]]@Baseline$y) #calcola la Y eed aggiunge la baseline
           }
# update fit$y with sum of components
           tmp <- sapply(XPSSample[[indx]]@Components, function(z) matrix(data=z@ycoor))
           XPSSample[[indx]]@Fit$y <<- ( colSums(t(tmp)) - length(XPSSample[[indx]]@Components)*(XPSSample[[indx]]@Baseline$y))
        }
        EndPts <<- FALSE
   }


#--- Variables ---

   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }

   XPSSample <- NULL
   XPSSampleBkp <- NULL
   XPSSampleList <- XPSFNameList()
   SpectList <- ""
   BasLinType <- NULL
   index <- NULL
   Xrange0 <- NULL
   Yrange0 <- NULL
   Xrange1 <- NULL
   Yrange1 <- NULL
   Corners <- list(x=NULL, y=NULL)
   splinePoints <- list(x=NULL, y=NULL)
   ExitWhile <- NULL
   SetZoom <- FALSE
   EndPts <- FALSE
   plot.new() #reset the graphic window

#--- GUI ---

   MBLwin <- gwindow("ADJUST BASELINE", parent= c(100, 0), visible=FALSE)
   MBLgroup <- ggroup(label="", horizontal=FALSE, container=MBLwin)

   MBLFrame1 <- gframe("SELECT THE XPS-SAMPLE", horizontal=TRUE,container=MBLgroup)
   SourceFile <- gcombobox(XPSSampleList, selected=-1, editable=FALSE, expand=FALSE, handler=updateObj, container = MBLFrame1)

   MBLFrame2 <- gframe(text="SELECT THE CORELINE", spacing=5, container=MBLgroup)
   SourceCoreline <- gcombobox(SpectList, selected=-1, editable=FALSE, container=MBLFrame2)

   MBLFrame3 <- gframe(text="ADJUST BASELINE", horizontal=FALSE, spacing=5, container=MBLgroup)

   MBLbutton <- gbutton(text="Set Baseline End Points", spacing=5, handler=function(h,...){
                    BasLinType <<- XPSSample[[indx]]@Baseline$type[1]
                    if (BasLinType == "spline") {
                       txt <- "Spline background: \n ==> LEFT click to set spline points; RIGHT click to exit"
                       gmessage(msg=txt, title="HELP INFO", icon="info")
                       splinePoints <<- list(x=NULL, y=NULL)
                       pos <- c(1,1) # only to enter in  the loop
                       while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                              pos <- locator(n=1, type="p", col=3, cex=1.5, lwd=2, pch=1)
                              if (length(pos) > 0) {
                                  splinePoints$x <<- c(splinePoints$x, pos$x)  # $x and $y must be separate to add new coord to splinePoints
                                  splinePoints$y <<- c(splinePoints$y, pos$y)
                              }
                       }
                    } else {
                       txt <- paste(BasLinType, " background found!\n  ==> Set the Baseline Limits")
                       gmessage(msg=txt, title="HELP INFO", icon="info")
                       pos <- locator(n=2, type="p", pch=1, cex=1.5, col="red", lwd=2)
                       decr <- FALSE #Kinetic energy set
                       if (XPSSample[[indx]]@Flags[1] == TRUE) { decr <- TRUE } #Binding Energy set
                       idx <- order(pos$x, decreasing = decr)
                       pos$x <- pos$x[idx] #put Pos$X, Pos$y elements in the correct order
                       pos$y <- pos$y[idx]
                       if (XPSSample[[indx]]@Flags[1]) { #Binding energy set
                           idx <- order(pos$x, decreasing = TRUE)
                           Xrange1 <<- pos$x <- pos$x[idx] #put Pos$X, Pos$y elements in the correct order
                           pos$y <- pos$y[idx]
                           idx1 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[2])  #we must have idx1 < idx2
                           idx2 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[1])
                       } else {                      #Kinetic energy set
                           idx <- order(pos$x, decreasing = FALSE)
                           Xrange1 <<- pos$x <- pos$x[idx] #put Pos$X, Pos$y elements in the correct order
                           pos$y <- pos$y[idx]
                           idx1 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[1])
                           idx2 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[2])
                       }
                       XPSSample[[indx]]@RegionToFit$x <<- XPSSample[[indx]]@.Data[[1]][idx1:idx2] # idx1 < idx2
                       XPSSample[[indx]]@RegionToFit$y <<- XPSSample[[indx]]@.Data[[2]][idx1:idx2]
                       Yrange1 <<- range(XPSSample[[indx]]@RegionToFit$y)
                       XPSSample[[indx]]@Boundaries <<- pos
                    }
                    EndPts <<- TRUE
                 }, container=MBLFrame3)

   ADBLbutton <- gbutton("  Adjust Baseline  ", handler=function(h,...){
                    if (EndPts == FALSE) {
                        gmessage(msg="Please set the end-points before adjusting the BaseLine", title="WARNING", icon="warning")
                        return()
                    }
                    MakeBaseLine()
                    #changing the Baseline requires the fit components and the fit have to be recomputed
                    LL <- length(XPSSample[[indx]]@Components)
                    tmp <- NULL
                    if (LL > 0) {
                       for(ii in 1:LL) {
                          XPSSample[[indx]]@Components[[ii]] <<- Ycomponent(XPSSample[[indx]]@Components[[ii]], x=XPSSample[[indx]]@RegionToFit$x, y=XPSSample[[indx]]@Baseline$y) #computes the Y and add the Baseline
                       }
                       tmp <- sapply(XPSSample[[indx]]@Components, function(z) matrix(data=z@ycoor))
                       XPSSample[[indx]]@Fit$y <<- (colSums(t(tmp)) - length(XPSSample[[indx]]@Components)*(XPSSample[[indx]]@Baseline$y))
                    }
                    plot(XPSSample[[indx]], xlim=Xrange1, ylim=Yrange1)  #if zoom is present keeps new X, Y limits
                 }, container = MBLFrame3)

   ZMgroup <- ggroup(label="", horizontal=TRUE, container=MBLFrame3)

   SZAbutton <- gbutton("        Set the Zoom Area        ", handler = function(h, ...){
                    row1 <- " => Left click to define the 2 ZOOM REGION CORNERS (opposite in diagonal)"
                    row2 <- "\n => Click near corners to adjust Zoom Region Dimensions"
                    row3 <- "\n => Right click to Make Zoom when Zoom Region OK"
                    msg<-paste(row1, row2, row3, sep="")
                    gmessage( msg, icon="warning")
                    SetZoom <<- TRUE
                    pos <- locator(n=2, type="p", pch=3, cex=1, col=4, lwd=2)
                    Corners$x <<- c(pos$x[1],pos$x[1],pos$x[2],pos$x[2])
                    Corners$y <<- c(pos$y[1],pos$y[2],pos$y[1],pos$y[2])
                    decr <- FALSE #Kinetic energy set
                    if (XPSSample[[indx]]@Flags[1] == TRUE) { decr <- TRUE }
                    Xrange1 <<- sort(c(pos$x[1], pos$x[2]), decreasing=decr)
                    Yrange1 <<- sort(c(pos$y[1], pos$y[2]), decreasing=FALSE)
                    ReDraw()  #plots the zoom region
                    pos <- list(x=NULL, y=NULL)
                    ExitWhile <- 1
                    while(ExitWhile > 0 ){
                          pos <- locator(n=1)
                          if (is.null(pos) ){
                              ExitWhile <- -1
                              SetZoom <<- FALSE
                              if (XPSSample[[indx]]@Flags[1]) { #Binding energy set
                                  Corners$x <<- sort(Corners$x, decreasing=TRUE)
                              } else {                      #Kinetic energy set
                                  Corners$x <<- sort(Corners$x, decreasing=FALSE)
                              }
                              break()  #stops the while loop and exit
                          }
                          if (pos$x < Xrange0[1]) {pos$x <- Xrange0[1]}
                          if (pos$x > Xrange0[2]) {pos$x <- Xrange0[2]}
                          if (pos$y < Yrange0[1]) {pos$y <- Yrange0[1]}
                          if (pos$y > Yrange0[2]) {pos$y <- Yrange0[2]}

                          Dist <- NULL
                          Dmin <- ((pos$x-Corners$x[1])^2 + (pos$y-Corners$y[1])^2)^0.5  #valore di inizializzazione
                          for (ii in 1:4) {
                               Dist <- ((pos$x-Corners$x[ii])^2 + (pos$y-Corners$y[ii])^2)^0.5  #dist P0 P1
                               if(Dist <= Dmin){
                                  Dmin <- Dist
                                  idx <- ii
                               }
                          }
                          if (idx == 1){
                              Corners$x[1] <<- Corners$x[2] <<- pos$x
                              Corners$y[1] <<- Corners$y[3] <<- pos$y
                          } else if (idx == 2){
                              Corners$x[1] <<- Corners$x[2] <<- pos$x
                              Corners$y[2] <<- Corners$y[4] <<- pos$y
                          } else if (idx == 3){
                              Corners$x[3] <<- Corners$x[4] <<- pos$x
                              Corners$y[1] <<- Corners$y[3] <<- pos$y
                          } else if (idx == 4){
                              Corners$x[3] <<- Corners$x[4] <<- pos$x
                              Corners$y[2] <<- Corners$y[4] <<- pos$y
                          }
                          Xrange1 <<- c(Corners$x[1], Corners$x[3])  #modify only the Y range to give possibility to re-define the Baseline edges
                          Yrange1 <<- sort(c(Corners$y[1], Corners$y[2]), decreasing=FALSE)  #modify only the Y range to give possibility to re-define the Baseline edges
                          ReDraw()
                    } ### while loop end
                    plot(XPSSample[[indx]], xlim=Xrange1, ylim=Yrange1)
                 }, container = ZMgroup)

   RSTZMbutton <- gbutton("           Reset Zoom            ", handler=function(h,...){
                   Xrange1 <<- range(XPSSample[[indx]]@RegionToFit$x) #if Baseline present limit the
                   Yrange1 <<- range(XPSSample[[indx]]@RegionToFit$y) #plot limits to the RegionToFit
                   XPSSample[[indx]]@Boundaries$x <<- Xrange1
                   XPSSample[[indx]]@Boundaries$y <<- Yrange1
                   LL <- length(XPSSample[[indx]]@RegionToFit$x)
                   if (XPSSample[[indx]]@Flags[1]) { #Binding energy set
                       Xrange1 <<- sort(Xrange1, decreasing=TRUE)  #pos$x in decreasing order
                   } else {
                       Xrange1 <<- sort(Xrange1, decreasing=FALSE) #pos$x in increasing order
                   }
                   Yrange1 <<- sort(Yrange1, decreasing=FALSE) #pos$ in increasing order
                   SetZoom <<- FALSE  #definition of zoom area disabled
                   ReDraw()
                }, container = ZMgroup)

   RSTbutton <- gbutton("    RESET    ", handler=function(h,...){
                    Yrange <<- range(XPSSample[[indx]]@.Data[[2]])
                    SetZoom <<- FALSE

                    LL <- length(XPSSample[[indx]]@Components)
                    XPSSample[[indx]] <<- XPSSampleBkp[[indx]]
                    ReDraw()
                 }, container = MBLgroup)

   SAVbutton <- gbutton("    SAVE     ", handler=function(h,...){
    	              assign(activeFName, XPSSample, envir=.GlobalEnv)
                    XPSSaveRetrieveBkp("save")
                 }, container = MBLgroup)


   SAVEXbutton <- gbutton(" SAVE & EXIT ", handler=function(h,...){
                    dispose(MBLwin)
     	              assign(activeFName, XPSSample, envir=.GlobalEnv)
                    XPSSaveRetrieveBkp("save")
                 }, container = MBLgroup)

   enabled(MBLbutton) <- FALSE
   enabled(ADBLbutton) <- FALSE
   enabled(SZAbutton) <- FALSE
   enabled(RSTZMbutton) <- FALSE
   enabled(RSTbutton) <- FALSE
   enabled(SAVbutton) <- FALSE
   enabled(SAVEXbutton) <- FALSE
   visible(MBLwin) <- TRUE

}

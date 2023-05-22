## =======================================================================================
## XPSAnalysis function to add baseline and fit components for best fit and quantification
## =======================================================================================

#' @title XPSAnalysis to analyze XPS-Corelines and XPS spectra in general
#' @description XPSAnalysis() function is an interactive GUI to add BaseLines
#'   and Fitting components to a CoreLine needed for the best fit and quantification
#' @return Returns the processed \code{object}.
#' @examples
#' \dontrun{
#'    XPSAnalysis()
#' }
#' @export
#'                          


XPSAnalysis <- function() {

  GetCurPos <- function(SingClick){
       tabMain <- svalue(nbMain)
       coords <<- NULL
       enabled(BaselineType) <- FALSE   #prevent exiting Analysis if locatore active
       enabled(T2group1) <- FALSE   #prevent exiting Analysis if locatore active
       enabled(ButtFrame) <- FALSE
       EXIT <- FALSE
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                enabled(BaselineType) <- TRUE
                enabled(T2group1) <- TRUE
                enabled(ButtFrame) <- TRUE
                EXIT <- TRUE
            } else {
                if ( SingClick ){
                     enabled(BaselineType) <- TRUE
                     enabled(T2group1) <- TRUE
                     enabled(ButtFrame) <- TRUE
                     EXIT <- TRUE
                }
                if (tabMain == 1 && SetZoom == TRUE) {
                    coords <<- unlist(pos)
                    RBmousedown()  #selection of the zoom area
                }
                if (tabMain == 1 && BType != "spline") {
                    coords <<- unlist(pos)
                    LBmousedown()  #selection of the BaseLine Edges
                }
                if (tabMain == 1 && BType == "spline") {
                    coords <<- unlist(pos)
                    RBmousedown()  #selection of the BaseLine
                }
                if (tabMain == 2 ) {
                    coords <<- unlist(pos)
                    LBmousedown()
                }
            }
       }
       return()
  }

  LBmousedown <- function() {   #Set marker position for Baseline and Fit Component
     tabMain <- svalue(nbMain)
     tabComp <- svalue(nbComponents)
     if (BType=="spline") {
         if (hasBaseline(Object[[coreline]])){
             gmessage("Press RESET BASELINE to modify the spline", title="WARNING", icon="warning")
             return()
         } else {
             gmessage("Left mouse button to define spline-points, Right button to exit", title="WARNING", icon="warning")
             return()
         }
     }
     if (coreline == 0) { return() }
     if ( SetZoom == TRUE ) {   #when Set Zoom set only mouseRGT button has to be used
        return()
     }
     if (tabMain == 1 && length(Object[[coreline]]@Components) > 0) {
         gmessage("Fit present: \nchange notebook page, Baseline operations not allowed", title="WARNINR", icon="warning")
         return()
     }
     if (coreline != 0) { #coreline != "All Spectra"
           xx <- coords[1]
            yy <- coords[2]
            #compute the distance between consecutive clicks
         if (Object[[coreline]]@Flags[1]) { #Binding energy set
             Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE)  #point.coords$x in decreasing order
         } else {
             Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #point.coords$x in increasing order
         }
         if (! is.null(point.coords$x[1]) && tabMain==1 ) { #initially point.coords contains the spectrum boundaries
               d.pts <- (point.coords$x - xx)^2  #distance between spectrum edge and marker position
               point.index <<- min(which(d.pts == min(d.pts)))  #which is the edge nearest to the marker?
          } else {
             point.index <<- 1
         }

         if (tabMain == 1 && SetZoom == FALSE) {  # Baseline notebook page
                 point.coords$x[point.index] <<- xx   # set the marker position or modify the position for the baseline (
                 point.coords$y[point.index] <<- yy
             Object[[coreline]]@Boundaries <<- point.coords
             if (Object[[coreline]]@Flags[1]) {   #Binding energy set
               Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
             } else {
               Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
             }
             splinePoints <<- point.coords
             Make.Baseline()
         }
         if (tabMain == 1 && SetZoom == TRUE) {   # Baseline notebook page
                 point.coords$x[point.index] <<- xx   # set the marker position or modify the position for the baseline (
                 point.coords$y[point.index] <<- yy
             Object[[coreline]]@Boundaries <<- Corners
             if (Object[[coreline]]@Flags[1]) { #Binding energy scale
                 Xlimits <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE)  # X zoom limits
                 Ylimits <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE) # Y zoom limits
             } else {                           #Kinetic energy scale
                 Xlimits <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE)
                 Ylimits <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             }
             point.coords$x <<- Xlimits   #Baseline edges == X zoom limits
             idx <- findXIndex(Object[[coreline]]@.Data[1], Xlimits[1])
             point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx]
             idx <<- findXIndex(Object[[coreline]]@.Data[1], Xlimits[2])
             point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx]
         }
         if (tabMain == 2 && tabComp == 1) { # add FitComp 
             point.coords$x <<- xx
             point.coords$y <<- yy
             point.index <<- 1
             add.component()
         }
         if (tabMain == 2 && tabComp == 2) { # Components/Move Notebook page
             point.coords$x <<- xx
             point.coords$y <<- yy
             point.index <<- 1
             move.Comp()
         }
     }
     Marker$Points <<- point.coords
     replot()
     return()
  }

  RBmousedown <- function() {   #Right mouse button down
     xx <- coords[1]
     yy <- coords[2]

     if (BType=="spline") {
         splinePoints$x <<- c(splinePoints$x, coords[1])
         splinePoints$y <<- c(splinePoints$y, coords[2])
         Marker$Points <<- splinePoints
     } else if ( SetZoom == TRUE ) {   #RGT click to define zoom area
         point.coords$x[point.index] <<- coords[1]   #add component 3 to abscissa
         point.coords$y[point.index] <<- coords[2]   #add component 3 to ordinate
           xlim <- sort(Xlimits,decreasing=FALSE)
           DY <- (Ylimits[2]-Ylimits[1])/30
         if(xx < xlim[1]) { xx <- xlim[1] }
         if(xx > xlim[2]) { xx <- xlim[2] }
         if(yy < (Ylimits[1]-DY)) { yy <- Ylimits[1] }
         if(yy > (Ylimits[2]+DY)) { yy <- Ylimits[2] }
         if (point.index==1) {
             Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
             Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
             point.index <<- 3    #to add comp. 3 to points.coord and save the new marker position
         } else if (point.index==3) {
             D <- vector("numeric", 4)
             Dmin <- ((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
             for (ii in 1:4) {
                 D[ii] <- ((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
                 if(D[ii] <= Dmin){
                    Dmin <- D[ii]
                    idx <- ii
                 }
             }
             if (idx == 1){
                Corners$x[1] <<- Corners$x[2] <<- point.coords$x[3]
                Corners$y[1] <<- Corners$y[3] <<- point.coords$y[3]
             } else if (idx==2){
                Corners$x[1] <<- Corners$x[2] <<- point.coords$x[3]
                Corners$y[2] <<- Corners$y[4] <<- point.coords$y[3]
             } else if (idx==3){
                Corners$x[3] <<- Corners$x[4] <<- point.coords$x[3]
                Corners$y[1] <<- Corners$y[3] <<- point.coords$y[3]
             } else if (idx==4){
               Corners$x[3] <<- Corners$x[4] <<- point.coords$x[3]
               Corners$y[2] <<- Corners$y[4] <<- point.coords$y[3]
             }
             if (Object[[coreline]]@Flags[1]) { #Binding energy set
                point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decreasing order
                point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             } else {
                point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in increasing order
                point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             }
         }
         Marker$Points <<- Corners
     }
     replot()
     return()
  }

  replot <- function(...) {
     if (coreline == 0) {      #coreline == "All spectra"
         plot(Object)
     } else {
         tabMain <- svalue(nbMain)
         tabComp <- svalue(nbComponents)
         if (tabMain == 1) {   #Baseline definition
             plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
             if (SetZoom == TRUE) {  #in Zoom Mode  BType is set to ""
                 points(Marker$Points, col=Marker$col, cex=Marker$cex, lwd=Marker$lwd, pch=Marker$pch) #draw zoom corners
                 rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])      #draw zoom frame area
             } else {
                 points(Marker$Points, col=Marker$col, cex=Marker$cex, lwd=Marker$lwd, pch=Marker$pch) #plot markers at the CoreLine edges to define the Baseline
             }
         } else if (tabMain == 2 && (tabComp == 1)) {  #Components-Add/Delete Tab
             if (svalue(plotFit) == "residual" && hasFit(Object[[coreline]])) {
                 XPSresidualPlot(Object[[coreline]])
             } else {
                 plot(Object[[coreline]])
                 points(point.coords, col=2, cex=1.2, lwd=2, pch=3)  #draw component position
             }
         } else if ((tabMain == 2) && (tabComp == 2) ){#Components-Move Tab
             MaxCompCoords <- getMaxOfComponents(Object[[coreline]])
             point.coords$x <<- MaxCompCoords[[1]][compIndx]
             point.coords$y <<- MaxCompCoords[[2]][compIndx]
             if (svalue(plotFit) == "residual" && hasFit(Object[[coreline]])) {
                 XPSresidualPlot(Object[[coreline]])
             } else {
                 plot(Object[[coreline]])
             }
             points(point.coords, col=2, cex=1, lwd=2, pch=1)
         }
         if (is.null(point.coords$x) || is.null(point.coords$y)){
             svalue(stat.Bar) <- " "
         } else {
             svalue(stat.Bar) <- sprintf(paste("x =",round(point.coords$x[1],2), " y =",round(point.coords$y[2],2), sep=" "))
         }
     }
  }


  Set.Coreline <- function(h, ...) {
     svalue(BaselineType, index=TRUE) <- -1
     coreline <<- svalue(Core.Lines)
     coreline <<- unlist(strsplit(coreline, "\\."))   #"number." and "CL name" are separated
     assign("activeSpectName",coreline[2], envir=.GlobalEnv)
     coreline <<- as.integer(coreline[1])
     assign("activeSpectIndx", coreline, envir=.GlobalEnv)
     if (coreline == 0) {    #coreline == "All spectra"
             enabled(T2group1) <- FALSE
     } else {
         enabled(ZRbutton) <- FALSE
         enabled(MZbutton) <- FALSE
         enabled(ZObutton) <- FALSE
         if (length(Object[[coreline]]@Baseline) > 0 ){
             enabled(T2group1) <- TRUE
         }
         # Zoom disabled
         SetZoom <<- FALSE
         # if boundaries already defined
         if (hasBaseline(Object[[coreline]]) ) {
            LL <- length(Object[[coreline]]@RegionToFit$x)
            point.coords$x <<- c(Object[[coreline]]@RegionToFit$x[1], Object[[coreline]]@RegionToFit$x[LL])
            point.coords$y <<- c(Object[[coreline]]@RegionToFit$y[1], Object[[coreline]]@RegionToFit$y[LL])
            Xlimits <<- range(Object[[coreline]]@RegionToFit$x)
            Ylimits <<- range(Object[[coreline]]@RegionToFit$y)
         } else if (!hasBoundaries(Object[[coreline]]) ) {
            LL <- length(Object[[coreline]]@.Data[[1]])
            point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
            point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
            Xlimits <<- range(Object[[coreline]]@.Data[1])
            Ylimits <<- range(Object[[coreline]]@.Data[2])
         } else {
            Reset.Baseline()   #defines the spectral limits and marker positions
         }
         if (hasBaseline(Object[[coreline]]) ) {
             BasLinType <- Object[[coreline]]@Baseline$type
             BasLinType <- tolower(BasLinType)
             if (length(grep(BasLinType,BaseLines)) > 0){
                 svalue(BaselineType) <- BasLinType
             }
             Xlimits <<- range(Object[[coreline]]@RegionToFit$x)
             Ylimits <<- range(Object[[coreline]]@RegionToFit$y)
             svalue(nbMain) <- 1
         }
         if (hasComponents(Object[[coreline]]) ) {
             svalue(nbMain) <- 2
             svalue(nbComponents) <- 1
             CompNames <<- names(Object[[coreline]]@Components)
             delete(T2Frame3, FitComp)
             FitComp <<- gcombobox(CompNames, selected=-1, handler=function(h, ...){
                               compIndx <- svalue(FitComp)
                               compIndx <- unlist(strsplit(compIndx, split="C"))   #index of the chosen component
                               compIndx <<- as.integer(compIndx[2])
                               replot()
                               gmessage("\nNew Position Left mouse button. \nRight button to stop slection", title="WARNING", icon="warning")
                               tcl("update", "idletasks") #closes the gmessage window
                               GetCurPos(SingClick=FALSE)
                            }, container = T2Frame3)
             add(T2Frame3, FitComp)
         }
         Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
         Object[[coreline]]@Boundaries <<- point.coords #boundaries correspond to coreline limits or RegToFit limits
     }
     svalue(nbMain) <- 1 #when a coreline is selected Baseline Notebook page is set
     replot()
  }

  Make.Baseline <- function(){     #deg, Wgt, splinePoints
     #Now generate the Base-Line
     cat("\n ==> Generating Baseline")
     if (BType == "") {
         gmessage("Select the Base-Line please", title="WARNING", icon="warning")
         return() 
     }
     if (BType == "spline" && is.null(splinePoints$x)) { return() }

     Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])
     Object[[coreline]] <<- XPSbaseline(Object[[coreline]], BType, deg, Wgt, splinePoints )
     Object[[coreline]] <<- XPSsetRSF(Object[[coreline]], Object[[coreline]]@RSF)
     replot()
     enabled(T2group1) <- TRUE
  }

  Reset.Baseline <- function(h, ...) {
     if (coreline != 0) {   #coreline != "All spectra"
        if (FreezeLimits == FALSE) {  #ZOOM not activated
           if (Object[[coreline]]@Flags[1] == TRUE){  #Binding Energy scale
              point.coords$x <<- sort(range(Object[[coreline]]@.Data[[1]]),decreasing=TRUE)
           } else {                                   #Kinetic energy scale
              point.coords$x <<- sort(range(Object[[coreline]]@.Data[[1]]),decreasing=FALSE)
           }
           Xlimits <<- point.coords$x
           idx1 <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[1])
           point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx1]
           idx2 <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[2])
           point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx2]
           Ylimits <<- c(min(Object[[coreline]]@.Data[[2]][idx1:idx2]), max(Object[[coreline]]@.Data[[2]][idx1:idx2]))
             Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
           splinePoints <<- list(x=NULL, y=NULL)  #reset preivous baseline
        } else {  #a zoom is present: we preserve Xlimits and Ylimits and point.coords values
           Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
           splinePoints <<- list(x=NULL, y=NULL)  #reset preivous baseline
        }
          enabled(T2group1) <- FALSE
       }
       Object[[coreline]]@Boundaries <<- point.coords
  }


  update.outputArea <- function(...) {
     if (coreline != 0 ) {
           if (svalue(disp_area)) { XPScalc(Object[[coreline]]) }
           if (hasFit(Object[[coreline]]) && svalue(disp_quant)) {
                 XPSquantify(Object)
           }
     }
  }

  add.component <- function() {
     if (coreline != 0 && hasBaseline(Object[[coreline]]) ) {
          if (is.null(point.coords$x[1])) {
             return() 
         } else {
             LL <- length(point.coords$x)
             for(ii in 1:LL){
                     Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = svalue(fit.Funct),
                                                 peakPosition = list(x = point.coords$x[point.index], y = point.coords$y[point.index]))
#                    Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = svalue(fit.Funct),
#                                                 peakPosition = list(x = point.coords$x[ii], y = point.coords$y[ii]))
##  to update fit remove Component@Fit and make the sum of Component@ycoor including the newone
                   tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))  #create a matrix formed by ycoor of all the fit Components
                   CompNames <<- names(Object[[coreline]]@Components)
                   delete(T2Frame3, FitComp)
                 FitComp <<- gcombobox(CompNames, selected=-1, handler=function(h, ...){
                                       compIndx <- svalue(FitComp)
                                       compIndx <- unlist(strsplit(compIndx, split="C"))   #indice della componente scelta class numeric
                                       compIndx <<- as.integer(compIndx[2])
                                       replot()
                                       gmessage("\nNew Position Left mouse button. \nRight button to stop slection", title="WARNING", icon="warning")
                                       tcl("update", "idletasks") #closes the gmessage window
                                       GetCurPos(SingClick=FALSE)
                                    }, container = T2Frame3)
                add(T2Frame3, FitComp)
                  Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                  replot()
             }
#                 point.coords <<- list(x=NULL,y=NULL)
         }
     }
  }

  del.component <- function(h, ...) {
     if (gconfirm(msg="All Constraints will be lost! Proceed anyway?", title="DELETE", icon="warning")) {
          LL<-length(Object[[coreline]]@Components)
          for (ii in 1:LL) { #remove all CONSTRAINTS
               Object[[coreline]]<<-XPSConstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
         }
         if (coreline != 0 && hasComponents(Object[[coreline]]) ) {
               delWin <- gwindow("DELETE FIT COMPONENT", parent = c(50,10), visible = FALSE)
               size(delWin) <-c(250, 150)
               delGroup <- ggroup(horizontal=FALSE, container=delWin)
               glabel("Select the fit component to delete", container=delGroup)
               gseparator(container=delGroup)
               IdxGroup <- ggroup(horizontal=FALSE, container=delGroup)
               compIdx <- gcombobox(c(CompNames,"All"), selected=1, container = IdxGroup)
               gbutton("REMOVE", handler=function(...){
                          if (svalue(compIdx) != "All"){
                              indx <- as.numeric(svalue(compIdx, index=TRUE))
                              Object[[coreline]] <<- XPSremove(Object[[coreline]], what="components", number=indx )
                              CompNames <<- names(slot(Object[[coreline]],"Components"))
                              if (length(Object[[coreline]]@Components) > 0 ) {
                                 #update fit without deteted component
                                 tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
                                 Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                              }
                              delete(IdxGroup, compIdx)
                              compIdx <<- gcombobox(c(CompNames,"All"), selected=1, container = IdxGroup)
                          } else {
                              Object[[coreline]] <<- XPSremove(Object[[coreline]], "components")
                              CompNames <<- ""
                          }
                          delete(T2Frame3,FitComp)
                          FitComp <<- gcombobox(CompNames, selected=-1, handler=function(h, ...){  #Update component selection in MOVE COMP panel
                                      compIndx <- svalue(FitComp)
                                      compIndx <- unlist(strsplit(compIndx, split="C"))    #index of the selected component
                                      compIndx <<- as.integer(compIndx[2])
                                      replot()
                                      gmessage("\nNew Position Left mouse button. \nRight button to stop slection", title="WARNING", icon="warning")
                                      tcl("update", "idletasks") #closes the gmessage window
                                      GetCurPos(SingClick=FALSE)
                                }, container = T2Frame3)
                          svalue(plotFit) <<- "normal"
                          point.coords <<- list(x=NULL,y=NULL)
                          replot()
                 }, container=delGroup)
                 gbutton("SAVE", handler=function(H, ...){
                                     assign(activeFName, Object, envir = .GlobalEnv)
                                     replot()
                          }, container=delGroup)
                 gbutton("EXIT", handler=function(h, ...) {
                                     dispose(delWin)
                          }, container=delGroup)
                 visible(delWin) <- TRUE
         }
     }
  }

  move.Comp <- function(...) {
     compIndx <- svalue(FitComp)
     compIndx <- unlist(strsplit(compIndx, split="C"))   #index selected component
     compIndx <- as.integer(compIndx[2])
       if (coreline != 0) {
          xx <- point.coords$x[point.index]
          yy <- point.coords$y[point.index]
             varmu <- sort(getParam(Object[[coreline]]@Components[[compIndx]],variable="mu"))
             minmu <- varmu$start-varmu$min
             maxmu <- varmu$max-varmu$start
             newmu <- c(xx, xx-minmu, xx+maxmu)
             newy <- yy - Object[[coreline]]@Baseline$y[max(which(Object[[coreline]]@Baseline$x>xx))+1]

             Object[[coreline]]@Components[[compIndx]] <<- setParam(Object[[coreline]]@Components[[compIndx]], parameter=NULL, variable="mu", value=newmu)
             Object[[coreline]]@Components[[compIndx]] <<- setParam(Object[[coreline]]@Components[[compIndx]], parameter="start", variable="h", value=newy)
             Object[[coreline]]@Components[[compIndx]] <<- Ycomponent(Object[[coreline]]@Components[[compIndx]], x=Object[[coreline]]@RegionToFit$x, y=Object[[coreline]]@Baseline$y)

             tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
             Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y) )
             Object[[coreline]] <<- sortComponents(Object[[coreline]]) #order components in decreasing order
             update.outputArea()
     }
  }


#---  Variables  -----------------------------

  FNameList <- XPSFNameList() #list of XPSSamples
  if(is.null(FNameList)) { return()}
  Object <- get(activeFName,envir=.GlobalEnv)
  SpectList <- XPSSpectList(activeFName) #list of XPSSample Corelines
  coreline <- 0
  deg <- 1   #by default the baseline polynom degree = 1
  Wgt <- 0.3 #LinearPolynomial weigth in LP.Shirley
  BType <- "" #defaul BKground
  splinePoints <- list(x=NULL, y=NULL)
  ZoomPoints <- list(x=NULL, y=NULL)
  BaseLines <- c("linear", "polynomial", "spline",
                 "Shirley", "2P.Shirley", "3P.Shirley", "LP.Shirley",
                 "2P.Tougaard", "3P.Tougaard", "4P.Tougaard")
  FitFunct <- c("Gauss", "Lorentz", "Voigt", "Sech2", "GaussLorentzProd", "GaussLorentzSum",
              "AsymmGauss", "AsymmLorentz", "AsymmVoigt", "AsymmGaussLorentz", "AsymmGaussVoigt",
              "AsymmGaussLorentzProd", "DoniachSunjic", "DoniachSunjicTail",
              "DoniachSunjicGauss", "DoniachSunjicGaussTail", "SimplifiedDoniachSunjic", "ExpDecay",
              "PowerDecay", "Sigmoid")
  CompNames <- "   "
  SetZoom <- FALSE
  FreezeLimits <- FALSE

  coords <- NULL # for printing mouse coordinates on the plot
  point.coords <- list(x=NULL, y=NULL)
  point.index <- 1
  Corners <- point.coords
  compIndx <- 1
  Marker <- list(Points=list(x=NULL, y=NULL), col=2, cex=2, lwd=1.5, pch=10)
  Xlimits <- NULL
  Ylimits <- NULL

  BLgroup <- list()
  BLvalue <- list()

  O.Sys <- unname(tolower(Sys.info()["sysname"]))
  WinPointers <- NULL
  WinSize <- XPSSettings$General[4]
  plot(Object)


#----- ANALYSIS GUI -----------------------------
  ## Start
  MainWindow <- gwindow("XPS Analysis GUI", parent=c(50,10), visible = FALSE)
  addHandlerDestroy(MainWindow, handler=function(h, ...){  #if MainWindow unproperly closed with X
                               EXIT <<- TRUE
                               XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                               assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                               plot(Object[[activeSpectIndx]])         #replot the CoreLine
                           })
  MainGroup <- ggroup(container = MainWindow, horizontal = TRUE)

  ## XPS Sample & Core lines
  Pgroup1 <- ggroup(horizontal = FALSE, spacing = 3, expand = TRUE, container = MainGroup)

  Pframe1 <- gframe(text = " XPS Sample and Core line Selection ",horizontal = TRUE, container = Pgroup1)
  XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                                 activeFName <<- svalue(XPS.Sample)
                                 Object <<- get(activeFName, envir=.GlobalEnv)
                                 SpectList <<- XPSSpectList(activeFName)
                                 delete(Pframe1, Core.Lines)
                                 Core.Lines <<- gcombobox(c("0.All spectra", SpectList), selected=1, expand = FALSE, handler = Set.Coreline, container = Pframe1)
                                 coreline <<- 0
                                 replot()
                       }, container = Pframe1)
  svalue(XPS.Sample)<-activeFName

  Core.Lines <- gcombobox(c("0.All spectra", SpectList), selected=1, expand = FALSE, handler = Set.Coreline, container = Pframe1)


#----- Notebook -----------------------------
  nbMain <- gnotebook(expand = FALSE, container = Pgroup1)

  size(nbMain) <- c(440, 350)  #this are the minimal dimensions of the notebook to visualize all the widgets

#----- TAB1: Baseline -----
  T1group1 <- ggroup(label = "Baseline", horizontal=FALSE, spacing=2, container = nbMain)

  T1Frame1 <- gframe(text = " Baseline functions ", horizontal=FALSE, spacing=1, container = T1group1)

  BaselineLyt <- glayout(spacing=1, container=T1Frame1)
  BaselineType <- gradio(items=BaseLines, selected=-1, spacing=1, horizontal = TRUE, handler = function(h, ...){
                                   #now make gcheckboxGroup to work as a RadioButton
                                   Sel <- svalue(BaselineType)
                                   BType <<- svalue(BaselineType)    #save the last selection
                                   if (coreline == 0) {
                                       gmessage("Please select the Core-Line to analyze!", title="WARNING", icon="warning")
                                       svalue(BaselineType) <- -1
                                       return()
                                   }
                                   if (hasBaseline(Object[[coreline]])){
                                       gmessage("ATTENTION: BaseLine already Defined! \nPress Reset Baseline Buttomn to Change the Lineshape ", title="WARNING", icon="warning")
                                       return()
                                   }
                                   if (BType=="") {
                                       gmessage("Please select the BaseLine type!", title="WARNING", icon="warning")
                                       return()
                                   }
                                   if (hasBoundaries(Object[[coreline]]) == FALSE) {
                                       gmessage("Set BaseLine edges. Right button to stop selection", title="WARNING", icon="warning")
                                       return()
                                   }
                                   if (BType == "spline" && length(splinePoints)==0) { #splinePoints not defined skip XPSBaseline()
                                       gmessage("Please select the spline points with the RIGHT mouse button!", title="WARNING", icon="warning")
                                       return()
                                   }
                                   splinePoints <<- list(x=NULL, y=NULL)
                                   Reset.Baseline()
                                   switch(BType,
                                      "linear" = {
                                                 #fast generation of background do not need background reset
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)       #delete content of Frame Settings
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("  ", container=T1group2) #add two empty row
                                                 glabel("  ", container=T1group2)
                                                 add(T1Frame1, T1group2)
                                                 },
                                      "polynomial" = {
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("Polynom degree", container=T1group2)
                                                 BLgroup <<- ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                                 BLvalue <<- gcombobox(c("1", "2", "3", "4", "5", "6"), width=20, selected=1, container= BLgroup)
                                                 gbutton("Make BaseLine", handler = function(h, ...) {
                                                            deg <<- as.numeric(svalue(BLvalue))
                                                            Make.Baseline()
                                                        }, container= BLgroup)
                                                 add(T1Frame1, T1group2)
                                                 },
                                      "spline" = {
                                                 Marker <<- list(Points=point.coords, col=3, cex=1.15, lwd=2, pch=16)
                                                 delete(T1Frame1, T1group2)
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("Left button select spline points - Right button EXIT", container=T1group2)
                                                 cat("\n Left button select spline points - Right button EXIT")
                                                 BLbutt <- gbutton("Make Spline Baseline", handler = function(h, ...) {
                                                 #--- Interactive mouse control ---
                                                        if (length(splinePoints$x) <= 2) {
                                                            svalue(BaselineType) <- ""
                                                            BType <<- "" #"linear" #plot Linear baseline until splinePoints are selected
                                                            splinePoints <<- point.coords
                                                            Make.Baseline()
                                                        } else {
                                                            decr <- FALSE #Kinetic energy set
                                                            if (Object[[coreline]]@Flags[1] == TRUE) { decr <- TRUE }
                                                            idx <- order(splinePoints$x, decreasing=decr)
                                                            splinePoints$x <<- splinePoints$x[idx] #splinePoints$x in ascending order
                                                            splinePoints$y <<- splinePoints$y[idx] #following same order select the correspondent splinePoints$y
                                                            LL <- length(splinePoints$x)
                                                            Object[[coreline]]@Boundaries$x <<- Xlimits <<- c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
                                                            Object[[coreline]]@Boundaries$y <<- c(splinePoints$y[1],splinePoints$y[LL])
                                                            Marker <<- list(Points=splinePoints, col=3, cex=1.15, lwd=2, pch=16)
                                                            Make.Baseline()
                                                        }
                                                    },container = T1group2)
                                                 add(T1Frame1, T1group2)
                                                 },
                                      "Shirley" = {
                                                 #fast generation of background do not need background reset
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("   ", spacing=1, container=T1group2) #add space with empty text rows
                                                 glabel("   ", spacing=1, container=T1group2)
                                                 add(T1Frame1, T1group2)
                                                 },
                                      "2P.Shirley" = {
                                                 #fast generation of background do not need background reset
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("   ", spacing=1, container=T1group2) #add space with empty text rows
                                                 glabel("   ", spacing=1, container=T1group2)
                                                 add(T1Frame1, T1group2)
                                                 },
                                      "3P.Shirley" = {
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("Distortion Parameter Ds", spacing=1, container=T1group2)
                                                 BLgroup <<- ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                                 BLvalue <<- gedit("0.3", width=20, spacing=1, container= BLgroup)
                                                 gbutton("Make BaseLine", handler = function(h, ...) {
                                                        Wgt <<- as.numeric(svalue(BLvalue))
                                                        slot(Object[[coreline]],"Boundaries") <<- point.coords
                                                        Make.Baseline()
                                                   }, container= BLgroup)
                                                 add(T1Frame1, T1group2)
                                                 },
                                      "LP.Shirley" = {
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)  #Change T1group2 content changing Lab1, Lab2
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("B coeff.", spacing=1, container=T1group2)
                                                 BLgroup <- ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                                 BLvalue <- gedit("0.005", width=20, spacing=1, container= BLgroup)
                                                 gbutton("Make BaseLine", handler = function(h, ...) {
                                                        Object[[coreline]]@Boundaries <<- point.coords
                                                        Wgt <<- as.numeric(svalue(BLvalue))
                                                        Make.Baseline()
                                                   }, container= BLgroup)
                                                 add(T1Frame1, T1group2)
                                                 Wgt <<- 0.005
                                                 },
                                      "2P.Tougaard" = {
                                                 #fast generation of background do not need background reset
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)  #cancello il contenuto Frame Settings
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("   ", spacing=1, container=T1group2) #Add a white space in T1group1
                                                 glabel("   ", spacing=1, container=T1group2) #Add a white space in T1group1
                                                 add(T1Frame1, T1group2)
                                                 },
                                      "3P.Tougaard" = {
                                                 #fast generation of background do not need background reset
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)  #to reset the widget T1group2
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                                                 glabel("   ", spacing=1, container=T1group2) #to add  space in the widget
                                                 glabel("   ", spacing=1, container=T1group2) #to add  space in the widget
                                                 add(T1Frame1, T1group2) #add the new T1group2 widget
                                                 },
                                      "4P.Tougaard" = {
                                                 Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                                                 delete(T1Frame1, T1group2)  #to reset the widget T1group2
                                                 T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1) #update T1group2 content
                                                 glabel("C coeff.", spacing=1, container=T1group2)
                                                 BLgroup <- ggroup(horizontal=TRUE, spacing=1, container = T1group2)
                                                 BLvalue <- gedit("0.5", width=20, spacing=1, container= BLgroup)
                                                 gbutton("Make BaseLine", handler = function(h, ...) {
                                                        Object[[coreline]]@Boundaries <<- point.coords
                                                        Wgt <<- as.numeric(svalue(BLvalue))
                                                        Make.Baseline()
                                                   }, container= BLgroup)
                                                 add(T1Frame1, T1group2) #add the new T1group2 widget
                                                 }
                                   )  #end switch
                                   Make.Baseline()
                                   replot()
                                   gmessage("\nSet BaseLine edges. Right button to stop selection", title="WARNING", icon="warning")
                                   GetCurPos(SingClick=FALSE)
                   },container = T1Frame1)
  for(ii in 1:10) {
      tkpack.forget(BaselineType$widgets[[ii]])  # unparent widgets (uses library call)
  }
  for(ii in 1:3){  #breaks the gcheckgroup in three rows
      BaselineLyt[1,ii] <- BaselineType$widgets[[ii]]  #first row 3 element
  }
  for(ii in 1:4){
      BaselineLyt[2,ii] <- BaselineType$widgets[[(ii+3)]]  #second row 4 elements
  }
  for(ii in 1:3){
      BaselineLyt[3,ii] <- BaselineType$widgets[[(ii+7)]]  #third row 3 elements
  }

  T1group2 <- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
  glabel("   ", spacing=1, container=T1group2)  #to add  space in the window frame
  glabel("   ", spacing=1, container=T1group2)  #to add  space in the window frame

  ActionFrame <- gframe(text = " Reset ", spacing=1, container = T1group1)
  gbutton("   Reset Baseline   ", handler = function(h, ...) {
                   splinePoints <<- list(x=NULL, y=NULL)
                       svalue(BaselineType) <- -1
                   BType <<- ""  #otherwise conflict between mouseRGT-selected points for zooming and for splinePoints
                   Reset.Baseline()
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   delete(T1Frame1, T1group2)       #delete content of Frame Settings
                   T1group2 <<- ggroup(horizontal=FALSE, spacing=1, container = T1Frame1)
                   glabel("  ", container=T1group2) #add two empty row
                   glabel("  ", container=T1group2)
                   add(T1Frame1, T1group2)
                   replot()
                }, container = ActionFrame)

  T1Frame4 <- gframe(text = " Plot ", spacing=1, horizontal=TRUE, container = T1group1)

  ZRbutton<-gbutton("SET ZOOM REGION", handler = function(h, ...){
                   point.coords <<- list(x=NULL, y=NULL)   #point.coords contain the X, Y data ranges
                   msg <- paste("Left click near the blue corners to adjust the zoom area",
                                "\n right click to stop and select zoo area", sep="")
                   gmessage(msg, title="WARNING", icon="warning")
                   svalue(BaselineType) <- 1
                   BType <<- "linear"  #otherwise conflict between mouseRGT-selected points for zooming and for splinePoints
                   point.index <<- 3
                   answ <- gconfirm(msg="Reset Baseline and CoreLine Fit and start from beginning?", title="WARNING", icon="warning")
                   if(answ){
                      Reset.Baseline()
                      point.coords$x <<- range(Object[[coreline]]@.Data[[1]])
                      point.coords$y <<- range(Object[[coreline]]@.Data[[2]])
                   } else {
                      if(hasRegionToFit(Object[[coreline]])){
                         LL <- length(Object[[coreline]]@RegionToFit$x)
                         Object[[coreline]]@Boundaries$x <<- range(Object[[coreline]]@RegionToFit$x)
                         Object[[coreline]]@Boundaries$y <<- range(Object[[coreline]]@RegionToFit$y)
                         point.coords$x <<- Object[[coreline]]@Boundaries$x
                         point.coords$y <<- Object[[coreline]]@Boundaries$y
                      } else {
                         point.coords$x <<- range(Object[[coreline]]@.Data[[1]])
                         point.coords$y <<- range(Object[[coreline]]@.Data[[2]])
                      }
                   }
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                   }
                   Xlimits <<- point.coords$x
                   Ylimits <<- point.coords$y
                   Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
                   Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
                   Marker <<- list(Points=Corners, col=4, cex=1.2, lwd=2.5, pch=3)
                   SetZoom <<- TRUE    #definition of zoom area disabled
                   FreezeLimits <<- TRUE  #reset spectrum range disabled
                   enabled(ZRbutton) <- TRUE
                   enabled(MZbutton) <- TRUE
                   enabled(ZObutton) <- TRUE
                   replot()
                   GetCurPos(SingClick=FALSE)
                }, container = T1Frame4)

  MZbutton<-gbutton("  MAKE ZOOM  ", handler = function(h, ...){
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                   } else {
                       point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
                   }
                   point.coords$y <<- sort(c(point.coords$y[1], point.coords$y[2]), decreasing=FALSE) #pos$x in increasing order
                   Xlimits <<- point.coords$x  #Baseline edges == X zoom limits
                   Ylimits <<- point.coords$y
                   idx <- findXIndex(Object[[coreline]]@.Data[[1]], Xlimits[1])
                   point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx]
                   idx <- findXIndex(Object[[coreline]]@.Data[[1]], Xlimits[2])
                   point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx]
                   Object[[coreline]]@Boundaries <<- point.coords
                   BType <<- ""  #otherwise conflict between mouseRGT-selected points for zooming and for splinePoints
                   svalue(BaselineType)<- 1
                   BType <<- "linear"  #otherwise conflict between mouseRGT-selected points for zooming and for splinePoints
                   point.index <<- 1
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   SetZoom <<- FALSE  #definition of zoom area disabled
                   FreezeLimits <<- TRUE #reset spectrum range disabled
#                   tkconfigure(Graph, cursor = "crosshair")
                   replot()
                   enabled(ZObutton)<-TRUE
                }, container = T1Frame4)

  ZObutton<-gbutton("   ZOOM OUT   ", handler = function(h, ...) {
                   Xlimits <<- range(Object[[coreline]]@.Data[1])  #Set plot limits to the whole coreline extension
                   Ylimits <<- range(Object[[coreline]]@.Data[2])
                   Object[[coreline]]@Boundaries$x <<- Xlimits
                   Object[[coreline]]@Boundaries$y <<- Ylimits
                   if ( hasBaseline(Object[[coreline]]) ) {
                       Xlimits <<- range(Object[[coreline]]@RegionToFit$x) #if Baseline present limit the
                       Ylimits <<- range(Object[[coreline]]@RegionToFit$y) #plot limits to the RegionToFit
                       LL<-length(Object[[coreline]]@RegionToFit$x)
                       point.coords$x[1] <<- Object[[coreline]]@RegionToFit$x[1]
                       point.coords$x[2] <<- Object[[coreline]]@RegionToFit$x[LL]
                       point.coords$y[1] <<- Object[[coreline]]@RegionToFit$y[1]
                       point.coords$y[2] <<- Object[[coreline]]@RegionToFit$y[LL]
                   } else {
                       LL<-length(Object[[coreline]]@.Data[[1]])
                       point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1]
                       point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL]
                       point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1]
                       point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL]
                       point.index <<- 1
                   }
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       Xlimits <<- sort(Xlimits, decreasing=TRUE)  #pos$x in decreasing order
                   } else {
                       Xlimits <<- sort(Xlimits, decreasing=FALSE) #pos$x in increasing order
                   }
                   Ylimits <<- sort(Ylimits, decreasing=FALSE) #pos$ in increasing order
                   SetZoom <<- FALSE  #definition of zoom area disabled
                   FreezeLimits <<- FALSE #reset spectrum range enabled
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   replot()
                   enabled(ZRbutton) <- FALSE
                   enabled(MZbutton) <- FALSE
                   enabled(ZObutton) <- FALSE
                }, container = T1Frame4)


#----- TAB2: Components -----
  T2group1 <- ggroup(label = "Components", spacing=2, horizontal=FALSE, container = nbMain)

  nbComponents <- gnotebook(container = T2group1, expand = FALSE)
  T2group2  <- ggroup(container = nbComponents, spacing=1, horizontal=FALSE, label = " Add/Delete ")
  T2group3  <- ggroup(container = nbComponents, spacing=1, horizontal=FALSE, label = " Move ")


#----- Add/Delete component subtab
  T2Frame1 <- gframe(text = " Component LineShape ", spacing=1, container = T2group2)
  fit.Funct <- gcombobox(FitFunct, selected = -1, handler = function(h, ...){
                               svalue(stat.Bar) <- sprintf("Selected component type %s", svalue(h$obj))
                               txt <- paste("1) Select the fit function. \n",
                                           "2) Press 'Add' button to add the fit components. \n",
                                           "2) Left mouse button to enter the Fit component position \n",
                                           "3) Stop entering positions with rigth mouse button. \n",
                                           "5) Change fit function and restart from point (2).", sep="")
                               gmessage(msg=txt, title=" WARNING                                   ", icon="warning")
                               tcl("update", "idletasks") #closes the gmessage window
                          }, container = T2Frame1)

  T2Frame2 <- gframe(text = " Set Fit Components ", horizontal=FALSE, container = T2group2)
  glabel(" Press 'Add' to add Fit Components", container=T2Frame2)
  add_btn <- gbutton("Add", handler = function(h, ...){
                                GetCurPos(SingClick=FALSE)
                          }, container = T2Frame2)

  del_btn <- gbutton("Delete", container = T2Frame2, expand=FALSE, handler = del.component )


#----- Move component subtab
  T2Frame3 <- gframe(text = " Select Component ", spacing=1, container = T2group3)
  FitComp <- gcombobox(CompNames, selected=-1, handler=function(h, ...){   #CompNames
                               compIndx <- svalue(FitComp)
                               compIndx <- unlist(strsplit(compIndx, split="C"))   #index of the selected component (numeric)
                               compIndx <<- as.integer(compIndx)
                               replot()
                               gmessage("\nSelect the Fit Component to move. \nRight button to stop slection", title="WARNING", icon="warning")
                               tcl("update", "idletasks") #closes the gmessage window
                               GetCurPos(SingClick=FALSE)
                           }, container = T2Frame3)

  T2Frame4 <- gframe(text = " Display ", container = T2group3, horizontal=TRUE)
  disp_area <- gcheckbox("Area table", checked=FALSE, container=T2Frame4, handler = update.outputArea )
  disp_quant <- gcheckbox("Quantification table", checked=FALSE, container=T2Frame4, handler = update.outputArea )

  ## plot type : Residual or simple
  T2Frame5 <- gframe(text = " Plot ", spacing=1, container = T2group1)
  plotFit <- gradio(items=c("normal", "residual"), selected=1, spacing=1, container = T2Frame5, expand = TRUE, horizontal = TRUE, handler = replot)



#----- SAVE&CLOSE buttons -----
  ButtFrame <- gframe(text="Save & Exit", spacing=3, container=Pgroup1)
  Buttlyt <- glayout(spacing=3, container=ButtFrame)

  Buttlyt[1,1] <- gbutton("                 RESET                ", handler = function(h, ...){
                  svalue(BaselineType, index=TRUE) <- 1
                  BType <<- "linear"  #by default initial Baseline is linear
                  delete(T1Frame3, T1group2)  #cancello il contenuto Frame Settings
                  T1group2 <<- ggroup(horizontal=FALSE, spacing=7, container = T1Frame3)
                  glabel("  ", container=T1group2) #add two empty row
                  glabel("  ", container=T1group2)
                  add(T1Frame3, T1group2)
                  Object<<-get(activeFName, envir = .GlobalEnv)
                  if (length(Object[[coreline]]@Components) > 0) {
                      Xlimits <<- range(Object[[coreline]]@RegionToFit[1])
                      Ylimits <<- range(Object[[coreline]]@RegionToFit[2])
                  } else if (length(Object[[coreline]]@Components) == 0) {
                      Xlimits <<- range(Object[[coreline]]@.Data[1])
                      Ylimits <<- range(Object[[coreline]]@.Data[2])
                      Reset.Baseline()
                      Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                      splinePoints <<- point.coords
                      svalue(nbMain) <<- 1
                  }
                  enabled(ZRbutton) <- FALSE
                  enabled(MZbutton) <- FALSE
                  enabled(ZObutton) <- FALSE
                  replot()
              }, container = Buttlyt)

  Buttlyt[1,2] <- gbutton("                 SAVE                 ", handler = function(h, ...){
                  coreline <- svalue(Core.Lines)
                  coreline <- unlist(strsplit(coreline, "\\."))
                  assign(activeFName, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", as.integer(coreline[1]), envir=.GlobalEnv)
                  assign("activeSpectName", coreline[2], envir = .GlobalEnv)
                  XPSSaveRetrieveBkp("save")
              }, container = Buttlyt)

  Buttlyt[2,1] <- gbutton("             SAVE & EXIT              ", handler=function(h,...){
                  dispose(MainWindow)
                  coreline <- svalue(Core.Lines)
                  coreline <- unlist(strsplit(coreline, "\\."))
                  assign(activeFName, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", as.integer(coreline[1]), envir=.GlobalEnv)
                  assign("activeSpectName", coreline[2], envir = .GlobalEnv)
                  XPSSaveRetrieveBkp("save")
              }, container = Buttlyt)

  Buttlyt[2,2] <- gbutton("                 EXIT                 ", handler=function(h,...){
                  plot(Object[[activeSpectIndx]])   #plot selected XPSSample with all the corelines
                  dispose(MainWindow)
                  XPSSaveRetrieveBkp("save")
                  return()
              }, container = Buttlyt)

  stat.Bar <- gstatusbar("status", container = MainWindow)


#----- Change tab handler -----
  addHandlerChanged(nbMain, handler=function(h,...) {
        nbPage <- svalue(nbMain, index=TRUE)
        coreline <- svalue(Core.Lines)
        coreline <- unlist(strsplit(coreline, "\\."))   #split string in correspondence of the point: coreline[1]=index, coreline[2]=spect name
        coreline <- as.numeric(coreline[1])               #this is the coreline index
        if ( nbPage > 1 ) { point.coords <<- list(x = NULL, y = NULL) }
        svalue(plotFit) <- "normal"
        svalue(stat.Bar) <- sprintf("On page %s", h$page.no)
        if (coreline > 0 && length(Object[[coreline]]@RegionToFit) > 0 ) {
            enabled(T2group1) <- TRUE
        }   #baseline already defined enable component selection
     }
  )
  enabled(T2group1) <- FALSE
  enabled(ZRbutton) <- FALSE
  enabled(MZbutton) <- FALSE
  enabled(ZObutton) <- FALSE

  visible(MainWindow) <- TRUE
  svalue(nbComponents) <- 1
  svalue(nbMain) <- 1
#  tkevent.generate(MainWindow$widget, "<Expose>", when="now") #forces the MainWindow to be exposed
  tcl("update", "idletasks") #Complete the idle tasks
#  MainWindow$set_modal(TRUE)

}
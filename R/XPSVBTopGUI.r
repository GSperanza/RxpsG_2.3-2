## =====================================================
## VBtop: funciton to compute the upper edge of the HOMO
## =====================================================

#' @title XPSVBTop
#' @description XPSVBTop function to estimate the position of the Valence Band Top
#'   the interactive GUI adds a BaseLines and Fitting components to
#'   the region of the VB proximal to the Fermi Edge needed
#'   for the estimation of the VB-Top position
#' @examples
#' \dontrun{
#'  XPSVBTop()
#' }
#' @export
#'                             

XPSVBTop <- function() {

  GetCurPos <- function(SingClick){
       coords <<- NULL
       enabled(T1group1) <- FALSE   #prevent exiting Analysis if locatore active
       enabled(T2group1) <- FALSE
       enabled(ButtGroup) <- FALSE
       EXIT <- FALSE
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                enabled(T1group1) <- TRUE
                enabled(T2group1) <- TRUE
                enabled(ButtGroup) <- TRUE
                EXIT <- TRUE
            } else {
                if ( SingClick ){ 
                    coords <<- c(pos$x, pos$y)
                    enabled(T1group1) <- TRUE
                    enabled(T2group1) <- TRUE
                    enabled(ButtGroup) <- TRUE
                    EXIT <- TRUE
                } else {
                    Xlim1 <- min(range(Object[[coreline]]@.Data[[1]]))   #limits coordinates in the Spectrum Range
                    Xlim2 <- max(range(Object[[coreline]]@.Data[[1]]))
                    Ylim1 <- min(range(Object[[coreline]]@.Data[[2]]))
                    Ylim2 <- max(range(Object[[coreline]]@.Data[[2]]))

                    if (pos$x < Xlim1 ) {pos$x <- Xlim1}
                    if (pos$x > Xlim2 ) {pos$x <- Xlim2}
                    if (pos$y < Ylim1 ) {pos$y <- Ylim1}
                    if (pos$y > Ylim2 ) {pos$y <- Ylim2}
                    coords <<- c(pos$x, pos$y)
                    LBmousedown()  #selection of the BaseLine Edges
                }
            }
       }
       return()
  }

  LBmousedown <- function() {
     tab1 <- svalue(nbMain)
     tab2 <- svalue(nbVBfit)
#--- define point.coords
     if (is.null(point.coords$x) && VBlimOK==FALSE) {
        if (hasBoundaries == FALSE) {
           Object[[coreline]]@Boundaries$x <- range(Object[[coreline]]@.Data[[1]])
           Object[[coreline]]@Boundaries$y <- range(Object[[coreline]]@.Data[[2]])
        }
        point.coords <<- Object[[coreline]]@Boundaries  #point.coord list was reset
     }
     if (coreline != 0 && tab1 == 1) { #coreline != "All Spectra" and tab Baseline
        xx <- coords[1]
        yy <- coords[2]
        if (! is.na(point.coords$x[1]) ) {
# Crtl which marker position at VB ends has to be changed
           tol.x <- abs(diff(point.coords$x)) / 25
           tol.y <- abs(diff(point.coords$y)) / 25
           d.pts <- (point.coords$x - xx)^2 #+ (point.coords$y - yy)^2   #distance between mouse position and initial marker position
           point.index <<- min(which(d.pts == min(d.pts)))  #which of the two markers has to be moved in the new position?
        } else {
           point.index <<- 1
        }
        point.coords$x[point.index] <<- xx
        point.coords$y[point.index] <<- yy
     }
#---make plot changes upon mouse position and option selection
     if (is.null(point.coords$x) && VBlimOK==FALSE) { point.coords <<- Object[[coreline]]@Boundaries } #point.coord list was reset
     if (coreline != 0 && tab1==1) {   #coreline != "All spectra"  and Baseline tab
         point.coords$x[point.index] <<- coords[1]
         point.coords$y[point.index] <<- coords[2]
         tab1 <- svalue(nbMain)
         tab2 <- svalue(nbVBfit)
         if (tab1 == 1 && BType=="linear") {    ### notebook tab Baseline
            point.coords$y[1] <<- point.coords$y[2]   #keep linear BKG alligned to X
         }
         slot(Object[[coreline]],"Boundaries") <<- point.coords
         MakeBaseline(deg, splinePoints)  #modify the baseline
         if (VBbkgOK==FALSE){ #we are still modifying the Shirley baseline
            LL <- length(Object[[coreline]]@.Data[[1]])
            VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points
         }
         replot()
     }
     if (tab1 == 2 && tab2 == 1) { ### tab=VB Fit, Linear Fit
         if (coreline == 0) {
            gmessage(msg="Please select te VB spectrum", title = "WARNING: WRONG CORELINE SELECTION",  icon = "warning")
         }
         if (VBlimOK==TRUE) {
            point.coords$x <<- c(point.coords$x, coords[1])
            point.coords$y <<- c(point.coords$y, coords[2])
            replot()
         } else {
            gmessage(msg="Region proximal to Fermi not defined! ", title = "LIMITS FOR VB LINEAR FIT NOT CONFIRMED",  icon = "warning")
            return()
         }
     }
     if (tab1 == 2 && tab2 == 2) { ### tab=VB Fit, NON-Linear Fit
         if (coreline == 0) {
            gmessage(msg="Please select the VB spectrum", title = "WARNING: WRONG CORELINE SELECTION",  icon = "warning")
         }
#         point.coords$x <<- c(point.coords$x, coords[1])
#         point.coords$y <<- c(point.coords$y, coords[2])
         point.coords$x <<- coords[1]
         point.coords$y <<- coords[2]
         add.FitFunct()
         replot()
     }
     if (tab1 == 2 && tab2 == 3) { ### tab=VB Fit, Hill Sigmoid Fit
         if (coreline == 0) {
            gmessage(msg="Please select the VB spectrum", title = "WARNING: WRONG CORELINE SELECTION",  icon = "warning")
         }
         if (VBlimOK==FALSE) {
            gmessage(msg="Region proximal to Fermi not defined! ", title = "LIMITS FOR VB HILL SIGMOID FIT NOT CONFIRMED",  icon = "warning")
            return()
         }
         point.coords$x <<- c(point.coords$x, coords[1])
         point.coords$y <<- c(point.coords$y, coords[2])
         replot()
     }
     return()
  }

  replot <- function(...) {
     tab1 <- svalue(nbMain)
     tab2 <- svalue(nbVBfit)
     if (coreline == 0) {     # coreline == "All spectra"
         plot(Object)
     } else {
        if (tab1 == 1) {  ### tab1 Baseline
            if (svalue(baseline.zoom)) {
               lastX <- length(Object[[coreline]][[2]])
               baseline.ylim <- c( min(Object[[coreline]][[2]]),
                                2*max( c(Object[[coreline]][[2]][1], Object[[coreline]][[2]][lastX]) ) )
               plot(Object[[coreline]], ylim=baseline.ylim)
               points(point.coords, col="red", cex=SymSiz, lwd=1.5, pch=MarkSym)
            } else {
               plot(Object[[coreline]])     #plots the Baseline limits
               points(point.coords, col="red", cex=SymSiz, lwd=1.5, pch=MarkSym)
            }
        } else if ((tab1 == 2) && (tab2==1) ){ ### tab VB Fit, Linear Fit
            Xrng <- range(Object[[coreline]]@RegionToFit$x)
            Yrng <- range(Object[[coreline]]@RegionToFit$y)
            plot(Object[[coreline]], xlim=Xrng, ylim=Yrng)  #plot confined in the original X, Y range
            if (length(point.coords$x) > 0 && VBtEstim == FALSE) { #Points defining the 2 regions for the linear fit
                points(point.coords, col="green", cex=1.2, lwd=2, pch=3)
            }
            if (VBtEstim == TRUE) {     #Point defining the intercept of the two linear fit
                points(point.coords$x, point.coords$y, col="orange", cex=3, lwd=2, pch=3)
            }
        } else if ((tab1 == 2) && (tab2==2) ){ ### tab VB Fit, NON-Linear Fit
            if (svalue(plotFit) == "residual" && hasFit(Object[[coreline]])) {
                XPSresidualPlot(Object[[coreline]])
            }
            if (VBtEstim == FALSE){
                plot(Object[[coreline]])
                points(point.coords, col="green", cex=1.2, lwd=2, pch=3)       #plots the point where to add the component
            }
            if (VBtEstim == TRUE) {
                plot(Object[[coreline]])
                points(point.coords, col="orange", cex=3, lwd=2, pch=3)  #plots the VB top
            }
        } else if ((tab1 == 2) && (tab2==3) ){ ### tab VB Fit, Hill Sigmoid Fit
            if (svalue(plotFit) == "residual" && hasFit(Object[[coreline]])) {
                XPSresidualPlot(Object[[coreline]])
            }
            if (VBtEstim == FALSE){
                plot(Object[[coreline]])
                points(point.coords, col="green", cex=1.2, lwd=2, pch=3)       #plots the point where to add the component
            }
            if (VBtEstim == TRUE) {
                plot(Object[[coreline]])
                points(point.coords, col="orange", cex=3, lwd=2, pch=3)    #plots the VB top
            }
        }
     }
  }

  LoadCoreLine <- function(h, ...){
     Object_name <- get("activeFName", envir=.GlobalEnv)
     Object <<- get(Object_name, envir=.GlobalEnv)  #load the XPSSample from the .Global Environment
     ComponentList <<- names(slot(Object[[coreline]],"Components"))
     if (length(ComponentList)==0) {
         gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
         return()
     }
     replot()   #replot spectrum of the selected component
  }


  set.coreline <- function(h, ...) {
     CL <- svalue(Core.Lines)
     CL <- unlist(strsplit(CL, "\\."))   #select the NUMBER. before the CoreLine name
     coreline <<- as.integer(CL[1])

     if (coreline == 0) {    #coreline == "All spectra"
         svalue(plotFit) <- "normal"
         enabled(T1group1) <- FALSE  #block NB-baseline  tab
         enabled(T2group1) <- FALSE  #block NB-components tab
         plot(Object)
         gmessage(msg="Please select a Valence Band spectrum", title="WRONG SPECTRUM", icon="error")
         return()
     } else {
         if (length(Object[[coreline]]@Components) > 0) {
             gmessage(msg="Analysis already present on this Coreline!", title = "WARNING: Analysis Done",  icon = "warning")
             return()
         }
         VBtEstim <<- FALSE
         enabled(T1group1) <- TRUE   #enable NB-baseline
         enabled(OK_btn1) <- TRUE
         enabled(OK_btn2) <- FALSE

# Now computes the VB integral needed for the VBtop estimation by NON-Linear Fit
# By default a Shirley baseline is defined on the whole VB
         if (length(Object[[coreline]]@Baseline$x) != 0 ) {
             reset.baseline() 
         }
         Object[[coreline]]@RSF <<- 0 #set the VB sensitivity factor to zero to avoid error wornings

# reset zoom
         svalue(baseline.zoom) <- FALSE
# if boundaries already defined
         if (hasBoundaries(Object[[coreline]])) {
             point.coords <<- slot(Object[[coreline]],"Boundaries")
         } else {
             reset.baseline()
         }
# enable notebook pages
         if (hasBaseline(Object[[coreline]]) ) {
             svalue(nbMain) <- 1
         }
         if (hasComponents(Object[[coreline]]) ) {
             if (VBbkgOK==TRUE) {enabled(T2group1) <- TRUE}   #enable VB-fit tab
             svalue(nbMain) <- 2
             svalue(nbVBfit) <- 1
         }
     }
     ObjectBKP <<- Object[[coreline]]
     svalue(nbMain) <- 1 #when a coreline is selected, Baseline NB oage is selected
     replot()
  }


  MakeBaseline <- function(deg,splinePoints, ...){
     if ( coreline != 0 && hasBoundaries(Object[[coreline]]) ) {
        Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
        Object[[coreline]] <<- XPSbaseline(Object[[coreline]], BType, deg, splinePoints )
        Object[[coreline]] <<- XPSsetRSF(Object[[coreline]])
        if (VBbkgOK==TRUE && VBlimOK==TRUE) {enabled(T2group1) <- TRUE}   #abilito VB-fit tab
        replot()
     }
     svalue(nbVBfit) <- 1 #quando seleziono una coreline mi metto sulla pagina Add/Delete Components
  }


  reset.baseline <- function(h, ...) {
     if (coreline != 0) {   #coreline != "All spectra"
         LL <- length(Object[[coreline]]@.Data[[1]])
         if (BType == "Shirley"){
            Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
            Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
            point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
            point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
            Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
            Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "Shirley", deg, splinePoints )
            VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points
         }
         if (BType == "linear"){
            Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
            Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][LL], Object[[coreline]]@.Data[[2]][LL])
            point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
            point.coords$y <<- c(Object[[coreline]]@.Data[[2]][LL], Object[[coreline]]@.Data[[2]][LL])
            Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
            Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "linear", deg, splinePoints )
         }
         enabled(T2group1) <<- FALSE
     }
  }


  update.outputArea <- function(...) {
     coreline <<- svalue(Core.Lines)
     coreline <<- unlist(strsplit(coreline, "\\."))   #drops the NUMBER. before the CoreLine name
     coreline <<- as.integer(coreline[1])
  }


#--- Functions, Fit and VB_Top estimation

  reset.LinRegions <- function(h, ...) {
     point.coords <<- list(x=NULL, y=NULL)
     Object[[coreline]]@Components <<- list()
     Object[[coreline]]@Fit <<- list()
     replot()
  }


  add.FitFunct <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     tab2 <- svalue(nbVBfit)
     if (coreline != 0 && hasBaseline(Object[[coreline]])) {
         Xrange <- Object[[coreline]]@Boundaries$x
         Sigma <- abs(Xrange[2]-Xrange[1])/7
         if (!is.null(point.coords$x[1]) && tab2==2 ) {   #NON-Linear Fit
#Fit parameter are set in XPSAddComponent()
             Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = svalue(Fit.type),
                                             peakPosition = list(x = point.coords$x, y = point.coords$y), sigma=Sigma)
## to update fit remove Component@Fit and make the sum of Component@ycoor including the newone
             tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))  #create a matrix formed by ycoor of all the fit Components
             CompNames <<- names(Object[[coreline]]@Components)
             Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y)) #Subtract NComp*Baseline because for each Component a baseline was added
             point.coords <<- list(x=NULL,y=NULL)
             replot()
         }
         if (!is.null(point.coords$x[1]) && tab2==3 ) { #Hill Sigmoid Fit
#Fit parameter are set in XPSAddComponent()
             if(length(point.coords$x) > 3){
                gmessage("Attention: more than the Max, Flex, Min points were defined. Only the first three points will be taken", title="WARNING", icon="warning")
                point.coords$x <<- point.coords$x[1:3]
                point.coords$y <<- point.coords$y[1:3]
             }
             if(Object[[coreline]]@Flags[[2]] == TRUE){ #Binding energy
                idx <- order(point.coords$x, decreasing=TRUE)
                point.coords$x <<- point.coords$x[idx]  #point.coords could be entered in sparse order
                point.coords$y <<- point.coords$y[idx]  #here we will have MAX, FLEX, MIN positions
             } else {                                   #Kinetic energy
                idx <- order(point.coords$x, decreasing=FALSE)
                point.coords$x <<- point.coords$x[idx]
                point.coords$y <<- point.coords$y[idx]
             }
             Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "HillSigmoid",
                                             peakPosition = list(x = point.coords$x, y = point.coords$y), ...)
             Object[[coreline]]@Fit$y <<- Object[[coreline]]@Components[[1]]@ycoor-Object[[coreline]]@Baseline$y #subtract the Baseline
             point.coords <<- list(x=NULL,y=NULL)
             Object[[coreline]]@RegionToFit$x <- ObjectBKP@RegionToFit$x #restore original abscissas changed in XPSAddComponent()
             replot()
         }
     }
  }


  del.FitFunct <- function(h, ...) {  #title="DELETE COMPONENT KILLS CONSTRAINTS!!!"
     ObjectBKP <<- Object[[coreline]]
     if (gconfirm(msg="Deleting fit function. Are you sure you want to proceed?", title="DELETE", icon="warning")) {
         LL<-length(Object[[coreline]]@Components)
         for (ii in 1:LL) { #Rimuovo tutti i CONSTRAINTS
              Object[[coreline]] <<- XPSConstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
         }
         if (coreline != 0 && hasComponents(Object[[coreline]])) {
             txt <- c("Select the fit component to delete")
             delWin <- gwindow("DELETE", parent = c(50,10), visible = FALSE)
             g <- gvbox(container=delWin); g$set_borderwidth(10L)
             glabel(txt, container=g)
             gseparator(container=g)
             compIdx <- gcombobox(c(names(slot(Object[[coreline]],"Components")),"All"), selected=1, container = g, handler = NULL)
             bg <- ggroup(container=g); addSpring(bg)
             gbutton("OK", container=bg, handler=function(...){
                     if (svalue(compIdx) != "All"){
                         indx <- as.numeric(svalue(compIdx, index=TRUE))
                         Object[[coreline]] <<- XPSremove(Object[[coreline]], what="components", number=indx )
                         if (length(Object[[coreline]]@Components) > 0 ) {
                             #to update the plot:
                             tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
                             Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                         }
                     } else {
                         Object[[coreline]] <<- XPSremove(Object[[coreline]], "components")
                     }
                     svalue(plotFit) <- "normal"
                     point.coords <<- list(x=NULL,y=NULL)
                     replot()
                     dispose(delWin)
             } )
             gbutton("Cancel", container=bg, handler=function(...) dispose(delWin))
             visible(delWin) <- TRUE
         }
     }
  }


  Edit.FitParam <- function(h, ...) { #Edit Fit parameters to set constraints on fit components
     FitParam <- NULL
     newFitParam <- NULL
     indx <- NULL

     EditWin <- gwindow("EDIT", parent = c(50,10), visible = FALSE)
     size(EditWin) <- c(450, 230)
     EditGroup1 <- ggroup(horizontal = FALSE, container = EditWin)
     Editframe1 <- gframe(" Select the Function To Edit", spacing=5, container=EditGroup1)
     Editframe2 <- gframe(" EDIT FIT PARAMETERS ", horizontal=FALSE, container=EditGroup1)
     EditGroup2 <- ggroup(horizontal = FALSE, container = Editframe2)
     DFrame <- gdf(items=NULL, container=EditGroup2) #DFrame e' il puntatore a gdf()
     size(DFrame) <-c (400,150)
     compIndx <- gcombobox(c(names(slot(Object[[coreline]],"Components"))), selected=-1, , handler = function(h, ...){
                          indx <<- as.numeric(svalue(compIndx, index=TRUE))
                          FitParam <-Object[[coreline]]@Components[[indx]]@param #Load parameters in a Dataframe correspondent to the selected coreline
                          VarNames <- rownames(FitParam)  #extract parameter names
                          FitParam <- as.matrix(FitParam) #transform the dataframe in a marix
                          FitParam <<- data.frame(cbind(VarNames,FitParam), stringsAsFactors=FALSE)  #add varnames in the first column of the paramMatrix and make resave data in a Dataframe to enable editing
                          newFitParam <<- FitParam
                          delete(EditGroup2, DFrame)
                          DFrame <<- gdf(items=FitParam, container=EditGroup2) #DFrame points to gdf()
                          size(DFrame) <-c (400,150)
                          addHandlerChanged(DFrame, handler=function(h,...){ #addHandlerChanged load the modified dataframe in NewFirParam
                                           newFitParam <<- h$obj[]
                          })
                 }, container = Editframe1)

     gbutton("     SAVE      ", handler=function(h,...){
                #Now drop the added Param Names columns and transform char to num
                newFitParam <- lapply(newFitParam[,1:ncol(newFitParam)], function(x) {as.numeric(x)} ) #in dataframe data are characters
                FitParam <- FitParam[,-1]   #drop the column with param Names
                FitParam[, 1:ncol(FitParam)] <- newFitParam   #with this assignment is maintaned the class(fitParam)=data.base needed to save parameters in the relative CoreLine slot
                Object[[coreline]]@Components[[indx]]@param <<- FitParam #Load modified parameters in the relative CoreLine slot
                delete(EditGroup2, DFrame)
                DFrame <<- gdf(items=NULL, container=EditGroup2)
                size(DFrame) <- c(400,150)
                FitParam <<- NULL
                newFitParam <<- NULL
             }, container = Editframe2)

     gbutton("     EXIT      ", handler=function(h,...){
                dispose(EditWin)
             }, container = EditGroup1)
     visible(EditWin) <- TRUE
  }


  MakeFit <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     FitRes <- NULL
     tab1 <- svalue(nbMain)
     tab2 <- svalue(nbVBfit)

     if (coreline != 0 && tab2==1) {  #VB Linear Fit
         if (length(point.coords$x)<4) {
             gmessage(msg="4 points are needed for two Linear fits: please complete!", title = "WARNING: region limits lacking",  icon = "warning")
             return()
         }
         ###First Linear fit considered as component to compute the VB Top
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "Linear",
                                             peakPosition = list(x = NA, y = NA), ...)
         #restrict the RegionToFit to the FIRST rengion selected with mouse for the linear fit
         idx1 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[1]) #Inside object@RegionToFit$x extract the region between selected points: limit1
         idx2 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[2]) #Inside object@RegionToFit$x extract the region between selected points: limit2
         tmp <- sort(c(idx1, idx2), decreasing=FALSE)   #maybe the definition of the fit region is from low to high BE
         idx1 <- tmp[1]
         idx2 <- tmp[2]
         X <- Object[[coreline]]@RegionToFit$x[idx1:idx2]
         Y <- Object[[coreline]]@RegionToFit$y[idx1:idx2]
         YpltLim <- max(range(Object[[coreline]]@RegionToFit$y))/5
         #Linear Fit
         Fit1 <- FitLin(X,Y)  #Linear Fit returns c(m, c) (see XPSUtilities.r)
         LL <- length(Object[[coreline]]@RegionToFit$x)
         for(ii in 1:LL){
             FitRes[ii] <- Fit1[1]*Object[[coreline]]@RegionToFit$x[ii]+Fit1[2]
             if (FitRes[ii] < -YpltLim) { FitRes[ii] <- NA  }   #to limit the Yrange to positive values in the plots
         }
         #store fit1 values
         Object[[coreline]]@Components[[1]]@param["m", "start"] <<- Fit1[1]
         Object[[coreline]]@Components[[1]]@param["c", "start"] <<- Fit1[2]
         Object[[coreline]]@Components[[1]]@ycoor <<- FitRes #-Object[[coreline]]@Baseline$y   #Baseline has to be subtracted to match the orig. data

         ###Second Linear fit considered as component to compute the VB Top
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "Linear",
                                             peakPosition = list(x = NA, y = NA), ...)

         #restrict the RegionToFit to the SECOND rengion selected with mouse for the linear fit
         idx1 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[3]) #All-interno di object@RegionToFit$x estraggo la regione selezionata per i fit lineare: estremo1
         idx2 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[4]) #All-interno di object@RegionToFit$x estraggo la regione selezionata per i fit lineare: estremo2
         tmp <- sort(c(idx1, idx2), decreasing=FALSE)   #maybe the definition of the fit region is from low to high BE
         idx1 <- tmp[1]
         idx2 <- tmp[2]

         X <- Object[[coreline]]@RegionToFit$x[idx1:idx2]
         Y <- Object[[coreline]]@RegionToFit$y[idx1:idx2]
         #Linear Fit
         Fit2 <- FitLin(X,Y)  #Linear Fit returns c(m, c) (see XPSUtilities.r)
         LL <- length(Object[[coreline]]@RegionToFit$x)
         for(ii in 1:LL){
            FitRes[ii] <- Fit2[1]*Object[[coreline]]@RegionToFit$x[ii]+Fit2[2]
            if (FitRes[ii] < -YpltLim) { FitRes[ii] <- NA  }   #to limit the Yrange to positive values in the plots
         }
         #store  fit2 values
         Object[[coreline]]@Components[[2]]@param["m", "start"] <<- Fit2[1]
         Object[[coreline]]@Components[[2]]@param["c", "start"] <<- Fit2[2]
         Object[[coreline]]@Components[[2]]@ycoor <<- FitRes #-Object[[coreline]]@Baseline$y   #Baseline has to be subtracted to match the orig. data
         Object[[coreline]]@Fit$y <- FitRes
         replot()   #plot of the two linear fits
     }
     if (coreline != 0 && tab2==2) {  #VB NON-Linear Fit
         if (reset.fit==FALSE){
             NComp <- length(Object[[coreline]]@Components)
             for(ii in 1:NComp){
                 Object[[coreline]]@Components[[ii]]@param["sigma", "min"] <- 0.5 #limits the lower limit of component FWHM
             }
             Xbkp <- Object[[coreline]]@RegionToFit$x  #save the original X coords = RegionToFit$x
#Fit parameter are set in XPSAddComponent()
             Object[[coreline]] <<- XPSFitLM(Object[[coreline]], plt=FALSE, verbose=FALSE)  #Lev.Marq. fit returns all info stored in Object[[coreline]]
             Object[[coreline]]@RegionToFit$x <<- Xbkp
             replot()
         } else if (reset.fit==TRUE){
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"fit")
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"components")
             reset.fit <<- FALSE
             replot()
         }
     }
     if (coreline != 0 && tab2==3) {  #VB Hill Sigmoid
         if (reset.fit==FALSE){
#Fit parameter and new X coords are set in XPSAddComponent()
#HillSigmoid was defined using the new X coords
#New X coords must be used also for the fit

             LL <- length(Object[[coreline]]@RegionToFit$x)
             dx <- (Object[[coreline]]@RegionToFit$x[2]-Object[[coreline]]@RegionToFit$x[1])
             Xbkp <- Object[[coreline]]@RegionToFit$x  #save the original X coords

#Hill sigmoid defined only for positive X abscissas. Then (i)generate a temporary X array
#(ii)generate the  Hill sigmoid. (iii)Perform fitting (iv)restore the original X values
#compute the HillSigmoid position MU on the original abscissas
             Object[[coreline]]@RegionToFit$x <<- Object[[coreline]]@Fit$x  #Set sigmoid Xcoords as modified in XPSAddFitComp()

             Object[[coreline]] <<- XPSFitLM(Object[[coreline]], plt=FALSE, verbose=FALSE)   #Lev.Marq. fit returns all info stored in Object[[coreline]]
             FlexPos <- Object[[coreline]]@Components[[1]]@param[2,1] #MU = fitted flex position HillSigmoid position
             Object[[coreline]]@Fit$idx <<- FlexPos
#temporary abscissa X = seq(1,lenght(RegToFit))
#Observe that in the temporary abscissaa, each X represents both the value and the X-index
#FlexPos*dx represents how many dx are needed to reach MU starting form X[1]

             Object[[coreline]]@RegionToFit$x <<- Xbkp  #restore the original X coods
#now compute the HS position on the original X abscissas
             FlexPos <- Object[[coreline]]@RegionToFit$x[1]+dx*(FlexPos-1)
             Object[[coreline]]@Components[[1]]@param[2,1] <<- FlexPos #save Hill Sigmoid position
             replot()
         } else if (reset.fit==TRUE){
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"fit")
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"components")
             reset.fit <<- FALSE
             replot()
         }
     }
  }

  CalcVBTop <- function(h, ...) {
     tab1 <- svalue(nbMain)
     tab2 <- svalue(nbVBfit)
     if ((tab1 == 2) && (tab2==1) ){ ##VB Fit tab, Linear Fit
         #recover linear fit1, fit2 parameters
         Fit2 <- Fit1 <- c(NULL, NULL)
         Fit1[1] <- Object[[coreline]]@Components[[1]]@param["m", "start"]
         Fit1[2] <- Object[[coreline]]@Components[[1]]@param["c", "start"]
         Fit2[1] <- Object[[coreline]]@Components[[2]]@param["m", "start"]
         Fit2[2] <- Object[[coreline]]@Components[[2]]@param["c", "start"]
         #Fit intersection occurs at x==
         VBtopX <- (Fit2[2]-Fit1[2])/(Fit1[1]-Fit2[1])
         idx1 <- findXIndex(Object[[coreline]]@RegionToFit$x,VBtopX)
         #estimation the value of VB corresponding to VBtopX:
         dX <- Object[[coreline]]@RegionToFit$x[idx1+1]-Object[[coreline]]@RegionToFit$x[idx1]
         dY <- Object[[coreline]]@RegionToFit$y[idx1+1]-Object[[coreline]]@RegionToFit$y[idx1]
         #VBtopX falls between RegToFit[idx1] and RegToFit[idx+1]: VBtopY found through proportionality relation
         VBtopY <- dY*(VBtopX-Object[[coreline]]@RegionToFit$x[idx1])/dX+Object[[coreline]]@RegionToFit$y[idx1]
         point.coords$x <<- VBtopX
         point.coords$y <<- VBtopY
         VBtopX <- round(VBtopX, 3)
         VBtopY <- round(VBtopY, 3)

         svalue(sb) <- txt <- paste("Estimated position of VB top:", VBtopX, VBtopY, sep="  ")
         cat("\n",txt)
         #creation of component3 of type VBtop to store VBtop position
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBtop", ...)
         LL <- length(Object[[coreline]]@Baseline$x)
         #VBtop is stored in component3  param mu
         Object[[coreline]]@Components[[3]]@param["mu", "start"] <<- VBtopX
         Object[[coreline]]@Components[[3]]@param["h", "start"] <<- VBtopY
         Object[[coreline]]@Info <<- paste("   ::: VBtop: x=", VBtopX,"  y=", VBtopY, sep="")
     }
     if ((tab1 == 2) && (tab2==2) ){ #VB Fit tab, NON-Linear Fit
         VBTop <<- TRUE #set the VBTop graphic mode (see draw.plot()
         if (length(Object[[coreline]]@Fit)==0 ) { #No fit present: Object[[coreline]]@Fit$y is lacking
             gmessage(msg="VB NON-Linear Fitting is missing!", title = "WARNING: VB NON-Linear FIT",  icon = "warning")
             return()
         } else if ( coreline != 0 && hasComponents(Object[[coreline]]) ) {
         ## Control on the extension of the VB above the Fermi

#             VBtresh <<- VBintg/5   #define a treshold for VBtop estimation
             VBtresh <<- (max(Object[[coreline]]@Fit$y)-min(Object[[coreline]]@Fit$y))/10
             LL <- length(Object[[coreline]]@Fit$y)
             for(idxTop in LL:1){ #scan the VBfit to find where the spectrum crosses the threshold
                if (Object[[coreline]]@Fit$y[idxTop] >= VBtresh) break
             }
             VBtopX <- Object[[coreline]]@RegionToFit$x[idxTop]  #abscissa from Region to Fit
             VBtopY <- Object[[coreline]]@RegionToFit$y[idxTop]  #ordinata from Fit
             point.coords$x <<- VBtopX
             point.coords$y <<- VBtopY
             replot()
             VBTop <<- FALSE
             VBtopX <- round(VBtopX, 3)
             VBtopY <- round(VBtopY, 3)
             svalue(sb) <- txt <- paste("Estimated position of VB top:", VBtopX, VBtopY, sep="  ")
             cat("\n",txt)
             # now add a component to store VBtop Position in param mu
             Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBtop", ...)
             LL <- length(Object[[coreline]]@Components)
             Object[[coreline]]@Components[[LL]]@param["mu", "start"] <<- VBtopX  # VBtop stored in param "mu"
             Object[[coreline]]@Components[[LL]]@param["h", "start"] <<- VBtopY  # VBtop stored in param "mu"
             Object[[coreline]]@Info <<- paste("   ::: VBtop: x=", VBtopX,"  y=", VBtopY, sep="")
         }
     }
     if ((tab1 == 2) && (tab2==3) ){ #VB Fit tab, Hill Sigmoid Fit
         LL <- length(Object)
         dx <- (Object[[coreline]]@RegionToFit$x[2]-Object[[coreline]]@RegionToFit$x[1])
         VBTop <<- TRUE      #set the VBTop graphic mode (see draw.plot()
         mu <- Object[[coreline]]@Components[[1]]@param[2,1]
         pow <- Object[[coreline]]@Components[[1]]@param[3,1]
         A <- Object[[coreline]]@Components[[1]]@param[4,1]
         B <- Object[[coreline]]@Components[[1]]@param[5,1]
         TmpMu <- Object[[coreline]]@Fit$idx   #MU position on the temporary X scale
#Now computes MU*(1-2/pow) = knee position of the HS curve. See Bartali et al Mater Int. (2014), 24, 287
#This position has to be computed on the temporary X scale (is positive and increasing)
         TmpVtop <- TmpMu*(1+2/pow)            #bottom knee position of the Hill sigmoid
         idx <- as.integer(TmpVtop)
         bgnd <- Object[[coreline]]@Baseline$y[idx] #baseline value at the TmpVtop point
         VBtopX <- Object[[coreline]]@RegionToFit$x[1]+dx*(TmpVtop-1) #knee position on the original scale
         VBtopY <- A - A*TmpVtop^pow/(TmpMu^pow + TmpVtop^pow)+bgnd   #ordinate correspondent to TmpVtop
         point.coords$x <<- VBtopX
         point.coords$y <<- VBtopY
         replot()
         VBTop <<- FALSE
         VBtopX <- round(VBtopX, 3)
         VBtopY <- round(VBtopY, 3)
         svalue(sb) <- txt <- paste("Estimated position of VB top:", VBtopX, VBtopY, sep="  ")
         # now add a component to store VBtop Position in param mu
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBtop", ...)
         LL <- length(Object[[coreline]]@Components)
         Object[[coreline]]@Components[[LL]]@param["mu", "start"] <<- VBtopX  # VBtop stored in param "mu"
         Object[[coreline]]@Components[[LL]]@param["h", "start"] <<- VBtopY  # VBtop stored in param "mu"
         Object[[coreline]]@Info <<- paste("   ::: VBtop: x=", VBtopX,"  y=", VBtopY, sep="")      
      }
      VBtEstim <<- TRUE
      replot()
  }
#----Set Default Variable Values

  ResetVars <- function(){
     Object[[coreline]] <<- XPSremove(Object[[coreline]],"all")

     LL <- length(Object[[coreline]]@.Data[[1]])
     Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
     Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
     Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
     Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "Shirley", deg, splinePoints )
     VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points

     VBbkgOK <<- FALSE
     VBlimOK <<- FALSE
     VBTop <<- FALSE
     VBtEstim <<- FALSE
     BType <<- "Shirley"
     LinFit <<- FALSE
     VBintg <<- NULL    #BKG subtracted VB integral
     CompNames <<- "   "
     reset.fit <<- FALSE
     MarkSym <<- 10
     SymSiz <<- 1.8
     point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
     point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
     svalue(sb) <<- "Estimated position of VB top : "
     svalue(nbMain) <<- 1
     enabled(T2group1) <<- FALSE
     enabled(OK_btn1) <<- TRUE
     enabled(OK_btn2) <<- FALSE
  }

#=====VARIABLES==================

  if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
  }

  Object <- get(activeFName,envir=.GlobalEnv)  #this is the XPS Sample
  Object_name <- get("activeFName", envir = .GlobalEnv) #XPSSample name
  ObjectBKP <- NULL   #CoreLine bkp to enable undo operation
  FNameList <- XPSFNameList() #list of XPSSamples
  SpectList <- XPSSpectList(activeFName) #list of XPSSample Corelines
  point.coords <- list(x=NULL, y=NULL)
  coreline <- 0
  plot_win <- as.numeric(get("XPSSettings", envir=.GlobalEnv)$General[4]) #the plot window dimension
  coords <- NA # for printing mouse coordinates on the plot
  deg <- 1 #per default setto a 1 il grado del polinomio per Baseline
  BType <- "Shirley" #defaul BKground
  VBbkgOK <- FALSE
  VBlimOK <- FALSE
  VBTop <- FALSE
  VBtEstim <- FALSE
  LinFit <- FALSE
  VBintg <- NULL    #BKG subtracted VB integral
  FitFunct <- c("Gauss", "Voigt", "ExpDecay", "PowerDecay", "Sigmoid")
  CompNames <- "   "
  reset.fit <- FALSE
  MarkSym <- 10
  SymSiz <- 1.8

  WinSize <- as.numeric(XPSSettings$General[4])


#====== Widget definition =======

  VBwindow <- gwindow("XPS VB Top GUI", parent=c(50, 10), visible = FALSE)
  addHandlerDestroy(VBwindow, handler=function(h, ...){  #if MainWindow unproperly closed with X
                                 EXIT <<- TRUE
                                 XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                                 assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                                 plot(Object) #replot the CoreLine
                           })
  VBGroup <- ggroup(container = VBwindow, horizontal = TRUE)

  ## Core lines
  MainGroup <- ggroup(expand = FALSE, horizontal = FALSE, spacing = 5, container = VBGroup)

  SelectFrame <- gframe(text = " XPS Sample and Core line Selection ",horizontal = TRUE, container = MainGroup)
  XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                                 activeFName <- svalue(XPS.Sample)
                                 Object <<- get(activeFName, envir=.GlobalEnv)
                                 Object_name <<- activeFName
                                 SpectList <<- XPSSpectList(activeFName)
                                 compIndx <<- grep("VB", SpectList)
                                 delete(SelectFrame, Core.Lines)
                                 Core.Lines <<- gcombobox(c("0.All spectra", SpectList), selected=1, handler = set.coreline, container = SelectFrame)
                                 coreline <<- 0
                                 VBbkgOK <<- FALSE
                                 VBlimOK <<- FALSE
                                 BType <<- "Shirley"
                                 reset.baseline()
                                 enabled(T2group1) <<- FALSE
                                 enabled(OK_btn2) <<- FALSE
                                 replot()
                       }, container = SelectFrame)
  svalue(XPS.Sample) <- activeFName

  Core.Lines <- gcombobox(c("0.All spectra", SpectList), selected=1, handler = set.coreline, container = SelectFrame)

#===== Notebook==================
  nbMain <- gnotebook(container = MainGroup, expand = FALSE)
  size(nbMain) <- c(400, 430)

#----- TAB1: Baseline -----
  T1group1 <- ggroup(label = "Baseline", horizontal=FALSE, container = nbMain)

  T1Frame1 <- gframe(text = " Processing ", horizontal=FALSE, container = T1group1)

  T1Frame2 <- gframe(text = " WARNING! ", horizontal=FALSE, container = T1Frame1)
  WarnLab1 <- glabel("Check the Shirley BKG and set it properly below the WHOLE VB", container = T1Frame2)
  WarnLab2 <- glabel("Modify BKG Markers and press 'Define the VB Integral'", container = T1Frame2)
  T1group2 <- ggroup(horizontal=TRUE, spacing = 15, container = T1Frame2)
  OK_btn1 <- gbutton(" Set the Baseline ", handler = function(h, ...) {
                   svalue(WarnLab1) <- "Set the EXTENSION and LEVEL of the Background \nto Select the VBtop Region and Define the VB Integral"
                   svalue(WarnLab2) <- "Then press \n'Define the VB region proximal to the Fermi Edge' to proceed"
                   LL <- length(Object[[coreline]]@.Data[[1]])
                   Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
                   Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
                   Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
                   Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "Shirley", deg, splinePoints )
                   VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points
                   gmessage("Please set the background ends. \nAlways Left Button to Enter Positions Right Button to Stop", title="WARNING", icon="warning")
                   cat("\n Please set the background ends to define the VB integral ")
                   GetCurPos(SingClick=FALSE)   #activates locator to define the edges of the Baseline for VB background subtraction

                   VBbkgOK <<- TRUE
                   BType <<- "linear"
                   reset.baseline()  #reset baseline from Shirley to linear BKG
                   MarkSym <<- 9
                   SymSiz <<- 1.5
                   enabled(OK_btn2) <<- TRUE
                   enabled(OK_btn1) <- FALSE
                   replot()
                }, container = T1group2)

  Reset_Btn11 <- gbutton(" Reset Baseline ", handler = function(h, ...) {
                   reset.baseline()
                   ResetVars()
                   MarkSym <<- 10
                   SymSiz <<- 1.8
                   replot()
                }, container = T1group2)
  addSpring(T1group2)

  T1Frame3 <- gframe(text = "DEFINE THE ANALYSIS REGION", horizontal=FALSE, container = T1Frame1)
  OK_btn2 <- gbutton("Define VB region proximal to the Fermi Edge", handler = function(h, ...) {
                   svalue(WarnLab1) <- "Set the Extension of the VB-portion analyze /nin Proximity of the Fermi Level"
                   svalue(WarnLab2) <- "Extension of the VB must allow fitting the \n descending tail towards 0 eV"
                   GetCurPos(SingClick=FALSE)   #Activates the locator to define the region proximal to the Fermi
                   slot(Object[[coreline]],"Boundaries") <<- point.coords
                   Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
                   VBlimOK <<- TRUE
                   point.coords <<- list(x=NULL, y=NULL)
                   enabled(T2group1) <- TRUE
                   ObjectBKP <<- Object[[coreline]]
                   enabled(OK_btn2) <- FALSE
                   replot()
                   svalue(nbMain) <- 2     #switch to the secvond page
                }, container = T1Frame3)


  gwin23 <- gframe(text = " Plot ", container = T1Frame1)
  baseline.zoom <- gcheckbox("zoom Y scale", checked=FALSE, container=gwin23, handler= replot )


#----- TAB2: Fit Functions -----
  T2group1 <- ggroup(label = "VB Fit", horizontal=FALSE, container = nbMain)


  ## plot type : Residual or simple
  T2Frame1 <- gframe(text = " Plot ", spacing=1, container = T2group1)
  plotFit <- gradio(items=c("normal", "residual"), selected=1,  expand=TRUE, horizontal = TRUE, handler = replot, container = T2Frame1)

  nbVBfit <- gnotebook(container = T2group1, expand = FALSE)
  T2group2 <- ggroup( horizontal=FALSE, label = " Linear Fit ", container = nbVBfit)
  T2group3 <- ggroup( horizontal=FALSE, label = " NON-Linear Fit ", container = nbVBfit)
  T2group4 <- ggroup( horizontal=FALSE, label = " Hill Sigmoid Fit ", container = nbVBfit)


#----- Linear Fit subtab

  T21Frame1 <- gframe(text = " Linear Fit Regions ", horizontal=FALSE, container = T2group2)
  T21group1 <- ggroup( horizontal=TRUE, container = T21Frame1)

  Lbl1 <- glabel("Left Mouse Butt. to Set Edges Right to Stop    ", container=T21group1)
  font(Lbl1) <- list(family="sans", size=11)

  Hlp21_btn1 <- gbutton("?", handler = function(h, ...){
                              txt <- paste("Two regions must to be defined to perform the linear fits: \n",
                                        "the first on the descending tail near to the Fermi edge and \n",
                                        "the second on the flat background. Using the left mouse button,\n",
                                        "define the two edges of the first and the second regions.\n",
                                        "Green crosses will indicate the region boundaries. Then press the\n",
                                        "button FIT and a linear fit will be performed in the selected\n",
                                        "regions. Press ESTIMATE VB TOP button to obtain the abscissa\n",
                                        "of to the fit intersection which is taken as position of the VBtop.")
                              gmessage(msg=txt,icon="info")
                           }, container = T21group1 )

  SetPts21_Btn1 <- gbutton("Set Linear Region Edges", expand=FALSE, handler = function(h, ...){
                              GetCurPos(SingClick=FALSE)
                           }, container = T21Frame1 )
  Reset21_Btn1 <- gbutton("Reset Fit Regions", expand=FALSE, handler = reset.LinRegions, container = T21Frame1 )

  Fit21_btn1 <- gbutton("Fit", expand=FALSE, handler = MakeFit, container = T21Frame1 )

  VBTop21_btn1 <- gbutton("Estimate VB Top", expand=FALSE, handler = CalcVBTop, container = T21Frame1 )

  Reset21_Btn2 <- gbutton("Reset Analysis ", expand=FALSE, handler = function(h, ...){
                              LL <- length(Object[[coreline]]@.Data[[1]])
                              Object[[coreline]] <<- ObjectBKP
                              point.coords <<- list(x=NULL, y=NULL)
                              VBTop <<- FALSE
                              MarkSym <<- 10
                              SymSiz <<- 1.8
#                              svalue(VBlbl1) <- "Estimated position of VB top : "
                              svalue(sb) <- "Estimated position of VB top : "
                              replot()
                           }, container = T21Frame1 )

  Reset21_btn3 <- gbutton("Reset All", expand=FALSE, handler = function(h, ...){
                              ResetVars()
                              enabled(OK_btn1) <- TRUE
                              enabled(OK_btn2) <- FALSE
                              replot()
                           }, container = T21Frame1 )

  glabel("   ", container=T21Frame1)



#----- NON-Linear Fit subtab

  T22Frame1 <- gframe(text = " Fit Components ", container = T2group3)
  T22group1 <- ggroup( horizontal=TRUE, container = T22Frame1)
  Fit.type <- gcombobox(FitFunct, selected = 1, handler = function(h, ...){
                              svalue(sb) <- sprintf("Selected component type %s", svalue(h$obj))
                           }, container = T22group1 )
  Lbl2 <- glabel("Left Mouse Butt to Add Fit Comp. Right to Stop  ", container=T22group1)
  font(Lbl2) <- list(family="sans", size=11)

  Hlp22_btn1 <- gbutton("?", expand=FALSE, handler = function(h, ...){
                              txt <- paste("The idea is to use the fit of the descending tail of the VB to \n",
                                       "get rid from noise and obtain a better estimate the VBtop.\n",
                                       "First select the desired component lineshape (Gaussian is suggested)\n",
                                       "Then click with the left mouse button in the positions to add fit components\n",
                                       "Click with the right mouse button to stop adding fit components\n",
                                       "Press DELETE FIT COMPONENT to delete a reduntant fit component\n",
                                       "Press RESET FIT to restart the procedure.\n",
                                       "Add as many components as needed to model the VB in the defined region\n",
                                       "Press the FIT button to make the fit which must correctly reproduce the VB tail\n",
                                       "Pressing the ESTIMATE VB TOP button, a predefined treshold based on the VB \n",
                                       "  integral intensity, is the utilized to estimate the VB top position \n",
                                       "Pressing the RESET ALL button one resets the whole analysis and restarts from very beginning")
                              gmessage(msg=txt,icon="info")
                           }, container = T22group1 )
  tkconfigure(Hlp22_btn1$widget, width=5)

  T22Frame3 <- gframe(text = " Options ", horizontal=FALSE, spacing=1, container = T2group3)

  add22_btn1 <- gbutton("Add Fit Component", spacing=1, handler = function(h, ...){
                              GetCurPos(SingClick=FALSE)
                           }, container = T22Frame3)

  del22_btn1 <- gbutton("Delete Component", spacing=1, handler = del.FitFunct, container = T22Frame3 )

#  edit_btn2 <- gbutton("Edit Fit Parameters", spacing=1, handler = Edit.FitParam, container = T22Frame3 )

  Fit22_btn1 <- gbutton("Fit", expand=FALSE, spacing=1, handler = MakeFit, container = T22Frame3 )

  Reset22_btn1 <- gbutton("Reset Fit", spacing=1, handler = function(h, ...){
                              point.coords <<- list(x=NULL,y=NULL)
                              reset.fit <<- TRUE
                              MakeFit()
                              svalue(sb) <- "Estimated position of VB top : "
                          }, container = T22Frame3 )

  VBTop22_btn1 <- gbutton("Estimate VB Top", spacing=1, handler = CalcVBTop, container = T22Frame3 )

  Reset22_btn2<- gbutton("Reset All", spacing=1, handler = function(h, ...){
                              ResetVars()
                              enabled(OK_btn1) <- TRUE
                              enabled(OK_btn2) <- FALSE
                              replot()
                           }, container = T22Frame3 )

  T22group2 <- ggroup(spacing=5, expand=TRUE, container=T22Frame3)


#----- Hill Sigmoid subtab

  T23Frame1 <- gframe(text = " Options ", horizontal=FALSE, container = T2group4)
  T23group1 <- ggroup( horizontal=TRUE, container = T23Frame1)

  Lbl3 <- glabel("Left Mouse Butt. to Set Sigmoid Max, \nFlex Point, Min.  Right Butt. to Stop     ", container=T23group1)
  font(Lbl3) <- list(family="sans", size=11)

  Hlp23_btn1 <- gbutton("?", expand=FALSE, handler = function(h, ...){
                              txt <- paste("Three points are needed to define a Hill Sigmoid: the Sigmoid maximum M (max of the\n",
                                       "  VB in the selected region, the sigmoid flex point FP in the middle of the descending\n",
                                       "  tail and the sigmoid minimum m (background level).\n",
                                       "Press 'Add Hill Sigmoid' and Click with the Left Mouse button to add the M, FP and m points\n",
                                       "Click with the right mouse button to stop entering positions and add the ADD HILL SIGMOID\n",
                                       "Press the FIT button to model the VB using the Hill Sigmoid",
                                       "Press RESET FIT to restart the fitting procedure\n",
                                       "Pressing the ESTIMATE VB TOP button, the VB top is determined matematically as\n",
                                       "   the point with abscissa [FPx * (1-2/n)] where FPx is the abscissa of FP, \n",
                                       "   n is the sigmoid power (see manual for more details).\n",
                                       "Pressing RESET ALL button one resets all the analysis and restarts from very beginning")
                              gmessage(msg=txt,icon="info")
                           }, container = T23group1 )

  add23_btn1 <- gbutton("Add Hill Sigmoid", handler = function(h, ...){
                              GetCurPos(SingClick=FALSE)
                              add.FitFunct()
                           }, container = T23Frame1)
  Fit23_btn1 <- gbutton("Fit", expand=FALSE, handler = MakeFit, container = T23Frame1 )
  Reset23_btn1 <- gbutton("Reset Fit", expand=FALSE, handler = function(h, ...){
                              point.coords <<- list(x=NULL,y=NULL)
                              reset.fit <<- TRUE
                              MakeFit()
                              svalue(sb) <- "Estimated position of VB top : "
                           }, container = T23Frame1 )

  VBTop23_btn1 <- gbutton("Estimate VB Top", expand=FALSE, handler = CalcVBTop, container = T23Frame1 )
  Reset23_btn2 <- gbutton("Reset All", expand=FALSE, handler = function(h, ...){
                              ResetVars()
                              enabled(OK_btn1) <- TRUE
                              enabled(OK_btn2) <- FALSE
                              replot()
                           }, container = T23Frame1 )


#----- SAVE&CLOSE button -----
#  gseparator(container = MainGroup)
  ButtGroup <- ggroup(expand = FALSE, horizontal = TRUE, spacing = 3, container = MainGroup)

  gbutton("           SAVE           ", handler = function(h, ...){
                  if (VBtEstim == FALSE && length(Object[[coreline]]@Fit) > 0){  #VB fit done but VBtop estimation not
                      answ <- gconfirm(msg="VBtop estimation not performed. Would you proceed?", title="WARNING", icon="warning")
                      if (answ == FALSE) return()
                  }
                  LL <- length(Object)
                  Object[[LL+1]] <<- Object[[coreline]]
                  Object@names[LL+1] <<- "VBt"
                  assign(Object_name, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", (LL+1), envir = .GlobalEnv)
                  assign("activeSpectName", "VBt", envir = .GlobalEnv)
                  replot()
                  XPSSaveRetrieveBkp("save")
              }, container = ButtGroup)

  SaveBtn <- gbutton("        SAVE & EXIT        ", handler=function(h,...){
                  if (VBtEstim == FALSE && length(Object[[coreline]]@Fit) > 0){  #VB fit done but VBtop estimation not
                      answ <- gconfirm(msg="VBtop estimation not performed. Would you proceed?", title="WARNING", icon="warning")
                      if (answ == FALSE) return()
                  }
                  LL <- length(Object)
                  activeSpecIndx <- LL+1
                  Object[[LL+1]] <<- Object[[coreline]]
                  Object[[LL+1]]@Symbol <<- "VBt"
                  Object@names[LL+1] <<- "VBt"
                  Object[[coreline]] <<- XPSremove(Object[[coreline]],"all")   #remove fit stored in the original coreline
                  assign(Object_name, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
                  assign("activeSpectName", coreline, envir = .GlobalEnv)
                  dispose(VBwindow)
                  plot(Object)
              }, container = ButtGroup)

  ExitBtn <- gbutton("           EXIT           ", handler=function(h,...){
                  dispose(VBwindow)
                  plot(Object)
              }, container = ButtGroup)

  sb <- gstatusbar("status", container = VBwindow)

  enabled(OK_btn2) <- FALSE
  enabled(T1group1) <- FALSE
  enabled(T2group1) <- FALSE

  visible(VBwindow) <- TRUE
  svalue(nbVBfit) <- 2     #refresh notebook pages
  svalue(nbVBfit) <- 1
  svalue(nbMain) <- 2
  svalue(nbMain) <- 1
  
}

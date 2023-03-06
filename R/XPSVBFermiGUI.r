
#' @title XPSVBFermi
#' @description XPSVBFermi function to estimate the position of the Valence Band Top
#'   Interactive GUI to add BaseLines and Fitting components to
#'   the region of the VB proximal to the Fermi Edge needed
#'   for the estimation of the VB-Top position
#' @examples
#' \dontrun{
#'  XPSVBFermi()
#' }
#' @export
#'


XPSVBFermi <- function() {

  FindPattern <- function(TxtVect, Pattern){
      chrPos <- NULL
      LL <- length(TxtVect)
      for(ii in 1:LL){
          Pos <- gregexpr(pattern=Pattern,TxtVect[ii])  #the result of gregexpr is a list containing the character position of D.x x=differentiation degree
          if (Pos[[1]][1] > 0) {
              chrPos[1] <- ii
              chrPos[2] <- Pos[[1]][1]
              break
          }
      }
      return(chrPos)
  }

  LoadCoreLine <- function(h, ...){
     Object_name <- get("activeFName", envir=.GlobalEnv)
     Object <<- get(Object_name, envir=.GlobalEnv)  #load the XPSSample from the .Global Environment
     ComponentList <<- names(slot(Object[[coreline]],"Components"))
     if (length(ComponentList)==0) {
         gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
         return()
     }
     plot(Object)   #replot spectrum of the selected component
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
             gmessage(msg=" Analysis already present on this Coreline", title = "WARNING: Analysis Done",  icon = "warning")
             return()
         }
         enabled(T1group1) <- TRUE   #enable NB-baseline
         enabled(T2group1) <- TRUE   #enable plot options
         Object[[coreline]]@RSF <<- 0 #set the VB sensitivity factor to zero to avoid error wornings

         LL <- length(Object[[coreline]]@.Data[[1]])
         Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
         Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
         Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
         BaseLevel <- mean(Object[[coreline]]@.Data[[2]][(LL-5):LL])
         Object[[coreline]]@Baseline$x <<- Object[[coreline]]@RegionToFit$x
         Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)

     }
     ObjectBKP <<- Object[[coreline]]
     plot(Object[[coreline]])
  }


  reset.baseline <- function(h, ...) {

     if (coreline != 0) {   #coreline != "All spectra"
         LL <- length(Object[[coreline]]@.Data[[1]])
         Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
         Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][LL], Object[[coreline]]@.Data[[2]][LL])
         Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
         BaseLevel <- mean(Object[[coreline]]@.Data[[2]][(LL-5):LL])
         Object[[coreline]]@Baseline$x <<- Object[[coreline]]@RegionToFit$x
         Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)
         enabled(T2group1) <<- FALSE
     }
  }

  SelectRegion <- function(h, ...){
      txt <- "LEFT button to set the zoom area; RIGHT button to exit \n Click near markers to modify the zoom area"
      gmessage(msg=txt , title = "WARNING",  icon = "warning")
      pos <- locator(n=2, type="p", pch=3, col="red", lwd=1.5) #first the two corners are drawn
      Xlim1 <- min(range(Object[[coreline]]@.Data[[1]]))   #limits coordinates in the Spectrum Range
      Xlim2 <- max(range(Object[[coreline]]@.Data[[1]]))
      Ylim1 <- min(range(Object[[coreline]]@.Data[[2]]))
      Ylim2 <- max(range(Object[[coreline]]@.Data[[2]]))
      if (Object[[coreline]]@Flags[1]) { #Binding energy set
          pos$x <- sort(pos$x, decreasing=TRUE)  #point.coords$x in decreasing order
          if (pos$x[2] < Xlim1 ) {pos$x[2] <- Xlim1}
          if (pos$x[1] > Xlim2 ) {pos$x[1] <- Xlim2}
      } else {
          pos$x <- sort(pos$x, decreasing=FALSE) #point.coords$x in increasing order
          if (pos$x[1] < Xlim1 ) {pos$x[1] <- Xlim1}
          if (pos$x[2] > Xlim2 ) {pos$x[2] <- Xlim2}
      }
      pos$y <- sort(pos$y, decreasing=FALSE)
      if (pos$y[1] < Ylim1 ) {pos$y[1] <- Ylim1}
      if (pos$y[2] > Ylim2 ) {pos$y[2] <- Ylim2}
      rect(pos$x[1], pos$y[1], pos$x[2], pos$y[2])  #marker-Corners are ordered with ymin on Left and ymax on Right
      Object[[coreline]]@Boundaries <<- pos
 			  idx1 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x[1])
 			  idx2 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x[2])
      Object[[coreline]]@Baseline$x <<- Object[[coreline]]@Baseline$x[idx1:idx2]
      Object[[coreline]]@Baseline$y <<- Object[[coreline]]@Baseline$y[idx1:idx2]
      Object[[coreline]]@Baseline$type  <<- "linear"
      Object[[coreline]]@RegionToFit$x <<- Object[[coreline]]@.Data[[1]][idx1:idx2]  #Define RegionToFit see XPSClass.r
      Object[[coreline]]@RegionToFit$y <<- Object[[coreline]]@.Data[[2]][idx1:idx2]  #Define RegionToFit see XPSClass.r
      plot(Object[[coreline]])
      XX <- Object[[coreline]]@Boundaries$x
      idx1 <- 1
      idx2 <- length(Object[[coreline]]@RegionToFit$x)
      YY <- c(Object[[coreline]]@Baseline$y[idx1], Object[[coreline]]@Baseline$y[idx2])
      points(XX, YY, type="p", pch=9, cex=1.5, col="red", lwd=1.5)

      pos <- list(x=0, y=0)
      while (length(pos) > 0) {      #if pos not NULL a mouse button was pressed
         pos <- locator(n=1) #to modify the zoom limits
         if (length(pos$x) > 0) { #if the right mouse button NOT pressed
            dX1 <- abs(XX[1] - pos$x)
            dX2 <- abs(XX[2] - pos$x)
            if (dX1 < dX2) {
                XX[1] <- pos$x
                idx1 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x)
                YY[1] <- Object[[coreline]]@Baseline$y[idx1]
                Object[[coreline]]@Boundaries$x[1] <<- XX[1]
                Object[[coreline]]@Boundaries$y <<- range(Object[[coreline]]@RegionToFit$y[idx1:idx2])
            }
            if (dX2 < dX1) {
                XX[2] <- pos$x
                idx2 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x)
                YY[2] <- Object[[coreline]]@Baseline$y[idx2]
                Object[[coreline]]@Boundaries$x[2] <<- XX[2]
                Object[[coreline]]@Boundaries$y <<- range(Object[[coreline]]@RegionToFit$y[idx1:idx2])
            }
            plot(Object[[coreline]], xlim=XX, ylim=Object[[coreline]]@Boundaries$y)  #refresh graph
            points(XX, YY, type="p", pch=9, cex=1.5, col="red", lwd=1.5)
         }
      }
      Object[[coreline]]@Baseline$x <<- Object[[coreline]]@Baseline$x[idx1:idx2]
      Object[[coreline]]@Baseline$y <<- Object[[coreline]]@Baseline$y[idx1:idx2]
      Object[[coreline]]@RegionToFit$x <<- Object[[coreline]]@RegionToFit$x[idx1:idx2]  #ReDefine RegionToFit see XPSClass.r
      Object[[coreline]]@RegionToFit$y <<- Object[[coreline]]@RegionToFit$y[idx1:idx2]  #ReDefine RegionToFit see XPSClass.r
      plot(Object[[coreline]])
  }


#--- Functions, Fit and VB_Top estimation


  add.FitFunct <- function(h, ...) {

     ObjectBKP <<- Object[[coreline]]
     gmessage(msg="Please define the mid point of the VB descendent tail", title="DEFINE POSITION", icon="warning")
     pos <- locator(n=1, type="p", col="red", lwd=2, cex=1.5, pch=3)
     if (coreline != 0 && hasBaseline(Object[[coreline]])) {
#Fit parameter are set in XPSAddComponent()
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBFermi",
                                             peakPosition = list(x = pos$x, y = 2*pos$y), ...)
         Object[[coreline]]@Fit$y <<- Object[[coreline]]@Components[[1]]@ycoor-Object[[coreline]]@Baseline$y #subtract the Baseline
         Object[[coreline]]@RegionToFit$x <- ObjectBKP@RegionToFit$x #restore original abscissas changed in XPSAddComponent()
         plot(Object[[coreline]])
     }
  }


  del.FitFunct <- function(h, ...) {  #title="DELETE COMPONENT KILLS CONSTRAINTS!!!"

     ObjectBKP <<- Object[[coreline]]
     if (gconfirm(msg="Deleting fit function. Are you sure you want to proceed?", title="DELETE", icon="warning")) {
         LL<-length(Object[[coreline]]@Components)
         for (ii in 1:LL) { #Remove all CONSTRAINTS
              Object[[coreline]] <<- XPSConstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
         }
         if (coreline != 0 && hasComponents(Object[[coreline]])) {
             txt <- c("Select the fit component to delete")
             delWin <- gwindow("DELETE", parent = window, visible = FALSE)
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
                     dispose(delWin)
             } )
             gbutton("Cancel", container=bg, handler=function(...) dispose(delWin))
             visible(delWin) <- TRUE
         }
     }
  }

  do.Fit <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     FitRes <- NULL

     if (coreline != 0) {  #VB Hill Sigmoid
         if (reset.fit==FALSE){
#Fit parameters and X coords are set in XPSAddComponent()
#also the FermiSigmoid was defined using the X coords
             Object[[coreline]] <<- XPSFitLM(Object[[coreline]], plt=FALSE, verbose=FALSE)   #Levenberg Marquardt fit
             if (svalue(plotFit) == "normal") plot(Object[[coreline]])             
             if (svalue(plotFit) == "residual") XPSresidualPlot(Object[[coreline]])
         } else if (reset.fit==TRUE){
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"fit")
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"components")
             reset.fit <<- FALSE
             plot(Object[[coreline]])
         }
     }
  }

  do.VBFermi <- function(h, ...) {
      #retrieve fitted h, Ef and K values
      h <- Object[[coreline]]@Components[[1]]@param[1,1]  #fitted h value
      Ef.Posx <- Object[[coreline]]@Components[[1]]@param[2,1]  #fitted Ef value
      Ef.Posx <- round(Ef.Posx, digits=3)
      idx <- findXIndex(Object[[coreline]]@RegionToFit$x, Ef.Posx)
      bkg <- Object[[coreline]]@Baseline$y[idx]
      Ef.Posy <- h/2 + bkg   #Ef.Posy correspondent to Ef.Posx
      Object[[coreline]]@Components[[1]]@label <<- "VBFermi"  #Label indicating the VBFermi in the plot
      Object[[coreline]]@Components[[1]]@param[1,1] <<- Ef.Posy  # VBFermi stored in param "h"

      charPos <- FindPattern(Object[[coreline]]@Info, "   ::: Fermi Edge position = ")[1]  #charPos[1] = row index of Info where "::: Fermi Edge position" is found
      if(length(charPos) > 0){ #Overwrite  the Fermi Edge Position in FName Info
cat("\n aaaa", Ef.Posy)
         Object[[coreline]]@Info[charPos] <<- paste("   ::: Fermi Edge position = ", Ef.Posy, sep="") #overwrite previous MAx\Min Dist value
      } else {
         answ <- gconfirm(msg="Save new value of Fermi Edge POsition?", title="WARNING", icon="warning")
         if (answ == TRUE){
cat("\n bbbb", Ef.Posy)
             nI <- length(Object[[coreline]]@Info)+1
             Object[[coreline]]@Info[nI] <<- paste("   ::: Fermi Edge position = ", Ef.Posy, sep="")
         }
      }
      plot(Object[[coreline]])
      lines(x=c(Ef.Posx, Ef.Posx), y=c(0,h+1), col="blue")
      points(Ef.Posx, Ef.Posy, col="orange", cex=2, lwd=2, pch=3)
      txt <- paste("==> Estimated position of Fermi Level : ", Ef.Posx, sep="")
      svalue(sb) <- txt
      cat("\n", txt)
  }


#----Set Default Variable Values
  ResetVars <- function(){
     Object[[coreline]] <<- XPSremove(Object[[coreline]],"all")
     LL <- length(Object[[coreline]]@.Data[[1]])
     Object[[coreline]]@Boundaries$x <<- c(XPSSample[[coreline]]@.Data[[1]][1], XPSSample[[coreline]]@.Data[[1]][LL])
     Object[[coreline]]@Boundaries$y <<- c(XPSSample[[coreline]]@.Data[[2]][1], XPSSample[[coreline]]@.Data[[2]][LL])
     Object[[coreline]]@RegionToFit$x <<- XPSSample[[coreline]]@.Data[[1]]  #Define RegionToFit see XPSClass.r
     Object[[coreline]]@RegionToFit$y <<- XPSSample[[coreline]]@.Data[[2]]  #Define RegionToFit see XPSClass.r
     Object[[coreline]]@Boundaries$y[1] <<- min(XPSSample[[coreline]]@Boundaries$y)
     Object[[coreline]]@Boundaries$y[2] <<- max(XPSSample[[coreline]]@Boundaries$y)
     BaseLevel <- mean(XPSSample[[coreline]]@.Data[[2]][(LL-5):LL])
     Object[[coreline]]@Baseline$x <<- XPSSample[[coreline]]@.Data[[1]]
     Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)

     VBbkgOK <<- FALSE
     VBlimOK <<- FALSE
     svalue(sb) <<- "Estimated position of VB top : "
     enabled(T2group1) <<- FALSE
     plot(Object[[coreline]])
     return()
  }

#--- variables

  if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
  }

  XPSSample <- get(activeFName,envir=.GlobalEnv)  #this is the XPS Sample
  Object <- XPSSample
  Object_name <- get("activeFName", envir = .GlobalEnv) #XPSSample name
  ObjectBKP <- NULL   #CoreLine bkp to enable undo operation
  FNameList <- XPSFNameList() #list of XPSSamples
  SpectList <- XPSSpectList(activeFName) #list of XPSSample Corelines
  coreline <- 0
  VBbkgOK <- FALSE
  VBlimOK <- FALSE
  FitFunct <- "VBFermi"
  reset.fit <- FALSE

#--- Widget definition

  VBwindow <- gwindow("XPS VB FERMI EDGE", parent=c(80, 0), toolkit = "tcltk", visible = FALSE)
  size(VBwindow) <- c(400, 430)
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
                                 reset.baseline()
                                 enabled(T2group1) <<- FALSE
                                 plot(Object)
                       }, container = SelectFrame)
  svalue(XPS.Sample) <- activeFName

  Core.Lines <- gcombobox(c("0.All spectra", SpectList), selected=1, handler = set.coreline, container = SelectFrame)

  T1group1 <- ggroup(horizontal=FALSE, container = MainGroup)
  T1Frame1 <- gframe(text = " Set VB upper region ", horizontal=FALSE, container = T1group1)

  glabel(" Press 'Define the VB Upper Region' Button \n LEFT CLICKING THE OPPOSITE CORNERS", container=T1Frame1)
  T2group1 <- ggroup(horizontal=FALSE, container = MainGroup)
  T2Frame1 <- gframe(text = " Plot ", spacing=1, container = T2group1)
  plotFit <- gradio(items=c("normal", "residual"), selected=1,  expand=TRUE, horizontal = TRUE, handler = plot, container = T2Frame1)


#----- Fermi Function

  Select_btn <- gbutton("Define the VB Upper Region", handler = SelectRegion, container = T1Frame1)
  add_btn <- gbutton("Add Fermi-Dirac function", handler = add.FitFunct, container = T1Frame1)
  Fit_btn <- gbutton("Fit", expand=FALSE, handler = do.Fit, container = T1Frame1 )
  Fit_GetFermi <- gbutton("Get Fermi Edge Position", expand=FALSE, handler = do.VBFermi, container = T1Frame1 )
  Reset_btn <- gbutton("Reset Analysis", expand=FALSE, handler = function(h, ...){
#                              reset.fit <<- TRUE
#                              do.Fit()
                              ResetVars()
                              svalue(sb) <- "Estimated position of VB top : "
                           }, container = T1Frame1 )


#----- SAVE&CLOSE button -----
#  gseparator(container = MainGroup)
  ButtGroup <- ggroup(expand = FALSE, horizontal = TRUE, spacing = 3, container = MainGroup)

  gbutton("           SAVE           ", handler = function(h, ...){
                  activeSpecIndx <- coreline[1]
                  assign(Object_name, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
                  assign("activeSpectName", coreline[2], envir = .GlobalEnv)
                  plot(Object[[coreline]])
                  XPSSaveRetrieveBkp("save")
              }, container = ButtGroup)

  gbutton("        SAVE & EXIT        ", handler=function(h,...){
                  LL <- length(XPSSample)
                  XPSSample[[LL+1]] <- Object[[coreline]]
                  XPSSample[[LL+1]]@Symbol <- "VBf"
                  XPSSample@names[LL+1] <- "VBf" #Object coreline was the initial VB possessing non-NULL name "VB"
                  activeSpecIndx <- coreline
                  assign(Object_name, XPSSample, envir = .GlobalEnv)
                  assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
                  assign("activeSpectName", coreline, envir = .GlobalEnv)
                  dispose(VBwindow)
                  XPSSaveRetrieveBkp("save")
                  plot(XPSSample[[LL+1]])
              }, container = ButtGroup)

  gbutton("           EXIT           ", handler=function(h,...){
                  dispose(VBwindow)
                  XPSSaveRetrieveBkp("save")
                  plot(Object[[coreline]])
              }, container = ButtGroup)

  sb <- gstatusbar("status", container = VBwindow)

  enabled(T1group1) <- FALSE
  enabled(T2group1) <- FALSE

  visible(VBwindow) <- TRUE
}

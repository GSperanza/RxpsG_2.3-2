#-----------------------------------------
# XPS processing with gWidgets2 and tcltk
#-----------------------------------------
#' @title XPSExtract extract a portion of spectrum from a XPS survey
#'
#' @description XPSExtract function extracts spectral features from
#'   a XPS survey in a XPSSample. Mouse is used to identify the portion
#'   of the spectrum to extract. The user is asked to assign a name
#'   (i.e. Cl2p, Li1s, Br3d...) to the extracted spectrum representing
#'   a Core-Line associated to an element. Coherently with the
#'   name, a RSF will be automatically assigned to that spectrum.
#' @return XPSExtract returns the extracted spectrum, and the original 
#'   XPSSample will show an additional coreline
#' @examples
#' \dontrun{
#'  XPSextractGUI()
#' }
#' @export
#'

XPSExtract <- function() {

  devset <- function(){ #returns the ID of the current active graphic device
      if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
  }

  my.coords <- function(buttons, x, y) { #converts the normalized device coords in user coords
      xx <- grconvertX(x, from="ndc", to="user")
      yy <- grconvertY(y, from="ndc", to="user")
      coords <<- c(xx, yy)

      Xlim1 <- min(range(Object@.Data[[1]]))   #limits coordinates in the Spectrum Range
      Xlim2 <- max(range(Object@.Data[[1]]))
      Ylim1 <- min(range(Object@.Data[[2]]))
      Ylim2 <- max(range(Object@.Data[[2]]))

      if (xx < Xlim1 ) {xx <- Xlim1}
      if (xx > Xlim2 ) {xx <- Xlim2}
      if (yy < Ylim1 ) {yy <- Ylim1}
      if (yy > Ylim2 ) {yy <- Ylim2}
      if (buttons == 0) { LBmousedown() }
      if (buttons == 2) { cat("\n Please use Left Mouse Button") }
      return()
  }

  keydown <- function(key) {  #blocks the mouseHandler
     cat("\n Key pressed", key)
     if (key == "q") {
        EXIT <<- TRUE
        eventEnv$onMouseMove <<- NULL
        enentEnv <<- NULL
        NULL
        return()
     }
  }

  LBmousedown <- function() {
      point.coords$x[point.index] <<- coords[1]   #abscissa
      point.coords$y[point.index] <<- coords[2]   #ordinate
      if (point.index == 1) {
         point.index<<-2    #to modify the second edge of the selected area
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
      } else if (point.index == 2) {
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
         point.index<<-3
      } else if (point.index == 3) {
         D<-vector("numeric", 4)
         Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #initialization value
         for (ii in 1:4) {
             D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #distance P0 - P1
             if(D[ii] <= Dmin){
                Dmin<-D[ii]
                idx=ii
             }
         }
         if (idx == 1){
            Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
            Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx == 2){
             Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
             Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         } else if (idx == 3){
             Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
             Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx == 4){
            Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
            Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         }
         if (Object@Flags[1]) { #Binding energy set
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE)
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         } else {
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE)
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         }
      }
      replot()
  }


  undo.plot <- function(...){
      if (SelReg == 1) {
         reset.boundaries()
         replot()
      } else if (SelReg > 1) {
         Object@Boundaries$x <<- OldCoords$x
         Ylimits <<- OldCoords$y
         replot()
      }
  }


  replot <- function(...) {
      if (point.index == 0) {   #Extract active
          plot(Object, xlim=Xlimits, ylim=Ylimits)
      } else if (point.index == 1 || point.index == 2) { #generic plot
          plot(Object, xlim=Xlimits)
          points(point.coords, col="red", cex=1, lwd=1.5, pch=3)
      } else if (point.index > 2){   #plot spectrum and frame for region selection
          plot(Object, xlim=Xlimits, ylim=Ylimits)
          points(Corners, type="p", col="red", cex=1, lwd=1.5, pch=3)
          rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
      }
      svalue(statbar) <- sprintf(paste("x =",round(coords[1],1), " y =",round(coords[2]), sep=" "))
  }

  reset.boundaries <- function(h, ...) {
     Object <<- XPSSample[[oldcoreline]]   #switch to the initially selected coreline (Survey)
     Object <<- XPSremove(Object, "all")
     LL <- length(Object@.Data[[1]])
     point.coords$x[1] <<- Object@.Data[[1]][1]  #abscissa of the first survey edge
     point.coords$y[1] <<- Object@.Data[[2]][1]  #ordinate of the first survey edge
     point.coords$x[2] <<- Object@.Data[[1]][LL] #abscissa of the second survey edge
     point.coords$y[2] <<- Object@.Data[[2]][LL] #ordinate of the second survey edge
     slot(Object,"Boundaries") <<- point.coords
     Xlimits <<- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
     Ylimits <<- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
     OldCoords <<- point.coords #for undo
     Corners <- point.coords
     point.index <<- 1
     replot()
  }

  Extract <- function(h, ...){
         slot(Object,"Boundaries") <<- point.coords
         Xlimits <<- Object@Boundaries$x   #visualize selected region
         Ylimits <<- sort(Object@Boundaries$y, decreasing=FALSE)
         point.index <<- 0
         replot()

         winExt <- gwindow("Extract Spectral Feature", visible=FALSE, parent=window) #
         gBox <- gvbox(container=winExt)
         flyt <- gformlayout(container=gBox)
         elesymbol <- gedit("", label="Element Name:", container=flyt)
         gseparator(container=gBox)
         bg <- ggroup(container=gBox)
         gbutton("OK", container=bg, handler=function(...){   #input spectrum name
                 dispose(winExt)
                 Symbol <- svalue(elesymbol)
                 Element <- Symbol <- gsub(" ", "", Symbol)   #eliminates white spaces from Symbol
                 Orbital <- c("1s", "2s", "3s", "4s", "5s", "6s", "2p", "3p", "4p", "5p", "6p", "3d", "4d", "5d", "4f")
                 for(ii in 1:length(Orbital)){
                     Element <- sub(Orbital[ii], "", Element) #eliminates the orbital from the Symbol
                 }
                 Orbital <- sub(Element, "", Symbol) #retrieve the orbital
                 if(nchar(Orbital)==0){
                    gmessage(" ATTENTION: incorrect Core-Line Name, Orbital lacking!", title="WARNING", icon="warning")
                    return()
                 }
                 pattern <- c("[[:alpha:]]")         #matches letters in Symbol (dot excluded)
                 idx <- regexpr(pattern, Element)     #returns the index of the first letter in Symbol
                 ## symbol element
                 Element <- substr(Element, start=idx, stop=nchar(Element))
                 if (ElementCheck(Element)==FALSE ) {    #see XPSelement.r
                     yesno <- gconfirm(msg=" ATTENTION: element Name NOT found in Element Table! \n Proceed anyway?", icon="warning")
                     if (yesno==FALSE){
                         dispose(winExt)
                         return()
                    }
                 }
                 Symbol <- paste(Element, Orbital, sep="") #biold the exact CoreLine name

                 newcoreline <- Object   #creates a new coreline
                 Xmax <- max(range(newcoreline@.Data[1]))
                 Xmin <- min(range(newcoreline@.Data[1]))
                 #is selected region out of limits?
                 if (point.coords$x[1] > Xmax) {point.coords$x[1] <<- Xmax}
                 if (point.coords$x[1] < Xmin) {point.coords$x[1] <<- Xmin}
                 if (point.coords$x[2] > Xmax) {point.coords$x[2] <<- Xmax}
                 if (point.coords$x[2] < Xmin) {point.coords$x[2] <<- Xmin}

                 idx1 <- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[1]) #index corresponding to the selected BE1 (or KE1 value) of RegionToFit
                 idx2 <- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[2]) #index corresponding to the selected BE2 (or KE2 value) of RegionToFit
                 tmp <- unlist(Object@.Data[1])  #extract correspondent X values for the selected region
                 newcoreline@.Data[[1]] <- tmp[idx1:idx2]    #save the X values in the new coreline
                 OldEnergyScale <<- newcoreline@.Data[[1]]
                 newcoreline@Boundaries$x <- c(tmp[idx1], tmp[idx2])
                 tmp <- unlist(Object@.Data[2])  #extract correspondent Y values for the selected region
                 newcoreline@.Data[[2]] <- tmp[idx1:idx2]    #save the Y values in the new coreline
                 newcoreline@Boundaries$y <- range(tmp)
                 tmp <- unlist(Object@.Data[3])  #extract correspondent transmission Factor values for the selected region
                 newcoreline@.Data[[3]] <- tmp[idx1:idx2]    #save the transmission Factor values in the new coreline
                 slot(newcoreline,"Symbol") <- Symbol
                 ## add extracted coreline to original XPSSample
                 idx <- length(XPSSample) + 1
                 XPSSample[[idx]] <<- newcoreline
                 names(XPSSample) <<- unname(sapply(XPSSample, slot, "Symbol"))
                 point.coords$x[1] <<- Object@.Data[[1]][1]
                 point.coords$y[1] <<- Object@.Data[[2]][1]
                 point.coords$x[2] <<- Object@.Data[[1]][LL]
                 point.coords$y[2] <<- Object@.Data[[2]][LL]
                 svalue(statbar) <- sprintf("New %s added.", svalue(elesymbol))
                 Object <<- XPSSample[[idx]]
                 coreline <<- idx
                 assign("activeSpectIndx", idx, envir=.GlobalEnv)
                 assign("activeSpectName", Symbol, envir=.GlobalEnv)
                 replot()
         })
         gbutton("Cancel", container=bg, handler=function(...) dispose(winExt))
         visible(winExt) <- TRUE
  }

  ResetVars <- function(){
         point.index <<- 1
         coords <<- NA # for printing mouse coordinates on the plot
         xx <<- NULL
         yy <<- NULL
         NO.Fit <<- FALSE

#Coreline boundaries
         point.coords$x[1] <<- Object@.Data[[1]][1]
         point.coords$y[1] <<- Object@.Data[[2]][1]
         point.coords$x[2] <<- Object@.Data[[1]][LL]
         point.coords$y[2] <<- Object@.Data[[2]][LL]
         Xlimits <<- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
         Ylimits <<- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
         Object@Boundaries$x <<- c(point.coords$x)
         Object@Boundaries$y <<- c(point.coords$y)

         Corners <<- point.coords
         OldCoords <<- point.coords #for undo
         OldEnergyScale <<- Object@.Data[[1]]
         OldFlag <<- Object@Flags[1]
         OldUnits <<- Object@units[1]
         SelReg <<- 0
         coreline <<- oldcoreline

         WinSize <<- as.numeric(XPSSettings$General[4])
  }

#----- Variables -----

  XPSSample <- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
  coreline <- get("activeSpectIndx", envir=.GlobalEnv)
  Object <- XPSSample[[coreline]]
  point.coords <- list(x=NA,y=NA)
  point.index <- 1
  coords <- NA # for printing mouse coordinates on the plot
  xx <- NULL
  yy <- NULL
  NO.Fit <- FALSE

#Coreline boundaries
  LL<-length(Object@.Data[[1]])
  point.coords$x[1] <- Object@.Data[[1]][1]
  point.coords$y[1] <- Object@.Data[[2]][1]
  point.coords$x[2] <- Object@.Data[[1]][LL]
  point.coords$y[2] <- Object@.Data[[2]][LL]
  Xlimits <- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
  Ylimits <- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
  Corners <- point.coords
  Object@Boundaries$x <- c(point.coords$x)
  Object@Boundaries$y <- c(point.coords$y)

  OldCoords <- point.coords #for undo
  OldEnergyScale <- Object@.Data[[1]]
  OldFlag <- Object@Flags[1]
  OldUnits <- Object@units[1]
  SelReg <- 0
  FNameList <- XPSFNameList()
  SpectList <- XPSSpectList(activeFName)
  oldcoreline <- coreline
  if ( OldFlag == TRUE) {Eidx <- 1 }
  if ( OldFlag == FALSE) {Eidx <- 2 }

  WinSize <- as.numeric(XPSSettings$General[4])
  WinScale  <- NULL
  cat("\n Please select the portion of the Survey containing the CoreLine to extract. \n")


#====== Widget definition =======
  Ewindow <- gwindow("XPS extract GUI", parent=c(100, 0), toolkit = "tcltk", visible = FALSE)
  size(Ewindow) <- c(250, 300)
  addHandlerDestroy(Ewindow, handler=function(h, ...){  #if MainWindow unproperly closed with X
#-----                         stopping mouse handler
                               setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
                               eventEnv$onMouseMove <<- NULL
                               eventEnv <<- NULL
                               plot(XPSSample) #replot the CoreLine
                           })
  Egroup1 <- ggroup(container = Ewindow, horizontal = TRUE)

  ## XPSSample and Core lines
  Egroup2 <- ggroup(expand = FALSE, horizontal = FALSE, spacing = 5, container = Egroup1)

  gframe20 <- gframe(text = " XPS Sample and Core line Selection ", container = Egroup2)
  XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                                 activeFName <<- svalue(XPS.Sample)
                                 XPSSample <<- get(activeFName, envir=.GlobalEnv)
                                 SpectList <<- XPSSpectList(activeFName)
                                 indx <- grep("Survey", SpectList, value=FALSE)
                                 Object <<- XPSSample[[indx[1]]]
                                 ResetVars()
                                 delete(gframe20, Core.Lines)
                                 Core.Lines <<- gcombobox(SpectList, selected=1, expand = FALSE, handler = function(h, ...){
                                                CLine <- svalue(Core.Lines)
                                                CLine <- unlist(strsplit(CLine, "\\."))   #"number." and "CL name" are separated
                                                if (CLine[2] != "survey" && CLine[2] != "Survey"){
                                                   yesno<-gconfirm(msg=" ATTENTION: This is NOT a Survey spectrum! \n Proceed anyway?", icon="warning")
                                                   if (yesno==FALSE){
                                                       return()
                                                   }
                                                }
                                                coreline <<- as.integer(CLine[1])   # Coreline index
                                                Object <<- XPSSample[[coreline]]
                                                LL<-length(Object@.Data[[1]])
                                                point.coords$x[1] <<- Object@.Data[[1]][1]
                                                point.coords$y[1] <<- Object@.Data[[2]][1]
                                                point.coords$x[2] <<- Object@.Data[[1]][LL]
                                                point.coords$y[2] <<- Object@.Data[[2]][LL]
                                                Xlimits <<- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
                                                Ylimits <<- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
                                                Corners <<- point.coords
                                                Object@Boundaries$x <<- c(point.coords$x)
                                                Object@Boundaries$y <<- c(point.coords$y)
                                                OldCoords <<- point.coords #for undo
                                                OldEnergyScale <<- Object@.Data[[1]]
                                                OldFlag <<- Object@Flags[1]
                                                OldUnits <<- Object@units[1]
                                                SelReg <<- 0
                                                if ( OldFlag == TRUE ) {Eidx <<- 1 }
                                                if ( OldFlag == FALSE) {Eidx <<- 2 }
                                                replot()
                                 }, container = gframe20)
                                 replot()
                       }, container = gframe20)
  svalue(XPS.Sample) <- activeFName

  Core.Lines <- gcombobox(SpectList, selected=-1, expand = FALSE, handler = function(h, ...){
              CLine <- svalue(Core.Lines)
              CLine <- unlist(strsplit(CLine, "\\."))   #"number." and "CL name" are separated
              if (CLine[2] != "survey" && CLine[2] != "Survey"){
#                 gmessage("Wrong coreline: please select a survey spectrum", title="WRONG CORELINE", icon="warning")
                  yesno<-gconfirm(msg=" ATTENTION: This is NOT a Survey spectrum! \n Proceed anyway?", icon="warning")
                  if (yesno==FALSE){
                      return()
                  }
              }
              coreline <<- as.integer(CLine[1])   # Coreline index
              oldcoreline <<- coreline
              Object <<- XPSSample[[coreline]]
              LL <- length(Object@.Data[[1]])
              point.coords$x[1] <<- Object@.Data[[1]][1]
              point.coords$y[1] <<- Object@.Data[[2]][1]
              point.coords$x[2] <<- Object@.Data[[1]][LL]
              point.coords$y[2] <<- Object@.Data[[2]][LL]
              Xlimits <<- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
              Ylimits <<- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
              Corners <<- point.coords
              Object@Boundaries$x <<- c(point.coords$x)
              Object@Boundaries$y <<- c(point.coords$y)
              OldCoords <<- point.coords #for undo
              OldEnergyScale <<- Object@.Data[[1]]
              OldFlag <<- Object@Flags[1]
              OldUnits <<- Object@units[1]
              SelReg <<- 0
              replot()
         }, container = gframe20)

  gframe22 <- gframe(text = " Options ", horizontal = FALSE, container = Egroup2)
  gbutton("SELECT REGION", handler = function(h, ...){
              OldCoords <<- Object@Boundaries
              SelReg <<- SelReg+1
              rngX <- range(point.coords$x)
              rngX <- (rngX[2]-rngX[1])/20
              rngY <- range(point.coords$y)
              rngY <- (rngY[2]-rngY[1])/20
              if (Object@Flags[1]) { #Binding energy set
                 point.coords$x <<- sort(point.coords$x, decreasing=TRUE)  #pos$x in decreasing order
                 point.coords$x[1] <<- point.coords$x[1]+rngX/20
                 point.coords$x[2] <<- point.coords$x[2]-rngX/20
              } else {
                 point.coords$x <<- sort(point.coords$x, decreasing=FALSE) #pos$x in increasing order
                 point.coords$x[1] <<- point.coords$x[1]-rngX/20
                 point.coords$x[2] <<- point.coords$x[2]+rngX/20
              }
              point.coords$y <<- sort(point.coords$y, decreasing=FALSE)
              Xlimits <<- c(point.coords$x[1]-rngX/10, point.coords$x[2]+rngX/10)
              Ylimits <<- c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
              slot(Object,"Boundaries") <<- point.coords
              replot()
         }, container = gframe22 )

  gbutton("EXTRACT REGION", handler = function(h, ...){
              Extract()
              replot()
         }, container = gframe22 )

  gbutton("UNDO", handler = function(h, ...) {
              undo.plot()
         }, container = gframe22 )

  gbutton("RESET BOUNDARIES", handler = function(h, ...) {
              Object@.Data[[1]] <<- OldEnergyScale
              Object@Flags[1] <<- OldFlag
              reset.boundaries()
         }, container = gframe22 )

  gframe23 <- gframe(text = " Plot ", container = Egroup2)

  SwitchE <- gradio(c("BINDING ENERGY SCALE", "KINETIC ENERGY SCALE"), selected=Eidx, horizontal = TRUE, handler = function(h, ...) {
                     BE.KE <- svalue(SwitchE, index=TRUE)
                     XEnergy <- get("XPSSettings", envir=.GlobalEnv)$General[5] #the fifth element of the first column of XPSSettings
                     XEnergy <- as.numeric(XEnergy)
                     if (BE.KE == 2){ #Convert to Kinetic Energy
                         if ( XPSSample[[coreline]]@Flags[1] == TRUE) { #original energy scale Binding
                            XPSSample[[coreline]]@.Data[[1]] <<- XEnergy-XPSSample[[coreline]]@.Data[[1]]
                            XPSSample[[coreline]]@Boundaries$x <<- sort(range(XPSSample[[coreline]]@.Data[[1]]), decreasing=FALSE)
                            point.coords$x <<- XEnergy-XPSSample[[coreline]]@Boundaries$x
                            Corners$x <<- XEnergy-Corners$x
                            XPSSample[[coreline]]@Flags[1] <<- FALSE   #set KE scale
                            XPSSample[[coreline]]@units[1] <<- "Kinetic Energy [eV]"
                         }
                     }
                     if (BE.KE == 1){ #Convert to Binding Energy
                         if ( XPSSample[[coreline]]@Flags[1] == FALSE) { #original energy scale Kinetic
                            XPSSample[[coreline]]@.Data[[1]] <<- XEnergy-XPSSample[[coreline]]@.Data[[1]]
                            XPSSample[[coreline]]@Boundaries$x <<- sort(range(XPSSample[[coreline]]@.Data[[1]]), decreasing=TRUE)
                            point.coords$x <<- XEnergy-XPSSample[[coreline]]@Boundaries$x
                            Corners$x <<- XEnergy-Corners$x
                            XPSSample[[coreline]]@Flags[1] <<- TRUE   #set BE scale
                            XPSSample[[coreline]]@units[1] <<- "Binding Energy [eV]"
                         }
                     }
                     plot(XPSSample[[coreline]])
         }, container=gframe23 )

  gframe24 <- gframe(text = "Graphic Window Dimensions", horizontal=FALSE, container = Egroup2)
  Egroup3 <- ggroup(horizontal=TRUE, spacing=3, container=gframe24)
  glabel("Graphical Window size: ", container=Egroup3)
  txt <- paste("Graphical Window size: ", as.character(WinSize), sep="")
  WSvalue <- glabel(text=txt, spacing=3, container=Egroup3)

  WSize <- gslider(from = 0.5, to = 1.5, by = 0.1, value = WinSize, horizontal=TRUE, handler=function(h,...){
                        WinSize <- svalue(WSize)
                        svalue(WSvalue) <- paste("Graphical Window size: ", as.character(WinSize), sep="")
                        WinSize <<- dev.size()*WinSize   #rescale the graphic window
#                        graphics.off()
#                        OS <- Sys.info["sysname"]
#                        switch (OS,
#                           "Linux" =   {x11(type='Xlib', xpos=600, ypos=5, title=' ', width=WinSize[1], height=WinSize[2])},
#                           "Windows" = {x11(xpos=600, ypos=5, title=' ', width=WinSize[1], height=WinSize[2])},
#                           "MacOS-X" = {quartz(title=' ')},  #quartz() does allow setting the opening position
#                           "Mac OS"  = {quartz(title=' ')},
#                           "macOS"   = {quartz(title=' ')},
#                           "Darwin"  = {quartz(title=' ')})
#                        refresh <<- FALSE #now plot also the component marker
#                        devset()
                        replot()

         }, container=gframe24)

#---- Buttons ----
  gbutton("SAVE", expand=FALSE, handler = function(h, ...){
              assign(activeFName, XPSSample, envir = .GlobalEnv)
              assign("activeSpectIndx", coreline, envir = .GlobalEnv)
              assign("activeSpectName", XPSSample[[coreline]]@Symbol, envir = .GlobalEnv)
              XPSSaveRetrieveBkp("save")
         }, container = Egroup2 )


  gbutton("SAVE & EXIT", expand=FALSE, handler = function(h, ...){
#-----        stopping mouse handler
              setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
              eventEnv$onMouseMove <<- NULL
              eventEnv <<- NULL

              assign(activeFName, XPSSample, envir = .GlobalEnv)
              assign("activeSpectIndx", coreline, envir = .GlobalEnv)
              assign("activeSpectName", XPSSample[[coreline]]@Symbol, envir = .GlobalEnv)
              dispose(Ewindow)  #this calls the handlerdestroy(Ewindow...)
              XPSSaveRetrieveBkp("save")
              plot(XPSSample)
         }, container = Egroup2 )

  gbutton("EXIT", expand=FALSE, handler = function(h, ...){
#-----        stopping mouse handler
              setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
              eventEnv$onMouseMove <<- NULL
              eventEnv <<- NULL

              dispose(Ewindow)
              XPSSaveRetrieveBkp("save")
              plot(XPSSample)
         }, container = Egroup2 )

#---- Status bar
  statbar <- gstatusbar("status", container = Ewindow)

  visible(Ewindow) <- TRUE

#--- Interactive mouse control ---
  setGraphicsEventHandlers(prompt="Waiting for mouse clicks", onMouseDown = my.coords, onKeybd = keydown, which = dev.cur())
  eventEnv <- getGraphicsEventEnv()
  devset()
  getGraphicsEvent()

}

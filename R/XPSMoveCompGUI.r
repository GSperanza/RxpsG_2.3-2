#   Fit components moved manually with Graphic Event Handler

#'XPSMoveComp function to modify component position and intensity in a fit
#'
#'Provides a userfriendly interface change position and intensity of each
#'individual fitting component of a selected XPSCoreline. Changes are saved
#'in the .GlobalEnv main software memory.
#'No parameters are passed to this function.
#'
#'@examples
#'
#'\dontrun{
#'	XPSMoveComp()
#'}
#'
#'@export
#'


XPSMoveComp <- function(){

   LoadCoreLine<-function(){
      XPSSample <<- get(activeFName, envir=.GlobalEnv)     #load the XPSSample data from main memory
      Indx <<- get("activeSpectIndx", envir=.GlobalEnv)    #get active Spectrum index
      Object <<- XPSSample[[Indx]]
      assign("Object", Object, envir=.GlobalEnv)
      Xlimits <<- range(Object@RegionToFit$x)
      Ylimits <<- range(Object@RegionToFit$y)
      point.coords$x <<- Xlimits #set original X range
      point.coords$y <<- Ylimits #set original Y range

      ComponentList <<- names(slot(Object,"Components"))
      if (length(ComponentList)==0) {
          gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
          return()
      }
      delete(MCFrame2, FitComp) # selecting a new core line needs a new gradio running on the coreline fit-components

      if (length(ComponentList) > 1){    #gradio works with at least 2 items Less than 2 items gcheckbox will be used
          FitComp <<- gradio(ComponentList, selected=1, handler = function(h,...){   #gradio handler has to be redefined
                   refresh <<- TRUE
                   replot()   #replot spectrum without marker
                   FComp <- svalue(FitComp)
                   FComp <- as.numeric(unlist(strsplit(FComp, split="C")))  #index of the selected component
                   FComp <- FComp[2]
                   xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                   Rng <- range(Object@RegionToFit[[1]])
                   if (xx < Rng[1]) {xx <- Rng[1]}
                   if (xx > Rng[2]) {xx <- Rng[2]}
                   FuncName <- Object@Components[[FComp]]@funcName
                   yy <- Object@Components[[FComp]]@param[1,1] #component height h
                   yy <- yy/GetHvalue(Object,FComp, FuncName, 1)
                   Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                   Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                   yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                   coords[1] <<- xx
                   coords[2] <<- yy
                   refresh <<- FALSE #now plot also the component marker
                   replot()   #replot all
               },  container = MCFrame2)
      } else {
         FitComp <<- gcheckboxgroup(ComponentList, checked=TRUE, handler = function(h,...){ #gradio handler has to be redefined
                   refresh <<- TRUE
                   replot()   #replot spectrum without marker
                   FComp <- svalue(FitComp)
                   FComp <- as.numeric(unlist(strsplit(FComp, split="C")))
                   FComp <- FComp[2]
                   xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                   Rng <- range(Object@RegionToFit[[1]])
                   if (xx < Rng[1]) {xx <- Rng[1]}
                   if (xx > Rng[2]) {xx <- Rng[2]}
                   FuncName <- Object@Components[[FComp]]@funcName
                   yy <- Object@Components[[FComp]]@param[1,1] #component height h
                   yy <- yy/GetHvalue(Object,FComp, FuncName, 1)
                   Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                   Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                   yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                   coords[1] <<- xx
                   coords[2] <<- yy
                   refresh <<- FALSE #now plot also the component marker
                   replot()   #plot all
              },  container = MCFrame2)
      }
      xx <- Object@Components[[1]]@param[2,1] #component position mu
      yy <- Object@Components[[1]]@param[1,1] #component height h
      Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
      Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
      yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
      coords[1] <<- xx
      coords[2] <<- yy
      refresh <<- FALSE #now plot the component marker
      replot()   #replot spectrum and marker of selected fit component
  }

  devset <- function(){ #returns the ID of the current active graphic device
      if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
  }


  my.coords <- function(buttons, x, y) { #converts the normalized device coords in user coords
      xx <- grconvertX(x, from="ndc", to="user")
      yy <- grconvertY(y, from="ndc", to="user")

      Xlim1 <- min(Object@RegionToFit[[1]])   #limits coordinates in the Spectrum Range
      Xlim2 <- max(Object@RegionToFit[[1]])
      Ylim1 <- min(Object@RegionToFit[[2]])
      Ylim2 <- max(Object@RegionToFit[[2]])

      if (xx < Xlim1 ) {xx <- Xlim1}
      if (xx > Xlim2 ) {xx <- Xlim2}
      if (yy < Ylim1 ) {yy <- Ylim1}
      if (yy > Ylim2 ) {yy <- Ylim2}

      Estep <- abs(Object@RegionToFit[[1]][1] - Object@RegionToFit[[1]][2])
      Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
      yy_BasLin <- yy-Object@Baseline$y[Xindx]  #spectral intensity at xx without Baseline
      coords <<- c(xx, yy, yy_BasLin)

      if (buttons == 0) { LBmousedown() }
      if (buttons == 2) { RBmousedown() }

      return()
  }

  keydown <- function(key) {  #blocks the mouseHandler
     cat("\n Key pressed", key)
     if (key == "q") {
        EXIT <<- TRUE
        eventEnv$onMouseMove <<- NULL
        enentEnv <<- NULL
        NULL
        return(1)
     }
  }

  LBmousedown <- function() {   #Left mouse button down
	    xx <- coords[1]
	    yy <- coords[2]
     if (SetZoom==FALSE) { #left button works only when SET ZOOM REGION inactive
        do.move()
        XPSquantify(XPSSample)
        refresh <<- FALSE
     }
     replot()  
  }

  RBmousedown <- function() {   #Right mouse button down
	    xx <- coords[1]
	    yy <- coords[2]
     if (SetZoom == TRUE) { #left button works only when SET ZOOM REGION button pressed
     	  point.coords$x[point.index] <<- coords[1]   #abscissa
     	  point.coords$y[point.index] <<- coords[2]   #ordinate
     	  if (point.index == 1) {   #First rect corner C1
     	     point.index <<- 2
           Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
           Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
  	     } else if (point.index == 2) { #Second rect corner C2 opposite to C1
           point.index <<- 3
           Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
           Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
  	     } else if (point.index == 3) { #modifies corner positions
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
           if (Object@Flags[1]) { #Binding energy set
              point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decreasing order
              point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           } else {
              point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in increasing order
              point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           }
        }
        replot()
     }
  }

  do.move <- function(...) {
     FComp <- svalue(FitComp)
     if (length(FComp)==0) {
         gmessage(msg="Select Component Please", title="WARNING", icon = "warning")
     } else {
        FComp <- as.numeric(unlist(strsplit(FComp, split="C")))   #index of the selected component
        FComp <- FComp[2]
        xx <- coords[1]
        yy <- coords[2]  #Component max value with baseline
        zz <- coords[3]  #Component max value without baseline

        FitFunct <- Object@Components[[FComp]]@funcName
        newh <- GetHvalue(Object, FComp, FitFunct, zz)  #Get value computes the Component value given the fit parameters and the Ymax value

        #range limits for mu
        varmu <- getParam(Object@Components[[FComp]],variable="mu")
        minmu <- varmu$start-varmu$min
        maxmu <- varmu$max-varmu$start
        newmu <- c(xx, xx-minmu, xx+maxmu)
        #range limits for h
        varh <- getParam(Object@Components[[FComp]],variable="h")
        minh <- varh$start-varh$min
        maxh <- varh$max-varh$start

        if (maxh > 0) {
            newh <- c(newh, 0, newh*5)    # No constraints on h
        }
        if (maxh==0){
            newh <- c(newh, newh, newh)   # h is fixed
        }
        if (maxh < 0){
            newh <- c(newh, 0, newh*5)    # maxh cannot be <0: => force newH to correct values
        }
        if (varh$start < 0) {
            newh <- c(0, 0, 1e5)   #set a positive value for an hypotheic fit
        }
        Object@Components[[FComp]] <<- setParam(Object@Components[[FComp]], parameter=NULL, variable="mu", value=newmu)
        Object@Components[[FComp]] <<- setParam(Object@Components[[FComp]], parameter=NULL, variable="h", value=newh)
        Object@Components[[FComp]] <<- Ycomponent(Object@Components[[FComp]], x=Object@RegionToFit$x, y=Object@Baseline$y) #eomputes the Y value and add baseline
#Fit computed addind fit components with the modified ones
        tmp <- sapply(Object@Components, function(z) matrix(data=z@ycoor))
        Object@Fit$y <<- ( colSums(t(tmp)) - length(Object@Components)*(Object@Baseline$y))

        Object <<- sortComponents(Object)
#if component order changed then re-number them
        LL <- length(Object@Components) #N. fit components
        for (ii in 1:LL){
           if (xx == Object@Components[[ii]]@param["mu",1]) { #compare marker position with component positions
             indx <- ii
             break()
           }
        }
        svalue(FitComp) <- paste("C", indx, sep="")  #update component gradio
        XPSSample[[Indx]] <<- Object
        assign("Object", Object, envir=.GlobalEnv)
     }
  }

  replot <- function(...) {
     Object <- get("Object", envir=.GlobalEnv) #Load XPSSample because changes in XPSSample[[activeSpectIndx]]
                                               #may derive from changes of fit-parameters done in XPSConstraintsGui()
                                               #which are saved in the global variable activeFName[[activeSpectIndx]]
     if (point.index==1 && refresh==FALSE) {  #point.index==1 when moving mcomponent
         plot(Object, xlim=Xlimits, ylim=Ylimits)
         points(x=coords[1], y=coords[2], col=2, cex=1.2, lwd=2, pch=1)  # if refresh==FALSE plot spectrum with component marker
     } else if (SetZoom == TRUE){   #set zoom area corners
	        if (point.index <= 2) {    #define zoom area corners
 	           plot(Object)
             points(point.coords, type="p", col=3, cex=1.2, lwd=2.5, pch=3)
  	      } else if (point.index > 2){  #plot zoom area corners
 	           plot(Object, xlim=Xlimits, ylim=Ylimits)
             points(Corners, type="p", col=3, cex=1.2, lwd=2.5, pch=3)
             rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
         }
     } else {
         plot(Object, xlim=Xlimits, ylim=Ylimits)
     }
     svalue(StatBar) <- paste("x =",round(coords[1],2), "y =",round(coords[2],2), sep="   ")
  }

  reset.plot <- function(h, ...) {
       point.coords$x <<- range(Object@RegionToFit$x) #set original X range
       point.coords$y <<- range(Object@RegionToFit$y) #set original Y range
       Object@Boundaries <<- point.coords
       Xlimits <<- point.coords$x
       Ylimits <<- sort(point.coords$y, decreasing=FALSE)
       Corners <- list(x=c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2]),
                       y=c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2]))
       replot()
  }

  reset.vars <- function(){
     XPSSample <<- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     OldXPSSample <<- XPSSample
     SpectName <<- get("activeSpectName", envir=.GlobalEnv)
     Indx <- grep(SpectName, XPSSpectList(activeFName))     #set the CoreLine Indx
     Object <<- XPSSample[[Indx]]
     ComponentList <<- names(slot(Object,"Components"))
     if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        Xlimits <<- range(Object@.Data[1])
        Ylimits <<- range(Object@.Data[2])
        NoFit <<- TRUE
        return()
     }

     FComp <<- svalue(FitComp)
     FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
     FComp <<- FComp[2]

     coords <<- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <<- c(xx=NA, yy=NA, yy_BasLin=NA)
     FuncName <- Object@Components[[FComp]]@funcName
     vscale <<- GetHvalue(Object,FComp, FuncName, 1)
     xx <<- Object@Components[[FComp]]@param[2,1] #component position mu
     Rng <- range(Object@RegionToFit[[1]])
     if (xx < Rng[1]) {xx <- Rng[1]}
     if (xx > Rng[2]) {xx <- Rng[2]}
     Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
     Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
     yy <<- Object@Components[[FComp]]@param[1,1] #component height h
     yy <<- yy/hscale+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
     coords[1] <<- xx
     coords[2] <<- yy
     point.coords$x <<- range(Object@RegionToFit$x) #ordinata secondo estremo del survey
     point.coords$y <<- range(Object@RegionToFit$y) #ordinata secondo estremo del survey
     Xlimits <<- range(Object@RegionToFit$x)
     Ylimits <<- range(Object@RegionToFit$y)
     Object@Boundaries$x <<- Xlimits
     Object@Boundaries$y <<- Ylimits
     assign("Object", Object, envir=.GlobalEnv)
     Corners <- list(x=c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2]),
                     y=c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2]))
     point.index <<- 1
     refresh <<- TRUE
     SetZoom <<- FALSE
     NoFit <<- FALSE
  }



# --- Variable definitions ---
     XPSSample <- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     OldXPSSample <- XPSSample
     SpectName <- get("activeSpectName", envir=.GlobalEnv)
     Indx <- get("activeSpectIndx", envir=.GlobalEnv)
     Object <- XPSSample[[Indx]]
     assign("Object", Object, envir=.GlobalEnv)
     ComponentList <-names(slot(Object,"Components"))
     FNameList <- XPSFNameList()
     SpectList <- XPSSpectList(activeFName)
     FComp <- NULL

     WinSize <- as.numeric(XPSSettings$General[4])
     hscale <- hscale <- as.numeric(WinSize)
     WinScale  <- NULL

     coords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     point.coords <- list(x=range(Object@RegionToFit$x), #set original X range
                          y=range(Object@RegionToFit$y)) #set original Y range
     xx <- NULL
     yy <- NULL
     Corners <- list(x=c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2]),
                     y=c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2]))

     point.index <- 1

     refresh <- TRUE
     SetZoom <- FALSE
     NoFit <- FALSE
#Coreline boundaries
     if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        Xlimits <- range(Object@.Data[1])
        Ylimits <- range(Object@.Data[2])
        NoFit <- TRUE
     } else {
        LL<-length(Object@.Data[[1]])
        point.coords$x <- range(Object@RegionToFit$x) #set the X window extension == x range
        point.coords$y <- range(Object@RegionToFit$y) #set the Y window extension == y range
        Xlimits <- range(Object@RegionToFit$x)
        Ylimits <- range(Object@RegionToFit$y)
        Object@Boundaries$x <- Xlimits
        Object@Boundaries$y <- Ylimits
     }


#--- GWidget definition ---

     MCWindow <- gwindow("XPS MOVE COMPONENT", parent=c(100,0), visible = FALSE)
     addHandlerDestroy(MCWindow, handler=function(h, ...){  #if MainWindow unproperly closed with X
#-----                         stopping mouse handler
                               setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
                               eventEnv$onMouseMove <<- NULL
                               enentEnv <<- NULL
                               plot(XPSSample[[Indx]])         #replot the CoreLine
                           })

#--- Selection Group ---
     SelectGroup <- ggroup(horizontal=FALSE, spacing = 5, container=MCWindow)

     MCFrame1 <- gframe(text = " XPS Sample ", container = SelectGroup)

     XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                               ActFName <- svalue(XPS.Sample)
                               assign("activeFName", ActFName, envir=.GlobalEnv)
                               reset.vars()
                               if (NoFit == TRUE) { return() }
                               LoadCoreLine()
                               refresh <<- FALSE #now plot also the component marker
                               assign("Object", XPSSample[[Indx]], envir=.GlobalEnv)
                               replot()
                           }, container = MCFrame1)
     svalue(XPS.Sample) <- activeFName

     CCLbutton <- gbutton(" Change Core Line ", handler=function(h,...){
                               CLmainwin <- gwindow("CORELINE SELECTION", parent=MCWindow, visible=FALSE)
                               size(CLmainwin) <- c(200, 100)
                               CLgroup <- ggroup(label="", container=CLmainwin)

                               CLframe <- gframe(text="SELECT THE CORELINE", spacing=5, container=CLgroup)
                               SpectList <- XPSSpectList(activeFName)
                               CLobj <- gradio(SpectList, selected=Indx, horizontal=FALSE, handler=function(h,...){
                                    XPS.CL <- svalue(CLobj)
                                    XPS.CL <- unlist(strsplit(XPS.CL, "\\."))   #drop "NUMber." in component name
                                    Indx <- as.integer(XPS.CL[1])
                                    SpectName <- XPS.CL[2]
                                    assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                                    assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                                    LoadCoreLine()
                                    dispose(CLmainwin)
                               }, container=CLframe)
                               visible(CLmainwin)<-TRUE
                           }, container = SelectGroup)

     MCFrame2 <- gframe(text = " COMPONENTS ", spacing=5, container = SelectGroup)
     FitComplyt <- glayout(spacing=5, container=MCFrame2)
     if (length(ComponentList) > 1){    #gradio works with at least 2 items
         FitComp <- gradio(ComponentList, selected=1, handler = function(h,...){
                               refresh <<- TRUE    #cancel previous selections
                               replot()   #plot spectrum without marker
                               FComp <- svalue(FitComp)
                               FComp <- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                               FComp <- FComp[2]
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit[[1]])
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               FuncName <- Object@Components[[FComp]]@funcName
                               yy <- yy/GetHvalue(Object,FComp, FuncName, 1)
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]]>xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh <<- FALSE #now plot also the component marker
                               assign("Object", Object, envir=.GlobalEnv)
                               replot()   #replot spectrum and marker
                           },  container = MCFrame2)
               LL <- length(ComponentList)
               NCol <- ceiling(LL/5)   #gradio will be split in solumns of 5 elements
               for(ii in 1:LL){
                   tkpack.forget(FitComp$widgets[[ii]])  # unparent widgets (uses library call)
               }
               for(kk in 1:NCol){
                   NN <- (kk-1)*5
                   for (ii in 1:5) {
                        if((ii+NN) > LL) {break}
                        FitComplyt[ii,kk] <- FitComp$widgets[[(ii+NN)]]
                   }
               }
     } else {
         FitComp <- gcheckboxgroup(ComponentList, checked=TRUE, handler = function(h,...){
                               refresh<<-TRUE    #cancel previous component markers
                               replot()   #plot spectrum only
                               FComp <- svalue(FitComp)
                               FComp <- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected compoent
                               FComp <- FComp[2]
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit[[1]])
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh <<- FALSE #now plot spectrum + component marker
                               replot()
                           },  container = MCFrame2)
     }

     MCFrame3 <- gframe(text = " OPTIONS ", spacing=5, container = SelectGroup)
     Buttlyt <- glayout(spacing=5, container=MCFrame3)

     Buttlyt[1,1] <- LBFitbutton <- gbutton("          FIT Lev.Marq.        ", handler=function(h,...){
                               FComp <- svalue(FitComp)
                               Object <<- XPSFitLM(Object, plt=FALSE)
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx #coords of marker of the first fit component
                               coords[2] <<- yy
                               Object <<- sortComponents(Object)
                               refresh <<- FALSE  #now plot also the component marker
                               assign("Object", Object, envir = .GlobalEnv)
                               replot()
                           }, container = Buttlyt)

     Buttlyt[1,2] <- MFFitbutton <- gbutton("           FIT Modfit          ", handler=function(h,...){
                               FComp <- svalue(FitComp)
                               Object <<- XPSModFit(Object, plt=FALSE)
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               Object <<- sortComponents(Object)
                               refresh <<- FALSE #now plot also the component marker
                               assign("Object", Object, envir = .GlobalEnv)
                               replot()
                           }, container = Buttlyt)

     Buttlyt[2,1] <- ZRbutton<-gbutton("         SET ZOOM REGION       ", handler = function(h, ...){
                               CompCoords <<- coords   #save the of position component_marker
                               point.coords <<- NULL   #point.coords contain the X, Y data ranges
                               enabled(CCLbutton) <- FALSE
                               enabled(LBFitbutton) <- FALSE
                               enabled(MFFitbutton) <- FALSE
                               enabled(RSTbutton) <- FALSE
                               row1<-" => Right Clicks to define the 2 ZOOM REGION CORNERS (opposite in diagonal)"
                               row2<-"\n => Right click near corners to adjust Zoom Region Dimensions"
                               row3<-"\n => When Zoom Region OK, press MAKE ZOOM"
                               msg<-paste(row1, row2, row3, sep="")
                               gmessage( msg, icon="warning")
                               SetZoom <<- TRUE
                           }, container = Buttlyt)

     Buttlyt[2,2] <- MZbutton<-gbutton("           MAKE ZOOM           ", handler = function(h, ...){
                               if (Object@Flags[1]) { #Binding energy set
                                  point.coords$x <- sort(point.coords$x, decreasing=TRUE) #pos$x in decreasing order
                                  point.coords$x[1] <- point.coords$x[1]
                                  point.coords$x[2] <- point.coords$x[2]
                               } else {
                                  point.coords$x<-sort(point.coords$x, decreasing=FALSE) #pos$x in increasing order
                                  point.coords$x[1] <- point.coords$x[1]
                                  point.coords$x[2] <- point.coords$x[2]
                               }
                               Xlimits <<- point.coords$x
                               Ylimits <<- sort(point.coords$y, decreasing=FALSE)
 	                             slot(Object,"Boundaries") <<- point.coords
 	                             point.index <<- 1
 	                             coords <<- CompCoords #restore of position component_marker
 	                             refresh <<- FALSE
                               SetZoom <<- FALSE
                               assign("Object", Object, envir = .GlobalEnv)
                               replot()
                               enabled(CCLbutton) <- TRUE
                               enabled(LBFitbutton) <- TRUE
                               enabled(MFFitbutton) <- TRUE
                               enabled(RSTbutton) <- TRUE
                          }, container = Buttlyt)

     Buttlyt[3,1] <- RSTbutton <- gbutton("           RESET PLOT          ", handler = function(h, ...) {
                               SetZoom <<- FALSE
                               refresh <<- FALSE
  	                            point.index <<- 1
  	                            reset.plot()
                          }, container = Buttlyt)

     Buttlyt[3,2] <- gbutton("              UNDO             ", handler = function(h, ...) {
                               FComp <- svalue(FitComp)
                               XPSSample <<- OldXPSSample
                               assign("Object", XPSSample[[Indx]], envir = .GlobalEnv)
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               replot()
                          }, container = Buttlyt)

     Buttlyt[4,1] <- gbutton("          RE-LOAD DATA         ", handler=function(h,...){
                               LoadCoreLine()
                               OldXPSSample <<- XPSSample
                          }, container = Buttlyt)

     Buttlyt[4,2] <- gbutton("              SAVE             ", handler=function(h,...){
#    With button SAVE the Component parameters are updated and are now available for FiTConstraints
                               Indx <- get("activeSpectIndx", envir=.GlobalEnv)
                               XPSSample[[Indx]] <<- Object
                               OldXPSSample[[Indx]] <<- XPSSample[[Indx]]
                               assign("Object", XPSSample[[Indx]], envir = .GlobalEnv)
                               assign(activeFName, XPSSample, envir = .GlobalEnv)
                               replot()
                               XPSSaveRetrieveBkp("save")
                          }, container = Buttlyt)

     Buttlyt[5,1] <- gbutton("               EXIT            ", handler=function(h,...){
#-----            stopping mouse handler
                               setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
                               eventEnv$onMouseMove <<- NULL
                               enentEnv <<- NULL

                               dispose(MCWindow)    #Disposing MCWindow will activate GDestroyHandler which re-opens the graphic window
                               XPSSample <- get(activeFName, envir=.GlobalEnv) #Update XPSSample with all changes before plotting
                               XPSSaveRetrieveBkp("save")
                               plot(XPSSample[[Indx]])         #replot the CoreLine
                          }, container = Buttlyt)

     LabBox <- ggroup(spacing=1, horizontal=TRUE, container=SelectGroup)
     glabel("Graphical Window size: ", container=LabBox)
     WSvalue <- glabel(text=as.character(hscale), container=LabBox)
     WSize <- gslider(from = 0.5, to = 1.5, by = 0.1, value = hscale, horizontal=TRUE, handler=function(h,...){
                               WinSize <- svalue(WSize)
                               WinSize <- dev.size()*WinSize   #rescale the graphic window
                               delete(WSgroup, WSvalue)
                               WSvalue <<- glabel(text=as.character(WinSize), container=WSgroup)
#                               graphics.off()
#                               OS <- Sys.info["sysname"]
#                               switch (OS,
#                                  "Linux" =   {x11(type='Xlib', xpos=600, ypos=5, title=' ', width=WinSize[1], height=WinSize[2])},
#                                  "Windows" = {x11(xpos=600, ypos=5, title=' ', width=WinSize[1], height=WinSize[2])},
#                                  "MacOS-X" = {quartz(title=' ')},  #quartz() does allow setting the opening position
#                                  "Mac OS"  = {quartz(title=' ')},
#                                  "macOS"   = {quartz(title=' ')},
#                                  "Darwin"  = {quartz(title=' ')})
#                               refresh <<- FALSE #now plot also the component marker
#                               devset()
                               replot(Object)
                          }, container=SelectGroup)

     StatBar <- gstatusbar("status", container = MCWindow)

#--- Marker-----
     if (NoFit==FALSE){
        coords[1] <- Object@Components[[1]]@param[2,1] #component position mu
        coords[2] <- Object@Components[[1]]@param[1,1] #component1 height h
        FuncName <- Object@Components[[1]]@funcName
        coords[2] <- coords[2]/GetHvalue(Object,1, FuncName, 1)
        Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
        Xindx <- which(Object@RegionToFit[[1]] > coords[1]-Estep/2 & Object@RegionToFit[[1]] < coords[1]+Estep/2) #indice del vettore X corrispondente alla posizione della componente
        coords[2] <- coords[2]+Object@Baseline$y[Xindx]
        refresh <- FALSE
        replot()
        refresh <- TRUE
     }

     enabled(CCLbutton) <- TRUE
     enabled(LBFitbutton) <- TRUE
     enabled(MFFitbutton) <- TRUE
     enabled(RSTbutton) <- TRUE
     visible(MCWindow) <- TRUE

#--- Interactive mouse control ---
     setGraphicsEventHandlers(prompt="Waiting mouse clicks", onMouseDown = my.coords, onKeybd = keydown, which = dev.cur())
     eventEnv <- getGraphicsEventEnv()
     devset()
     getGraphicsEvent()

}






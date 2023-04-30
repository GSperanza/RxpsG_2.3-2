#   Fit components moved manually with Graphic Event Handler

#' @title XPSMoveComp
#' @description XPSMoveComp function to modify component position and intensity in a fit
#'   this function provides a userfriendly interface change position and intensity of each
#'   individual fitting component of a selected XPSCoreline. Changes are saved
#'   in the .GlobalEnv main software memory.
#' @examples
#' \dontrun{
#' 	XPSMoveComp()
#' }
#' @export
#'


XPSMoveComp <- function(){

  GetCurPos <- function(single){
       coords <<- NULL
       EXIT <- FALSE
       Estep <- abs(Object@RegionToFit[[1]][1] - Object@RegionToFit[[1]][2])
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                EXIT <- TRUE
            } else {
                if ( single ){ EXIT <- TRUE }
                if (SetZoom == TRUE) {  #define zoom area
                    xx <- pos$x
                    yy <- pos$y
                    Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                    yy_BasLin <- yy-Object@Baseline$y[Xindx]  #spectral intensity at xx without Baseline
                    coords <<- c(xx, yy, yy_BasLin)
                    RBmousedown()  #selection of the zoom area
                } else {
                    xx <- pos$x
                    yy <- pos$y
                    Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                    yy_BasLin <- yy-Object@Baseline$y[Xindx]  #spectral intensity at xx without Baseline
                    coords <<- c(xx, yy, yy_BasLin)
                    LBmousedown()  #selection of the BaseLine Edges
                }
            }
       }
       return()
   }


  my.coords <- function(buttons, x, y) { #converts the normalized device coords in user coords
      xx <- grconvertX(x, from="ndc", to="user")
      yy <- grconvertY(y, from="ndc", to="user")

      Xlim1 <- min(Object@RegionToFit[[1]])   #limits coordinates in the Spectrum Range
      Xlim2 <- max(Object@RegionToFit[[1]])
      Ylim1 <- min(Object@RegionToFit[[2]])
      Ylim2 <- max(Object@RegionToFit[[2]])

      if (xx < 0.95*Xlim1 ) {xx <- Xlim1}
      if (xx > 1.05*Xlim2 ) {xx <- Xlim2}
      if (yy < 0.95*Ylim1 ) {yy <- Ylim1}
      if (yy > 1.05*Ylim2 ) {yy <- Ylim2}

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
        eventEnv <<- NULL
        NULL
        return(1)
     }
  }

  LBmousedown <- function() {   #Left mouse button down
	    xx <- coords[1]
	    yy <- coords[2]
     if (SetZoom==FALSE) { #left button works only when SET ZOOM REGION inactive
        MoveComp()
    	   ## loop on spectra and retrieve Pass Energy
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
     	     point.index <<- 3
           Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
           Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
#  	     } else if (point.index == 2) { #Second rect corner C2 opposite to C1
#           point.index <<- 3
#           Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
#           Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
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

  Check.PE <- function(){
    	PassE <- NULL
       	PassE <- sapply( XPSSample, function(x) {
                      info <- x@Info[1]   #retrieve info containing PE value
                      sss <- strsplit(info, "Pass energy")  #extract PE value
                      PE <- strsplit(sss[[1]][2], "Iris") #PE value
                      PE <- gsub(" ", "", PE[[1]][1], fixed=TRUE) #drop white spaces in string PE
                      PE <- as.integer(PE)
                      return(PE)
		                  }
                  )
        SpectList <- XPSSpectList(activeFName)
        idx <- grep("Survey", SpectList)       #recognize presence of a Survey
        if (length(idx) == 0){ idx <- grep("survey", SpectList) }
        if (length(idx) == 0){ return(TRUE) }  #No survey spectra are present in XPSSample: continue working

        PEsur <- PassE[[ idx[1] ]]             #if idx is a vector, select the first element

        if (length(idx) > 0){                  #a survey is present
           SpectList <- SpectList[-idx]        #eliminate the "Survey" from list of Spectral Names
           PassE <- PassE[-idx]                #eliminate the PassE(survey) to compare PE of only core-lines
        }
        Extracted <- which(PassE == 160)
        LL <- length(Extracted)
        if (LL > 0){
            for(ii in 1:LL){
                Indx <- Extracted[ii]
                Indx <- unlist(strsplit(SpectList[Indx], "\\."))   
                Indx <- as.integer(Indx[1])                    #select "NUMber." in component name
                if ( hasRegionToFit(XPSSample[[Indx]])){
                     txt <- paste(SpectList[Extracted], collapse="  ")
                     txt <- paste(" Found Core Line: ", txt, " extracted from Survey.
                     \nCannot perform quantification here!
                     \nPlease exit  MOVE COMPONENT  and run  QUANTIFY  option to correct Core Line intensity")
                     gmessage(msg=txt, title="WARNING", icon="warning")
                     return(FALSE)
                     break
                }
            }
        }
        return(TRUE)
  }

  MoveComp <- function(...) {
#     FComp <- svalue(FitComp)

     if (length(FComp)==0) {
         gmessage(msg="Select Component Please", title="WARNING", icon = "warning")
     } else {
#        FComp <- as.numeric(unlist(strsplit(FComp, split="C")))   #index of the selected component
#        FComp <- FComp[2]
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
#Now compute the new component Y values for the new xy position
        Object@Components[[FComp]] <<- Ycomponent(Object@Components[[FComp]], x=Object@RegionToFit$x, y=Object@Baseline$y) #eomputes the Y value and add baseline
#Fit computed addind fit components with the modified ones
        tmp <- sapply(Object@Components, function(z) matrix(data=z@ycoor))
        Object@Fit$y <<- ( colSums(t(tmp)) - length(Object@Components)*(Object@Baseline$y))

        Object <<- sortComponents(Object)
#if component order changed then re-number them
        LL <- length(Object@Components) #N. fit components
        for (ii in 1:LL){
           if (xx == Object@Components[[ii]]@param["mu",1]) { #compare marker position with component positions
             idx <- ii
             break()
           }
        }
        svalue(FitComp) <- paste("C", idx, sep="")  #update component gradio
        XPSSample[[Indx]] <<- Object
     }
  }

  replot <- function(...) {
     if (point.index==1 && refresh==FALSE) {  #point.index==1 when moving mcomponent
         plot(Object, xlim=Xlimits, ylim=Ylimits)
         points(x=coords[1], y=coords[2], col=2, cex=1.2, lwd=2.5, pch=1)  # if refresh==FALSE plot spectrum with component marker
     } else if (SetZoom == TRUE){   #set zoom area corners
	        if (point.index == 1) {    #normal plot
 	           plot(Object)
             points(point.coords, type="p", col=4, cex=1.2, lwd=2.5, pch=3)
  	      } else if (point.index == 3){  #plot zoom area corners
 	           plot(Object, xlim=Xlimits, ylim=Ylimits)
             points(Corners, type="p", col=4, cex=1.2, lwd=2.5, pch=3)
             rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
         }
     } else {
         plot(Object, xlim=Xlimits, ylim=Ylimits)
     }
     if (! is.null(coords)){
         svalue(StatBar) <- paste("x =",round(coords[1],2), "y =",round(coords[2],2), sep="   ")
     }
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

  ComponentMenu <- function(){
       FComp <<- 1
       delete(MCFrame3,MCGroup3)
       MCGroup3 <<- ggroup(spacing=3, horizontal=FALSE, container=MCFrame3)
       FitComplyt <<- glayout(spacing=5, container=MCGroup3)
       if (length(ComponentList) > 1){    #gradio works with at least 2 items
           FitComp <<- gradio(ComponentList, selected=1, handler = function(h,...){
                               FComp <<- svalue(FitComp)
                               cat("\n selected component:", FComp)
                               FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                               FComp <<- FComp[2]
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit$x)
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               FuncName <- Object@Components[[FComp]]@funcName
                               yy <- yy/GetHvalue(Object,FComp, FuncName, 1)  #provides the correct yy value for complex functions
                               Estep <- abs(Object@RegionToFit$x[1]-Object@RegionToFit$x[2])
                               Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh <<- TRUE    #cancel previous selections
                               replot()   #plot spectrum without marker
                               refresh <<- FALSE #now plot also the component marker
                               replot()   #replot spectrum and marker
                               if (O.Sys == "linux"){ #in Linux locator is activated to read cursor positions
                                   if (ShowMsg==TRUE){
                                       gmessage("Left click to enter Fit Component position. Right click to stop slection", title="WARNING", icon="warning")
                                       tcl("update", "idletasks") #closes the gmessage window
                                   }
                                   GetCurPos(single=FALSE)
                               }
                           },  container = MCGroup3)
           LL <- length(ComponentList)
           NCol <- ceiling(LL/5)   #gradio will be split in solumns of 5 elements
           for(ii in 1:LL){
               tkpack.forget(FitComp$widgets[[ii]])  # unparent widgets (uses library call)
           }
           for(kk in 1:NCol){
               NN <- (kk-1)*5
               for (ii in 1:5) {
                    if((ii+NN) > LL) {break}
                    FitComplyt[ii,kk] <<- FitComp$widgets[[(ii+NN)]]
               }
           }
       }
       if (length(ComponentList) == 1){    #gradio works with at least 2 items
          FitComp <<- gcheckboxgroup(ComponentList, checked=TRUE, handler = function(h,...){
                               FComp <<- svalue(FitComp)
                               FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected compoent
                               FComp <<- FComp[2]
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               Rng <- range(Object@RegionToFit[[1]])
                               if (xx < Rng[1]) {xx <- Rng[1]}
                               if (xx > Rng[2]) {xx <- Rng[2]}
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               refresh<<-TRUE    #cancel previous component markers
                               replot()   #plot spectrum only
                               refresh <<- FALSE #now plot spectrum + component marker
                               replot()
                               if (O.Sys == "linux"){ #in Linux locator is activated to read cursor positions
                                   if (ShowMsg==TRUE){
                                       gmessage("Left click to enter Fit Component position. Right click to stop slection", title="WARNING", icon="warning")
                                       tcl("update", "idletasks") #closes the gmessage window
                                   }
                                   GetCurPos(single=FALSE)
                               }
                           },  container = MCGroup3)
     }
     if (O.Sys == "linux"){ #in Linux locator is activated to read cursor positions
         txt <- paste("The selection of a Core-Line or Fit-Component always activates reading the [X,Y] cursor position.",
                      "\n=> Left click with the mouse to enter the cursor coordinates.",
                      "\n=> Right click to stop position selection and cursor position reading when not required.",
                      "\n",
                      "=> Do not show these WARNING messages again press YES, let WARNINGS active press NO", sep="")
         ShowMsg <<- !gconfirm(msg=txt, title="WARNING", icon="question", width=30) #ShowMsg==FALSE if answer=YES
         tcl("update", "idletasks") #closes the gmessage window
         GetCurPos(single=FALSE)
     }

  }

  LoadCoreLine<-function(){
      Xlimits <<- range(Object@RegionToFit$x)
      Ylimits <<- range(Object@RegionToFit$y)
      Object@Boundaries$x <<- Xlimits
      Object@Boundaries$y <<- Ylimits
      point.coords$x <<- Xlimits #set original X range
      point.coords$y <<- Ylimits #set original Y range

      ComponentList <<- names(slot(Object,"Components"))
      if (length(ComponentList)==0) {
          gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
          return()
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
      if (UpdateCompMenu == TRUE){ ComponentMenu() }
  }

  editFitFrame <- function(h,...){
      FComp <- svalue(FitComp)
      FComp <- as.integer(gsub("[^0-9]", "", FComp))   #index selected component
      fitParam <<- Object@Components[[FComp]]@param #load DataFrame relative to the selected component
      VarNames <- rownames(fitParam)
      idx <- grep("lg", VarNames)
      if(length(idx) > 0){VarNames[idx] <- "Mix.L.G"}
      idx <- grep("gv", VarNames)
      if(length(idx) > 0){VarNames[idx] <- "Mix.G.V"}
      fitParam <<- as.matrix(fitParam) #this is needed to construct correctly the data.frame
      fitParam <<- data.frame(cbind(VarNames,fitParam), stringsAsFactors=FALSE) #in the dataframe add a column with variable names
      newFitParam <<- fitParam
      Label=paste("C", FComp, "- COMPONENT FIT PARAMETERS")
      DFwin <- gwindow(title=Label, visible=FALSE) # open a window to edit the dataframe
      DFgroup <- ggroup(horizontal=FALSE, container=DFwin)       
      txt <- paste("Fit Function: ", Object@Components[[FComp]]@funcName, sep="")
      glabel(txt, container=DFgroup)
      DFrame <- gdf(items=fitParam, container=DFgroup)
      size(DFrame) <- c(550, 200) 
      tkconfigure(DFrame$widget, width=550, height=200)      
      addHandlerChanged(DFrame, handler=function(h,...){ #addHandlerChanged dowload the dataFrame with modified parameters in NewFirParam (global variable)
         newFitParam <<- h$obj[]
      })

      gbutton("     SAVE AND EXIT      ", handler=function(h,...){
                newFP <- lapply(newFitParam[,2:ncol(newFitParam)], function(x) {as.numeric(x)} ) #the dataframe contais strings
                fitParam <<- fitParam[,-1]   #remove labels introduced in the first column of the DataFrame
                fitParam[, 1:ncol(fitParam)] <<- newFP   #this operation preserves the class(fitParam)=data.base nneded to save parameters in the relative slot of XPSSSample
                Object@Components[[FComp]]@param <<- fitParam #save parameters in the slot of XPSSample
                XPSSample[[Indx]] <<- Object
                NComp <- length(Object@Components)
                tmp <- NULL
                for(ii in 1:NComp){
                    Object@Components[[ii]] <<- Ycomponent(Object@Components[[ii]], x=Object@RegionToFit$x, y=Object@Baseline$y)
	                   tmp <- cbind(tmp, Object@Components[[ii]]@ycoor)  #fit is the sum of fitting components
	                   Object@Fit$y <<- colSums(t(tmp)) - length(Object@Components)*(Object@Baseline$y) #substract NComp*Baseline
                }
                Object <<- sortComponents(Object)
                xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                Rng <- range(Object@RegionToFit[[1]])
                if (xx < Rng[1]) {xx <- Rng[1]}
                if (xx > Rng[2]) {xx <- Rng[2]}
                yy <- Object@Components[[FComp]]@param[1,1] #component height h
                FuncName <- Object@Components[[FComp]]@funcName
                yy <- yy/GetHvalue(Object,FComp, FuncName, 1)  #provides the correct yy value for complex functions
                Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                yy <- yy + Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                coords[1] <<- xx   #Marker coords
                coords[2] <<- yy
                assign(activeFName, XPSSample, envir = .GlobalEnv)
                dispose(DFwin)
                XPSSaveRetrieveBkp("save")
                replot()
                return()
             }, container = DFgroup)
      visible(DFwin) <- TRUE
  }

  reset.vars <- function(){
     XPSSample <<- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     Indx <- activeSpectIndx
     if(activeSpectIndx > length(XPSSample)) { Indx <<- 1 }
     OldXPSSample <<- XPSSample
     Object <<- XPSSample[[Indx]]
     SpectName <<- NULL
     ComponentList <<- names(Object@Components)
     FNameList <<- XPSFNameList()
     SpectList <<- XPSSpectList(activeFName)
     FComp <<- 1
     FitComp <<- NULL
     UpdateCompMenu <<- TRUE     
     coords <<- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <<- c(xx=NA, yy=NA, yy_BasLin=NA)

     refresh <<- TRUE
     SetZoom <<- FALSE
     NoFit <<- FALSE
     ShowMsg <<- TRUE
     WinSize <<- as.numeric(XPSSettings$General[4])
     hscale <<- hscale <- as.numeric(WinSize)
     WinScale  <<- NULL

     if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        Xlimits <<- range(Object@.Data[1])
        Ylimits <<- range(Object@.Data[2])
        NoFit <<- TRUE
        return()
     }
  }

# --- Variable definitions ---
     XPSSample <- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     Indx <- activeSpectIndx
     if(activeSpectIndx > length(XPSSample)) { Indx <- 1 }
     OldXPSSample <- XPSSample
     Object <- XPSSample[[Indx]]
     SpectName <- NULL
     ComponentList <<- names(Object@Components)
     FNameList <- XPSFNameList()
     SpectList <- XPSSpectList(activeFName)
     FComp <- 1
     FitComp <- NULL
     UpdateCompMenu <- TRUE

     WinSize <- as.numeric(XPSSettings$General[4])
     hscale <- hscale <- as.numeric(WinSize)
     WinScale  <- NULL

     coords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     xx <- NULL
     yy <- NULL

     refresh <- TRUE
     SetZoom <- FALSE
     NoFit <- FALSE
     ShowMsg <- TRUE
#Coreline boundaries
     if (length(ComponentList)==0) {
        gmessage("ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        point.index <- 1
        point.coords <- list(x=range(Object@RegionToFit$x), #set the X window extension == x range
                             y=range(Object@RegionToFit$y)) #set the Y window extension == x range
        Xlimits <- range(Object@.Data[1])
        Ylimits <- range(Object@.Data[2])
        Object@Boundaries$x <- Xlimits
        Object@Boundaries$y <- Ylimits
        Corners <- list(x=NULL, y=NULL)
        NoFit <- TRUE
     } else {
        LL <- length(Object@.Data[[1]])
        point.index <- 1
        point.coords <- list(x=range(Object@RegionToFit$x), #set the X window extension == x range
                             y=range(Object@RegionToFit$y)) #set the Y window extension == x range
        Corners <- list(x=c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2]),
                        y=c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2]))
        Xlimits <- range(Object@RegionToFit$x)
        Ylimits <- range(Object@RegionToFit$y)
        Object@Boundaries$x <- Xlimits
        Object@Boundaries$y <- Ylimits
     }
     plot(Object)

     if (Check.PE() == FALSE) { return() }
     O.Sys <- unname(tolower(Sys.info()["sysname"]))
     eventEnv <- NULL

#--- GWidget definition ---
     MCWindow <- gwindow("XPS MOVE COMPONENT", parent=c(50,10), visible = FALSE)
     addHandlerDestroy(MCWindow, handler=function(h, ...){  #if MainWindow unproperly closed with X
#-----                         stopping mouse handler
                               setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
                               eventEnv <<- NULL
                               plot(XPSSample[[Indx]])         #replot the CoreLine
                           })

#--- Selection Group ---
     SelectGroup <- ggroup(horizontal=FALSE, spacing = 5, container=MCWindow)

     MCGroup1 <- ggroup(horizontal=TRUE, spacing = 5, container=SelectGroup)
     MCFrame1 <- gframe(text = " XPS Samples ", container = MCGroup1)
     XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                               reset.vars()
                               ActFName <- svalue(XPS.Sample)
                               assign("activeFName", ActFName, envir=.GlobalEnv)
                               XPSSample <<- get(ActFName, envir=.GlobalEnv)
                               Indx <<- activeSpectIndx
                               if(activeSpectIndx > length(XPSSample)) { Indx <<- 1 }
                               Object <<- XPSSample[[Indx]]
                               SpectList <- XPSSpectList(activeFName)
                               delete(MCFrame1, Core.Lines)
                               Core.Lines <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                                                   XPS.CL <- svalue(Core.Lines)
                                                   XPS.CL <- unlist(strsplit(XPS.CL, "\\."))
                                                   Indx <<- as.integer(XPS.CL[1])               #select "NUMber." in component name
                                                   SpectName <<- XPS.CL[2]
                                                   Object <<- XPSSample[[Indx]]
                                                   ComponentList <<- names(Object@Components)
                                                   assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                                                   assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                                                   point.index <<- 1
                                                   UpdateCompMenu <- TRUE
                                                   LoadCoreLine()
                                                }, container = MCFrame1)
                               refresh <<- FALSE #now plot also the component marker
                               Xlimits <<- range(Object@RegionToFit$x)
                               Ylimits <<- range(Object@RegionToFit$y)
                               Object@Boundaries$x <<- Xlimits
                               Object@Boundaries$y <<- Ylimits
                               plot(Object)
                           }, container = MCFrame1)
     svalue(XPS.Sample) <- activeFName

     MCFrame2 <- gframe(text = " Core-Lines ", container = MCGroup1)
     Core.Lines <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                               XPS.CL <- svalue(Core.Lines)
                               XPS.CL <- unlist(strsplit(XPS.CL, "\\."))
                               Indx <<- as.integer(XPS.CL[1])               #select "NUMber." in component name
                               SpectName <<- XPS.CL[2]
                               Object <<- XPSSample[[Indx]]
                               ComponentList <<- names(slot(Object,"Components"))
                               assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                               assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                               point.index <<- 1
                               UpdateCompMenu <- TRUE
                               LoadCoreLine()
                           }, container = MCFrame2)
     svalue(Core.Lines) <- SpectList[Indx]

     MCFrame3 <- gframe(text = " COMPONENTS ", spacing=5, container = SelectGroup)
     MCGroup3 <- ggroup(spacing=3, horizontal=FALSE, container=MCFrame3)
     FitComplyt <- glayout(spacing=5, container=MCGroup3)
     FitComplyt[1, 1] <- glabel("    ", container=FitComplyt)  #just to add a space in MCFrame3

     MCFrame4 <- gframe(text = " OPTIONS ", spacing=5, container = SelectGroup)
     Buttlyt <- glayout(spacing=5, container=MCFrame4)

     Buttlyt[1,1] <- LMFitbutton <- gbutton("          FIT Lev.Marq.        ", handler=function(h,...){
                               FComp <<- svalue(FitComp)
                               FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                               FComp <<- FComp[2]
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
                               if( is.na(match("FME", Pkgs)) == TRUE ){       #check if the package 'FME' is installed 
                                  txt <- "Package 'FME' not installed. \nOption 'ModFit' not available"
                                  gmessage(msg=txt, title="WARNING", icon="error")
                                  return()
                               }
                               FComp <<- svalue(FitComp)
                               FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                               FComp <<- FComp[2]
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

     Buttlyt[2,1] <- ZRbutton <- gbutton("         SET ZOOM REGION       ", handler = function(h, ...){
                               CompCoords <<- coords   #save the of position component_marker
                               point.coords <<- NULL   #point.coords contain the X, Y data ranges
                               enabled(LMFitbutton) <- FALSE
                               enabled(MFFitbutton) <- FALSE
                               enabled(RSTbutton) <- FALSE
                               SetZoom <<- TRUE
                               point.coords$x <<- range(Object@RegionToFit$x)
                               point.coords$y <<- range(Object@RegionToFit$y)
                               if (Object@Flags[1]) { #Binding energy set
                                   point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                               }
                               Xlimits <<- point.coords$x
                               Ylimits <<- point.coords$y
                               Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
                               Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
                               Marker <<- list(Points=Corners, col=4, cex=1.2, lwd=2.5, pch=3)
                               point.index <<- 3 #plot initial zoom area
                               replot()
                               if (O.Sys == "windows"){
                                   msg <- paste("\n => Right click near corners to adjust Zoom Region Dimensions",
                                                "\n => When Zoom Region OK, press MAKE ZOOM", sep="")
                                   gmessage(msg, title="WARNING", icon="warning")
                               } else if (O.Sys == "linux"){
                                   msg <- paste("\n => Left click near corners to adjust Zoom Region Dimensions",
                                                "\n => When Zoom Region OK, right click and press  MAKE ZOOM", sep="")
                                   gmessage(msg, title="WARNING", icon="warning")
                                   tcl("update", "idletasks") #closes the gmessage window
                                   GetCurPos(single=FALSE)
                               }
                           }, container = Buttlyt)

     Buttlyt[2,2] <- MZbutton <- gbutton("           MAKE ZOOM           ", handler = function(h, ...){
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
                               enabled(LMFitbutton) <- TRUE
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
                               FComp <<- svalue(FitComp)
                               FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                               FComp <<- FComp[2]
                               XPSSample <<- OldXPSSample
                               Object <<- XPSSample[[Indx]]
                               xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                               yy <- Object@Components[[FComp]]@param[1,1] #component height h
                               Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                               Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                               yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                               coords[1] <<- xx
                               coords[2] <<- yy
                               replot()
                          }, container = Buttlyt)

     Buttlyt[4,1] <- gbutton("        EDIT PARAMETERS        ", handler=editFitFrame, container = Buttlyt)

     Buttlyt[4,2] <- gbutton("          RE-LOAD DATA         ", handler=function(h,...){
                               UpdateCompMenu <- FALSE
                               LoadCoreLine()
                               OldXPSSample <<- XPSSample
                          }, container = Buttlyt)

     Buttlyt[5,1] <- gbutton("              SAVE             ", handler=function(h,...){
#    With button SAVE the Component parameters are updated and are now available for FiTConstraints
                               Indx <- get("activeSpectIndx", envir=.GlobalEnv)
                               XPSSample[[Indx]] <<- Object
                               OldXPSSample[[Indx]] <<- XPSSample[[Indx]]
                               assign("Object", XPSSample[[Indx]], envir = .GlobalEnv)
                               assign(activeFName, XPSSample, envir = .GlobalEnv)
                               replot()
                               XPSSaveRetrieveBkp("save")
                          }, container = Buttlyt)

     Buttlyt[5,2] <- gbutton("               EXIT            ", handler=function(h,...){
#-----            stopping mouse handler
                               setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
                               eventEnv <<- NULL
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
                               svalue(WSvalue) <- paste("Graphical Window size: ", as.character(WinSize), sep="")
                               WinSize <<- dev.size()*WinSize   #rescale the graphic window
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

     enabled(LMFitbutton) <- TRUE
     enabled(MFFitbutton) <- TRUE
     enabled(RSTbutton) <- TRUE
     visible(MCWindow) <- TRUE
     tkevent.generate(MCWindow$widget, "<Expose>", when="now") #forces the MCWindow to be exposed
     tcl("update", "idletasks") #Complete the idle tasks

#--- Interactive mouse control ---
     if (O.Sys == "windows"){
         ComponentMenu()
         devset <- function(){ #returns the ID of the current active graphic device
                   if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
                }
         setGraphicsEventHandlers(prompt="Waiting for mouse clicks", which = dev.cur())
         eventEnv <- getGraphicsEvent(onMouseDown = my.coords, onKeybd = keydown)
     }
     if (O.Sys == "linux" && length(ComponentList)>0 && visible(MCWindow)){
        ComponentMenu()
     }
}






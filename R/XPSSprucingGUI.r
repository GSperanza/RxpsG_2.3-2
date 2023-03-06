#-----------------------------------------
# XPS Sprucing with gWidgets2 and tcltk
#-----------------------------------------

#'@title XPSSprucingGUI
#'@description XPSSprucingGUI function to correct original XPS spectral data
#'@return Returns the \code{Object} with the corrected spectrum.
#'@examples
#'\dontrun{
#'	XPSSprucingGUI()
#'}
#'@export
#'

XPSSprucingGUI <- function() {

  devset <- function(){ #returns the ID of the current active graphic device
      if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
  }

  my.coords <- function(buttons, x, y) { #converts the normalized device coords in user coords
      xx <- grconvertX(x, from="ndc", to="user")
      yy <- grconvertY(y, from="ndc", to="user")
      coords <<- c(xx, yy)

      Xlim1 <- min(range(Object[[coreline]]@.Data[[1]]))   #limits coordinates in the Spectrum Range
      Xlim2 <- max(range(Object[[coreline]]@.Data[[1]]))
      Ylim1 <- min(range(Object[[coreline]]@.Data[[2]]))
      Ylim2 <- max(range(Object[[coreline]]@.Data[[2]]))

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
     	if (point.index==1) {
     	   point.index<<-2    #to modify the second edge of the selected area
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
  	   } else if (point.index==2) {
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
         point.index<<-3
  	   } else if (point.index==3) {
         D<-vector("numeric", 4)
         Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
         for (ii in 1:4) {
             D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
             if(D[ii] <= Dmin){
                Dmin<-D[ii]
                idx=ii
             }
         }
         if (idx==1){
            Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
            Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx==2){
             Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
             Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         } else if (idx==3){
             Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
             Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx==4){
            Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
            Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         }
         if (Object[[coreline]]@Flags[1]) { #Binding energy set
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         } else {
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #ordina pos$x in ordine crescente
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         }
      }
      replot()
  }

  undo.plot <- function(...){
      if (SelReg==1) {
         reset.boundaries()
     	   replot()
      } else if (SelReg>1) {
	      Object[[coreline]]@Boundaries$x <<- OldCoords$x
	      Ylimits <<- OldCoords$y
     	   replot()
  	   }
  }

  replot <- function(...) {
      if (coreline == 0) {     # coreline == "All spectra"
          plot(Object)
      } else {
         Xlimits <- Object[[coreline]]@Boundaries$x
	        if (point.index <= 2) {
	  	         plot(Object[[coreline]], xlim=Xlimits)
             points(point.coords, col="blue", cex=1, lwd=2.5, pch=3)
 	       } else if (point.index>2){
	  	         plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
             points(Corners, type="p", col="blue", cex=1, lwd=2.5, pch=3)
             rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
	        }
      }
  }

  reset.boundaries <- function(h, ...) {
	     Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
      LL<-length(Object[[coreline]]@.Data[[1]])
      point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1] #ascissa primo estremo 1 del survey
      point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1] #ordinata primo estremo 1 del survey
      point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
      point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
      slot(Object[[coreline]],"Boundaries") <<- point.coords
      Ylimits<<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
      OldCoords <<- point.coords #for undo
      Corners <- point.coords
      point.index <<- 1
      replot()
  }

  do.editRegion <- function(h, ...){
              idx1 <<- point.coords$x[1]
              idx2 <<- point.coords$x[2]
              newcoreline <- Object[[coreline]]
              idx1 <<- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[1]) #indice relativo al limite superiore (inferiore KE?) della RegionToFit
              idx2 <<- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[2]) #indice relativo al limite inferiore (superiore KE?) della RegionToFit
              tmp <- unlist(Object[[coreline]]@.Data[1]) #estraggo le ascisse regione selezionata
              newcoreline@.Data[[1]] <- tmp[idx1:idx2]     #aggiungo e ascisse regione selezionata
              tmp <- unlist(Object[[coreline]]@.Data[2]) #estraggo le ordinate regione selezionata
              newcoreline@.Data[[2]] <- tmp[idx1:idx2]     #aggiungo le ordinate regione selezionata
              DataTable <<- as.data.frame(cbind(newcoreline@.Data[[1]], newcoreline@.Data[[2]]))
              names(DataTable) <<- c("  X  ", "  Y  ")
              delete(EditGroup, DataToChange) #the pointer DataToChange is still alive. I add the pointer to a still unknown widget to EditGroup
              DataToChange <<- gdf(items=DataTable, container=EditGroup) #Now I define the widget associated to the pointer DataToChange
              size(DataToChange) <<- c(200,180)
              addHandlerChanged(DataToChange, handler=function(h,...){ #addHandlerChanged scarica il dataframe modificato in NewFirParam che e' salvato al di fuori di saveFitParam attraverso la <<-
                                     DataTable <<- h$obj[]
                               })
              enabled(ButtGroup) <- TRUE
  }


#====== VARIABLES DEFINITION=======
  Object <- get(activeFName,envir=.GlobalEnv)
  SpectList <- XPSSpectList(activeFName)
  coreline <- 0 #get("activeSpectIndx",envir=.GlobalEnv)
  XPSSettings <- get("XPSSettings",envir=.GlobalEnv)
  WinSize <- as.numeric(XPSSettings$General[4])
  DataTable <- NULL
  idx1 <- NULL
  idx2 <- NULL

  point.coords <- list(x=NA,y=NA)
  point.index <- 1
  coords <- NA # for printing mouse coordinates on the plot
  XLimits <- Ylimits <- NULL
  OldCoords <- point.coords #for undo
  Corners <- point.coords
  xx <- NULL
  yy <- NULL
#Coreline boundaries
  SelReg <- 0
  DataToChange <- NULL
  
  LL <- length(Object)
  if (LL == 1) { #XPSSample contains only one Core-Line
      coreline <- 1
      LL <- length(Object[[coreline]]@.Data[[1]])
      point.coords$x[1] <- Object[[coreline]]@.Data[[1]][1]  #ascissa primo estremo 1 del survey
      point.coords$y[1] <- Object[[coreline]]@.Data[[2]][1]  #ordinata primo estremo 1 del survey
      point.coords$x[2] <- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
      point.coords$y[2] <- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
      Ylimits<-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
      OldCoords <- point.coords #for undo
      Corners <- point.coords
      Object[[coreline]]@Boundaries$x <- c(point.coords$x)
      Object[[coreline]]@Boundaries$y <- c(point.coords$y)
  }


#====== Widget Definition =======
  SPwin <- gwindow("XPS SPRUCING GUI", parent=c(50,10), toolkit = "tcltk", visible = FALSE)
  addHandlerDestroy(SPwin, handler=function(h, ...){  #if MainWindow unproperly closed with X
#-----                         stopping mouse handler
                               setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
                               eventEnv$onMouseMove <<- NULL
                               eventEnv <<- NULL
                               plot(Object)
                           })

  size(SPwin) <- c(250, 600)
  MainGroup <- ggroup(horizontal = FALSE, spacing=3, container = SPwin)

#  SPlayout <- glayout(homogeneous=FALSE, spacing=3, container=MainGroup)

#====== OPTIONS SECTION =======
#  SPlayout[1, 1] <- OptGroup <- ggroup(horizontal = FALSE, spacing = 3, container = SPlayout)
#  OptGroup <- ggroup(horizontal = FALSE, spacing = 3, container = MainGroup)

  gframe10 <- gframe(text="SELECT CORELINE", expand=TRUE, spacing=3, container=MainGroup)
  CLobj10 <- gcombobox(SpectList, selected=coreline, editable=FALSE, handler=function(h,...){
              XPSCL <- svalue(CLobj10)
              XPSCL <- unlist(strsplit(XPSCL, "\\."))   #remove number at CoreLine beginning
              coreline <<- as.numeric(XPSCL[1])
              #Coreline boundaries
              LL <- length(Object[[coreline]]@.Data[[1]])
              point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1] #ascissa primo estremo 1 del survey
              point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1] #ordinata primo estremo 1 del survey
              point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
              point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
              Ylimits <<- c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
              OldCoords <<- point.coords #for undo
              Corners <<- point.coords
              SelReg <<- 0
              Object[[coreline]]@Boundaries$x <<- c(point.coords$x)
              Object[[coreline]]@Boundaries$y <<- c(point.coords$y)
              cat("\n Please select the portion of the spectrum to control")
              refresh <<- FALSE # this causes plotting markers at boundaries
              replot()
         }, container=gframe10)

  gframe22 <- gframe(text = " SELECT DATA ", spacing=3, horizontal = FALSE, container = MainGroup)

  gbutton(" SELECT REGION ", handler = function(h, ...){
              OldCoords <<- Object[[coreline]]@Boundaries
              SelReg <<- SelReg+1
              rngX <- range(point.coords$x)
              rngX <- (rngX[2]-rngX[1])/20
              rngY <- range(point.coords$y)
              rngY <- (rngY[2]-rngY[1])/20

              if (Object[[coreline]]@Flags[1]) { #Binding energy set
                 point.coords$x<-sort(point.coords$x, decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]+rngX/20
                 point.coords$x[2] <- point.coords$x[2]-rngX/20
              } else {
                 point.coords$x<-sort(point.coords$x, decreasing=FALSE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]-rngX/20
                 point.coords$x[2] <- point.coords$x[2]+rngX/20
              }
              point.coords$y <- sort(point.coords$y, decreasing=FALSE)
              Ylimits <<- c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
 	            slot(Object[[coreline]],"Boundaries") <<- point.coords
              replot()
         }, container = gframe22 )

  gbutton(" EDIT REGION ", handler = function(h, ...){
              do.editRegion()
  	      }, container = gframe22 )

  gbutton(" UNDO ", handler = function(h, ...) {
  	           undo.plot()
  	      }, container = gframe22 )

  gbutton(" RESET BOUNDARIES ", handler = function(h, ...) {
  	           reset.boundaries()
  	      }, container = gframe22 )

  gframe23 <- gframe(text = " SPRUCING ", horizontal=TRUE, container = MainGroup)

  EditGroup <- ggroup(container=gframe23)
  DataToChange <- gtext(" Data to correct:  ", container=EditGroup)
  size(DataToChange) <- c(200,180)

  ButtGroup <- ggroup(horizontal=FALSE, container=gframe23)
  buttOK<-gbutton("OK", handler=function(h, ...){
              Object[[coreline]]@.Data[[1]][idx1:idx2] <<- DataTable[[1]]
              Object[[coreline]]@.Data[[2]][idx1:idx2] <<- DataTable[[2]]
              delete(EditGroup, DataToChange)
              DataToChange <<- gtext(" Data to correct:  ", container=EditGroup)
              size(DataToChange) <<- c(200,180)
              add(EditGroup, DataToChange)
              replot(Object[[coreline]])
            }, container=ButtGroup)

  buttCanc<-gbutton("CANCEL", handler=function(...) {
              delete(EditGroup, DataToChange)
              DataToChange <<- gtext(" Data to correct:  ", container=EditGroup)
              size(DataToChange) <<- c(200,180)
              add(EditGroup, DataToChange)
              reset.boundaries()
            }, container=ButtGroup)
  enabled(ButtGroup) <- FALSE

#=== Graphic Win Size ===
  gframe24 <- gframe(text = "Graphic Window Dimensions", horizontal=FALSE, container = MainGroup)
  Egroup3 <- ggroup(horizontal=FALSE, spacing=3, container=gframe24)
  txt <- paste("Graphical Window size: ", as.character(WinSize), sep="")
  WSvalue <- glabel(text=txt, spacing=3, container=Egroup3)

  WSize <- gslider(from = 0.5, to = 1.5, by = 0.1, value = WinSize, horizontal=TRUE, handler=function(h,...){
                        WinSize <- svalue(WSize)
                        svalue(WSvalue) <- paste("Graphical Window size: ", as.character(WinSize), sep="")
                        WinSize <<- dev.size()*as.numeric(WinSize)   #rescale the graphic window
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

         }, container=Egroup3)


#=== CLOSE button ===
  gbutton("SAVE", handler = function(h, ...){
	             assign(activeFName, Object, envir = .GlobalEnv)
  	           reset.boundaries()
  	           XPSSaveRetrieveBkp("save")
         }, container = MainGroup)

  gbutton("SAVE & EXIT", handler = function(h, ...) {
#-----        stopping mouse handler
              setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
              eventEnv$onMouseMove <<- NULL
              eventEnv <<- NULL

	             assign(activeFName, Object, envir = .GlobalEnv)
              dispose(SPwin)
              XPSSaveRetrieveBkp("save")
              plot(Object)
         }, container = MainGroup)

  gbutton("EXIT", handler = function(h, ...) {
#-----        stopping mouse handler
              setGraphicsEventHandlers(prompt = "EXIT", onMouseDown = NULL, onKeybd = NULL, which = dev.cur())
              eventEnv$onMouseMove <<- NULL
              eventEnv <<- NULL

              dispose(SPwin)
              XPSSaveRetrieveBkp("save")
              plot(Object)
         }, container = MainGroup)

  visible(SPwin) <- TRUE

#--- Interactive mouse control ---
  setGraphicsEventHandlers(prompt="Waiting for mouse clicks", onMouseDown = my.coords, onKeybd = keydown, which = dev.cur())
  eventEnv <- getGraphicsEventEnv()
  devset()
  getGraphicsEvent()

}

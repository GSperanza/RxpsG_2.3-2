#-----------------------------------------
# XPS Sprucing with gWidgets2 and tcltk
#-----------------------------------------

#'@title XPSSprucingGUI
#'@description XPSSprucingGUI function to correct original XPS spectral data
#'@return Returns the \code{Object} with the corrected spectrum.
#'@examples
#'\dontrun{
#' XPSSprucingGUI()
#'}
#'@export
#'

XPSSprucingGUI <- function() {

    GetCurPos <- function(SingClick){
       coords <<- NULL
       enabled(OptFrame) <- FALSE   #prevent exiting Analysis if locatore active
       enabled(ButtGroup) <- FALSE
       enabled(ExitFrame) <- FALSE
       EXIT <- FALSE
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                enabled(OptFrame) <- TRUE
                enabled(ButtGroup) <- TRUE
                enabled(ExitFrame) <- TRUE
                EXIT <- TRUE
            } else {
                if ( SingClick ){ 
                    coords <<- c(pos$x, pos$y)
                    enabled(OptFrame) <- TRUE
                    enabled(ButtGroup) <- TRUE
                    enabled(ExitFrame) <- TRUE
                    EXIT <- TRUE
                } else {
                    Xlim1 <- min(range(Object[[coreline]]@.Data[[1]]))   #limits coordinates in the Spectrum Range
                    Xlim2 <- max(range(Object[[coreline]]@.Data[[1]]))
                    Ylim1 <- 0.95*min(range(Object[[coreline]]@.Data[[2]]))
                    Ylim2 <- 1.05*max(range(Object[[coreline]]@.Data[[2]]))

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
      point.coords$x[point.index] <<- coords[1]   #abscissa
      point.coords$y[point.index] <<- coords[2]   #ordinate
      if (point.index==1) {
         point.index <<- 2    #to modify the second edge of the selected area
         Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
      } else if (point.index==2) {
         Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
         point.index <<- 3
      } else if (point.index==3) {
         D <- vector("numeric", 4)
         Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
         for(ii in 1:4) {
             D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
             if(D[ii] <= Dmin){
                Dmin <- D[ii]
                idx=ii
             }
         }
         if (idx==1){
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
            point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
            point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         } else {
            point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #ordina pos$x in ordine crescente
            point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         }
      }
      replot()
  }

  undo.plot <- function(...){
      if (SelReg == 1) {
         reset.boundaries()
         replot()
      } else if (SelReg > 1) {
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
             points(point.coords, col="blue", cex=1.5, lwd=2, pch=3)
         } else if (point.index>2){
             plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
             points(Corners, type="p", col="blue", cex=1.5, lwd=2, pch=3)
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
      Ylimits <<- c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
      OldCoords <<- point.coords #for undo
      Corners <- point.coords
      point.index <<- 1
      replot()
  }

  EditRegion <- function(h, ...){
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
  FNameList <- XPSFNameList()
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
      Ylimits <-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
      OldCoords <- point.coords #for undo
      Corners <- point.coords
      Object[[coreline]]@Boundaries$x <- c(point.coords$x)
      Object[[coreline]]@Boundaries$y <- c(point.coords$y)
  }


#====== Widget Definition =======
  SPwin <- gwindow("XPS SPRUCING GUI", parent=c(50,10), toolkit = "tcltk", visible = FALSE)
  addHandlerDestroy(SPwin, handler=function(h, ...){  #if MainWindow unproperly closed with X
                               EXIT <<- TRUE
                               XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                               assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                               plot(Object)
                           })

  size(SPwin) <- c(250, 600)
  MainGroup <- ggroup(horizontal = FALSE, spacing=3, container = SPwin)

#  SPlayout <- glayout(homogeneous=FALSE, spacing=3, container=MainGroup)

#====== OPTIONS SECTION =======
#  SPlayout[1, 1] <- OptGroup <- ggroup(horizontal = FALSE, spacing = 3, container = SPlayout)
#  OptGroup <- ggroup(horizontal = FALSE, spacing = 3, container = MainGroup)


  gframe10 <- gframe(text="SELECT XPS SAMPLE and CORELINE", expand=TRUE, horizontal=TRUE, spacing=3, container=MainGroup)
  XPObj10 <- gcombobox(FNameList, selected=-1, editable=FALSE, spacing=10, handler=function(h,...){
                       XPSSample <- svalue(XPObj10)  
                       Object <<- get(XPSSample, envir=.GlobalEnv) 
                       plot(Object) 
                       SpectList <<- XPSSpectList(XPSSample)   
                       delete(gframe10, CLobj10) 
                       CLobj10 <<- gcombobox(SpectList, selected=coreline, editable=FALSE, handler=function(h,...){
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
                        }  , container = gframe10)
  svalue(XPObj10) <- activeFName


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

  OptFrame <- gframe(text = " SELECT DATA ", spacing=3, horizontal = FALSE, container = MainGroup)

  gbutton(" SELECT REGION ", handler = function(h, ...){
              OldCoords <<- Object[[coreline]]@Boundaries
              SelReg <<- SelReg+1
              if (point.coords$x == Object[[coreline]]@Boundaries$x) {
                  txt <- paste("Left Mouse Button to Define the Region to Edit\n",
                               "Right Mouse Button to ZOOM\n",
                               "Then Optimize the Selected Region Clicking near Corners\n",
                               "When OK Right Mouse Button and then Press the EDIT REGION Button", sep="")
                  gmessage(msg=txt, title="WARNING", icon="warning")
              }
              GetCurPos(SingClick=FALSE)
              rngX <- range(point.coords$x)
              rngX <- (rngX[2]-rngX[1])/20
              rngY <- range(point.coords$y)
              rngY <- (rngY[2]-rngY[1])/20
              if (Object[[coreline]]@Flags[1]) { #Binding energy set
                 point.coords$x <- sort(point.coords$x, decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]+rngX/20
                 point.coords$x[2] <- point.coords$x[2]-rngX/20
              } else {
                 point.coords$x <- sort(point.coords$x, decreasing=FALSE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]-rngX/20
                 point.coords$x[2] <- point.coords$x[2]+rngX/20
              }
              point.coords$y <- sort(point.coords$y, decreasing=FALSE)
              Ylimits <<- c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
              slot(Object[[coreline]],"Boundaries") <<- point.coords
              replot()
              GetCurPos(SingClick=FALSE)
         }, container = OptFrame )

  gbutton(" EDIT REGION ", handler = function(h, ...){
              EditRegion()
         }, container = OptFrame )

  gbutton(" UNDO ", handler = function(h, ...) {
              undo.plot()
         }, container = OptFrame )

  gbutton(" RESET BOUNDARIES ", handler = function(h, ...) {
              reset.boundaries()
         }, container = OptFrame )

  EditFrame <- gframe(text = " DATA SPRUCING ", horizontal=TRUE, container = MainGroup)

  EditGroup <- ggroup(container=EditFrame)
  DataToChange <- gtext(" Data to correct:  ", container=EditGroup)
  size(DataToChange) <- c(200,180)

  ButtGroup <- ggroup(horizontal=FALSE, container=EditFrame)
  buttOK <- gbutton("OK", handler=function(h, ...){
              Object[[coreline]]@.Data[[1]][idx1:idx2] <<- DataTable[[1]]
              Object[[coreline]]@.Data[[2]][idx1:idx2] <<- DataTable[[2]]
              delete(EditGroup, DataToChange)
              DataToChange <<- gtext(" Data to correct:  ", container=EditGroup)
              size(DataToChange) <<- c(200,180)
              add(EditGroup, DataToChange)
              reset.boundaries()
              replot(Object[[coreline]])
            }, container=ButtGroup)

  buttCanc <- gbutton("RESET", handler=function(...) {
              delete(EditGroup, DataToChange)
              DataToChange <<- gtext(" Data to correct:  ", container=EditGroup)
              size(DataToChange) <<- c(200,180)
              add(EditGroup, DataToChange)
              reset.boundaries()
            }, container=ButtGroup)
  enabled(ButtGroup) <- FALSE

#=== CLOSE button ===
  ExitFrame <- gframe(text = "    Save & Exit    ", horizontal=TRUE, container = MainGroup)
  gbutton("SAVE", handler = function(h, ...){
              assign(activeFName, Object, envir = .GlobalEnv)
              reset.boundaries()
              XPSSaveRetrieveBkp("save")
         }, container = ExitFrame)

  SaveBtn <- gbutton("    SAVE & EXIT    ", handler = function(h, ...) {
              assign(activeFName, Object, envir = .GlobalEnv)
              dispose(SPwin)
              XPSSaveRetrieveBkp("save")
              plot(Object)
         }, container = ExitFrame)

  ExitBtn <- gbutton("    EXIT    ", handler = function(h, ...) {
              dispose(SPwin)
              XPSSaveRetrieveBkp("save")
              plot(Object)
         }, container = ExitFrame)

  visible(SPwin) <- TRUE
}

#Function to show the list of XPSSamples loaded in RXPSGUI and the list of their core lines

#' @title XPSSetFNameCLine
#' @description XPSSetFNameCLine function to select an objects of class XPSSample
#'   and objects of class CoreLine
#'   The list of XPS-Samples loaded is presented for selection.
#'   After selection of the XPS-Sample the list of correspondent Corelines is available for selection.
#' @examples
#' \dontrun{
#' 	XPSSetFNameCLine()
#' }
#' @export
#'


XPSSetFNameCLine <- function() {

   updateObj <- function(h,...){
      SelectedFName <- svalue(T1obj1)
      FName <<- get(SelectedFName,envir=.GlobalEnv)  #Load the XPSSample
      SpectList <<- XPSSpectList(SelectedFName)
      delete(T2frame1,T2obj1)
      T2obj1 <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=SetSpectrum, container=T2frame1)
      add(T2frame1,T2obj1)
      assign("activeFName", SelectedFName,envir=.GlobalEnv) #set the core-line to the actual active Spectrum
      Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
      cutPos <- regexpr("title='", Gdev)+6
      Gdev <- substr(Gdev, start=1, stop=cutPos)
      Gdev <- paste(Gdev, activeFName,"')", sep="")     #add the correct window title
      graphics.off() #switch off the graphic window
      eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
      plot(FName)  #plot selected XPSSample with all the corelines
   }



   SetSpectrum <- function(h,...){
      SpectName <- svalue(T2obj1)
      SpectName <- unlist(strsplit(SpectName, "\\."))   #split the Spect name in core-line index  and   core-line name
      indx <<- as.integer(SpectName[1])
      assign("activeSpectName", SpectName[2], envir=.GlobalEnv) #save the active spectrum name
      assign("activeSpectIndx", indx, envir=.GlobalEnv) #save the active spectgrum index
      plot(FName[[indx]])
   }




# --- variables
   FName <- get(activeFName,envir=.GlobalEnv)   #load the active XPSSample
   FNameList<-XPSFNameList()
   LL=length(FNameList)
   SampID<-""
   SpectList<-""

#===== NoteBook =====

   win <- gwindow("XPSsample & CORELINE SELECTION", visible=FALSE)
   nb <- gnotebook(expand=TRUE, container = win)

# --- Tab1 ---
   T1group1 <- ggroup(label=" SELECT XPSsample ", horizontal=FALSE, container=nb)
   T1layout <- glayout(container=T1group1)
   T1layout[1,1] <- T1frame1 <- gframe(" SELECT XPSsample ", spacing=5, container=T1layout)
   T1obj1 <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=updateObj, container = T1frame1)

   T1layout[2,1] <- T1obj2 <- gbutton("     EXIT      ", handler=function(h,...){
                    dispose(win)
                    XPSSaveRetrieveBkp("save")
                 }, container = T1layout)

# --- Tab2 ---
   T2group1 <- ggroup(label=" SELECT CORELINE ",horizontal=FALSE, container=nb)
   T2layout <- glayout(container=T2group1)
   T2layout[1,1] <- T2frame1 <- gframe(text=" SELECT CORELINE ", spacing=5, container=T2layout)
   T2obj1 <- gcombobox(SpectList, selected=1, editable=FALSE, handler=SetSpectrum, container=T2frame1)

   T2layout[2,1] <- T2obj2<-gbutton("     EXIT      ", handler=function(h,...){
                    dispose(win)
                    XPSSaveRetrieveBkp("save")
                 }, container = T2layout)

   svalue(nb) <- 2     #refresh notebook pages
   svalue(nb) <- 1
   visible(win) <- TRUE

}

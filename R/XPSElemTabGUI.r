# XPSElemTab builds the element tables for assigning elements in a survey spectrum

#' @title XPSElemTab
#' @description XPSElemTab constructs the CoreLine and Auger Transition tables
#'   this function helps the identification of chemical elements in Survey spectra.
#' @examples
#' \dontrun{
#' 	XPSElemTab()
#' }
#' @export
#'

XPSElemTab <-function() {

ShowLines <- function(){
              LL<-length(elmt)
              if (elmt=="" || LL==0) { return() }
              if (svalue(HoldPlot)==FALSE){
                  plot(Object[[SpectIndx]])   #refresh plot
              }
              if (svalue(ShowCL)) {
                 idx <- grep(elmt, ElmtList1[,1])
                 for (ii in seq_along(idx)){
                     xx <- ElmtList1[idx[ii],3]
                     lines(x=c(xx, xx), y=rangeY, col="red")   #plot corelines of the selected elements
                 }
              }
              if (svalue(ShowAuger)) {
                  idx <- grep(elmt, ElmtList2[,1])
                  for (ii in seq_along(idx)){
                      xx <- ElmtList2[idx[ii],3]
                      lines(x=c(xx, xx), y=rangeY, col="blue") #plot corelines of the selected elements
                  }
              }
   }


#---- Var-Initialization
   plot.new()
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   Object<-get(activeFName,envir=.GlobalEnv)
   FNameList <- XPSFNameList() #list of XPSSamples
   SpectList <- XPSSpectList(activeFName)
   SpectIndx <- grep("Survey",SpectList)[1]   #switch to the survey spectrum
   if (length(SpectIndx) == 0 || is.na(SpectIndx)){
       SpectIndx <- grep("survey",SpectList)[1]
   }
   rangeX <- range(Object[[SpectIndx]]@.Data[2])
   rangeY <- range(Object[[SpectIndx]]@.Data[2])
   plot(Object[[SpectIndx]])
   RecPlot <- recordPlot()   #save graph for UNDO
   if (Object[[SpectIndx]]@Flags[3]) {
       ftype<-"scienta" #scienta filetype
   } else {
       ftype<-"kratos"  #kratos filetype
   }
   ElmtList1 <- ReadElmtList("CoreLines") #reads the CoreLine Table see XPSSurveyUtilities()
   ElmtList1 <- format(ElmtList1, justify="centre", width=10)
   ElmtList1 <- as.data.frame(ElmtList1,  stringsAsFactors = FALSE)
   ElmtList2 <- ReadElmtList("AugerTransitions") #reads the Auger Table see XPSSurveyUtilities()
   ElmtList2 <- format(ElmtList2, justify="centre", width=10)
   ElmtList2 <- as.data.frame(ElmtList2,  stringsAsFactors = FALSE)
   elmt <- ""


#----- GUI -----

	mainWin <- gwindow("CORE LINE AUGER TRANSITION TABLES", visible = FALSE)
	mainGroup <- ggroup(horizontal=FALSE, container = mainWin)

	gwin <- ggroup(label = "Peak Table", horizontal=TRUE, spacing = 15, container = mainGroup)

   gframe1 <- gframe(text = "Peak Table", spacing = 5, container = gwin)
   gtable1 <- gtable(ElmtList1, chosen.col=1, expand=TRUE, container = gframe1)
   size(gtable1) <- c(480,400)
   addHandlerDoubleclick(gtable1, handler = function(h, ...){
                     elmt <<- svalue(gtable1)
                     if (elmt=="") return()
                     RecPlot <<- recordPlot()   #save the graph for UNDO option
                     ShowLines()
               })


   gframe2 <- gframe(text = "Auger Transitions", spacing = 5, container = gwin)
   gtable2 <- gtable(ElmtList2, chosen.col=1, container = gframe2)
   size(gtable2) <- c(480,400)
   addHandlerDoubleclick(gtable2, handler = function(h, ...){
                     elmt <<- svalue(gtable2)
                     if (elmt=="") return()
                     RecPlot <<- recordPlot()   #save the graph for UNDO option
                     ShowLines()
               })

   gseparator(container = mainGroup) # separator
   elmtGroup <- ggroup( spacing=10, horizontal=TRUE, container=mainGroup)
   butnGroup <- ggroup( spacing=10, horizontal=TRUE, container=mainGroup)


   XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                     activeFName<-svalue(XPS.Sample)
                     Object <<- get(activeFName, envir=.GlobalEnv)
                     SpectList <<- XPSSpectList(activeFName)
                     SpectIndx <<- grep("survey",SpectList)   #switch to the survey spectrum
                     rangeX <<- range(Object[[SpectIndx]]@.Data[2])
                     rangeY <<- range(Object[[SpectIndx]]@.Data[2])
                     plot(Object[[SpectIndx]])
                     RecPlot <<- recordPlot()    #save graph for UNDO
               }, container = elmtGroup)
   svalue(XPS.Sample) <- activeFName


   Search <-gedit(initial.msg="Element?", spacing=1, handler=function(h, ...){
                     elmt <- svalue(Search)
                     if (elmt=="") return()
                     elmt <<- paste(elmt, " ", sep="")
                     RecPlot <<- recordPlot()   #save the graph for UNDO option
                     ShowLines()
                     svalue(Search) <- ""
               }, container=elmtGroup)
   tkconfigure(Search$widget, width=8)

   ShowCL <-gcheckbox("Core Lines", checked = FALSE, spacing=10, handler = function(h, ...){
                     ShowLines()
               }, container=elmtGroup)
   ShowAuger <- gcheckbox("Auger Transitions", checked = FALSE, spacing=10, handler = function(h, ...){
                     ShowLines()
               }, container=elmtGroup)
   HoldPlot <- gcheckbox("Hold plot", checked = FALSE, spacing=10, handler = function(h, ...){
                     elmt <<- ""
                     ShowLines()
               }, container=elmtGroup)
   gbutton("CURSOR POSITION", expand=FALSE, spacing=10, handler = function(h, ...) {
                     gmessage(msg="LEFT click to move marker's position; RIGHT to exit" , title = "WARNING",  icon = "warning", parent=mainWin)
                     RecPlot <<- recordPlot()   #save the graph for UNDO option
                     pos<-c(1,1) # only to enter in  the loop
                     while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                         pos <- locator(n=1, type="p", pch=3, cex=1.5, lwd=1.8, col="red")
                         if (length(pos) > 0) { #right mouse button not pressed
                            replayPlot(RecPlot) #refresh graph  to cancel previous cursor markers
                            points(pos, type="p", pch=3, cex=1.5, lwd=1.8, col="red")
                            pos <- round(x=c(pos$x, pos$y), digits=2)
                            txt <- paste("X: ", as.character(pos[1]), ", Y: ", as.character(pos[2]), sep="")
                            svalue(CurPos) <- txt
                            tcl("update", "idletasks")
                         }
                     }
                     replayPlot(RecPlot) #refresh graph  to cancel previous cursor markers
               },  container = elmtGroup)

   CurPos <- glabel(text = "X, Y: ", editable = FALSE, spacing=10, container = elmtGroup)
#   tkconfigure(CurPos$widget, width=18)  #limits the glabel to 18 chars
   ResetPlt <- gbutton(" UNDO ", handler=function(h,...){
                            elmt <<- ""
                            replayPlot(RecPlot)
                         }, container=butnGroup)

   gbutton("             REFRESH             ", expand=FALSE, spacing=10, handler = function(h, ...){
                            elmt <<- ""
                            plot(Object[[SpectIndx]])
                            RecPlot <<- recordPlot()   #save graph for UNDO
               }, container = butnGroup)
   gbutton("              CLOSE              ", expand=FALSE, handler = function(h, ...) dispose(mainWin), spacing=10, container = butnGroup)
   visible(mainWin) <- TRUE

}




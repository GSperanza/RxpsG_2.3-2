#Function to interpolate or decimate spectral data

#' @title XPSInterpDecim function to interpolate or decimate spectral data
#' @description function XPSInterpDecim acts on on objects of class 'XPSCoreLine'
#'   The original X, Y data are interpolated or downsampled (decimation) adding or eliminating
#'   N-data for each element of the original sequence
#'   No parameters are passed to this function
#' @examples
#' \dontrun{
#'	XPSInterpDecim()
#' }
#' @export
#'


XPSInterpDecim <- function() {

    CtrlDtaAnal <- function(){
       if (hasBaseline(XPSSample[[Indx]])){
           txt <- "Interpolation and Decimation act only on original non-analyzed data.\n
                   Interpolated decimated data will be saved in a new core-line.\n
                   Do you want to proceed?"
           answ <- gconfirm(msg=txt, title="WARNING", icon="warning")
           if (answ == TRUE) {
               enabled(I.D.obj5) <- TRUE
               enabled(I.D.obj6) <- TRUE
           } else {
               dispose(I.D.win)
               return()
           }
       }
    }



#--- Variables
   XPSSample <- NULL
   Indx <- NULL
   SelectedFName <- NULL
   FNameList <- XPSFNameList()
   SpectList <- " "
   IntpDec <- NULL
   Estep <- NULL

#--- widget ---

   I.D.win <- gwindow("SPECTRAL DATA INTERPOLATION - DECIMATION", parent=c(100,-10), visible=FALSE)

   I.D.group1 <- ggroup(spacing=3, horizontal=FALSE, container=I.D.win)
   I.D.group2 <- ggroup(spacing=3, horizontal=TRUE, container=I.D.group1)
   I.D.frame1 <- gframe(" SELECT XPSsample ", spacing=3, container=I.D.group2)
   I.D.obj1 <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h, ...){
                       SelectedFName <<- svalue(I.D.obj1)
                       XPSSample <<- get(SelectedFName, envir=.GlobalEnv)#load selected XPSSample
                       Gdev <- unlist(XPSSettings$General[6])            #retrieve the Graphic-Window type
                       Gdev <- strsplit(Gdev, "title")
                       Gdev <- paste(Gdev[[1]][1], " title='",SelectedFName,"')", sep="")     #add the correct window title
                       graphics.off() #switch off the graphic window
                       eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
                       plot(XPSSample)
                       SpectList <<- XPSSpectList(SelectedFName)
                       delete(I.D.frame2, I.D.obj2)
                       I.D.obj2 <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h, ...){
                                              SpectName <- svalue(I.D.obj2)
                                              SpectName <- unlist(strsplit(SpectName, "\\."))   #tolgo il N. all'inizio del nome coreline
                                              Indx <<- as.integer(SpectName[1])
                                              CtrlDtaAnal()
                                              Estep <<- abs(XPSSample[[Indx]]@.Data[[1]][2] - XPSSample[[Indx]]@.Data[[1]][1])
                                              Estep <<- round(Estep, 2)
                                              txt <- paste("Orig. E-step: ", Estep,"           ", sep="")
                                              txt <- substr(txt, start=1, stop=25)
                                              svalue(LabDE1) <- txt
                                              assign("activeSpectName", SpectName[2], envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                                              assign("activeSpectIndx", Indx, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                                              plot(XPSSample[[Indx]])
                                    }, container=I.D.frame2)
                       add(I.D.frame2, I.D.obj2)
               }, container = I.D.frame1)

   addSpring(I.D.group2)
   I.D.frame2 <- gframe(text=" SELECT CORELINE ", spacing=3, , container=I.D.group2)
   I.D.obj2 <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h, ...){
                       SpectName<-svalue(I.D.obj2)
                       LL <- length(SpectList)
                       SpectName <- unlist(strsplit(SpectName, "\\."))   #tolgo il N. all'inizio del nome coreline
                       Indx <<- as.integer(SpectName[1])
                       CtrlDtaAnal()
                       assign("activeSpectName", SpectName[2], envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                       assign("activeSpectIndx", Indx, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                       plot(XPSSample[[Indx]])
               }, container=I.D.frame2)

   I.D.group3 <- ggroup(horizontal=TRUE, container=I.D.group1)
   LabDE1 <- glabel ("Orig. E-step:            ", spacing=3, container=I.D.group3)
   LabDE2 <- glabel ("New E-step:              ", spacing=3, container=I.D.group3)
   addSpring(I.D.group3)
   HelpNdta <- gbutton ("N.Data Help", spacing=3,handler = function(h, ...){
                       txt <- paste("*** Operation INTERPOLATE:\n",
                                    "Number of Data = 2 will double the number of points \n",
                                    "E.step = original E.step/2 \n",
                                    "*** Operation DECIMATE:\n",
                                    "Number of Data = 2 will reduce the number of data to Ndata/2 \n",
                                    "E.step = original 2* E.step \n", sep="")
                       gmessage(msg=txt,icon="info")
               }, container=I.D.group3)
   tkconfigure(HelpNdta$widget, width=10)


   I.D.group4 <- ggroup(horizontal=TRUE, container=I.D.group1)
   I.D.frame3 <- gframe(text=" WHICH OPERATION? ", spacing=3, , container=I.D.group4)
   I.D.obj3 <- gcombobox(c("Interpolation", "Decimation"), selected=-1, editable=FALSE, container=I.D.frame3)
               
   addSpring(I.D.group4)
   I.D.frame4 <- gframe(text=" NUMBER of DATA ", horizontal = TRUE, spacing=5, container=I.D.group4)
   I.D.obj4 <- gedit(initial.msg="N.dta ?", spacing=5, handler=function(h, ...){
                       operation <- svalue(I.D.obj3)
                       Ndta <- as.integer(svalue(I.D.obj4))
                       dE <- NULL
                       if (length(operation)==0){
                           svalue(I.D.obj4) <- "  "
                           gmessage(msg="Please select which operation to apply first!", title="WARNING", icon="warning")
                           return()
                       }
                       if (operation == "Interpolation"){
                           dE <- round(Estep/Ndta, 2)
                           txt <- paste("New E-step: ", dE,"           ", sep="")
                           txt <- substr(txt, start=1, stop=25)
                           svalue(LabDE2) <- txt
                       }
                       if (operation == "Decimation"){
                           dE <- round(Estep*Ndta, 2)
                           txt <- paste("New E-step: ", dE,"           ", sep="")
                           txt <- substr(txt, start=1, stop=25)
                           svalue(LabDE2) <- txt
                       }
               }, container=I.D.frame4)

   I.D.frame5 <- gframe(text=" INTERPOLATE / DECIMATE ", horizontal = TRUE, spacing=3, , container=I.D.group1)
   I.D.obj5 <- gbutton("     INTERPOLATE     ", spacing=5, handler=function(h, ...){
                       XX2 <- NULL #interpolated X values
                       YY2 <- NULL #interpolated Y values
                       XX <- XPSSample[[Indx]]@.Data[[1]]
                       YY <- XPSSample[[Indx]]@.Data[[2]]
                       Npti <- as.integer(svalue(I.D.obj4))
                       LL <- length(XX)
                       cat("Initial number of data:", LL)
                       kk <- 1
                       dx <- (XX[2] - XX[1]) / Npti
                       for(ii in 1:(LL-1)){ # by interpolation of adjacent data
                           dy <- (YY[ii + 1] - YY[ii]) / Npti
                           for(jj in 1:Npti){
                               XX2[kk] <- XX[ii] + (jj - 1)*dx
                               YY2[kk] <- YY[ii] + (jj - 1)*dy
                               kk <- kk+1
                           }
                       }
                       XX2[kk] <- XX[LL]
                       YY2[kk] <- YY[LL]
                       cat("Number of data interpolated core-line:", kk)
                       LL <- length(XPSSample)
                       XPSSample[[LL+1]] <<- new("XPSCoreLine")
                       XPSSample[[LL+1]]@.Data[[1]] <<- XX2
                       XPSSample[[LL+1]]@.Data[[2]] <<- YY2
                       Symbol <- XPSSample[[Indx]]@Symbol
                       Symbol <- paste("Intp.", Symbol, sep="")
                       XPSSample[[LL+1]]@Symbol <<- Symbol
                       XPSSample@names[LL+1] <<- Symbol
                       plot(XPSSample[[LL+1]])
               }, container=I.D.frame5)


   addSpring(I.D.frame5)
   I.D.obj6 <- gbutton("       DECIMATE       ", spacing=3, handler=function(h, ...){
                       LL <- length(XPSSample[[Indx]]@.Data[[1]])
                       cat("Initial number of data:", LL)
                       Ndta <- as.integer(svalue(I.D.obj4))+1 #1 data is kept, Ndta are discarded
                       XX2 <- NULL #decimated X values
                       YY2 <- NULL #decimated Y values
                       jj <- 1
                       for(ii in seq(from=1, to=LL, by=Ndta)){
                           XX2[jj] <- XPSSample[[Indx]]@.Data[[1]][ii]
                           YY2[jj] <- XPSSample[[Indx]]@.Data[[2]][ii]
                           jj <- jj+1
                       }
                       cat("\nNumber of data decimated core-line:", (jj-1))
                       LL <- length(XPSSample)
                       XPSSample[[LL+1]] <<- new("XPSCoreLine")
                       XPSSample[[LL+1]]@.Data[[1]] <<- XX2
                       XPSSample[[LL+1]]@.Data[[2]] <<- YY2
                       Symbol <- XPSSample[[Indx]]@Symbol
                       Symbol <- paste("Dec.", Symbol, sep="")
                       XPSSample[[LL+1]]@Symbol <<- Symbol
                       XPSSample@names <<- c(names(XPSSample), Symbol)
                       plot(XPSSample)
               }, container=I.D.frame5)



  I.D.obj7 <- gbutton(" SAVE & EXIT ", spacing=3, handler=function(h,...){
                       assign(SelectedFName, XPSSample, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                       XPSSaveRetrieveBkp("save")
                       dispose(I.D.win)
               }, container = I.D.group1)

  I.D.obj8 <- gbutton(" EXIT ", spacing=3, handler=function(h,...){
                       XPSSaveRetrieveBkp("save")
                       dispose(I.D.win)
               }, container = I.D.group1)

  visible(I.D.win) <- TRUE

}

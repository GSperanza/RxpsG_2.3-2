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

    Interpolate <- function(IdxFrom, IdxTo, Npti){
       XX2 <- NULL #interpolated X values
       YY2 <- NULL #interpolated Y values
       XX <- XPSSample[[IdxFrom]]@.Data[[1]]
       YY <- XPSSample[[IdxFrom]]@.Data[[2]]
       LL <- length(XX)
       cat("\n\n ==> Initial number of data:", LL)
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
       cat("\n ==> Number of data interpolated core-line:", kk)
       if (IdxTo > IdxFrom) {XPSSample[[IdxTo]] <<- new("XPSCoreLine")}
       Symbol <- XPSSample[[IdxFrom]]@Symbol
       Symbol <- paste("Intp.", Symbol, sep="")
       XPSSample[[IdxTo]]@Symbol <<- Symbol
       XPSSample@names[IdxTo] <<- Symbol
       XPSSample[[IdxTo]]@.Data[[1]] <<- XX2
       XPSSample[[IdxTo]]@.Data[[2]] <<- YY2
       cat("\n ==> New core-line Energy step:", abs(XPSSample[[IdxTo]]@.Data[[1]][2]-XPSSample[[IdxTo]]@.Data[[1]][1]))
    }

    Decimate <- function(IdxFrom, IdxTo, Npti){
       LL <- length(XPSSample[[IdxFrom]]@.Data[[1]])
       cat("\n\n ==> Initial number of data:", LL)
       XX2 <- NULL #decimated X values
       YY2 <- NULL #decimated Y values
       jj <- 1
       for(ii in seq(IdxFrom=1, to=LL, by=Npti)){
           XX2[jj] <- XPSSample[[IdxFrom]]@.Data[[1]][ii]
           YY2[jj] <- XPSSample[[IdxFrom]]@.Data[[2]][ii]
           jj <- jj+1
       }
       cat("\n ==> Number of data decimated core-line:", (jj-1))
       if (IdxTo > IdxFrom) {XPSSample[[IdxTo]] <<- new("XPSCoreLine")}
       Symbol <- XPSSample[[IdxFrom]]@Symbol
       Symbol <- paste("Dec.", Symbol, sep="")
       XPSSample[[IdxTo]]@Symbol <<- Symbol
       XPSSample@names[IdxTo] <<- Symbol
       XPSSample[[IdxTo]]@.Data[[1]] <<- XX2
       XPSSample[[IdxTo]]@.Data[[2]] <<- YY2
       cat("\n ==> New core-line Energy step:", abs(XPSSample[[IdxTo]]@.Data[[1]][2]-XPSSample[[IdxTo]]@.Data[[1]][1]))
    }

    ResetVars <- function(){
       XPSSample <<- NULL
       Indx <<- NULL
       SelectedFName <- NULL
       FNameList <<- XPSFNameList()
       SpectList <- " "
       Estep1 <<- NULL
       Estep2 <<- NULL
       Operation <<- ""
       svalue(ID.obj1) <<- ""
       svalue(ID.obj2) <<- ""
       svalue(LabDE1) <<- "Orig. E-step:            "
       svalue(ID.obj4) <<- ""
       svalue(ID.obj5) <<- ""
       svalue(ID.obj6) <<- ""
       delete(ID.frame7, LabOperation)
       txt <- c("Operation:                                            \n",
                "                                                        ")
       LabOperation <<- glabel (txt, spacing=3, container=ID.frame7)
    }


#--- Variables
   XPSSample <- NULL
   Indx <- NULL
   SelectedFName <- NULL
   FNameList <- XPSFNameList()
   SpectList <- " "
   Estep1 <- NULL
   Estep2 <- NULL
   Operation <- ""

#--- widget ---

   ID.win <- gwindow("SPECTRAL DATA INTERPOLATION - DECIMATION", parent=c(100,-10), visible=FALSE)

   ID.group1 <- ggroup(spacing=3, horizontal=FALSE, container=ID.win)
   ID.group2 <- ggroup(spacing=3, horizontal=TRUE, container=ID.group1)
   ID.frame1 <- gframe(" SELECT XPSsample ", spacing=3, container=ID.group2)
   ID.obj1 <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h, ...){
                       SelectedFName <<- svalue(ID.obj1)
                       XPSSample <<- get(SelectedFName, envir=.GlobalEnv)#load selected XPSSample
                       Gdev <- unlist(XPSSettings$General[6])            #retrieve the Graphic-Window type
                       Gdev <- strsplit(Gdev, "title")
                       Gdev <- paste(Gdev[[1]][1], " title='",SelectedFName,"')", sep="")     #add the correct window title
                       graphics.off() #switch off the graphic window
                       eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
                       plot(XPSSample)
                       SpectList <<- XPSSpectList(SelectedFName)
                       delete(ID.frame2, ID.obj2)
                       ID.obj2 <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h, ...){
                                              SpectName <- svalue(ID.obj2)
                                              SpectName <- unlist(strsplit(SpectName, "\\."))   #tolgo il N. all'inizio del nome coreline
                                              Indx <<- as.integer(SpectName[1])
                                              CtrlDtaAnal()
                                              Estep1 <<- abs(XPSSample[[Indx]]@.Data[[1]][2] - XPSSample[[Indx]]@.Data[[1]][1])
                                              Estep1 <<- round(Estep1, 2)
                                              txt <- paste("Orig. E-step: ", Estep1,"           ", sep="")
                                              txt <- substr(txt, start=1, stop=25)
                                              svalue(LabDE1) <- txt
                                              assign("activeSpectName", SpectName[2], envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                                              assign("activeSpectIndx", Indx, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                                              plot(XPSSample[[Indx]])
                                    }, container=ID.frame2)
                       add(ID.frame2, ID.obj2)
               }, container = ID.frame1)

   addSpring(ID.group2)
   ID.frame2 <- gframe(text=" SELECT CORELINE ", spacing=3, , container=ID.group2)
   ID.obj2 <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h, ...){
                       SpectName <- svalue(ID.obj2)
                       LL <- length(SpectList)
                       SpectName <- unlist(strsplit(SpectName, "\\."))   #tolgo il N. all'inizio del nome coreline
                       Indx <<- as.integer(SpectName[1])
                       CtrlDtaAnal()
                       assign("activeSpectName", SpectName[2], envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                       assign("activeSpectIndx", Indx, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                       plot(XPSSample[[Indx]])
               }, container=ID.frame2)

   ID.frame3 <- gframe(text="CHANGE THE ENERGY STEP", horizontal = FALSE, spacing=3, container=ID.group1)
   ID.group3 <- ggroup(horizontal=TRUE, spacing=3, container=ID.frame3)
   LabDE1 <- glabel ("Orig. E-step:            ", spacing=3, container=ID.group3)
   font(LabDE1) <- list(family="sans", weight="bold", size="11")
   addSpring(ID.group3)
   HelpNdta <- gbutton ("HELP", spacing=3,handler = function(h, ...){
                       txt <- " After Core-Line selection the original Energy step is shown.\n Enter the new value of the energy step DE. RxpsG will INTERPOLATE\n or DECIMATE to provide spectrawith the chosen DE"
                       gmessage(msg=txt,title="HELP", icon="info")
               }, container=ID.group3)
   tkconfigure(HelpNdta$widget, width=10)

   LabDE2 <- glabel(text=" New ENERGY STEP ", container=ID.frame3)
   ID.group33 <- ggroup(horizontal=TRUE, spacing=3, container=ID.frame3)
   ID.obj4 <- gedit(initial.msg="E.step ?", spacing=5, handler=function(h, ...){
                       Estep2 <<- as.numeric(svalue(ID.obj4))
                       Operation <<- "NewEstep"
               }, container=ID.group33)

   ID.frame5 <- gframe(text=" DECIMATE ", horizontal = TRUE, spacing=3, , container=ID.group1)
   ID.obj5 <- gedit(initial.msg="N.pti ?", spacing=5, handler=function(h, ...){
                       Npti <- as.numeric(svalue(ID.obj5))
                       if((Npti-trunc(Npti)) > 0){
                           gmessage(msg="N. points to Decimate must be an integer", title="ERROR", icon="error")
                           return()
                       }
                       Operation <<- "Decimate"
               }, container=ID.frame5)

   ID.frame6 <- gframe(text=" INTERPOLATE ", horizontal = TRUE, spacing=3, , container=ID.group1)
   ID.obj6 <- gedit(initial.msg="N.pti ?", spacing=5, handler=function(h, ...){
                       Npti <- as.numeric(svalue(ID.obj6))
                       if((Npti-trunc(Npti)) > 0){
                           gmessage(msg="N. points to Interpolate must be an integer", title="ERROR", icon="error")
                           return()
                       }
                       Operation <<- "Interpolate"
               }, container=ID.frame6)

   ID.frame7 <- gframe(text=" OPERATION ", horizontal = FALSE, spacing=3, container=ID.group1)
   txt <- c("Operation:                                            \n",
            "                                                        ")
   LabOperation <- glabel (txt, spacing=3, container=ID.frame7)

   ID.frame8 <- gframe(text=" PROCESSING ", horizontal = FALSE, spacing=3, , container=ID.group1)
   ID.obj8 <- gbutton("     COMPUTE     ", handler=function(h, ...){
                       IdxFrom <- Indx
                       IdxTo <- length(XPSSample)+1
                       txt <- ""
                       switch(Operation, "NewEstep" = {
                              if (Estep1 < Estep2){ #decimation
                                  RR <- Estep2/Estep1
                                  RR <- round(RR, 3)
                                  Dec <- (RR-trunc(RR)) #gives the decimals
                                  if (Dec == 0){
                                      Npti <- Estep2/Estep1
                                      txt <- paste(txt, "Operation: DECIMATION by ", Npti, " \n\n", sep="")
                                      svalue(LabOperation) <- txt
                                      Decimate(IdxFrom, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                                  if (Dec > 0){
                                      Dec <- (Estep1-trunc(Estep1))
                                      ii <- 1
                                      while(Dec > 0){
                                         Npti <- Estep1*10^ii   # ii corresponds to the number of decimals
                                         Dec <- (Npti-trunc(Npti))
                                         ii <- ii+1
                                      }
                                      txt <- paste(txt, "Operation: INTERPOLATION by ", Npti, "\n", sep="")
                                      svalue(LabOperation) <- txt
                                      Interpolate(IdxFrom, IdxTo, Npti)
                                      Estep1 <- Estep1/Npti  #Estep1 = 1.2 =>  Npti = 12  =>  Estep2/Npti = 0.1
                                      Npti <- Estep2/Estep1
                                      txt <- paste(txt, "Operation: DECIMATION by ", Npti, sep="")
                                      svalue(LabOperation) <- txt
                                      Decimate(IdxTo, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                              }
                              if (Estep1 > Estep2){ #interpolation
                                  RR <- Estep1/Estep2
                                  RR <- round(RR, 3)
                                  Dec <- (RR-trunc(RR)) #gives the decimals
                                  if (Dec == 0){
                                      Npti <- Estep1/Estep2
                                      txt <- paste(Operation, "Operation: INTERPOLATION by ", Npti, " \n````````````````\n", sep="")
                                      svalue(LabOperation) <- txt
                                      Interpolate(IdxFrom, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                                  if (Dec > 0){
                                      Dec <- (Estep1-trunc(Estep1))
                                      ii <- 1
                                      while(Dec > 0){
                                         Npti <- Estep1*10^ii   # ii corresponds to the number of decimals
                                         Dec <- (Npti-trunc(Npti))
                                         ii <- ii+1
                                      }
                                      txt <- paste(txt, "Operation: DECIMATION by ", Npti, "\n", sep="")
                                      svalue(LabOperation) <- txt
                                      Decimate(IdxFrom, IdxTo, Npti)
                                      Estep1 <- Estep1/Npti  #Estep2 = 1.2 =>  EEstep2 = 12  =>  Estep2/EEstep2 = 0.1
                                      Npti <- Estep2/Estep1
                                      txt <- paste(txt, "Operation: INTERPOLATION by ", Npti, sep="")
                                      svalue(LabOperation) <- txt
                                      Interpolate(IdxTo, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                              }
                           },
                           "Decimate" = {
                               Npti <- as.numeric(svalue(ID.obj5))
                               txt <- paste(txt, "Operation: DECIMATION by ", Npti, " \n\n", sep="")
                               svalue(LabOperation) <- txt
                               Decimate(IdxFrom, IdxTo, Npti)
                           },
                           "Interpolate" = {
                               Npti <- as.numeric(svalue(ID.obj6))
                               txt <- paste(txt, "Operation: INTERPOLATION by ", Npti, " \n\n", sep="")
                               svalue(LabOperation) <- txt
                               Interpolate(IdxFrom, IdxTo, Npti)
                           } )
               }, container=ID.frame8)

  gbutton(" RESET ", handler=function(h,...){
                       ResetVars()
                       XPSSaveRetrieveBkp("save")
               }, container = ID.group1)


  gbutton(" SAVE ", handler=function(h,...){
                       assign(SelectedFName, XPSSample, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                       XPSSaveRetrieveBkp("save")
               }, container = ID.group1)

  gbutton(" EXIT ", handler=function(h,...){
                       XPSSaveRetrieveBkp("save")
                       dispose(ID.win)
               }, container = ID.group1)

  visible(ID.win) <- TRUE

}

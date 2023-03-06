#function to differentiate XPS-Sample spectra

#' @title XPSDiff for spectra differentiation
#' @description XPSDiff function is used to compute the derivative of the XPSSpectra.
#'   The user can select the degree of differentiation. Tje result of the differentiation
#'   is oplotted together with the original data. It is possible to amplify the
#'   differentiated spectrum to be on the scale of the original spectrum.
#' @examples
#' \dontrun{
#' 	XPSDiff()
#' }
#' @export
#'


XPSDiff <- function(){

   Differ <- function(Object){
        LL <- length(Object)
        tmp <- NULL
        for(ii in 2:LL){
           tmp[ii] <- Object[ii] - Object[ii-1]
        }
        tmp[1] <- tmp[2]
        return(tmp)
   }

   BkgSubtraction <- function(data){   #linear BKG subtraction
      BackGnd <<- NULL
      LL <- length(data)
      rng <- floor(LL/15)
      if (rng<3) {rng==3}
      if (rng>30) {rng==30}
      bkg1 <- mean(data[1:rng])
      bkg2 <- mean(data[(LL-rng):LL])
      stp <- (bkg1-bkg2)/LL
      Dta_Bkg <- NULL
      for (ii in 1:LL){
          Dta_Bkg[ii] <- data[ii]-(bkg1-ii*stp)
          BackGnd[ii] <<- bkg1-ii*stp
      }
      return(Dta_Bkg)
   }

   FindNearest <- function(){
      D <- NULL
      Dmin <- ((LocPos$x-Corners$x[1])^2 + (LocPos$y-Corners$y[1])^2)^0.5  #init value
      for (ii in 1:4) {
          D[ii] <- ((LocPos$x-Corners$x[ii])^2 + (LocPos$y-Corners$y[ii])^2)^0.5  #dist P0 P1
          if(D[ii] <= Dmin){
             Dmin <- D[ii]
             idx=ii
          }
      }
      if (idx==1){
          Corners$x[1] <<- Corners$x[2] <<- LocPos$x
          Corners$y[1] <<- Corners$y[3] <<- LocPos$y
      } else if (idx==2){
          Corners$x[1] <<- Corners$x[2] <<- LocPos$x
          Corners$y[2] <<- Corners$y[4] <<- LocPos$y
      } else if (idx==3){
          Corners$x[3] <<- Corners$x[4] <<- LocPos$x
          Corners$y[1] <<- Corners$y[3] <<- LocPos$y
      } else if (idx==4){
          Corners$x[3] <<- Corners$x[4] <<- LocPos$x
          Corners$y[2] <<- Corners$y[4] <<- LocPos$y
      }
      return(Corners)
   }

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

   MeasureMaxMinD <- function(){   # \U0394. = greek Delta used to indicate difference in Max-Min posiitions

      RefreshPlot <- function(){
           XXX <- cbind(unlist(FName[[SpectIndx]]@.Data[1]))  #transform the vector in a column of data
           YYY <- cbind(unlist(Differentiated[2]))
           Xlim <- range(XXX)
           if (FName[[SpectIndx]]@Flags[1]==TRUE) {
              Xlim <- rev(Xlim)  ## reverse x-axis
           }
           Ylim <- range(YYY)
           matplot(x=XXX, y=YYY, type="l", lty="solid", col="black",
                   xlim=Xlim, ylim=Ylim, xlab=FName[[SpectIndx]]@units[1], ylab="Differentiated Data [cps.]")
           return()
      }

      if(length(Differentiated[[2]]) == 0) {
         gmessage(msg="NON-Differentiated data. Cannot compute MAX-MIN difference", title="WARNING", icon="warning")
         return()
      }
      if(length(svalue(D1CoreLine)) == 0) {
         gmessage(msg="Please Select the Differentiated Core-Line", title="WARNING", icon="warning")
         return()
      }

      RefreshPlot()
      txt <- "LEFT button to set the MAX/MIN positions; RIGHT to exit \n Click near markers to modify the positions"
      gmessage(msg=txt , title = "WARNING",  icon = "warning")
      pos <- locator(n=2, type="p", pch=3, col="red", lwd=1.5) #first the two corners are drawn
      rect(pos$x[1], min(pos$y), pos$x[2], max(pos$y))  #marker-Corners are ordered with ymin on Left and ymax on Right
      Corners$x <<- c(pos$x[1],pos$x[1],pos$x[2],pos$x[2])
      Corners$y <<- c(pos$y[1],pos$y[2],pos$y[1],pos$y[2])
      points(Corners, type="p", pch=3, col="red", lwd=1.5)

      LocPos <<- list(x=0, y=0)
      while (length(LocPos) > 0) {  #if pos1 not NULL a mouse butto was pressed
         LocPos <<- locator(n=1, type="p", pch=3, col="red", lwd=2) #to modify the zoom limits
         if (length(LocPos$x) > 0) { #if the right mouse button NOT pressed
             FindNearest()
             if (FName[[SpectIndx]]@Flags[1]) { #Binding energy set
                 pos$x <- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decrescent ordered => Corners$x[1]==Corners$x[2]
             } else {
                 pos$x <- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in ascending order
             }
             pos$y <- sort(c(Corners$y[1],Corners$y[2]), decreasing=FALSE)
             RefreshPlot()  #refresh graph

             rect(pos$x[1], pos$y[1], pos$x[2], pos$y[2])
             points(Corners, type="p", pch=3, col="red", lwd=1.5)
         }
      }

      MaxMinD <<- round(abs(pos$x[1]-pos$x[2]), 2)
      Info <- FName[[SpectIndx]]@Info
      S3 <<- "\U0394."  #delta symbol
      charPos <- FindPattern(FName[[SpectIndx]]@Info, "   ::: Max.Min.\U0394. = ")[1]  #charPos[1] = row index of Info where "::: Max.Min.D = " is found
      if(length(charPos) > 0){ #save the Max/Min difference in FName Info
         FName[[SpectIndx]]@Info[charPos] <<- paste("   ::: Max.Min.", S3, " = ", MaxMinD, sep="") #overwrite previous MAx\Min Dist value
      } else {
         answ <- gconfirm(msg="Save new value of Max-Min difference?", title="WARNING", icon="warning")
         if (answ == TRUE){
             nI <- length(FName[[SpectIndx]]@Info)+1
             FName[[SpectIndx]]@Info[nI] <<- paste("   ::: Max.Min.", S3, " = ", MaxMinD, sep="")
         }
      }
      Symbol <- paste(S3, S2, DiffDeg, S1, sep="", collapse="") #compose the CoreLine Symbol  "\U0394." "D."  CLname
      if(hasRegionToFit(FName[[SpectIndx]])==FALSE){
         setClass("baseline", representation(baseline="list"), contains="list", prototype(baseline=list()))
         FName[[SpectIndx]]@RegionToFit$x <<- FName[[SpectIndx]]@.Data[[1]]   #defines the RegionToFit equal to original data
         FName[[SpectIndx]]@RegionToFit$y <<- FName[[SpectIndx]]@.Data[[2]]   #defines the RegionToFit equal to original data
         FName[[SpectIndx]]@Baseline$x  <<- FName[[SpectIndx]]@.Data[[1]]
         FName[[SpectIndx]]@Baseline$y  <<- rep(0, length(FName[[SpectIndx]]@.Data[[1]])) #defines a zero-Baseline
         FName[[SpectIndx]]@Baseline[["baseline"]] <<- new("baseline") #defines a new Baseline component of class baseline and package attribute .GlobalEnv
         attr(FName[[SpectIndx]]@Baseline$baseline, "package") <<- ".GlobalEnv"
         FName[[SpectIndx]]@Baseline$type  <<- "linear"
         FName[[SpectIndx]] <<- XPSaddComponent(FName[[SpectIndx]], type = "Derivative")
      }
      #Set the measured MaxMinD for the differeniated CoreLine
      FName[[SpectIndx]]@Symbol <<- Symbol # '\U0394' = Delta: the MaxMin diff was measured
      FName@names[SpectIndx] <<- Symbol            #update/add core line name to list of names

      FName[[SpectIndx]]@Components[[1]]@param["mu", "min"] <<- pos$x[1] #abscissas of Max/Min positions
      FName[[SpectIndx]]@Components[[1]]@param["mu", "max"] <<- pos$x[2]
      idx <- findXIndex(FName[[SpectIndx]]@.Data[[1]], pos$x[1])         #finds index corresponding to Max/min positions
      FName[[SpectIndx]]@Components[[1]]@param["h", "min"] <<- FName[[SpectIndx]]@.Data[[2]][idx] #ordinate of Max/min position
      idx <- findXIndex(FName[[SpectIndx]]@.Data[[1]], pos$x[2])         #finds index corresponding to Max/min positions
      FName[[SpectIndx]]@Components[[1]]@param["h", "max"] <<- FName[[SpectIndx]]@.Data[[2]][idx] #ordinate of Max/min position
      assign(activeFName, FName, envir=.GlobalEnv)
      assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
      cat("\n ==> Data saved")

      plot(FName)
      svalue(DistLab) <- MaxMinD
      enabled(SaveNewButt) <- FALSE
      enabled(OverWButt) <- TRUE

   }

   plotData <- function() {
      SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv) #set the activeSpectIndex to the actual CoreLine
      XXX <- cbind(unlist(FName[[SpectIndx]]@.Data[1]))
      LL <- length(XXX)
      YYY <- cbind(unlist(FName[[SpectIndx]]@.Data[2]), unlist(Differentiated[2]), rep(0, LL))
      Xlim <- range(XXX)
      if (FName[[SpectIndx]]@Flags[1]==TRUE) {
         Xlim <- rev(Xlim)  ## reverse x-axis
      }
      Ylim <- range(YYY)
      matplot(x=XXX, y=YYY, type="l", lty=c("solid", "solid", "dashed"),lw=c(1.75, 1, 1), col=c("black", "red3", "black"),
              xlim=Xlim, ylim=Ylim, xlab=FName[[SpectIndx]]@units[1], ylab=FName[[SpectIndx]]@units[2])
      return()
   }

   makeCombo <- function(){
      activeFName <<- svalue(D1XpsSpect)
      FName <<- get(activeFName,envir=.GlobalEnv)  #load the XPSSample
      assign("activeFName", activeFName, envir=.GlobalEnv)
      SpectList <<- XPSSpectList(activeFName)
      SpectIndx <<- 1
      delete(D1Frame2, D1CoreLine)
      D1CoreLine <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                        XPSCLname <- svalue(D1CoreLine)
                        tmp <- unlist(strsplit(XPSCLname, "\\."))   #skip the ".NUMBER" at beginning CoreLine name
                        SpectIndx <<- as.integer(tmp[1])
                        LL1 <- nchar(tmp[1])+2   # +2: substr(x,start,stop) start includes the character at 'start'
                        LL2 <- nchar(XPSCLname)
                        S1 <<- substr(XPSCLname, start=LL1, stop=LL2) #In case of XPSCLname="4.D1.C1s" SpectName must be "D1.C1s"
                        S3 <<- S2 <<- ""
                        Info <- FName[[SpectIndx]]@Info
                        chrPos <- FindPattern(Info, "Diff. degree: ")
                        if (length(chrPos[2]) > 0) {         #it is possible to start with a pre-differentiated CoreLine
                            Differentiated <<- FName[[SpectIndx]]@.Data
                            S1S2S3 <- unlist(strsplit(S1, ".", fixed=TRUE))
                            if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
                            jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. which gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
                            if(length(jj) > 0){
                               S2 <<- "D."
                               LL <- length(S1S2S3)
                               S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
#                               DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)]) #DiffDeg set in 'Differentiation'
                               DiffDeg <<- as.integer(S1S2S3[(jj+1)]) #DiffDeg set in 'Differentiation'
                            }
                        }
                        assign("activeSpectName", S1, envir=.GlobalEnv)
                        assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
                        plot(FName[[SpectIndx]])
                        enabled(DDeg) <- TRUE
                        CLBkp <<- FName[[SpectIndx]]         #backUp original data
                     }, container=D1Frame2)
      plot(FName)
   }



   SaveFName <- function(saveMode){
      if(DiffDeg == 0 && length(as.numeric(svalue(DDeg)))==0){
          gmessage(msg="Before saving data make differentiation please.", title="ERROR", icon="error")
          return()
      }

#save data in NEW CORELINE mode
      if (S2 != "" && saveMode=="NewCL") {  #"D." is present, one or more core-lines are differentiated
          Idx <- length(FName)              #number of corelines +1
          Info <- FName[[Idx]]@Info
          nI <- length(Info)
          N.spaces <-  gregexpr(" ", Info[nI])     #finds how many space characters are in info
          if (length(N.spaces[[1]]) != nchar(Info[nI])){  #if == FALSE if a blank raw is found
              nI <- nI +1                          #then add new Info line
          }
          Idx <- Idx+1
          FName[[Idx]] <<- new("XPSCoreLine") #Add the new Coreline using the starting core-line

          if (length(svalue(DDeg)) > 0){             #working on previously differentiated data
              FName[[Idx]]@.Data <<- Differentiated  #update New Coreline with dufferentiated data
#--- Cntrl if we are differentiating an already differentiated spectrum
              Info <- FName[[SpectIndx]]@Info
              chrPos <- FindPattern(Info, "Diff. degree: ")
              if (length(chrPos[2]) > 0) {         #it is possible to start with a pre-differentiated CoreLine
                  Differentiated <<- FName[[SpectIndx]]@.Data
                  CLname <- FName[[SpectIndx]]@Symbol
                  #now set the correct S1, S2, S3 and DiffDeg values
                  S1S2S3 <- unlist(strsplit(CLname, ".", fixed=TRUE))
                  if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
                  jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. which gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
                  if(length(jj) > 0){
                     S2 <<- "D."
                     LL <- length(S1S2S3)
                     S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
                     DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)]) #DiffDeg set in 'Differentiation'
                     Symbol <- paste(S3, S2, DiffDeg, S1, sep="")
                  }
              } else {
                  Symbol <- paste(S3, S2, DiffDeg, ".", S1, sep="") #DiffDeg updated in 'Differentiation'
              }
              FName[[Idx]]@Symbol <<- Symbol
              FName@names[Idx] <<- Symbol
          }

      }

#save data in OVERWRITE mode
      if (S2 != "" && saveMode=="Overwrite") {  #"D." is present: working on previously differentiated data, and overwrite new data
          Idx <- activeSpectIndx
          FName[[Idx]]@.Data <<- Differentiated #change the original diff. data with the new differented data
#--- Cntrl if we are differentiating an already differentiated spectrum
          Info <- FName[[SpectIndx]]@Info
          chrPos <- FindPattern(Info, "Diff. degree: ")
          if (length(chrPos[2]) > 0) {         #the CoreLine was pre-differentiated
              Differentiated <<- FName[[SpectIndx]]@.Data
              CLname <- FName[[SpectIndx]]@Symbol
              #now set the correct S1, S2, S3 and DiffDeg values
              S1S2S3 <- unlist(strsplit(CLname, ".", fixed=TRUE))
              if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
              jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. which gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
              if(length(jj) > 0){
                 S2 <<- "D."    # %in% search if "D" is present in S1S2S3. %in% gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
                 LL <- length(S1S2S3)
                 S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
                 DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)])   #add precedent diff. degree
                 Symbol <- paste(S3, S2, DiffDeg, S1, sep="")
              }
          } else {
              Symbol <- paste(S3, S2, DiffDeg, ".", S1, sep="")  #DiffDeg updated in 'Differentiation'
          }
          Info <- FName[[Idx]]@Info
          chrPos <- FindPattern(Info, "   ::: Differentiated ")
          nI <- chrPos[1]                  #Overwrite previous Info
          FName[[Idx]]@Symbol <<- Symbol
          FName@names[Idx] <<- Symbol
      }

      if (S2 == "" && saveMode=="Overwrite") { #"D." NOT present Differentiation not performed we should work on already differentiated data
#--- Cntrl if we are differentiating an already differentiated spectrum
          Info <- FName[[SpectIndx]]@Info
          chrPos <- FindPattern(Info, "Diff. degree: ")
          if (length(chrPos[2]) > 0) {         #it is possible to start with a pre-differentiated CoreLine
              Differentiated <<- FName[[SpectIndx]]@.Data
              CLname <- FName[[SpectIndx]]@Symbol
              #now set the correct S1, S2, S3 and DiffDeg values
              S1S2S3 <- unlist(strsplit(CLname, ".", fixed=TRUE))
              if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
              jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. which gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
              if(length(jj) > 0){
                 S2 <<- "D."    # %in% search if "D" is present in S1S2S3. %in% gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
                 LL <- length(S1S2S3)
                 S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
                 DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)])   #add precedent diff. degree
              } else {
                 gmessage(msg="ERROR: Wrong Data Information", title="ERROR", icon="error")
                 return()
              }
              Idx <- activeSpectIndx                  #number of corelines
              Symbol <- paste(S3, S2, DiffDeg, S1, sep="") # '\U0394.' = Delta representing the MaxMin diff
              FName[[Idx]]@Symbol <<- Symbol
              FName@names[Idx] <<- Symbol
         }
      }

      Info[nI] <- paste("   ::: Differentiated ", Symbol, ": Diff. degree: ", DiffDeg, sep="") #Update Differentiation information
      FName[[Idx]]@Info <<- Info
      assign(activeFName, FName,envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
      assign("activeSpectName", Symbol,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
      assign("activeSpectIndx", Idx,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
      enabled(SaveNewButt) <- FALSE
      enabled(OverWButt) <- FALSE
      makeCombo()
      return(Idx)
   }


#--- Variables
    plot.new()
    if (length(activeFName)==0){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
    }
#    activeFName <- get("activeFName", envir=.GlobalEnv)  #cload the XPSSample name (string)
    FName <- get(activeFName, envir=.GlobalEnv)  #load the active XPSSample (data)
    FNameList <- XPSFNameList()                  #list of loaded XPSSamples
    SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv) #load the index of the active CoreLine
    SpectName <- get("activeSpectName", envir=.GlobalEnv) #namer of the active CoreLine
    SpectList <- XPSSpectList(activeFName)       #list of CoreLine spectra belonging to FName XPSSample
    Differentiated <- NULL
    DiffBkp <- NULL
    CLBkp <- NULL
    BackGnd <- NULL
    Corners <- list(x=NULL, y=NULL)
    LocPos <- list()
    MaxMinD <- NULL
    DiffDeg <- 0
    S1 <- ""   #to store the 'FName@Symbol'
    S2 <- ""   #to store 'D.x' if differentiation was performed
    S3 <- ""   #to store '/U0395' if Max/Min diff was done


#===== Main Panel
    MainWin <- gwindow("DATA DIFFERENTIATION", parent=c(30, 30), visible=FALSE)
    size(MainWin) <- c(320, 200)
    mainFrame <- gframe(text="Differentiate", horizontal=FALSE, spacing=5, container=MainWin)
    Maingroup <- ggroup(spacing=5, horizontal=FALSE, container=mainFrame)

# --- Line 1 ---
    D1Group1 <-  ggroup(spacing=5, horizontal=TRUE, container=Maingroup)
    D1frame1 <- gframe(" Select XPS Sample ", spacing=5, container=D1Group1)
    D1XpsSpect <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                           makeCombo()
                           svalue(DistLab) <- "Max Min Dist: "
                 }, container=D1frame1)

    D1Frame2 <- gframe(" Select Coreline ", spacing=5, container=D1Group1)
    D1CoreLine <- gcombobox(" ", selected=-1, editable=FALSE, container = D1Frame2)

    D1Frame3 <- gframe(" Update XPSSample/Coreline Lists ", spacing=5, container=D1Group1)
    UpdateButt <- gbutton(" UPDATE ", handler=function(h,...){
                           DiffBkp <<- NULL
                           BackGnd <<- NULL
                           Differentiated <<- NULL
                           DiffDeg <<- 0
                           S1 <<- S2 <<- S3 <<- ""
                           svalue(DDeg) <- ""
                           svalue(AmpliDeg) <- ""
                           svalue(D1XpsSpect) <- ""
                           svalue(D1CoreLine) <- ""
                           enabled(DiffButt) <- FALSE
                           enabled(ResButt) <- FALSE
                           enabled(SaveNewButt) <- FALSE
                           enabled(OverWButt) <- FALSE
                 }, container = D1Frame3)


# --- Line 2 ---
    D2frame1 <- gframe(" Select Differentiation Degree ", spacing=5, container=Maingroup)
    DDeg <- gcombobox(c(1,2,3,4,5), selected=-1, editable=FALSE, handler=function(h,...){
                           enabled(DiffButt) <- TRUE
                           enabled(ResButt) <- TRUE
                 }, container = D2frame1)
    tkconfigure(DDeg$widget, width=10)

    Negative <- gcheckbox(text="Negative", checked = FALSE, container=D2frame1)

    DiffButt <- gbutton("            Differentiate            ", handler=function(h,...){
                           Ndiff <- as.numeric(svalue(DDeg))
                           if (length(Ndiff) == 0){
                               gmessage("Please Select the Differentiation Degree", title="Warning", icon="warning")
                               return()
                           }
                           SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv) #set the activeSpectIndex to the actual CoreLine
                           Differentiated <<- FName[[SpectIndx]]@.Data
                           Object <- FName[[SpectIndx]]@.Data[[2]]
                           for(ii in 1:Ndiff){
                               Object <- Differ(Object)
                           }
                           if (svalue(Negative) == TRUE) { Object <- -Object }
                           Differentiated[[2]] <<- Object
                           S2 <<- "D."
                           DiffDeg <<- Ndiff
                           DiffBkp <<- Differentiated
#                           BkgSubtraction(FName[[SpectIndx]]@.Data[[2]])  #plotData needs the backgrnd to be defined
                           plotData()
                           enabled(SaveNewButt) <- TRUE
                           enabled(OverWButt) <- TRUE
                 }, container = D2frame1)

    ResButt <- gbutton(" RESET ", handler=function(h,...){
                           DiffBkp <<- NULL
                           BackGnd <<- NULL
                           Differentiated <<- NULL
                           DiffDeg <<- 0
                           S1 <<- S2 <<- S3 <<- ""
                           svalue(DDeg) <- ""
                           svalue(AmpliDeg) <- ""
                           svalue(D1XpsSpect) <- ""
                           svalue(D1CoreLine) <- ""
                           SpectIndx <<- grep(S1, names(FName))  #restore
                           FName[[SpectIndx]] <<- CLBkp
                           plot(FName[[SpectIndx]])
                           enabled(DiffButt) <- FALSE
                           enabled(ResButt) <- FALSE
                           enabled(SaveNewButt) <- FALSE
                           enabled(OverWButt) <- FALSE
                 }, container = D2frame1)

# --- Line 2 ---
    D2frame2 <- gframe(" Amplify Diff. Data ", horizontal=TRUE, spacing=5, container=Maingroup)
    AmpliDeg <- gedit(initial.msg="10,50, 100...", handler=function(h,...){
                           AA <- as.integer(svalue(AmpliDeg))
                           Differentiated[[2]] <<- AA*DiffBkp[[2]]
                           plotData()
                 }, container = D2frame2)
    space <- glabel("      ", spacing = 5, container= D2frame2)

    gbutton("Measure Max-Min Dist.", handler=function(h,...){
                    MeasureMaxMinD()
                 }, container = D2frame2)

    DistLab <- glabel("Max Min Dist: ", spacing = 5, container= D2frame2)

#--- Common buttons

    DButtgroup <- ggroup(horizontal=TRUE, container=mainFrame)

    SaveNewButt <- gbutton("SAVE AS A NEW CORE LINE", handler=function(h,...){ #Filtered data saved in a new coreline
                    SaveFName("NewCL")
                    BackGnd <<- NULL
                    svalue(DDeg) <- ""
                    XPSSaveRetrieveBkp("save")
                    enabled(SaveNewButt) <- FALSE
                    enabled(OverWButt) <- FALSE
                 }, container = DButtgroup)

    OverWButt <- gbutton("OVERWRITE PREVIOUS DIFFERENTIATION", handler=function(h,...){ #Filtered data saved in a new coreline
                    SaveFName("Overwrite")
                    BackGnd <<- NULL
                    svalue(DDeg) <- ""
                    XPSSaveRetrieveBkp("save")
                    enabled(SaveNewButt) <- FALSE
                    enabled(OverWButt) <- FALSE
                 }, container = DButtgroup)



    gbutton("EXIT", handler=function(h,...){
                    dispose(MainWin)
                    XPSSaveRetrieveBkp("save")
                 }, container = DButtgroup)


    enabled(DDeg) <- FALSE
    enabled(DiffButt) <- FALSE
    enabled(SaveNewButt) <- FALSE
    enabled(OverWButt) <- FALSE
    visible(MainWin) <- TRUE

}

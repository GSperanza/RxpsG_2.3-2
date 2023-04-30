#Function to perform quantifications on XPS spectra
#allowing selection of corelines and fit components
#for the computation of the atomic concentrations.
#XPSquantify and XPScalc used only by XPSMoveComponent.

#' @title XPSQuant
#' @description XPSQuant() performs the elemental quantification 
#'   for a selected XPS-Sample. Provides a userfriendly interface 
#'   with the list of Corelines of the selected XPS-Sample
#'   Each Coreline can be count/omit from the elemental quantification.
#'   If peak fitting is present also each of the fitting component 
#'   can be count/omit from the elemental quantification.
#'   Finally also relative RSF of the coreline or individual fitting 
#'   components can be modified.
#' @examples
#' \dontrun{
#' 	XPSQuant()
#' }
#' @export
#'

XPSQuant <- function(){

#Extract PE from each slot @Info of the XPSSample spectra
   RetrivePE <- function(CL=NULL){  #Retrieve the PE value from the CL coreline information slot
      PEnergy <- NULL
      if(is.null(CL)){
         LL <- length(XPSSample)
         for(ii in 1:LL){
             info <- XPSSample[[ii]]@Info[1]   #retrieve info containing PE value
             xxx <- unlist(strsplit(info, "Pass energy"))[2]  #extract PE value
             PEnergy[ii] <- as.integer(gsub("\\D","", xxx)) # extract numeric characters from xxx = PE value
             if(is.na(PEnergy[ii])){
                txt <- paste("CoreLine ", XPSSample[[ii]]@Symbol, ": \nPass Energy unknown! Please provide the Pass Energy value", sep="")
                PEwin <- gwindow("INPUT CORE-LINE PASS ENERGY", parent=c(50, 10), visible=FALSE) #
                PEgroup1 <- ggroup(horizontal=FALSE, container=PEwin)
                PElabel <- glabel(txt, container=PEgroup1)
                font(PElabel) <- list(family="sans", size=12)
                CL.PE <- gedit("", initial.msg="CoreLine Pass Energy ", container=PEgroup1)
                gseparator(container=PEgroup1)
                PEgroup2 <- ggroup(horizontal=TRUE, container=PEgroup1)
                gbutton("OK", container=PEgroup2, handler=function(...){   #input spectrum name
                        PEnergy[ii] <<- as.integer(svalue(CL.PE))
                        XPSSample[[ii]]@Info[1] <<- paste(XPSSample[[ii]]@Info, " Pass energy ", PEnergy[ii], sep="")
                        dispose(PEwin)
                })
                gbutton("Cancel", container=PEgroup2, handler=function(...) dispose(PEwin))
                visible(PEwin) <- TRUE
                PEwin$set_modal(TRUE)
            }
         }
      } else {
         info <- CL@Info[1]   #retrieve info containing PE value
         xxx <- unlist(strsplit(info, "Pass energy"))[2]  #extract PE value
         PEnergy <- as.integer(gsub("\\D","", xxx)) # extract numeric characters from xxx = PE value
      }
      return(PEnergy)
   }

   CorrectPE <- function(CheckedCL, CK.PassE, SurPE, CLinePE){

      idx <- which(CK.PassE == SurPE)   #select all the corelines with PE==160. It is supposed that survey cannot be used for quantification. Only CL extracted from survey can be used
      LL <- length(idx)
      for (ii in 1:LL){
          jj <- CheckedCL[idx[ii]]
          info <- XPSSample[[jj]]@Info[1]   #retrieve info containing PE value
          xxx <- strsplit(info, "Pass energy")  #extract PE value
          PE <- strsplit(xxx[[1]][2], "Iris") #PE value
          info <- paste(xxx[[1]][1],"Pass energy ", CLinePE, "   Iris", PE[[1]][2], sep="")
          XPSSample[[jj]]@Info[1] <<- info
          assign(activeFName, XPSSample, envir=.GlobalEnv)
      }
   }


   CalcNormCoeff <- function(CheckedCL, CK.PassE, SurIdx){
# CheckedCL = CoreLines selected for quantification
# CK.PassE = PE of CheckedCL
# SurIdx = index of the survey
# It is supposed that a coreline was extracted from the survey
# For elements extracted from survey at high PE the photoelectron collection efficiency
# is different from that of the corelines with low PE (higher energy resolution)
# Spectra acquired at different PE need a normalization coefficient
# Let us consider C1s extracted from survey(PE=160eV in KratosXPS) and C1s coreline (PE=20eV)
# Different PE leads to markedly different signal intensities.
# Let A1 = area of C1s at PE=160,  A2 = area of C1s at PE=20
# It has discovered that a simple normalization for the PE: A1/160, A2/20   does not work
# In htis function a high resolution-Coreline (lower PE) is compared with the same spectrum 
# from the survey and a proportion coeff. is computed
# This proportion coeff. is used to normalize Corelines acquired at different PE.

#variables
      PassCL <- NULL
      BaseLine <- NULL
      X <- list()
      Y <- list()
      Area1 <- Area2 <- Area3 <- NULL
#------

      CLinePE <- min(CK.PassE)
      txt <- paste("Please Confirm if the Pass Energy of the High-Resolution Core Lines is ", as.character(CLinePE), sep="")
      answ <- gconfirm(txt, title="CORE LINE PASS ENERGY", icon="info")
      if (answ == FALSE){
          winCL <- gwindow("CORE LINE PASS ENERGY", visible=FALSE)
          size(winCL) <- c(250, 150)
          groupCL <- ggroup(horizontal=FALSE, container=winCL)
          labPE <- glabel(text="Please input the Pass Energy of the Core-Lines", container=groupCL)
          PassE.CL <- gedit(initial.msg = "PE ?", handler=function(h, ...){
                                CLinePE <<- as.integer(svalue(PassE.CL))
                             }, container=groupCL)
          gbutton("    OK     ", handler=function(h, ...){
                                dispose(winCL)
                             }, container=groupCL)
          visible(winCL) <- TRUE
          winCL$set_modal(TRUE)  #nothing can be done while running this macro
      }
      
      idx <- which(CK.PassE == CLinePE) #select all the corelines acquired at lower PE
      CL.PE <- CheckedCL[idx]  #CL.PE contains CL indexes which could be in sparse order
      LL <- length(CL.PE)
      MaxI <- NULL

#Now find the coreline with max intensity and compare this coreline
#with the same in the survey to compute the normalization factor
      for(ii in 1:LL){ #Among the selected corelines finds the one with max intensity
          MaxI[ii] <- max(XPSSample[[CL.PE[ii]]]@.Data[[2]])  #find the coreline with max intensity
      }
      idx <- which(MaxI == max(MaxI))  #index of the coreline with max intensity
      idx <- CL.PE[idx]   #index of the CL with max internsity and acquired with min PE

      if(XPSSample[[SurIdx]]@units[1] != XPSSample[[idx]]@units[1]){
         gmessage(msg="WARNING: Wide Spectrum and Core-Lines energy scale units are different. Check please!", title="WARNING", icon="warning")
         NormCoeff <<- -1
         return()
      }
      CLName <- XPSSample[[idx]]@Symbol
      X[[1]] <- unlist(XPSSample[[idx]]@RegionToFit$x) #resume the abscissa
      Y[[1]] <- unlist(XPSSample[[idx]]@RegionToFit$y)
      LL <- length(X[[1]])
      DY <- (Y[[1]][LL] - Y[[1]][1])/(LL-1)   #energy step
      for (ii in 1:LL){
          BaseLine[ii] <- Y[[1]][1]+(ii-1)*DY #this is the linear baseline
      }
#      matplot(matrix(c(X[[1]], X[[1]]), ncol=2), matrix(c(Y[[1]], BaseLine), ncol=2), type="l", lty=1, col=c("black","red"))
      Y[[1]] <- Y[[1]]-BaseLine
      E_stp <- abs(X[[1]][2]-X[[1]][1])
      Area1 <- sum(Y[[1]])*E_stp    #Area of high resolution coreline

      Xlim <- range(X[[1]])  #X range of the selected CL
      Xlim <- sort(Xlim, decreasing=FALSE)
      Xlim[1] <- Xlim[1]-2   #extend the CL X-range for higher PE (maybe PE=160eV)
      Xlim[2] <- Xlim[2]+2   #extend the CL X-range for higher PE
      if (XPSSample[[idx]]@Flags[1]==TRUE) {Xlim <- c(Xlim[2], Xlim[1])} #Binding energy scale
      idx1 <- findXIndex(XPSSample[[SurIdx]]@.Data[[1]], Xlim[1])
      idx2 <- findXIndex(XPSSample[[SurIdx]]@.Data[[1]], Xlim[2])
      X[[2]] <- unlist(XPSSample[[SurIdx]]@.Data[[1]][idx1:idx2])  #X values of coreline extracted from survey
      Y[[2]] <- unlist(XPSSample[[SurIdx]]@.Data[[2]][idx1:idx2])  #Y values of coreline extracted from survey
      Ylim <- range(sapply(Y, sapply, range))
      LL <- length(X[[2]])

      BaseLine <- NULL
      DY <- (Y[[2]][LL] - Y[[2]][1])/(LL-1)   #energy step
      for (ii in 1:LL){
          BaseLine[ii] <- Y[[2]][1]+(ii-1)*DY #this is the linear baseline
      }
#      matplot(x=matrix(c(X[[2]], X[[2]]), ncol=2), y=matrix(c(Y[[2]], BaseLine), ncol=2), type="l", lty=1, col=c("black","red"))

      Y[[2]] <- Y[[2]]-BaseLine
      E_stp <- abs(X[[2]][2]-X[[2]][1])
      Area2 <- sum(Y[[2]])*E_stp              #Area of the coreline extracted from survey

      NormCoeff <<- Area2/Area1
      X[[3]] <- X[[2]]
      Y[[3]] <- Y[[2]]/NormCoeff
      Area3 <- sum(Y[[3]])*E_stp
      cat("\n => Normalization coefficient: ", round(NormCoeff, 3))
#----- Graphics
      if (XPSSample[[SurIdx]]@Flags[[1]]){        #Binding energy set
          Xlim <- sort(Xlim, decreasing=TRUE)
      } else {
          Xlim <- sort(Xlim, decreasing=FALSE)
      }
      XLabel <- XPSSample[[SurIdx]]@units[1]
      YLabel <- XPSSample[[SurIdx]]@units[2]
      LL <- max(sapply(X, length))
      X <- sapply(X, function(x) {length(x)<-LL   #insert NAs if the length of X[] < LL
                                  return(x)})
      Y <- sapply(Y, function(x) {length(x)<-LL   #insert NAs if the length of Y[] < LL
                                  return(x)})
      X <- matrix(unname(unlist(X)), ncol=3)      #transform list in matrix
      Y <- matrix(unname(unlist(Y)), ncol=3)
      txt <- paste("Normalization coeff. computed on ", CLName, sep="")
      matplot(x=X, y=Y, xlim=Xlim, type="l", lty=1, lw=1, col=c("black","green","red"), main=txt, xlab=XLabel, ylab=YLabel)
      txt <- c("High res. CL", "Extracted CL ", "Normalized CL")
      legend(x=Xlim[1], y=Ylim[2]/1.02, legend=txt, text.col=c("black","green","red"))
      gmessage(msg="Check the graph: Black and Red spectra should have similar spectral areas", title="Compare HighRes and Normalized Spectra", icon="warning")
      SurPE <- RetrivePE(XPSSample[[SurIdx]]) # Retrieve used for the survey

      idx <- which(CK.PassE == SurPE)
      idx <- CheckedCL[idx] #retrieve the index of the selected corelines having PE=SurPE.
      for(ii in idx){
          XPSSample[[ii]]@.Data[[2]] <<- XPSSample[[ii]]@.Data[[2]]/NormCoeff
          XPSSample[[ii]]@RegionToFit$y <<- XPSSample[[ii]]@RegionToFit$y/NormCoeff
          XPSSample[[ii]]@Baseline$y <<- XPSSample[[ii]]@Baseline$y/NormCoeff
          XPSSample[[ii]]@RegionToFit$NormCoeff <<- NormCoeff
          if(hasComponents(XPSSample[[ii]]) == TRUE) {
             XPSSample[[ii]]@Components <<- sapply(XPSSample[[ii]]@Components, function(x) {
                                                          x@ycoor <- x@ycoor/NormCoeff
                                                          return(x) } )
             XPSSample[[ii]]@Fit$y <<- XPSSample[[ii]]@Fit$y/NormCoeff
          }
      }
      CorrectPE(CheckedCL, CK.PassE, SurPE, CLinePE)
      return()
}


#XPScalc() function cannot be applied since here you can select fit components and their RSF
   quant <- function(CoreLineComp){
      N_CL <- length(CoreLineComp)
      maxFitComp <- 0

      for(ii in 1:length(CoreLineComp)){
         LL <- length(CoreLineComp[[ii]])
         if (LL > maxFitComp) { maxFitComp <- LL }
      }
      AreaCL <- rep(0,N_CL)
      NormAreaCL <- rep(0,N_CL)
      sumComp <- matrix(0,nrow=N_CL,ncol=maxFitComp)  #define a zero matrix
      AreaComp <- matrix(0,nrow=N_CL,ncol=maxFitComp)
      maxNchar <- 0
      TabTxt <<- ""
      QTabTxt <<- ""

      for(ii in 1:N_CL){
         AreaCL[ii] <- 0
         indx <- CoreLineIndx[ii]
         if (svalue(CoreLineCK[[ii]])=="TRUE") {   #if a coreline is selected
            N_comp <- length(CoreLineComp[[ii]])   #this is the number of fit components
            RSF <- XPSSample[[indx]]@RSF               #Sensitivity factor of the coreline
            E_stp <- abs(XPSSample[[indx]]@.Data[[1]][2]-XPSSample[[indx]]@.Data[[1]][1]) #energy step
            if (RSF != 0) {  #Sum is made only on components with RSF != 0 (Fit on Auger or VB not considered)
               AreaCL[ii] <- sum(XPSSample[[indx]]@RegionToFit$y-XPSSample[[indx]]@Baseline$y)*E_stp      #Integral undeer coreline spectrum
               NormAreaCL[ii] <- AreaCL[ii]/RSF  #Coreline contribution corrected for the relative RSF
            } else {
               AreaCL[ii] <- sum(XPSSample[[indx]]@RegionToFit$y-XPSSample[[indx]]@Baseline$y)*E_stp #if RSF not defined the integral under the coreline is considered
               NormAreaCL[ii] <- AreaCL[ii]
            }
            txt <- as.character(round(AreaCL[ii], 2))

            if (hasComponents(XPSSample[[indx]])) {   #is fit present on the coreline?
               for(jj in 1:N_comp){    #ii runs on CoreLines, jj runs on coreline fit components
                  comp <- CoreLineComp[[ii]][jj]
                  RSF <- XPSSample[[indx]]@Components[[comp]]@rsf
                  if (RSF!=0) { #if the RSF is lacking(es. Auger, VB spectra...) : it is not possible to make correction for the RSF...
                     sumComp[ii,jj] <- (sum(XPSSample[[indx]]@Components[[comp]]@ycoor)-sum(XPSSample[[indx]]@Baseline$y))*E_stp       #simple area of the coreline
                     AreaComp[ii,jj] <- sumComp[ii,jj]/RSF  #(area of the single component -  area backgroound) corrected for the RSF
                  } else {
                     sumComp[ii,jj] <- (sum(XPSSample[[indx]]@Components[[comp]]@ycoor)-sum(XPSSample[[indx]]@Baseline$y))*E_stp    #if RSF is not defined the simple spectral integral is computed
                     AreaComp[ii,jj] <- sumComp[ii,jj]      #Component spectral integral
                  }
                  txt <- as.character(round(sumComp[ii,jj], 2))
                  Nch <- nchar(txt)
                  if (Nch > maxNchar) { maxNchar <- Nch } #calculate the max number of characters of numbers describing component areas
               }
               if (RSF != 0) {  #summation only on components with RSF !=0 (no fit on Auger o VB
                   NormAreaCL[ii] <- sum(AreaComp[ii,])
                   AreaCL[ii] <- sum(sumComp[ii, ])
               }
            }
         }
      }

      AreaTot <- sum(NormAreaCL)
      sumTot <- sum(AreaCL)
      txt <- as.character(round(sumTot, 2))  #print original integral area witout RSF corrections
      maxNchar <- max(c(10,nchar(txt)+2))

      lgth <- c(10, maxNchar, 8, 8, 8, 9)    #width of table columns "Components", "Area", ", FWHM", "BE(eV)", "RSF", "TOT%"
      totLgth <- sum(lgth)+1
      cat("\n")

      txt <- paste("   File Name:", XPSSample@Filename)  #Filename
      cell <- printCell("label",txt,CellB=" ",totLgth,"left")  #call cellprint in modality LABEL  alignment LEFT
      QTabTxt <<- c(QTabTxt,cell,"\n") #add QTabTxt the information to compose the quant table
      TabTxt <<- c(TabTxt,txt,"\n")
      txt <- "-"  #separator
      cell <- printCell("separator", "-","",totLgth,"left")     #call cellprint in modality SEPARATOR  alignment LEFT
      TabTxt <<- c(TabTxt, cell)

      QTabTxt <<- c(QTabTxt,"\n")
      TabTxt <<- c(TabTxt,"\n")

      txt <- c("Components", "Area(cps)", "FWHM", "RSF", "BE(eV)", "TOT.(%)")
      cell <- printCell("tableRow", txt, CellB=" ", lgth, "center")
      QTabTxt <<- c(QTabTxt,cell,"\n")
      TabTxt <<- c(TabTxt,cell,"\n")
      cell <- printCell("separator", "-","",totLgth,"left")
      TabTxt <<- c(TabTxt, cell)

      QTabTxt <<- c(QTabTxt,"\n")
      TabTxt <<- c(TabTxt,"\n")

  	   for(ii in 1:N_CL){
          indx <- CoreLineIndx[ii]
          if (svalue(CoreLineCK[[ii]])=="TRUE") {
              #PEAK data
              Component <- names(CoreLineComp[ii])
              Area <- sprintf("%1.2f", AreaCL[ii])  #round number to 2 decimals and trasform in string
              RSF <- sprintf("%1.3f",XPSSample[[indx]]@RSF)
              Mpos <- NULL
              Mpos <- findMaxPos(XPSSample[[indx]]@RegionToFit)    #Mpos[1]==position of spectrum max,    Mpos[2]==spectrum max value
              BE <- sprintf("%1.3f",Mpos[1])
              if (RSF=="0.000") { #RSF not defined (es. Auger, VB spectra...) : cannot make correction for RSF
                 Conc <- sprintf("%1.2f",0)
              } else {
                 Conc <- sprintf("%1.2f",100*NormAreaCL[ii]/AreaTot)
              }
 		           txt <- c(Component, Area, " ", RSF, BE, Conc )   #total concentration relative to the coreline: FWHM e BE not print
              cell <- printCell("tableRow", txt, CellB=" ", lgth, "center")
              QTabTxt <<- c(QTabTxt,cell,"\n")                  #this QTabTxt will appear in the Quant widget
              cell <- printCell("tableRow", txt, CellB="|", lgth, "center")
              TabTxt <<- c(TabTxt,cell,"\n")                    #this TabTxt will appear in the R consolle
              #FIT COMPONENT's data
              if (hasComponents(XPSSample[[indx]])) {
                 N_comp <- length(CoreLineComp[[ii]]) #number of fit components
                 for(jj in 1:N_comp){ #ii runs on the corelines, jj runs on the fit components
                    comp <- CoreLineComp[[ii]][jj]
                    Area <- sprintf("%1.2f",sumComp[ii,jj]) #area of component jj coreline ii
                    FWHM <- sprintf("%1.2f",XPSSample[[indx]]@Components[[comp]]@param[3,1]) #FWHM component ii
                    RSF <- sprintf("%1.3f",XPSSample[[indx]]@Components[[comp]]@rsf) #RSF component ii
                    BE <- sprintf("%1.2f",XPSSample[[indx]]@Components[[comp]]@param[2,1]) #BE component ii
                    if (RSF=="0.000") {
                       Conc <- sprintf("%1.2f",0)
                    } else {
                       Conc <- sprintf("%1.2f",100*AreaComp[ii,jj]/AreaTot)  #Concentration component ii
                    }
     		             txt <- c(comp, Area, FWHM, RSF, BE, Conc) #make string to print
                    cell <- printCell("tableRow", txt, CellB=" ", lgth, "center") #print string in modality TABLEROW
                    QTabTxt <<- c(QTabTxt,cell,"\n")
                    cell <- printCell("tableRow", txt, CellB="|", lgth, "center") #print string in modality TABLEROW
                    TabTxt <<- c(TabTxt,cell,"\n")
                 }
              }
              QTabTxt <<- c(QTabTxt,"\n")
              TabTxt <<- c(TabTxt,"\n")

          }
       }

       Font <- get("XPSSettings", envir=.GlobalEnv)[[1]][1]
       FStyle <- get("XPSSettings", envir=.GlobalEnv)[[1]][2]
       FSize <- get("XPSSettings", envir=.GlobalEnv)[[1]][3]
#       svalue(QTable) <<- capture.output(cat("\n", QTabTxt))
       insert(QTable, paste(QTabTxt, collapse=""))
       font(QTable) <- list(weight="light", family=Font, style=FStyle, size=FSize)
       cat("\n", TabTxt)
   }


   ResetComp <- function(ii){
       indx <- CoreLineIndx[ii]
       if (svalue(CoreLineCK[[ii]])=="FALSE") {   #if the coreline is not selected the fit component are disabled
          if (hasComponents(XPSSample[[indx]])) {     #Does the coreline possess fitting components?
             svalue(ComponentCK[[ii]]) <- ""
          }
          names(CoreLineComp)[ii] <<- ""
       }
       if (svalue(CoreLineCK[[ii]])=="TRUE") {    #if the coreline is selected all the fitting components are selected
          if (hasComponents(XPSSample[[indx]])) {
             svalue(ComponentCK[[ii]]) <- names(XPSSample[[indx]]@Components)
          }
          names(CoreLineComp)[ii] <<- CoreLineNames[ii]  #reset coreline name: it could be dropped if all fitting components are cancelled
       }
   }

   SetRSF <- function(ii){
        kk <- 1
        for(ii in 1:NCoreLines){
           Eq.RSF <- TRUE  #Eq.RSF==TRUE means all RSF are equal
           indx <- CoreLineIndx[ii]
           LL=length(unlist(OrigCoreLinComp[ii]))    #the list vectors may have different lengths (different N. Fit components)
           if (LL > 0){                              #The RSF may be changed then control the RSF of all the fit components
              refRSF <- svalue(RSFCK[[kk]])          #set the referenceRSF as the first of the CorelineComponent
              if (Eq.RSF == TRUE){                      #if all Component RSF are equal set same value also in the Coreline RSF-Slot
                  XPSSample[[indx]]@RSF <<- as.numeric(refRSF)
              }
              for(jj in 1:LL){
                 newRSF <- svalue(RSFCK[[kk]])
                 if (newRSF != refRSF) { Eq.RSF <- FALSE } #at end of for Eq.RSF==TRUE means all RSF are equal
                 slot(XPSSample[[indx]]@Components[[jj]], "rsf") <<- as.numeric(newRSF) #load the new RSFR in the relative slot of the XPSSample
                 kk <- kk+1
              }
           }
           if (LL == 0) {
              newRSF <- svalue(RSFCK[[kk]])
              XPSSample[[indx]]@RSF <<- as.numeric(newRSF) #load the new RSFR in the relative slot of the XPSSample
              kk <- kk+1
           }
        }
   }


   CKHandlers <- function(){
         for(ii in 1:NCoreLines){
#----HANDLER on Widget-CoreLineCK to call ResetComp()
            if ((svalue(CoreLineCK[[ii]]) != CLChecked[ii]) && length(CLChecked[ii])>0) {
               CLChecked[ii] <<- svalue(CoreLineCK[[ii]])
               ResetComp(ii)     #set/reset fit components
            }
#----HANDLER on Widget-ComponentCK
            tmp1 <- svalue(ComponentCK[[ii]])
            tmp2 <- unlist(CoreLineComp[[ii]])
            if(svalue(CoreLineCK[[ii]])==FALSE && is.null(tmp2)==TRUE) {  #Coreline has only baseline is un-selected
               tmp1 <- ""
               tmp2 <- ""
               svalue(CoreLineCK[[ii]]) <- FALSE
            } else if(length(tmp1)==0 && length(tmp2)>0) {  #Coreline with fit is un-selected
               tmp1 <- ""
               svalue(CoreLineCK[[ii]]) <- FALSE
               CLChecked[ii] <<- FALSE
            } else {
               svalue(CoreLineCK[[ii]]) <- TRUE
               CLChecked[ii] <<- TRUE
            }
            if(is.null(tmp2)=="TRUE") {
               tmp2 <- ""
            }  # the correspondent coreline does NOT possess fitting components but only the BaseLine
            if (all(tmp1==tmp2)==FALSE) {    #if the two vector contain different elements
               CoreLineComp[ii] <<- list(tmp1) #modify component [ii] of the list following the checkbox component selections
               if (nchar(tmp1)==0) {
                  names(CoreLineComp)[ii] <<- ""
                  CLChecked[ii] <<- ""
               }
               if (nchar(tmp1) > 0) {
                  CoreLineComp <<- setNames(CoreLineComp, CoreLineNames)
               }
            }
         }

   }


#----MakeNb makes the notebook: a coreline for each notebook page
   MakeNb <- function(){
      kk <- 1
#      CoreLineCK <- list()     #define a list of Gwidget
#      ComponentCK <- list()
#      RSFCK <- list()
      for(ii in 1:NCoreLines){
          Qgroup[[ii]] <<- ggroup(label=CoreLineNames[ii], horizontal=TRUE, container=Qnb)
          Qlayout[[ii]] <<- glayout(homogeneous=FALSE, spacing=3, container=Qgroup[[ii]])
          NoComp <- "FALSE"
          indx <- CoreLineIndx[ii]
          tmp <- names(XPSSample[[indx]]@Components)
          if (is.null(tmp)) { NoComp <- "TRUE" }
          LL <- length(tmp)    # the vector of the list may have different length (different N FitComp)
          CoreLineComp <<- c(CoreLineComp, list(tmp))   #create a list containing the fit components of corelines
          txt <- paste("CORE LINE", ii , sep="")

          Qlayout[[ii]][1,1] <<- CLframe[[ii]] <<- gframe(text=txt, horizontal=FALSE, spacing=5, container=Qlayout[[ii]])
          CoreLineCK[[ii]] <<- gcheckbox(CoreLineNames[ii], checked=CLChecked[ii], handler=function(h, ...){
                                             ii <- svalue(Qnb) #set the ii value to the active notebook page
                                             CKHandlers()
                                      }, container=CLframe[[ii]])  #here CoreLineName is an array

          Qlayout[[ii]][1,2] <<- CMPframe[[ii]] <- gframe(text="COMPONENTS", horizontal=FALSE, spacing=5, container=Qlayout[[ii]])
          ComponentCK[[ii]] <<- gcheckboxgroup(unlist(CoreLineComp[ii]), checked=TRUE, handler=function(h,...){
                                             ii <- svalue(Qnb) #set the ii value to the active notebook page
                                             CKHandlers()
                                      }, container=CMPframe[[ii]]) #here CoreLineName is an array

          Qlayout[[ii]][1,3] <<- RSFframe[[ii]] <- gframe(text="RSF", horizontal=FALSE, spacing=5, container=Qlayout[[ii]])
          if ( NoComp=="TRUE"){
             ComponentCK[[ii]] <<- ""   #NO fitting components only baseline
             OldRSF <<- XPSSample[[indx]]@RSF
             RSFCK[[kk]] <<- gedit(text=OldRSF, handler=NULL, container=RSFframe[[ii]])
             kk <- kk+1
          } else {
             for(jj in 1:LL){
                OldRSF <<- XPSSample[[indx]]@Components[[jj]]@rsf
                RSFCK[[kk]] <<- gedit(text=OldRSF, handler=NULL, container=RSFframe[[ii]])
                kk <- kk+1
             }
          }
      }
      CoreLineComp <<- setNames(CoreLineComp, CoreLineNames)   #the list contains the names of the corelines and relative FitComponents
      OrigCoreLinComp <<- CoreLineComp

      ii <- NCoreLines+1
      Qgroup[[ii]] <<- ggroup(label=" QUANTIFY ", horizontal=FALSE, container=Qnb)

      Qlayout[[ii]] <<- glayout(homogeneous=FALSE, spacing=3, container=Qgroup[[ii]])
      Qlayout[[ii]][1,1] <<- gbutton(" QUANTIFY ", handler=function(h,...){        #glayout is generated in MakeNB()
                                          SetRSF()
                                          #extract indexes of CoreLines selected for quantification
                                          CheckedCL <- names(CoreLineComp)
                                          CheckedCL <- CheckedCL[which(CheckedCL !="")]
                                          CheckedCL <- sapply(CheckedCL, function(x) { x <- unlist(strsplit(x, "\\."))
                                                                                       x <- unname(x)
                                                                                       return(x)
                                                                                     })
                                          CheckedCL <- as.integer(CheckedCL[1,])
                                          #Control on the Pass Energies
                                          CK.PassE <- PassE[CheckedCL] #extracts PE valued corresponding to the elements selected for quantification
                                          idx <- unname(which(CK.PassE != CK.PassE[1])) #are the selected elements acquired at different PE?
                                          is.NC <- TRUE #logic, TRUE if NormCoeff is already computed
                                          idx <- CheckedCL[idx]  #now idx correctly indicates the corelines
                                          for(ii in idx){
                                              if(length(XPSSample[[ii]]@RegionToFit) < 3){
                                              is.NC <- FALSE}    #NormCoeff not saved in ...RegionToFit@NormCoeff
                                          }
                                          if (length(idx) > 0 && is.NC == FALSE) {  #selected elements are acquired at different PE and NormCoeff not computed
                                             answ <- gconfirm(" Found spectra acquired at different Pass Energies. \n Do you want to quantify spectra EXTRACTED FROM SURVEY ?",
                                                               title="NORMALIZATION COEFFICIENT", icon="warning")
                                             if (answ == TRUE){
                                                 #Control how many Survey spectra in XPSSample
                                                 SurIdx <<- grep("Survey", SpectList) #indexes of the names components == "Survey"
                                                 if (length(SurIdx) == 0) {
                                                     SurIdx <<- grep("survey", SpectList)
                                                 }
                                                 N_Survey <- length(SurIdx)
                                                 if (N_Survey > 1) {
                                                     txt <- paste(" Found ", N_Survey, " in the XPSSample ", activeFName, ": \n select the Survey used to extract the Core Lines", sep="")
                                                     gmessage(txt, title="SELECT SURVEY", icon="warning")

                                                     winCL <- gwindow("SELECT SURVEY", visible=FALSE)
                                                     size(winCL) <- c(250, 150)
                                                     groupCL <- ggroup(horizontal=FALSE, container=winCL)
                                                     CL_Names <- SpectList[SurIdx]
                                                     selectSur <- gradio(CL_Names, selected=1, horizontal=TRUE, container=groupCL)
                                                     gbutton("    OK     ", handler=function(h, ...){
                                                            SurIdx <<- svalue(selectSur)
                                                            SurIdx <<- unlist(strsplit(SurIdx, "\\."))   #drop "NUMBER." at beginning of coreLine name
                                                            SurIdx <<- as.integer(SurIdx[1])
                                                            dispose(winCL)
                                                     }, container=groupCL)
                                                     visible(winCL) <- TRUE
                                                     winCL$set_modal(TRUE)  #nothing can be done while running this macro
                                                 }
                                                 cat("\n => Compute the normalization coefficient")
                                                 CalcNormCoeff(CheckedCL, CK.PassE, SurIdx)
                                                 if(NormCoeff == -1){  #Found wide spectrum and corelines acquired using different energy units.
                                                    return()
                                                 } 
                                             } else {
                                                 gmessage("Spectra acquired at different PE cannot be used for quantification", title="QUANTIFICATION NOT ALLOWED", icon="warning")
                                                 return()
                                             }
                                          }
                                          quant(CoreLineComp)
                                      }, container = Qlayout[[ii]])

      Qlayout[[ii]][1,2] <<- gbutton(" WRITE TO FILE ", handler=function(h,...){
                                          Filename <- unlist(strsplit(QTabTxt[2], ":"))[2]
                                          Filename <- unlist(strsplit(Filename, "\\."))[1]
                                          Filename <- paste(Filename, "txt", sep="")
                                          PathFile <- gfile(text = "", type = "save",  initial.filename = "Filename", initial.dir = getwd())
                                          NL <- length(QTabTxt)
                                          if (is.null(PathFile)) {
                                              gmessage(msg="File Name not defined. Please give the file name", title="NO FILENAME", icon="error")
                                              return()
                                          }
                                          OutFile <- file(PathFile, open="wt")
                                          for (ii in 1:NL){
                                               writeLines(QTabTxt[ii], sep="", OutFile)
                                          }
                                          close(OutFile)
                                      }, container = Qlayout[[ii]])

      QTable <<- gtext(text="", wrap=FALSE, container = Qgroup[[ii]])
      size(QTable) <<- c(550, 370)
      svalue(QTable) <- ""
      for (ii in (NCoreLines+1):1){
          svalue(Qnb) <<- ii #refresh all pages until from the last to the first
      }

      return(CoreLineComp)
   }

#----Reset vars resets variables to initial values
   ResetVars <- function(){
      XPSSample <<- get(activeFName, envir = .GlobalEnv)   #load the active XPSSample dataFrame
      XPSSampleList <<- XPSFNameList()                     #list of all XPSSamples
      XPSSampleIdx <<- grep(activeFName,XPSSampleList)
      SpectList <<- XPSSpectList(activeFName)          #list of all CoreLines of the active XPSSample
      PassE <<- RetrivePE() # Retrieve list of CoreLine Pass Energies
      NormCoeff <<- 1

      CoreLineNames <<- ""
      CoreLineIndx <<- NULL
      FitComp <<- ""
      NComp <<- NULL
      CoreLineComp <<- list()
      OrigCoreLinComp <<- list()
      CompChecked <<- NULL

      CoreLineCK <<- list()     #define a list of the selected CL for quantification
      ComponentCK <<- list()     #define the CL fit components selected CL for quantification
      RSFCKv <- list()

      Qgroup <<- list()
      Qlayout <<- list()
      CLframe <<- list()
      CMPframe <<- list()
      RSFframev <<- list()
      QTable <<- list()

      NCoreLines <<- length(SpectList)
      NoFitFoundv <<- 0
      NmaxFitCompv <<- 0
      QTabTxt <<- ""    #text containing Quantification results for the Qtable gtext()
      TabTxt <<- ""     #text containing Quantification results for the RStudio consolle

      RegionToFit <- 0
      jj <- 1
      for(ii in 1:NCoreLines){
         if (length(XPSSample[[ii]]@RegionToFit) > 0){ #a Baseline is defined
            RegionToFit <- 1
            CoreLineNames[jj] <<- SpectList[ii]  #Save the coreline name where a baseline is defined
            CoreLineIndx[jj] <<- ii              #vector containing indexes of the corelines where a baseline is defined
            jj <- jj+1
            NFC <- length(XPSSample[[ii]]@Components)
            if (NFC > NmaxFitComp) {NmaxFitComp <<- NFC}
         }
      }
      if (RegionToFit == 0){
         gmessage(msg="WARNING: NO FIT REGIONS DEFINED ON THIS XPS SAMPLE", title = "WARNING", icon = "warning")
         return()
      }
      NCoreLines <<- length(CoreLineIndx) #now only the corelines with baseline are considered for the quantification
      CLChecked <<- rep("TRUE",NCoreLines)
   }


#---- variables ----
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }

   XPSSample <- NULL
   XPSSample <- get(activeFName, envir = .GlobalEnv)   #load the active XPSSample
   XPSSampleList <- XPSFNameList()                     #list of all XPSSamples
   XPSSampleIdx <- grep(activeFName,XPSSampleList)
   SpectList <- XPSSpectList(activeFName)              #list of all the corelines of the active XPSSample
   PassE <- RetrivePE()                                # Retrieve list of CoreLine Pass Energies
   names(PassE) <- SpectList
   NormCoeff <<- 1

   CoreLineNames <- ""
   CoreLineIndx <- NULL
   FitComp <- ""
   NComp <- NULL
   CoreLineComp <- list()
   OrigCoreLinComp <- list()
   CompChecked <- NULL

   CoreLineCK <- list()     #define a list of the Gwidgets
   ComponentCK <- list()
   RSFCK <- list()

   Qgroup <- list()
   Qlayout <- list()
   CLframe <- list()
   CMPframe <- list()
   RSFframe <- list()
   QTable <- list()

   NCoreLines <- length(SpectList)
   NoFitFound <- 0
   NmaxFitComp <- 0
   QTabTxt <- ""    #text containing Quantification results for the Qtable gtext()
   TabTxt <- ""     #text containing Quantification results for the RStudio consolle

   RegionToFit <- 0
   jj <- 1
   for(ii in 1:NCoreLines){
      if (length(XPSSample[[ii]]@RegionToFit) > 0){ #a baseline is defined
         RegionToFit <- 1
         CoreLineNames[jj] <- SpectList[ii]  #Save the coreline name where a baseline is defined
         CoreLineIndx[jj] <- ii              #vector containing indexes of the corelines where a baseline is defined
         jj <- jj+1
         NFC <- length(XPSSample[[ii]]@Components)
         if (NFC > NmaxFitComp) {NmaxFitComp <- NFC}
      }
   }
   if (RegionToFit == 0){
      gmessage(msg="WARNING: NO FIT REGIONS DEFINED ON THIS XPS SAMPLE", title = "WARNING", icon = "warning")
      return()
   }
   NCoreLines <- length(CoreLineIndx) #now only the corelines with baseline are considered for the quantification
   CLChecked <- rep("TRUE",NCoreLines)



#===== GUI =====

   Qwin <- gwindow(" QUANTIFICATION FUNCTION ", visible=FALSE)
   size(Qwin) <- c(280, 530)
   QmainGroup <- ggroup(horizontal=FALSE, container=Qwin)

   Qgrp0 <- ggroup(horizontal= TRUE, spacing=2, container=QmainGroup)
   glabel(text="XPSSample", container=Qgrp0)
   SelXPSData <- gcombobox(XPSSampleList, width=15, selected=XPSSampleIdx, handler=function(h, ...){
                                            activeFName <<- svalue(SelXPSData)
                                            ResetVars()
                                            assign("activeSpectIndx", 1, envir=.GlobalEnv)
                                            assign("activeSpectName", SpectList[1], envir=.GlobalEnv)
                                            delete(Qframe1,Qnb)
                                            Qnb <<- gnotebook(expand=TRUE, container = Qframe1)
                                            add(Qframe1,Qnb)
                                            CoreLineComp <<- MakeNb()
                                            plot(XPSSample)
                                      }, container=Qgrp0)

   QReset <- gbutton(" RESET ", handler=function(h,...){
                                            ResetVars()
                                            delete(Qframe1,Qnb)
                                            Qnb <<- gnotebook(expand=TRUE, container = Qframe1)
                                            add(Qframe1,Qnb)
                                            CoreLineComp <<- MakeNb()
                                            plot(XPSSample)
                                      }, container = Qgrp0)

   QSave <- gbutton(" SAVE ", handler=function(h,...){
                                            SetRSF()
                                            assign(activeFName, XPSSample, .GlobalEnv)  #save the fit parameters in the activeSample
                                            XPSSaveRetrieveBkp("save")
                                      }, container = Qgrp0)


   QSaveExit <- gbutton(" SAVE & EXIT ", handler=function(h,...){
                                            SetRSF()
                                            assign(activeFName, XPSSample, .GlobalEnv)  #save the fit parameters in the activeSample
                                            dispose(Qwin)
                                            XPSSaveRetrieveBkp("save")
                                      }, container = Qgrp0)

   Qframe1 <- gframe(text="Core Lines", spacing=5,horizontal=FALSE, container=QmainGroup)
   Qnb <- gnotebook(expand=TRUE, container = Qframe1)
   CoreLineComp <- MakeNb()

   CoreLineComp <- setNames(CoreLineComp, CoreLineNames)   #the list contains the names of the coreline and the relative FitComponents
   OrigCoreLinComp <- CoreLineComp 

   visible(Qwin) <- TRUE
   for(ii in (NCoreLines+1):1){   #reset nb pages from the last to the first
       svalue(Qnb) <- ii
   }
   Qwin$set_modal(TRUE)
}

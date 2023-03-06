# Convolution and Deconvolution of arrays carried out using FFT
# routine written on the base of Matlab indications

#' @title XPSCnvDcnv concolution, deconvolution functions
#' @description XPSCnvDcnv() is used to calculate the convolution of 
#'   the two X, Y input arrays or deconvolve Y from X.
#'   deconvolution can be made using the fft and its inverse or the
#    van Cittert algorithm.
#' @param x array to convolve
#' @param y array to convolve
#' @param deco = FALSE convolution of X, Y arrays, TRUE deconvolution of Y from X
#' @examples
#' \dontrun{
#'  x <- c(4, 13, 28, 34, 32, 21)
#'  y <- c(1, 2, 3)
#'	 C <- XPSCnvDcnv(x, y, deco=TRUE)
#'  print(C)
#'  [1]  4 5 6 7
#' }
#' @export
#'

XPSCnvDcnv <- function(x, y, deco=FALSE){

   CtrlEstep <- function(){
       if (SpectIdx1 > 0 && SpectIdx2 > 0) {  #core-line1 and core-line2 must be selected
          dE1 <- abs(FName[[SpectIdx1]]@.Data[[1]][2] -  FName[[SpectIdx1]]@.Data[[1]][1])
          dE1 <- round(dE1, 2)
          dE2 <- abs(FName[[SpectIdx2]]@.Data[[1]][2] -  FName[[SpectIdx2]]@.Data[[1]][1])
          dE2 <- round(dE2, 2)
          cat("\n Energy step core-line1: ", dE1)
          cat("\n Energy step core-line2: ", dE2)
          if (dE1 != dE2) {
              txt <- c("Selected core-lines have different energy step: cannot proceed",
                       "Use option INTERPOLATE - DECIMATE to make the energy steps be the same")
              gmessage(msg=txt, title="ERROR", icon="error")
              return(-1)
          } else if (dE1 == dE2){
              return(1)
          }
       }
       return(-1) #if one or both the core-lines are not selected return(-1)
   }






   CnvDcnv <- function(x, y, deco=FALSE){
      cnv <- NULL
      dcnv <- NULL
      eps <- 1e-15

# CONVOLUTION
      if ( deco == FALSE){
           LLx <- length(x)
           LLy <- length(y)
           x <- c(x, rep(0, (LLy-1)))
           y <- c(y, rep(0, (LLx-1)))
           LL <- LLx+LLy-1
           cnv <- ( fft(x) * fft(y) )
           cnv <- Re(fft(cnv, inverse=TRUE))/LL
           return(cnv)
      }
# DECONVOLUTION deconvolve y from x
      if ( deco == TRUE ){
           LLx <- length(x)
           LLy <- length(y)
           y <- c(y, rep(0, (LLx-LLy)))
           dcnv <- ( (eps + fft(x)) / (eps + fft(y)) )
           dcnv <- Re(fft(dcnv, inverse = TRUE))/LLx
           return(dcnv)
      }
   }

#------- Van Citter END -------------------------
   VanCittert <- function(s, y){  #Deconvolution using the iterative VanCittert algorithm

   AlignCnv <- function(s, cnv, EE){
           LL <- length(s)
           LL2 <- LL*2
           s <- s/max(s)
           cnv <- cnv/max(cnv)
           err1 <- sum(abs(cnv[, 1]))
           for(ii in seq(1,LL,1)){
               err2 <- sum(abs(cnv[ii:(LL+ii-1), 1] - s))
               if(err2 < err1){
                  err1 <- err2
                  shift <- ii
               }
           }
           matplot(x=EE, y=cbind(s, cnv[shift:(shift+LL-1),1]), xlab="Kinetic energy [eV]", ylab="Intensity [a.u.]",
                                 type=c("l", "l"), lty=c(1,1), lw=1, col=c("black", "red"))

           #The low KE tail of the convolution needs to be cut to reduce length(cnv) to LL
           #we cannot simply extract data cnv[shift:(shift+LL)] from cnv
           #eliminate part of the tail MUST be done by forcing to zero C1s elements.
           #This corresponds to make part of the C1s tail = 0, i.e. y[1:zz] <- 0
           #Then we have to update the matrix Y to compute the correct convolution.

           TmpWin <- gwindow("DAMPING FACTOR", parent=c(50, 10), visible=FALSE)
           size(TmpWin) <- c(270, 130)
           TmpGroup <- ggroup(horizontal=FALSE, spacing = 7, container = TmpWin)
           TmpFrame1 <- gframe("Factor to Damp the Convolution tail to 0", horizontal=FALSE, spacing=7, container=TmpGroup)
           CutFact <- gedit(initial.msg="Damp Factor from 0 to 1 ", handler = function(h, ...){
                          DFact <- as.numeric(svalue(CutFact))
                          DFact <- as.integer(DFact*shift)+1 #DFact=0 no damping the cnv is just shifted
                          y[1:DFact] <- 0 #DFact=1 tail C1s to 0 => tail cnv is damped

                          Yc <<- c(y, rep(0, LL))        #now length(ss2) = LL2
                          Yc <<- matrix(data=Yc, ncol=1) #Yc is a prototype column for the YY matrix is 2*LL elements long

                          for(ii in 1:LL2){              #Y square matrix LL2 x LL2
                             Y[ ,ii] <<- Yc
                             Yc[,1] <<- c(0, Yc[1:(LL2-1)])
                          }
                          cnv <- Y %*% ss2
                          cnv <- cnv/max(cnv)
                          matplot(x=EE, y=cbind(x=s, y=cnv[shift:(shift+LL-1),1]), xlab="Kinetic energy [eV]", ylab="Intensity [a.u.]",
                                                type=c("l", "l"), lty=c(1,1), lw=1, col=c("black", "red"))
                       }, container=TmpFrame1)

           TmpFrame2 <- gframe("Apply de-noise to iterations", spacing=3, container=TmpGroup)
           DeNsGroup <- ggroup(horizontal=TRUE, spacing = 3, container = TmpFrame2)
           DeNs <- gcheckbox("YES   ", checked=FALSE, handler=function(h, ...){
                          svalue(NoDeNs) <- FALSE
                          DeNoise <<- TRUE
                       }, container=DeNsGroup)
           NoDeNs <- gcheckbox("NO", checked=FALSE, handler=function(h, ...){
                          svalue(DeNs) <- FALSE
                          DeNoise <<- FALSE
                       }, container=DeNsGroup)

           gbutton(text="SAVE Damp Factor & START ITERATION", handler=function(h,...){
                          if (svalue(DeNs)==FALSE && svalue(NoDeNs)==FALSE){
                              gmessage("ATTENTION: DeNoise Selection lacking!", title="WARNING", icon="warning")
                              return()
                          }
                          dispose(TmpWin)
                       }, container=TmpGroup)
           visible(TmpWin) <- TRUE
           TmpWin$set_modal(TRUE)
           return(shift)
   }
   
#-----
           LLs <- length(s)       #real spectrum to deconvolve
           LLy <- length(y)       #deconvolving array of data
           if (LLs < LLy){        #same length for the two vectors
               s  <- c(rep(s[1], (LLy-LLs)), s) #to respect s,y alignment zers at beginning
               LL <- LLy
           }
           if (LLy < LLs){
               y  <- c(rep(y[1], (LLs-LLy)), y) #to respect s,y alignment zers at beginning
               LL <- LLs
           }
           if (LLs == LLy) { LL <- LLs }
           #Now Length(s) = Length(y) = LL. If LL odd makes it even
           if (LL/2 > floor(LL/2)){ #LL is odd
               LL <- LL+1
               s[LL] <- 0
               y[LL] <- 0
           }
           EE <- CvDcv[[1]]@.Data[[1]]     #array of energies for the abscissa
           dE <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]
           for(ii in 1:(LL-LLs)){           #is length vector s is changed, harmonize the array of energies
               EE <- c((EE[1]-dE), EE)
           }

           LL2 <- 2*LL
           ss2 <<- c(s, rep(0, LL))      #now length(ss2) = LL2

           s <- matrix(data=s, ncol=1)   # transform s in a column of data
           y <- matrix(data=y, ncol=1)   # transform y in a column of data
           ss <- matrix(data=s, ncol=1)  # transform ss in a column of data
           ss2 <<- matrix(data=ss2, ncol=1)  # transform ss2 in a column of data
           cnv <- NULL
           dcnv <- matrix(nrow=LL, ncol=4)
           dcnv[ ,1] <- s
           Sum.S <- sum(abs(s))

#           MU <- readline("Mu values please: ") #relaxation factor mu see ref[@@@]
#           MU <- as.numeric(MU)
            MU <- 0.3

#Van Cittert:  x(k+1) = x(k) + [s - Y x ]
# [&&&] Chengqi Xu et al J.Opt.Soc.Am. (1994), 11(11), 2804
# [@@@] Bandzuch et al Nucl.Instr.Meth.Phys.Res.A (1997), 384, 506)


           cat("\n WORKING... please wait \n")

           cat("\n ==> Building data matrix Y for convolution  ")
           Y <<- matrix(nrow=LL2, ncol=LL2)
           Yc <<- c(y, rep(0, LL))        #now length(ss2) = LL2
           Yc <<- matrix(data=Yc, ncol=1) #Yc is a prototype column for the YY matrix is 2*LL elements long
           for(ii in 1:LL2){              # Y square matrix LL2 x LL2
               Y[ ,ii] <<- Yc
               Yc[,1] <<- c(0, Yc[1:(LL2-1)])
           }
           cnv <- Y %*% ss2          #this corresponds to the convolution s*y

           if(DecoC1s == TRUE){
              cat("\n ==> Reshaping the Convolution matrix  ")
              shift <- AlignCnv(s, cnv, EE)
           }

           AA <- err <- sum(abs(s))  #area of the spectrum to deconvolve
           AA <- AA/100
           iter <- 1

           cat("\n ==> Start van Cittert iteration  ")
           cat("\n ==> Press a key to proceed, 'x' to EXIT  ")
           DeNoise <- TRUE
           T1 <- TRUE
           T2 <- T3 <- FALSE
           LL <- length(s)
           while (err > AA){
               cnv <- Y %*% ss2            #update cnv using the new Y matrix defined in AlignCnv()
               if(DecoC1s == TRUE){
                  cnv <- cnv[shift:(shift+LL-1),1]#take only one half of the convolution
               } else {
                  cnv <- cnv[seq(1,LL2,2)] #decimation by 2
               }
               Sum.C <- sum(abs(cnv))
               if(Sum.C == 0){
                  NormFact <- 1
               } else {
                  NormFact <- Sum.S/sum(cnv)
               }
               cnv <- NormFact*cnv

#               if (iter == 1) { ss <- 0 * ss }
               if(DeNoise){
                  if(T1){
                     T1 <- T3 <- FALSE
                     T2 <- TRUE
                     tmp <- s                          #no shift applied
                  } else if(T2){
                     T1 <- T2 <- FALSE
                     T3 <- TRUE
                     tmp <- c(rep(0, 5), s[1:(LL-5)])  #shift s to right
                  } else if(T3){
                     T2 <- T3 <- FALSE
                     T1 <- TRUE
                     tmp <- c(s[6:LL], rep(0, 5))  #shift s to left
                  }
                  ss <- ss + MU*(tmp - cnv)
               } else {
                  ss <- ss + MU*(s - cnv)   #this corresponds to eq. (***)
               }
               ss2 <<- c(ss, rep(0, LL)) #now length(ss2) = LL2
               err <- sum(abs(s - cnv))
               dcnv[ ,2] <- cnv          #green
               dcnv[ ,3] <- MU*(s - cnv) #blue
               dcnv[ ,4] <- ss           #red
               matplot(x=EE, y=dcnv, type="l", xlab="Kinetic Energy [eV]", ylab="Intensity [a.u.]", 
                       lw=1, lty=c(1,1,1,1), col=c("black", "green", "blue", "red"))
               legend(x="topleft", bty="n", legend=c("Original Data", "Convolution","Difference","Deconvolved Spectrum"), text.col=c("black", "green", "blue", "red"), cex=1)

               txt <- paste("Iteration: ", as.character(iter), "   error = ", as.character(round(err, 3)), " ", sep="")
               print(interactive())
#               aaa <- readline(prompt=txt)
               aaa <- gconfirm(msg="Exit iteration?", title="block iteration", icon="warning")
               if (aaa){ break }
               if (aaa == "x"){ break }
               iter <- iter + 1
            }
            return(dcnv[ ,4])
   }
#------- Van Citter END -------------------------

   HannWin <- function(strt, stp){
       LL <- abs(stp-strt)+1 #length of the descending/ascending hanning branches
       Hann <- matrix(nrow=LL, ncol=2)
       for (ii in 1:LL){
           Hann[ii, 1] <- 0.5 - 0.5*cos(pi*(ii-1)/(LL-1))      #ascending hanning window branch
           Hann[ii, 2] <- 1-(0.5 - 0.5*cos(pi*(ii-1)/(LL-1)) ) #descending hanning window branch
       }
       #Hann = 0, 0.1, 0.3, 0.5, 0.8, 1  ,  1, 0.8 0.5, 0.3, 0,1, 0
       return(Hann)
   }

   interp <- function(s){
           LL <- length(s)
           s2 <- NULL #interpolated signal
           jj <- 1
           for(ii in 1:(LL-1)){ #s2 long 2*LL by interpolation of adjacent data
              s2[jj] <- s[ii]
              s2[jj+1] <- 0.5*(s[ii+1] - s[ii]) + s[ii]
              jj <- jj+2
           }
           s2[jj] <- s[LL]
           s2[jj+1] <- 0
           return(s2)
   }


#--- Variables ---
   CutOFF <- NULL
   FNameList <- XPSFNameList() #list of XPSSamples
   FName <- get(activeFName,envir=.GlobalEnv)
   SpectList <- XPSSpectList(activeFName)
   SpectIdx1 <- -1  #inital value must be non-null
   SpectIdx2 <- -1
   CLName <- ""
   CvDcv <- new("XPSSample")
   CvDcv[[1]] <- NULL
   CvDcv[[2]] <- NULL
   CvDcv[[3]] <- NULL
   CvDcv[[4]] <- NULL
   Y <- NULL
   Yc <- NULL
   ss2 <- NULL
   DecoC1s <- FALSE
   DeNoise <- NULL


#--- GUI ---
   ConvWin <- gwindow("CONVOLUTION", parent=c(50, 10), visible=FALSE)
   size(ConvWin) <- c(400, 450)

   ConvGroup1 <- ggroup(horizontal=FALSE, spacing = 3, container = ConvWin)
   ConvGroup2 <- ggroup(horizontal=TRUE, spacing = 3, container = ConvGroup1)
   glabel("  ", container=ConvGroup1) #just to separate buttons
   ConvGroup3 <- ggroup(horizontal=FALSE, spacing = 3, container = ConvGroup1)

   CVframe1 <- gframe(text="XPS Sample", horizontal=FALSE, spacing=5, container=ConvGroup2)

   XPS.Sample <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){

                                 activeFName <- svalue(XPS.Sample)
                                 FName <<- get(activeFName, envir=.GlobalEnv)
                                 SpectList <<- XPSSpectList(activeFName)
                                 plot(FName)
                                 delete(CVframe2, CoreLine1)
                                 CoreLine1 <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                                                          SpectName <- svalue(CoreLine1)
                                                          SpectName <- unlist(strsplit(SpectName, "\\."))
                                                          SpectIdx1 <<- as.integer(SpectName[1])
                                                          CvDcv[[1]] <<- FName[[SpectIdx1]]
                                                          plot(CvDcv)
                                                          if (CtrlEstep() == -1) {return()}
                                                          if (length(svalue(CoreLine2)) > 0){
                                                              enabled(ConvButton1) <- TRUE
                                                              enabled(ConvButton2) <- TRUE
                                                              enabled(DCnvButt1) <- TRUE
                                                              enabled(DCnvButt2) <- TRUE
                                                          }

                                                }, container=CVframe2)

                                 delete(CVframe3, CoreLine2)
                                 CoreLine2 <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                                                          SpectName <- svalue(CoreLine2)
                                                          SpectName <- unlist(strsplit(SpectName, "\\."))
                                                          SpectIdx2 <<- as.integer(SpectName[1])
                                                          CvDcv[[2]] <<- FName[[SpectIdx2]]
                                                          if (is.null(CvDcv[[1]])){
                                                              plot(CvDcv[[2]])
                                                          } else {
                                                              plot(CvDcv)
                                                          }
                                                          if (CtrlEstep() == -1) {return()}
                                                          if (length(svalue(CoreLine1)) > 0){
                                                              enabled(ConvButton1) <- TRUE
                                                              enabled(ConvButton2) <- TRUE
                                                              enabled(DCnvButt1) <- TRUE
                                                              enabled(DCnvButt2) <- TRUE
                                                          }
                                                          CtrlEstep()
                                                }, container=CVframe3)

                       }, container = CVframe1)


   CVframe2 <- gframe(text="CoreLine 1", horizontal=FALSE, spacing=5, container=ConvGroup2)
   CoreLine1 <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                     SpectName <- svalue(CoreLine1)
                     SpectName <- unlist(strsplit(SpectName, "\\."))
                     SpectIdx1 <<- as.integer(SpectName[1])
                     if (CtrlEstep() == -1) {return()}
                     plot()
                 }, container=CVframe2)

   CVframe3 <- gframe(text="CoreLine 2", horizontal=FALSE, spacing=5, container=ConvGroup2)
   CoreLine2 <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                     SpectName <- svalue(CoreLine2)
                     SpectName <- unlist(strsplit(SpectName, "\\."))
                     SpectIdx1 <<- as.integer(SpectName[1])
                     if (CtrlEstep() == -1) {return()}
                     plot()
                 }, container=CVframe3)


   ConvButton1 <- gbutton(" CONVOLVE Core Line 1 and Core Line 2 via FFT", spacing=7, handler=function(h,...){
                     Cnv <- CnvDcnv(CvDcv[[1]]@.Data[[2]], CvDcv[[2]]@.Data[[2]], deco=FALSE)
                     CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store Convolution
                     dE <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]  #energy step of CoreLine1
                     LL <- length(Cnv)
                     xx <- NULL
                     xx[1] <- FName[[SpectIdx1]]@.Data[[1]][1]
                     for(ii in 2:LL){    #Build energy scale for the Convolution
                        xx[ii] <- xx[(ii-1)] + dE
                     }
                     #store information in the New Convolution Core-Line
                     CvDcv[[3]]@.Data[[1]] <<- xx
                     CvDcv[[3]]@.Data[[2]] <<- Cnv
                     CvDcv[[3]]@units <<- FName[[SpectIdx1]]@units
                     CvDcv[[3]]@Flags <<- FName[[SpectIdx1]]@Flags
                     CvDcv[[3]]@Info <<- paste("Convolution of CoreLines ",FName[[SpectIdx1]]@Symbol," and ",FName[[SpectIdx2]]@Symbol, sep="")
                     CvDcv[[3]]@Symbol <<- "Convolution"
                     CLName <<- "Conv"
                     plot(CvDcv)
                 }, container=ConvGroup3)

   ConvButton2 <- gbutton(" CONVOLVE Core Line 1 and Core Line 2 by Sum of Products", spacing=7, handler=function(h,...){
                     x <- CvDcv[[1]]@.Data[[2]]
                     y <- CvDcv[[2]]@.Data[[2]]

                     LLx <- length(x)       #spectrum to deconvolve
                     LLy <- length(y)       #deconvolving spectrum
                     #if LLx != LLy make array length the same
                     if (LLx < LLy){
                        x  <- c(x,rep(0, (LLy-LLx))) #both long LL = LLy
                        LL <- LLy
                     }
                     if (LLy < LLx){
                        y  <- c(y,rep(0, (LLx-LLy))) #both long LL = LLx
                        LL <- LLx
                     }
                     if (LLx == LLy) { LL <- LLx }
                     #make array length equal to LL = LL1+LL2
                     x <- c(x,rep(0, LL))
                     y <- c(y,rep(0, LL))

                     LL2 <- 2*LL
                     Cnv <- rep(0, LL2)
                     dE <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]  #energy step of CoreLine1
                     EE <- NULL
                     EE[1] <- CvDcv[[1]]@.Data[[1]][1]
                     #now convolution
                     for (jj in 1:LL2){
                         summ <- 0
                         summ1 <- 0
                         for(ii in 1:jj){
                             summ <- summ + x[ii] * y[(jj-ii+1)]
#                             summ1 <- summ1 + x[(LL2-ii)] * y[(LL2-jj+ii)]
                         }
                         Cnv[jj] <- summ1   #convolution
                         EE[jj] <- EE[1] +(jj-1)*dE #array of energies (abscissa) for plotting Conv
                     }
                     #store information in the New Convolution Core-Line
                     CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store Convolution
                     CvDcv[[3]]@.Data[[1]] <<- EE
                     CvDcv[[3]]@.Data[[2]] <<- Cnv
                     CvDcv[[3]]@units <<- FName[[SpectIdx1]]@units
                     CvDcv[[3]]@Flags <<- FName[[SpectIdx1]]@Flags
                     CvDcv[[3]]@Info <<- paste("Convolution of CoreLines ",FName[[SpectIdx1]]@Symbol," and ",FName[[SpectIdx2]]@Symbol, sep="")
                     CvDcv[[3]]@Symbol <<- "Convolution"
                     CLName <<- "Conv"
                     plot(CvDcv)
                 }, container=ConvGroup3)

   DCnvButt1 <- gbutton("DECONVOLVE Core Line 2 from Core Line 1 via iFFT", spacing=7, handler=function(h,...){
                     dE1 <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]
                     dE2 <- CvDcv[[2]]@.Data[[1]][2] - CvDcv[[2]]@.Data[[1]][1]
                     if ( abs(dE1-dE2) > 1e3 ) {
                         gmessage("ERROR: Core-Line 1 and Core-Line 2 must have the same energy step", title="ERROR", icon="error")
                         return()
                     }
                     DeCnv <- CnvDcnv(CvDcv[[1]]@.Data[[2]], CvDcv[[2]]@.Data[[2]], deco=TRUE)
                     CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store DEconvolution
                     dE <- CvDcv[[2]]@.Data[[1]][2] - CvDcv[[2]]@.Data[[1]][1] #energy step of CoreLine2
                     xx <- NULL
                     xx[1] <- FName[[SpectIdx2]]@.Data[[1]][1]
                     LL <- length(DeCnv)
                     for(ii in 2:LL){    #Build energy scale for the DEconvolution
                        xx[ii] <- xx[(ii-1)] + dE
                     }
                     #store information in the New Convolution Core-Line
                     CvDcv[[3]]@.Data[[1]] <<- xx
                     CvDcv[[3]]@.Data[[2]] <<- DeCnv
                     CvDcv[[3]]@units <<- FName[[SpectIdx2]]@units
                     CvDcv[[3]]@Flags <<- FName[[SpectIdx2]]@Flags
                     CvDcv[[3]]@Info <<- paste("Deconvolution of CoreLine ",FName[[SpectIdx2]]@Symbol," from ",FName[[SpectIdx1]]@Symbol, sep="")
                     CvDcv[[3]]@Symbol <<- "Deconvolution"
                     CLName <<- "Deconv"
                     plot(CvDcv)
                 }, container=ConvGroup3)

   DCnvButt2 <- gbutton("DECONVOLVE Core Line 2 from Core Line 1 via VAN CITTERT", spacing=7, handler=function(h,...){
                     svalue(InfoMsg) <- "If Van Cittert iteration does not converge PRESS 'x' to EXIT"
                     dE1 <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]
                     dE2 <- CvDcv[[2]]@.Data[[1]][2] - CvDcv[[2]]@.Data[[1]][1]
                     if ( abs(dE1-dE2) > 1e3 ) {
                         gmessage("ERROR: Core-Line 1 and Core-Line 2 must have the same energy step", title="ERROR", icon="error")
                         return()
                     }
                     DecoC1s <<- gconfirm(msg="Is the FWHM of Core-Line 2 much\n narrower than FWHM of Core-Line1?", title="WARNING", icon="warning")
                     DeCnv <- VanCittert(CvDcv[[1]]@.Data[[2]], CvDcv[[2]]@.Data[[2]])
                     #store information in the New Convolution Core-Line
                     LL <- length(FName[[SpectIdx1]]@.Data[[1]])
                     CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store DEconvolution
                     CvDcv[[3]]@.Data[[1]] <<- FName[[SpectIdx1]]@.Data[[1]]
                     CvDcv[[3]]@.Data[[2]] <<- DeCnv[1:LL] #remember that in VanCittert() if LLx < LLy zeroPadding
                     CvDcv[[3]]@units <<- FName[[SpectIdx1]]@units
                     CvDcv[[3]]@Flags <<- FName[[SpectIdx1]]@Flags
                     CvDcv[[3]]@Info <<- paste("Deconvolution of CoreLine ",FName[[SpectIdx2]]@Symbol," from ",FName[[SpectIdx1]]@Symbol, sep="")
                     CvDcv[[3]]@Symbol <<- "Deconvolution"
                     CLName <<- "Deconv"

                     plot(CvDcv)
                 }, container=ConvGroup3)

#   WinButt <- gbutton("   Apply Hanning window to smooth spectrum edges    ", spacing=7, handler=function(h,...){
#                     CVframe4 <- gframe(text="Hanning Window", horizontal=FALSE, spacing=5, container=ConvGroup3)
#                     glabel("Select Spectrum to smooth", container=CVframe4)
#                     CoreLine3 <- gcombobox(SpectList, selected=-1, handler=function(h,...){
#                                            SpectName <- svalue(CoreLine3)
#                                            SpectName <- unlist(strsplit(SpectName, "\\."))
#                                            idx <- as.integer(SpectName[1])
#                                            glabel("Now define the edge extension to smooth", container=CVframe4)
#                                            visible(ConvWin) <- FALSE
#                                            visible(ConvWin) <- TRUE    #refresh ConvWin
#                                            plot(FName[[idx]])

#                                            tcl("update", "idletasks")
#                                            pos <- locator(n=2, type="p", col="red", pch=3)
#                                            idx1 <- findXIndex(FName[[idx]]@.Data[[1]], pos$x[1])
#                                            idx2 <- findXIndex(FName[[idx]]@.Data[[1]], pos$x[2])
#                                            WW <- abs(idx1-idx2)+1  #length of half Hanning window
#                                            LL <- length(FName[[idx]]@.Data[[2]])
#                                            Hann <- HannWin(1, WW) #Hann() generates two branches of lenght W
#                                            for(ii in 1:WW){ #Now apply Hann brances at beginning and at end of selected Core-Line
#                                                FName[[idx]]@.Data[[2]][ii] <<- FName[[idx]]@.Data[[2]][ii]*Hann[ii, 1]   #smooth the left Core-Line edge
#                                                FName[[idx]]@.Data[[2]][(LL-WW+ii)] <<- FName[[idx]]@.Data[[2]][(LL-WW+ii)]*Hann[ii, 2]  #smooth the right Core-Line edge
#                                            }
#                                            plot(FName[[idx]])
#                                            if (idx == SpectIdx1) {
#                                                CvDcv[[1]] <<- FName[[idx]]
#                                                delete(ConvGroup3, CVframe4)
#                                            } else if (idx == SpectIdx2){
#                                                CvDcv[[2]] <<- FName[[idx]]
#                                                delete(ConvGroup3, CVframe4)
#                                            } else if(SpectIdx1==-1 || SpectIdx2==-1){
#                                                gmessage("Please select Core-Lines to proceed", title="WARNING", icon="warning")
#                                            } else {
#                                                gmessage("Now ready to proceed", title="INFO", icon="info")
#                                            }
#                                  }, container=CVframe4)
#                 }, container=ConvGroup3)


   Reset <- gbutton("   RESET    ", spacing=7, handler=function(h,...){
                     SpectIdx1 <<- -1
                     SpectIdx2 <<- -1
                     Cnv <<- NULL
                     DeCnv <<- NULL
                     CutOFF <<- NULL
                     CvDcv[[1]] <<- NULL
                     CvDcv[[2]] <<- NULL
                     CvDcv[[3]] <<- NULL
                     Y <<- NULL
                     Yc <<- NULL
                     ss2 <<- NULL
                     DecoC1s <<- FALSE
                     svalue(XPS.Sample) <<- ""
                     svalue(CoreLine1) <<- ""
                     svalue(CoreLine2) <<- ""
                     plot.new()
                 }, container=ConvGroup3)

   SaveExit <- gbutton("   SAVE & EXIT    ", spacing=7, handler=function(h,...){
                      LL <- length(FName)+1
                      FName[[LL]] <<- CvDcv[[3]]
                      names(FName)[LL] <- CLName
                      assign(activeFName, FName, envir=.GlobalEnv)
                      dispose(ConvWin)
                 }, container=ConvGroup3)

   InfoMsg <- glabel(text="    ", spacing=7, container=ConvGroup3)
                 
                 

   enabled(ConvButton1) <- FALSE
   enabled(ConvButton2) <- FALSE
   enabled(DCnvButt1) <- FALSE
   enabled(DCnvButt2) <- FALSE

   visible(ConvWin) <- TRUE  
}



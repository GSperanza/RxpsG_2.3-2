# XPSDepthProfile reconstruction of Depth Profiles
#
#' @title XPSDepthProfile reconstruction of Depth Profiles
#' @description XPSDepthProfile function is used to manipulate single or
#'   multiple XPS-Samples acquired by varying the tilt angle (ARXPS) or after 
#'   sputter-etch cycles (sputter-depth-profiles)
#'   The function loads the spectra at variopus angles or at various sputter-cycles
#'   defines the Baselines and computes the element quantification.
#'   Results are automatically plotted as a function of the tilt angle or
#'   of the etch cycles
#' @examples
#' \dontrun{
#' 	XPSDepthProfile()
#' }
#' @export
#'


XPSDepthProfile <- function() {

     CK.Elmts <- function(){ #extracts the name of the profiled elements
             CLnames <<- names(XPSSample)
             TmpNames <- CLnames
             LL <- length(CLnames)
             ii <- 1
             jj <- 1
             while (LL > 0){
                Sym <- TmpNames[jj]
                if(Sym == "Survey" || Sym == "survey"){
                   jj <- jj+1
                } else {
                   idx1 <- which(CLnames == Sym)  #extracts indexes of all corelines having name == Sym
                   idx2 <- which(TmpNames == Sym) #extracts indexes of all corelines having name == Sym
                   CoreLineList[[ii]] <<- idx1
                   names(CoreLineList)[ii] <<- Sym
                   TmpNames <- TmpNames[-idx2]
                   ii <- ii+1
                   LL <- length(TmpNames)
                }
                if (jj > LL) {break}
             }
             CLnames <<- names(XPSSample)
     }

     MK.Panel <- function(){  #construct the list of checkboxes describing all the XPSSample corelines
             CL.Sym <<- names(CoreLineList)
             N.CL <<- length(CL.Sym) #Number of profiled elements
             N.Cycles <<- max(sapply(CoreLineList, function(x) length(x)))
             # (<<<===)  This operation transform a list in a matrix maintaining column names
             # length<- introduces NA if data lacking
             CoreLineList <<- sapply(CoreLineList, "length<-", N.Cycles)

             #CheckBoxGroup to slect the CL to analyze
             delete(T1frameCoreLines, CoreLineCK)
             CoreLineCK <<- gcheckboxgroup(CL.Sym,checked=FALSE, horizontal=TRUE, container=T1frameCoreLines)
     }

     MK.Baseline <- function(idx, splinePoints){
        if (BaseLine == "shirley"){BaseLine <- "Shirley"}       #different names for old/new RXPSG packages
        if (BaseLine == "2p.shirley"){BaseLine <- "2P.Shirley"} #transform to new BaseLineNames.
        if (BaseLine == "3p.shirley"){BaseLine <- "3P.Shirley"} #Exact Baseline Names required to generate the Baseline see XPSClass
        if (BaseLine == "lp.shirley"){BaseLine <- "LP.Shirley"}
        if (BaseLine == "2p.tougaard"){BaseLine <- "2P.Tougaard"}
        if (BaseLine == "3p.tougaard"){BaseLine <- "3P.Tougaard"}
        if (BaseLine == "4p.tougaard"){BaseLine <- "4P.Tougaard"}

        XPSSample[[idx]] <<- XPSsetRegionToFit(XPSSample[[idx]])
        XPSSample[[idx]] <<- XPSbaseline(XPSSample[[idx]], BaseLine, deg=0, Wgt=0, splinePoints )
        XPSSample[[idx]] <<- XPSsetRSF(XPSSample[[idx]], XPSSample[[idx]]@RSF)
     }


     ResetVars <- function(){
        CLnames <<- NULL          #named of the profiled elements
        N.XS <<- NULL
        N.CL <<- NULL
        N.Cycles <<- NULL
        CL <<- NULL
        CL.Sym <<- NULL
        Matched <<- NULL         #CoreLines present in all the XPSSample
        SelectedCL <<- NULL
        SelectedXPSSamp <<- list()
        XPSSample <<- NULL
        CoreLineList <<- list()   #define a list for the XPSSample corelines
        Angles <<- NULL
        BaseLine <<-  NULL        #by default baseline selected for BKGsubtraction
        splinePoints <<- list(x=NULL, y=NULL)
        BL.Ends <<- list(x=NULL, y=NULL)
        QntMat <<- NULL
        DPrflType <<- NULL
        TkoffAngles <<- NULL
     }

#--- Variables
     options(warn = -1)
     N.XS <- NULL
     N.CL <- NULL
     N.Cycles <- NULL
     CL <- NULL
     CL.Sym <- NULL
     Matched <- NULL         #CoreLines present in all the XPSSample
     SelectedCL <- NULL
     SelectedXPSSamp <- list()
     XPSSample <- NULL
     CoreLineList <- list()   #define a list for the XPSSample corelines
     CoreLineCK <- NULL       #pointer to the profiled checkbox elements
#     T1frameCoreLines <- NULL
     TkoffAngles <- NULL
     BaseLine <-  NULL        #by default baseline selected for BKGsubtraction
     splinePoints <- list(x=NULL, y=NULL)
     BL.Ends <- list(x=NULL, y=NULL)
     QntMat <- NULL
     DPrflType <- NULL
     ToffAngles <- NULL
     MatCol <- c("black", "red1", "limegreen","blue",
            "orange4","firebrick1","chartreuse4","deepskyblue",
            "yellow3","orange","palegreen","dodgerblue",
            "grey78","red4","green4", "lightblue3",
            "orangered","olivedrab1","deepskyblue","rosybrown3",
            "lightseagreen","goldenrod1","olivedrab1","dodgerblue2",
            "lightskyblue3","goldenrod4","olivedrab4","dodgerblue4",
            "lightskyblue4","deeppink4","darkgreen","darkblue",   "darkorchid2","gold","springgreen","darkmagenta",
            "deepskyblue4","brown4","olivedrab","blueviolet",     "grey40","orangered","green3","blue3",
            "steelblue4","yellow","yellowgreen","turquoise3",  "plum1","magenta3", "darkturquoise")

     if (is.na(activeFName)==TRUE){
         gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
         return()
     }
     FNameList <- XPSFNameList()     #list of all loaded XPSSamples in .GlobalEnv


#--- GUI
     DPwin <- gwindow(" XPS DEPTH PROFILE ", parent(50, 10), visible=FALSE)
     maingroup <- ggroup(horizontal=TRUE, container=DPwin)

     T1group1 <- ggroup(spacing=2, horizontal=FALSE, container=maingroup)
     T1frameFName <- gframe(text="Select the XPS-SAMPLES TO ANALYZE", horizontal=FALSE, spacing=2, container=T1group1)
     SourceFLyt <- glayout(spacing=2, container=T1frameFName)
     LL <- length(FNameList)
     NCol <- ceiling(LL/5)   #gcheckboxgroup will be split in solumns of 5 elements
     for(jj in 1:NCol){      #build the checkboxes for XPSSample selection
         NN <- (jj-1)*5
         FList <- FNameList[(NN+1):(NN+5)]
         if (NN+5 > LL) FList <- FNameList[(NN+1):LL]
         SourceFLyt[1, jj] <- gcheckboxgroup(FList, selected=-1, spacing=2, horizontal=FALSE, handler=function(h,...){
                                for(jj in 1:NCol){
                                    SelectedXPSSamp[[jj]] <<- svalue(SourceFLyt[1, jj])
                                }
                             }, container=SourceFLyt)
     }

     T1frameDPType <- gframe(text="ARXPS or Sputter Depth Profile?", horizontal=TRUE, spacing=2, container=T1group1)
     DepthPrflCK <- gcheckboxgroup(c("ARXPS", "Sputt. Dpth-Prf."),  spacing=2, selected=-1, horizontal=TRUE, handler = function(h, ...){
                                DPrflType <<- svalue(DepthPrflCK)
                                SelectedXPSSamp <<- unlist(SelectedXPSSamp)
                                if (length(SelectedXPSSamp) == 0){
                                    svalue(DepthPrflCK, index=TRUE) <- c(0,0)
                                    gmessage("Select the XPSSample to analyze first", title="WARNING", icon="warning")
                                    return()
                                }
                                if (DPrflType == "ARXPS"){
                                    winDP <- gwindow("ARXPS TILT ANGLES", visible=FALSE)
                                    groupDP1 <- ggroup(horizontal=FALSE, container=winDP) 
                                    txt1 <- glabel("How many Take-off Angles used in ARXPS?", container=groupDP1)
                                    font(txt1) <- list(family="sans",size=11)
                                    groupDP2 <- ggroup(horizontal=TRUE, container=groupDP1)
                                    glabel("Number of Take-off Angles: ", container=groupDP2)
                                    NTkOff <- gedit(initial.msg="N. Take-off", width=10, container=groupDP2)
                                    tkconfigure(NTkOff$widget, width=10)
                                    addHandlerChanged(NTkOff, handler=function(h, ...){
                                                  blockHandlers(NTkOff, ...)
                                                  Nang <- svalue(NTkOff)
                                                  if (Nang == ""){
                                                      svalue(DepthPrflCK, index=TRUE) <- c(0,0)
                                                      gmessage("Set the Number of Take-off Angles first!", title="WARNING", icon="warning")
                                                      return()
                                                  }
                                                  Nang <- as.integer(Nang)
                                                  msg <- "For each of the selected XPSSample set the take-off angle"
                                                  txt2 <- glabel(text=msg, container=groupDP1)
                                                  font(txt2) <- list(family="sans",size=11)
                                                  gseparator(horizontal=TRUE, container=groupDP1)
                                                  LayoutAngles <- glayout(spacing=2, container=groupDP1)
                                                  for(ii in 1:Nang){
                                                      txt <- paste("Take-off angle ", ii, sep="")
                                                      LayoutAngles[ii,1] <- glabel(txt, container=LayoutAngles)
                                                      LayoutAngles[ii,2] <- gedit(initial.msg = "Take-off angle", container=LayoutAngles)
                                                  }
                                                  gbutton("    OK     ", handler=function(h, ...){
                                                         for(ii in 1:Nang){
                                                             TkoffAngles[ii] <<- svalue(LayoutAngles[ii,2])
                                                         }
                                                         dispose(winDP)
                                                  }, container=groupDP1)
                                              })
                                    visible(winDP) <- TRUE
                                    #winDP$set_modal(TRUE)  #nothing can be done while running this macro
                                }

                                SelectedXPSSamp <<- unlist(SelectedXPSSamp)
                                N.XS <<- length(SelectedXPSSamp)
                                if (N.XS == 1) {
                                   XPSSample <<- get(SelectedXPSSamp[1], envir=.GlobalEnv)   #load the active XPSSample in memory
                                   assign("activeFName", SelectedXPSSamp[1], envir=.GlobalEnv)
                                } else if (N.XS > 1){  #more XPSSamp are loaded as for example in ARXPS: one XPSSample for each tilt angle
                                   Filename <- NULL
                                   XPSSample <<- new("XPSSample")
                                   for(ii in 1:N.XS){
                                       tmp <- get(SelectedXPSSamp[ii], envir=.GlobalEnv)
                                       XPSSample <<- c(XPSSample, tmp)
                                       Filename <- paste(Filename, tmp@Filename,",", sep="")
                                   }
                                   XPSSample@Project <<- ""
                                   XPSSample@Sample <<- dirname(tmp@Sample)
                                   XPSSample@Comments <<- paste(DPrflType, Filename, spe=" ")
                                   XPSSample@User <<- ""
#                                   XPSSample@names <- ""  #this is already set by c(X, tmp)
                                   if (DPrflType == "ARXPS"){  #assign the filename to save the analysis in a unique datafile
                                       XPSSample@Filename <<- "ARXPS.RData"
                                       assign("activeFName", "ARXPS.RData", envir=.GlobalEnv)
                                   } else if (DPrflType == "Sputt. Dpth-Prf."){
                                       XPSSample@Filename <<- "SputtDP.RData"
                                       assign("activeFName", "SputtDP.RData", envir=.GlobalEnv)
                                   }
                                }
                                plot(XPSSample)
                                CK.Elmts()
                                MK.Panel()
                                enabled(BaseLineCK1) <- TRUE
                                enabled(BaseLineCK2) <- TRUE
                                enabled(SelectButt) <- TRUE
                                enabled(ConcButt) <- TRUE

                             }, container=T1frameDPType)


     T1frameCoreLines <- gframe(text="Select the CORE LINES to analyze", horizontal=TRUE, spacing=2, container=T1group1)
     CoreLineCK <- glabel("     ", container=T1frameCoreLines) #just to insert some space in T1frameCoreLines

     T1frameBaseline <- gframe(text="Select the BASELINE to apply", horizontal=FALSE,  spacing=2, container=T1group1)
     BaseLineCK1 <- gradio(c("Linear", "Shirley", "2P.Shirley"),  spacing=2, selected=-1, horizontal=TRUE, handler = function(h, ...){
                                svalue(BaseLineCK2, index=TRUE) <- -1 #clear selection of gradio2
                                BaseLine <<- svalue(BaseLineCK1)
                             }, container=T1frameBaseline)
     BaseLineCK2 <- gradio(c("2P.Tougaard", "3P.Tougaard", "Spline"),  spacing=2, selected=-1, horizontal=TRUE, handler = function(h, ...){
                                svalue(BaseLineCK1, index=TRUE) <- -1 #clear selection of gradio1
                                BaseLine <<- svalue(BaseLineCK2)
                             }, container=T1frameBaseline)
     enabled(BaseLineCK1) <- FALSE
     enabled(BaseLineCK2) <- FALSE

     SelectButt <- gbutton(" SELECT/ADD BASELINE ", handler=function(h,...){
                                SelectedCL <<- svalue(CoreLineCK)
                                #--- Build data matrix for plotting
                                Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                N.CL <<- length(Matched)
                                for(ii in Matched){
                                    if (hasBaseline(XPSSample[[CL.Sym[ii]]])){
                                       gmessage("BaseLine already defined!", title="WARNING", icon="warning")
                                    } else {
                                       answ <- "FALSE"
                                       while(answ == "FALSE"){
                                          #---- Graphics: generate the data matrix (spectra only) for plotting
                                          X <- Y <- NULL
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[jj, ii]
                                              X <- cbind(X, XPSSample[[idx]]@.Data[[1]])
                                              Y <- cbind(Y, XPSSample[[idx]]@.Data[[2]])
                                          }
                                          xrange <- range(XPSSample[[idx]]@.Data[[1]])
                                          if (XPSSample[[idx]]@Flags[1]) xrange <- sort(xrange, decreasing=TRUE)
                                          matplot(X, Y, xlim=xrange, type="l", lty=1, col=MatCol[1:N.Cycles], main=CL.Sym[ii], cex.axis=1.25,
                                                 cex.lab=1.3, xlab=XPSSample[[idx]]@units[1], ylab=XPSSample[[idx]]@units[2])
                                          while(length(BaseLine)==0){
                                                txt <- paste(" Select BaseLine for Core-Line ", CL.Sym[ii],"\n Then press OK to proceed.", sep="")
                                                gmessage(msg=txt, title="SELECT BASELINE", icon="info")
                                          }



                                          #-----
                                          cat("\n ==> Applying ", BaseLine, "BaseLine for background subtraction")
                                          if (BaseLine == "Spline") {
                                              splinePoints <<- list(x=NULL, y=NULL)
                                              txt <- "Spline background \n ==> LEFT click to set spline points; RIGHT to exit"
                                              gmessage(msg=txt, title="HELP INFO", icon="info")
                                              pos <- c(1,1) # only to enter in  the loop
                                              while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                                                     pos <- locator(n=1, type="p", col="green3", cex=1.5, lwd=2, pch=16)
                                                     if (length(pos) > 0) {
                                                         splinePoints$x <<- c(splinePoints$x, pos$x)  # $x and $y must be separate to add new coord to splinePoints
                                                         splinePoints$y <<- c(splinePoints$y, pos$y)
                                                     }
                                              }
                                              decr <- FALSE #Kinetic energy set
                                              if (XPSSample[[idx]]@Flags[1] == TRUE) { decr <- TRUE }
                                              kk <- order(splinePoints$x, decreasing=decr)
                                              splinePoints$x <<- splinePoints$x[kk] #splinePoints$x in ascending order
                                              splinePoints$y <<- splinePoints$y[kk] #following same order select the correspondent splinePoints$y
                                              LL <- length(splinePoints$x)
                                              BL.Ends$x <- c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
                                              BL.Ends$y <- c(splinePoints$y[1],splinePoints$y[LL])
                                          } else {
                                              gmessage(msg="Please select the BaseLine end-Points", title="DEFINE END-POINTS", icon="info")
                                              BL.Ends <- locator(n=2, type="p", col=2, lwd=2, pch=16)
                                          }
                                          cat("\n ==> Perform Background subtraction")
                                          for(jj in 1:N.Cycles){    #given the BL.End$x limits finds the corres[pondent ordinates
                                              idx <- CoreLineList[jj, ii]
                                              XPSSample[[idx]]@Boundaries$x <<- BL.Ends$x
                                              kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], BL.Ends$x[1])
                                              XPSSample[[idx]]@Boundaries$y[1] <<- mean(XPSSample[[idx]]@.Data[[2]][(kk-2):(kk+2)])
                                              kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], BL.Ends$x[2])
                                              XPSSample[[idx]]@Boundaries$y[2] <<- mean(XPSSample[[idx]]@.Data[[2]][(kk-2):(kk+2)])
                                              if (BaseLine == "Spline") {   #given SplinePoints$X for the first spectrum find the SplinePoints$Y for the other spectra
                                                  Npti <- length(splinePoints$x)
                                                  for(ll in 1:Npti){
                                                      kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], splinePoints$x[ll])
                                                      splinePoints$y[ll] <<- mean(XPSSample[[idx]]@.Data[[2]][(kk-1):(kk+1)])
                                                  }
                                              }
                                              MK.Baseline(idx, splinePoints, ...)
                                          }
                                          #---- Graphics: generate the data matrix (spectra + BKGD) for plotting
                                          Colr <- NULL
                                          X <- Y <- NULL
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[jj, ii]
                                              X <- cbind(X, XPSSample[[idx]]@Baseline$x)
                                              Y <- cbind(Y, XPSSample[[idx]]@Baseline$y)
                                              Colr <- c(Colr, 584)  #Sienna color for the Baselines
                                          }
                                          Colr <- c(Colr, MatCol[1:N.Cycles])
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[jj, ii]
                                              X <- cbind(X, XPSSample[[idx]]@RegionToFit$x)
                                              Y <- cbind(Y, XPSSample[[idx]]@RegionToFit$y)
                                          }
                                          xrange <- range(XPSSample[[idx]]@RegionToFit$x)
                                          if (XPSSample[[idx]]@Flags[1]) xrange <- sort(xrange, decreasing=TRUE)
                                          matplot(X, Y, xlim=xrange, type="l", lty=1, col=Colr, main=CL.Sym[ii], cex.axis=1.25,
                                                  cex.lab=1.3, xlab=XPSSample[[idx]]@units[1], ylab=XPSSample[[idx]]@units[2])
                                          #-----
                                          svalue(BaseLineCK1, index=TRUE) <- -1 #clear selection of gradio1
                                          svalue(BaseLineCK2, index=TRUE) <- -1 #clear selection of gradio2
                                          answ <- gconfirm(msg="BaseLine OK?", title="BASELINE CTRL", icon="info")
                                          if(answ == "FALSE"){
                                             txt <- paste(" Change BaseLine for Core-Line ", CL.Sym[ii],"\n Then press OK to proceed.", sep="")
                                             gmessage(msg=txt, title="CHANGE BASELINE", icon="warning")
                                          }
                                       }
                                    }
                                    BaseLine <<- NULL #activate selection BaseLine for the next Core-Line
                                }
                            }, container=T1frameBaseline)
     enabled(SelectButt) <- FALSE

     T1frameProf <- gframe(text="Compute Concentration Profile",  horizontal=FALSE,  spacing=2, container=T1group1)
     ConcButt <- gbutton("CONCENTRATION PROFILING", handler=function(h,...){
#among the elements determine the max number of acquired CL. Likely same number of CL for all elements = N Cycles etching
                                QntMat <- matrix(data=NA, ncol=N.CL, nrow=N.Cycles)
                                SelectedCL <<- svalue(CoreLineCK)
                                if (length(SelectedCL) == 0){
                                    gmessage("Select Elements to Profile first!", title="WARNING", icon="warning")
                                    return()
                                }
                                for(jj in 1:N.Cycles){
                                    XSampTmp <- NULL
                                    XSampTmp <- new("XPSSample")  #generate a temporary XPSSample
                                    kk <- 0
                                    Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                    N.CL <<- length(Matched)
                                    for(ii in Matched){         #for now runs only on the selected CL
                                        idx <- CoreLineList[jj,ii]
                                        if(!is.na(CoreLineList[jj,ii])){
                                           kk <- kk+1
                                           XSampTmp[[kk]] <- XPSSample[[idx]] #load the acquired coreline atetch cycle ii
                                        }
                                    }
#cannot load directly quantification results into QntMat because acquisition of some coreline could be stopped
#this generates a CoreLineList where some elements (corelines) are lacking and NA is inserted see ((<<<===))
                                    tmp <- XPSquantify(XSampTmp, verbose=FALSE)   #compute the quantification
                                    TotQ <- sum(unlist(sapply(tmp, function(x) x$quant)))
                                    kk <- 0
                                    for(ii in Matched){
                                        if(!is.na(CoreLineList[jj, ii])){
                                            kk <- kk+1
                                            QntMat[jj,kk] <- tmp[[kk]]$quant/TotQ #load quantification data into the QntMat matrix
                                        }
                                    }
                                }

                                Lgnd <- NULL
                                for(ii in Matched){
                                    Lgnd <- c(Lgnd, CL.Sym[ii])
                                }
                                X <- seq(from=1, to=N.Cycles, by=1) #make the abscissa matrix
                                X <- rep(X, N.CL)
                                X <- matrix(X, nrow=N.Cycles, ncol=N.CL)
                                Xlab="Etch Cycles"
                                if(svalue(DepthPrflCK)=="ARXPS"){
                                   Xlab <- "Take-off Angle (deg.)"
                                   X <- as.numeric(unlist(strsplit(TkoffAngles, ",")))
                                   X <- rep(X, N.CL)
                                   X <- matrix(X, nrow=N.Cycles, ncol=N.CL)
                                }
                                xx <- min(X)
                                yy <- 1.2*max(QntMat)
                                SymIdx <- c(19,15,17,25,18,1,0,2,6,5,4,8,7,10,9,11,12,14,13,3)

                                matplot(X, QntMat, ylim=c(0,yy), type="b", lty=1, lw=2, pch=SymIdx, col=MatCol,
                                        cex=1.2, cex.axis=1.3, cex.lab=1.35, xlab=Xlab, ylab="Concentration (%)")
                                legend(x=xx, y=yy, xjust=0, legend=Lgnd, ncol=(N.CL+1), lty=1, pch=SymIdx,
                                        lw=2, bty="n", col=MatCol, border=MatCol, text.col=MatCol, text.font=2)
                                colnames(QntMat) <- CL.Sym[Matched]
                                QntMat <- 100*QntMat  #matrix of concentrations in percent

                                #--- NOW build textual table of concentrations
                                txt <- c("\n", paste(activeFName, " Depth Profile",sep=""), "\n")
                                txt <- c(txt, "\n", paste("Quantification: ", sep=""), "\n\n")
                                txt <- "           "  #space for N.Cycle or Take-off angle
                                for(ii in Matched){    #Column names
                                    txt <- c(txt, format(paste(CL.Sym[ii], "%", sep=""), digits=2, justify="right", width=6))
                                }
                                txt <- c(txt, "\n")
                                #separator
                                txt <- c(txt, paste(rep("--------", (N.CL+1)), collapse=""), "\n")
                                #now concentration table
                                for(jj in 1:N.Cycles){
                                    lbl <- paste("Etch Cycle ", jj, sep="")
                                    if(svalue(DepthPrflCK)=="ARXPS"){
                                       lbl <- unlist(strsplit(TkoffAngles, ","))[jj]
                                    lbl <- paste("Take-Off ", lbl, sep="")
                                    }
                                    txt <- c(txt, lbl)
                                    for(ii in 1:N.CL){
                                        txt <- c(txt, format(QntMat[jj,ii], digits=3, justify="centre", width=6))
                                    }
                                    txt <- c(txt, "\n")
                                }
                                cat("\n", txt)
#                                svalue(Results) <- capture.output(cat("\n ", txt))
                                insert(Results, paste(txt, collapse=""))

                            }, container=T1frameProf)
     enabled(ConcButt) <- FALSE
     gseparator(horizontal=TRUE, container=T1group1)

     Results <- gtext(" ",  wrap=FALSE, container=maingroup)
     size(Results) <- c(300, 300)

     gbutton(" RESET ANALYSIS ", handler=function(h,...){
                                LL <- length(FNameList)
                                NCol <- ceiling(LL/5)   #gcheckboxgroup will be split in solumns of 5 elements
                                for(jj in 1:NCol){      #build the checkboxes for XPSSample selection
                                    svalue(SourceFLyt[1, jj]) <- -1
                                }
                                svalue(DepthPrflCK) <- -1
                                delete(T1frameCoreLines, CoreLineCK)
                                CoreLineCK <<- glabel("     ", container=T1frameCoreLines) #just to insert some space in T1frameCoreLines
                                delete(maingroup, Results)  #erase gtext content
                                Results <<- gtext(" ", wrap=FALSE, container=maingroup)
                                size(Results) <- c(300, 300)
                                enabled(BaseLineCK1) <- FALSE
                                enabled(BaseLineCK2) <- FALSE
                                enabled(SelectButt) <- FALSE
                                enabled(ConcButt) <- FALSE
                                ResetVars()
                            }, container=T1group1)

     gbutton(" SAVE & EXIT ", handler=function(h,...){
     	                          dispose(DPwin)

                                for(jj in 1:N.Cycles){
                                    for(ii in Matched){
                                        if(svalue(DepthPrflCK)=="ARXPS"){
                                           lbl <- unlist(strsplit(TkoffAngles, ","))[jj]
                                           Info <- paste("   ::: ARXPS Take-off Angle (deg.): ", lbl, sep="")
                                        }
                                        if(svalue(DepthPrflCK)=="Sputt. Dpth-Prf."){
                                           Info <- paste("   ::: Sputter Depth-Profile Cycle N.", jj, sep="")
                                        }
                                        idx <- CoreLineList[jj, ii]
                                        XPSSample[[idx]]@Info <<- Info
                                    }
                                }
                                assign(activeFName, XPSSample, envir = .GlobalEnv)
                                XPSSaveRetrieveBkp("save")
                                options(warn = 0)
                            }, container=T1group1)


     gbutton(" EXIT ", handler=function(h,...){
     	                          dispose(DPwin)
                                assign("activeFName", SelectedXPSSamp[1], envir = .GlobalEnv)
                                XPSSaveRetrieveBkp("save")
                                options(warn = 0)
                            }, container=T1group1)

     visible(DPwin) <- TRUE
     DPwin$set_modal(TRUE)

}



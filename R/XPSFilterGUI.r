#function to remove noise from XPS-Sample spectra

#' @title XPSFilter smoothing functions to remove noise from spectra.
#' @description XPSFilter contains a list of different filters to remove noise from spectra.
#'  - In Sawitzy Golay filter at point i is a weighted average of data i-n... i ... i+n
#'  - In Autoregressive filters the output variable depends linearly on its own previous values.
#'  - In the Moving Average filters the output at point i is the average of data i-n ... i ... i+n .
#'  - The FFT filter applys the FFT transform to perform filtering.
#'  - The Wavelets fileter uses the wavelets to perform filtering.
#'  - The FIRfilter is a Finite Impulse Response with a zero distortion.
#'  - The Butterworth is an Infinite Impulse Response filter.
#' @examples
#' \dontrun{
#'	  XPSFilter()
#' }
#' @export
#'


XPSFilter <- function() {
   import::from(signal, filter, freqz, sgolay, hamming, fir1, filtfilt, butter)

   PlotData <- function() {
      XXX <- cbind(unlist(Object@.Data[1]))
      YYY <- cbind(unlist(Object@.Data[2]),Filtered)
      if (BkgSubtr) {
         YYY <- cbind(YYY, BackGnd)
      }
      Xlim <- range(XXX)
      if (Object@Flags[1]==TRUE) {
         Xlim <- rev(Xlim)  ## reverse x-axis
      }
      Ylim <- range(YYY)
      matplot(x=XXX, y=YYY, type="l", lty=c("solid", "solid", "solid"), lw=c(1,1.75), col=c("black", "red", "green"),
              xlim=Xlim, ylim=Ylim, xlab=Object@units[1], ylab=Object@units[2])
      return()
   }



   BkgSubtraction <- function(data){
      BackGnd <<- NULL
      LL <- length(data)
      rng <- 5
      bkg1 <- mean(data[1:5])
      bkg2 <- mean(data[(LL-5):LL])
      stp <- (bkg1-bkg2)/LL
      Dta_Bkg <- NULL
      for (ii in 1:LL){
          Dta_Bkg[ii] <- data[ii]-(bkg1-ii*stp)
          BackGnd[ii] <<- bkg1-ii*stp
      }
      return(Dta_Bkg)
   }

   alignSpect <- function(){            #levels the filtered spectrum to the original one
       LL <- length(Object@.Data[[2]])  #modifying the bacground intensity
       dH <- Object@.Data[[2]][1] - Filtered[1]
       Filtered <<- Filtered + dH    #the filtered spectrum is overlapped to the original data
       Err <- 10    #initialize the error
       OldErr <- sum((Object@.Data[[2]][1:10]-Filtered[1:10])^2) +    #sum of square differences
                 sum((Object@.Data[[2]][(LL-10):LL]-Filtered[(LL-10):LL])^2)
       dH <- dH/10
       Filtered <<- Filtered+dH
       Err <- sum((Object@.Data[[2]][1:10]-Filtered[1:10])^2) +
              sum((Object@.Data[[2]][(LL-10):LL]-Filtered[(LL-10):LL])^2)
       dErr <- abs(OldErr-Err)
       while(dErr > 0.0001){
             Filtered <<- Filtered+dH
             Err <- sum((Object@.Data[[2]][1:10]-Filtered[1:10])^2) +
                    sum((Object@.Data[[2]][(LL-10):LL]-Filtered[(LL-10):LL])^2)
             if (Err > OldErr) dH <- -dH/2
             if (abs(dH) < 10^-4) {
                 break
             }
             dErr <- abs(OldErr-Err)
             OldErr <- Err
       }
   }

   normalize <- function(data){
      FiltLgth <- 2*7+1
      coeff <- sgolay(p=1, n=FiltLgth, m=0, ts=1) #7 coefficient for Sawitzky-Golay filtering
      data <- unlist(data)
      LL <- length(data)
      #compute averaged noise free value around the data minimum
      data <- c(rep(data[1],FiltLgth), data, rep(data[LL],FiltLgth)) #FiltLgth Padding at edges
      FFF <- filter(filt=coeff, x=data) #Savitzky-Golay filtering
      data <- data[(FiltLgth+1):(LL+FiltLgth)]
      Min <- min(FFF)
      Max <- max(FFF)
      if (SaveAmpli) SigAmpli <<- c(Min, Max)
      data <- (data-Min)/(Max-Min)
      return(data)
   }

   resize <- function(data){
      LL <- length(data)
      NN <- 20
      while(NN > LL/2){           #if there are just few original Ndata reduce the extension of
         NN <- ceiling(NN/2)      #range to evaluate average at data edges
      }
      SaveAmpli <<- FALSE         #do not preserve ampliture range of original data
      data <- normalize(data)
      data <- data*(max(Object@.Data[[2]]) - min(Object@.Data[[2]]))+ min(Object@.Data[[2]])
      FF1 <- data[1:NN]
      FF2 <- data[(LL-NN):LL]
      avg  <- mean(data)/2
      #if edges of spectra are highly unleveled skip last refinement
      if(mean(FF1) < avg && mean(FF2) < avg){
         YY1 <- unlist(Object@.Data[[2]][1:NN])
         YY2 <- unlist(Object@.Data[[2]][(LL-NN):LL])

         if (BkgSubtr) {
             YY1 <- unlist((Object@.Data[[2]]-BackGnd)[1:NN])
             YY2 <- unlist((Object@.Data[[2]]-BackGnd)[(LL-NN):LL])
         }
         treshold <- sum((YY1-mean(YY1))^2)/NN
         Scarto <- sum(YY1-FF1)^2 + sum(YY2-FF2)^2
         SS <- Scarto+1  #just to enter in the while loop
         dCC <- -0.05
         CC <- 1

         while (SS > treshold){
            CC <- CC+dCC
            oldSS <- SS
            FF1 <- data[1:NN]*CC
            FF2 <- data[(LL-NN):LL]*CC
            SS <- sum(YY1-FF1)^2 + sum(YY2-FF2)^2
            if (abs(SS-oldSS) < 10^-4) {
                break
            }
            if (SS > oldSS) dCC <- -dCC/2
         }
         data <- data*CC
      }
      return(data)
   }


   SetAmplitude <- function(){
#amplitude filtered data does not correspond to that of original data: matching is needed
      LL <- length(Object@.Data[[2]])
      BkgSubtraction(Object@.Data[[2]])  #To Define just the background
      coeff <- sgolay(p=1, n=(2*7+1), ts=1) #7 coefficient for Sawitzky-Golay filtering
#Padding data at edges: adds a number of data corresponding to the N. coeff Sawitzki-Golay filter.
#to avoid negative indexing when min(data( corresponds to 1 or LL
      BackGndO <- BackGnd
      XXX <- Object@.Data[[2]]-BackGndO
      pad <- rep(mean(XXX[1:5], 7))
      XXX <- c(pad, XXX)
      pad <- rep(mean(XXX[(LL-5):LL], 7))
      XXX <- c(XXX, pad)

      Min <- min(XXX)      #minimum of BKG subtracted original data Object@.Data[[2]]
      indx <- which(XXX == Min)
      rng <- 10
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      FFF <- filter(filt=coeff, x=XXX[(indx-rng):(indx+rng)]) #Savitzky-Golay filtering  BKG subtr original data
      Min <- min(FFF)

      Max <- max(XXX)      #maximum of BKG subtracted original data Object@.Data[[2]]
      indx <- which(XXX == Max)
      rng <- 10
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      FFF <- filter(filt=coeff, x=XXX[(indx-rng):(indx+rng)]) #Savitzky-Golay filtering
      Max <- max(FFF)
      AmpliFact <- Max-Min              #amplification factor

#Do the same for filtered data
      XXX <- BkgSubtraction(Filtered)  #To Define just the background
      BackGndF <- BackGnd
      minF <- min(XXX)      #minimum of BKG subtracted filtered data
      indx <- which(XXX == minF)
      rng <- 5
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      minF <- mean(XXX[(indx-rng):(indx+rng)])  #average of BKG subtracted original data around minimum value

      maxF <- max(XXX)      #maximum of BKG subtracted original data
      indx <- which(XXX == maxF)
      rng <- 5
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      maxF <- mean(XXX[(indx-rng):(indx+rng)])  #average of BKG subtracted original data around maximum value
      DampFact <-  maxF-minF                      #damping factor

#Filtered data may contain noise: is needed a mean value around the maximum
      if (! BkgSubtr){
         Dta_Amp <- XXX*AmpliFact/DampFact+BackGndO #match amplitude of filtered data on BKG-UNSUBTRACTED original data
      } else {
         Dta_Amp <- XXX*AmpliFact/DampFact #match amplitude of filtered data on UNSUBTRACTED original data
      }

      return(Dta_Amp)
   }

   SavGolay <- function() {
      FiltInfo <<- paste("Savitzky Golay, Degree: ", svalue(F2FiltDeg), sep="")
      BkgSubtr <<- svalue(BkgSubtr2)
      FiltLgth <- as.numeric(svalue(F2FiltDeg))
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      FiltLgth <- 2*FiltLgth+1
      RawData <<- c(rep(RawData[1],FiltLgth), RawData, rep(RawData[LL],FiltLgth)) #FiltLgth Padding at edges
      Filtered <<- NULL
      coeff <<- NULL
      coeff <<- sgolay(p=1, n=FiltLgth, m=0, ts=1)
      #Be careful: sgolay provides a matrix of coeff. and you can get the smoothed value y[k] as y[k] = F[i,] * x[(k-i+1):(k+N-i)]
      #y[k] does not depend on i. The filtering must be symmetric with respect to the central value k
      #SG acts as a weighted sum of values of n data preceding the k point and n following data. In tital 2n+1 values = filter length
      #For filter length N = 2n+1 = 5, the matrix of coeff. F is organized as
      #      |   A   c1       |
      #      |       c1       |
      #  F = | c1 c1 c1 c1, c1|
      #      |       c1       |
      #      |       c1   At  |
      #is divided in four quadrants by the central column and row which are the coeff of the median value k
      #At is the transpose of A Then for example we will have
      #F[1,1] = F[2n+1,2n+1]; 
      #F[1,2] = F[2n,2n+1];
      #F[1,3] = c1 = F[2n-1,2n+1];
      #F[1,4] = F[2n-2,2n+1]
      #F[1,5] = F[2n-3,2n+1] = 1st element of column 2n+1
      #This holds for all the columns.
      #I suppose y=filter(F, x=Data) uses the whole matrix with: y[k+i-1] = = F[i,] * x[(k-i+1):(k+N-i)]
      #which allow obtaining 2n+1 values by using the whole F matrix.
      #Same result is obtained using y=filter(F[1, ], a=1, x=Data)  where a=1 eliminates the autoregressive iteration requiring the whole F matrix

      if (BkgSubtr) {
         RawData <<- BkgSubtraction(RawData)
      }
      Filtered <<- filter(filt=coeff, x=RawData) #Savitzky-Golay filtering
      Filtered <<- Filtered[(FiltLgth+1) : (FiltLgth+LL)]
      coeff <<- coeff[1, ] #select just the 1st row of SG coeff.
      PlotData()
      return()
   }                                 


   AutoRegMedMob <- function() {
      FiltInfo <<- paste(svalue(F3FiltType), ", Degree: ", svalue(F3FiltDeg), sep="")
      BkgSubtr <<- svalue(BkgSubtr3)
      FilterType <- svalue(F3FiltType)
      FiltLgth <- as.numeric(svalue(F3FiltDeg))    #Filter length
      coeff <<- NULL
      Filtered <<- NULL
      BackGnd <<- NULL
      #preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      if (BkgSubtr) { RawData <<- BkgSubtraction(RawData) }
      #now filtering
      if (FilterType=="AutoRegressive"){
#--- Autoregressive filter
         SaveAmpli <<- TRUE
         RawData <<- c(rep(RawData[1],2*FiltLgth), RawData, rep(RawData[LL],2*FiltLgth)) #FiltLgth Padding at edges
         Ndta <- length(RawData)
         Filtered <<- array(data=0, dim=Ndta)
         #define the AR coefficents
         if(FiltLgth == 1){
            coeff <<- 1
         } else if(FiltLgth == 2){
            coeff <<- c(1, 0.25)
         } else if(FiltLgth > 2){
            P <- FiltLgth-1
            for(ii in P:0){
                coeff[(P-ii+1)] <<- 1/P*sinpi(0.5*ii/P)
            }
         }
         #now apply the AR filter
         for(ii in (FiltLgth+1):Ndta){
             sum <- 0
             for(jj in 2:FiltLgth){
                 sum <- sum + coeff[jj]*Filtered[(ii-jj+1)]
             }
             Filtered[ii] <<- sum + coeff[1]*RawData[ii]
         }
         Filtered <<- Filtered[(2*FiltLgth+1) : (LL+2*FiltLgth)]
         #now set correct amplitude of filtered data
         Filtered <<- resize(Filtered)   #match the amplitude of Filtered with the original data
      } else if (FilterType=="MovingAverage") {
#--- Moving average filter
         coeff <<- rep(1/(2*FiltLgth+1),(2*FiltLgth+1))  #1/FiltLgth because of the average
         RawData <<- c(rep(RawData[1],FiltLgth), RawData, rep(RawData[LL],FiltLgth)) #FiltLgth Padding at edges
         for(ii in 1:LL){
             Filtered[ii] <<- mean(RawData[ii:(ii+2*FiltLgth)]) #Forward filtering
         }
      }
      if (BkgSubtr) { Filtered <<- Filtered + BackGnd }
#recover AutoRegressive filter distortions on signal amplitude
      if (svalue(MatchEdges3)){
         Filtered <<- BkgSubtraction(Filtered)
         Filtered <<- normalize(Filtered)

         Filtered <<- Filtered-0.5*(mean(Filtered[1:5]) + mean(Filtered[(LL-5):LL])) #shift the Filtered edges at zero
         BackGnd <- BkgSubtraction(Object@.Data[[2]])
         SaveAmpli <<- TRUE
         normalize(Object@.Data[[2]]-BackGnd)
         A1 <- SigAmpli[2]-SigAmpli[1]    #with SaveAmpli=TRUE normalize() returns also SigAmpli=c(min, max)
         Filtered <<- Filtered*A1         #Adapt filtered signal on the bckgnd subtracted original signal
         Filtered <<- Filtered + BackGnd  #add the background of the original data
      }
      PlotData()
      return()

   }
   
   medianFilt <- function(){
      BkgSubtr <<- svalue(BkgSubtr3)
      FilterType <- svalue(F3FiltType)
      FO <- as.numeric(svalue(F3FiltDeg))
      coeff <<- NULL
      Filtered <<- NULL
      BackGnd <<- NULL
      #preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      if (BkgSubtr) { RawData <<- BkgSubtraction(RawData) }
      SaveAmpli <<- TRUE
      RawData <<- normalize(RawData)
      #now filtering
      tmp1 <- NULL
      tmp2 <- NULL
      FFO <- FO/(15+1)  #input degree of moving average filter [0.1 : 0.99]
      coeff <<- c(FFO, 1-FFO)
      LL <- length(RawData)
      tmp1[1] <- RawData[1]
      tmp2[LL] <- RawData[LL]
      for(ii in 2:LL){
          tmp1[ii]=FFO*tmp1[ii-1] + (1-FFO)*RawData[ii];              #forward filtering
          tmp2[LL-ii+1]=FFO*tmp2[LL-ii+2]+(1-FFO)*RawData[LL-ii+1];   #backward filtering
      }
      Filtered <<- (tmp1+tmp2)/2
   }
   


   FFTfilt <- function() {

      HannWin <- function(strt, stp){
          W <- stp-strt  #total lenght of the window
          Hann1 <- NULL
          Hann2 <- NULL
          Hann <- NULL
          LL <- floor((stp-strt)/20) #length of the descending/ascending hanning branches
          for (ii in 1:LL){
              Hann1[ii] <- -(0.5 - 0.5*cos(2*pi*ii/(LL-1)) ) #descending hanning window branch
              Hann2[ii] <- 0.5 - 0.5*cos(2*pi*(5+ii)/(LL-1)) #ascending hanning window branch
          }
          #Hann = 1, 1, 1, 1, 1, 0.8 0.5, 0.3, 0,1, 0, 0, 0, 0.1, 0.3, 0.5, 0.8, 1, 1, 1, 1, 1
          Hann <- c(rep(1, strt), Hann1, rep(0, (W-2*LL)), Hann2, rep(1, strt))
          return(Hann)
      }

      FiltInfo <<- paste("FFT filter, Degree: ", svalue(F4FiltDeg), sep="")
      BkgSubtr <<- svalue(BkgSubtr4)
      FiltLgth <- as.numeric(svalue(F4FiltDeg))
      RawData <<- NULL
      Filtered <<- NULL
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      if (floor(LL/2) < ceiling(LL/2)){ #if LL is ODD
          RawData[LL+1] <<- RawData[LL]   #then RawData has a LL+1 even number of data
      }
      #preprocessing
      if (BkgSubtr) RawData <- BkgSubtraction(RawData) #background subtraction to avoid FFT spourious oscillations
      #now filtering
      NPad <- 20 #for FFT filter zero padding must be independent on the filrer order
      if( 20 > LL/2) NPad <- floor(LL/2)
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #NPad Padding at edges
      LL_DF <- length(RawData)     #LL_DF is even

      stopF <- 0.5* FiltLgth/15   #for rejection noise=1 the cutoff freq=0.5=Nyquist Freq., for rejection=15 cutoff freq=1/15*0.5 = 0.0333
      dF <- 0.5/(LL_DF*0.5)               #FFT is composed by LL_DF/2 points; from 1+LL_DF/2 to LL_DF the FFT is specular to the first half
      freq <- seq(from=0, to=0.5, by=dF)  #frequency axis from 0 to 0.5 step dF same length of RawData
      idx <- length(which(freq <= stopF)) #idx is the array index corresponding to frequency <= stopF
      fftTransf <- fft(RawData)/LL_DF  #FFT must be devided by its length as indicated in the FFT R-documentation
#      Hann <- HannWin(idx, (LL_DF-idx))
      fftRej <- fftTransf[1:idx] #also fftRej has an even number of data
      LLr <- length(fftRej)/2
      fftTransf[(idx+1):(LL_DF-idx)] <- 0+0i   #low filtering: force to zero frequencies > stopF in the first and second  half of the FFT
#     we want to characterize the rejected part of the signal corresponding to the
#     FFT coeff of the rejected frequencies
      coeff <<- Mod(fftRej[1:LLr]) #consider only the first half the second half is specular
      coeff <<- coeff/sum(coeff)
      Filtered <<- Re(fft(fftTransf, inverse = TRUE))
      Filtered <<- Filtered[(NPad+1) : (NPad+LL)]
      PlotData()
      return()
   }

   msSmoothMRA <- function(){
      FiltInfo <<- paste("Wavelets filter, N.wavelets: ", svalue(F5WTnum), " Degree: ", svalue(F5WTLevel), sep="")
      import::from(wavelets, mra, dwt) #cannot use import::here because rootSolve not listed in DESCRIPTION
#      mra <- wavelets::mra
#      dwt <- wavelets::dwt
      RawData <<- NULL
      Filtered <<- NULL
      coeff <<- NULL
      WTnum <- 2*as.numeric(svalue(F5WTnum))   #This defines the N. Daubechies wavelets of the transform
      NPad <- 4*as.numeric(svalue(F5WTnum))      #This defines the N.ZeroPadding data
      RejLev <- as.numeric(svalue(F5WTLevel))  #this defines the level of noise rejection
#     preprocessing
      RawData <<- (Object@.Data[[2]])
      if (BkgSubtr) RawData <<- BkgSubtraction(RawData)
      FiltAmpli <<- TRUE
#      RawData <<- normalize(RawData)
      LL <- length(RawData)
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #NPad Padding at edges
      #now filtering
      WTfilt <- paste("d", WTnum, sep="")  #d2 means Daubechies length=2,  d8 means Daubechies length=8
#      maximum level according to the length(x) and selected filter degree WTnum
#      maxLevels <- floor( log2((LL-1)/(WTnum-1) + 1 )  #following wavelets::mra suggestions
      maxLevels <- 10 #fixed n.levels of the decomposition. This allows increasing the RejLev (filter force) up to 10!

      if (RejLev > maxLevels){
         gmessage("Attention: Nlevel must be < 1/2 * Filter Order", title="BAD DEGREE OF NOISE REJECTION", icon="error")
      }
      Response <- mra(RawData, filter = WTfilt, n.levels = maxLevels,
                      method = "modwt", boundary="periodic", fast=TRUE)
      Response <- sapply(c(Response@D[RejLev], Response@S[RejLev]), cbind)
      Filtered <<- rowSums(Response)
      Filtered <<- Filtered[(NPad+1):(LL+NPad)] #eliminates head and tail padded values

      LL <- length(RawData)
      while(log2(LL) < 10){  #increase the length of RawData till to allow dwt - Nlevels=10
          RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #NPad Padding at edges
          LL <- length(RawData)
      }
      Response <- dwt(RawData, filter = WTfilt, n.levels = maxLevels, boundary="periodic", fast=TRUE)
      ff <- 10-RejLev+1
      coeff <<- Response@V[[ff]]/sum(Response@V[[ff]]) #the higher the level selected for Response@V the higher the detail degree
      PlotData()
      return()
   }

   FirFilt <- function() {
      FiltInfo <<- paste("FIR filter, ", svalue(F6FiltOrder), ", Degree: ", CutOFF, sep="")
      coeff <<- NULL
      RawData <<- NULL
      Filtered <<- NULL
      BkgSubtr <<- svalue(BkgSubtr6)
      FiltLgth <- as.numeric(svalue(F6FiltOrder))
      NPad <- 2*FiltLgth
      if (CutOFF == 0 || CutOFF > 20){
         gmessage(msg="WARNING: Cut-OFF frequency must be in the range 1 - 20", title="WRONG CUT-OFF F.", icon="warning")
         return()
      }
      CutOFF <<- (20.001-CutOFF)/20    #here cut-off were freq/Nyquist
      BackGnd <<- NULL
      #preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
#function filtfilt zeropadds the ends of the array introducing distortions if the signal
#intensity is different form zero leading to a discontinuity. FIR filter needs background subtraction
      if (BkgSubtr) RawData <<- BkgSubtraction(RawData)
      #now filtering
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #N,coeff = 2*FiltLgth = NPad padding at edges
      coeff <<- fir1(n=FiltLgth, w=CutOFF, type = "low", window = hamming(FiltLgth+1), scale = TRUE)
      Filtered <<- filtfilt(filt=coeff, x=RawData)
      Filtered <<- Filtered[(NPad+1):(NPad+LL)]          #eliminates initial and end padded values
      if (BkgSubtr) Filtered <<- Filtered + BackGnd  #after filtering add background

#recover filter distoritions on signal amplitude
      Filtered <<- BkgSubtraction(Filtered)
      Filtered <<- normalize(Filtered)
      Filtered <<- Filtered-0.5*(mean(Filtered[1:5]) + mean(Filtered[(LL-5):LL])) #shift the Filtered edges at zero
      tmp <- BkgSubtraction(Object@.Data[[2]])
      SaveAmpli <<- TRUE
      tmp <- normalize(tmp)
      A1 <- SigAmpli[2]-SigAmpli[1]
      Filtered <<- Filtered*A1         #Adapt filtered signal on the bckgnd subtracted original signal
      Filtered <<- Filtered + BackGnd  #add the background of the original data
      PlotData()
      return()
   }

   ButtFilt <- function() {
      FiltInfo <<- paste("Butterworth filter, ", svalue(F7FiltOrder), ", Degree: ", svalue(F7CutOFF), sep="")
      coeff <<- NULL
      RawData <<- NULL
      Filtered <<- NULL
      BkgSubtr <<- svalue(BkgSubtr7)
      FiltLgth <- as.numeric(svalue(F7FiltOrder))
      NPad <- FiltLgth*CutOFF
      if (NPad < 30) NPad <- 30      #minimum number of padding elements
      CutOFF <<- (20.5-CutOFF)/20    #here cut-off were freq/Nyquist for example 40/500Hz (sampling freq = 1kHz) where 40Hz is adapted to the typical XPS noise
      #preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      if (BkgSubtr) RawData <<- BkgSubtraction(RawData)
      LL <- length(RawData)
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #WTdeg Padding at edges

      #now filtering
      coeff <<- butter(n=FiltLgth, W=CutOFF, type = "low", window = hamming(FiltLgth+1), scale = TRUE)
      Filtered <<- filtfilt(filt=coeff, x=RawData)
      Filtered <<- Filtered[(NPad+1):(NPad+LL)]    #eliminates initial and end padded values
      #now set correct amplitude of filtered data
      PlotData()
      return()
   }

   makeCombo <- function(){
      ActiveFName <<- svalue(F1XpsSpect)
      FName <<- get(ActiveFName,envir=.GlobalEnv)  #carico in SampID il relativo XPSSAmple
      SpectList <<- XPSSpectList(ActiveFName)
      SpectIndx <<- 1
      delete(F1frame2, F1CoreLine)
      F1CoreLine <<- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                        XPSComponent <- svalue(F1CoreLine)
                        XPSComponent <- unlist(strsplit(XPSComponent, "\\."))   #skip the ".NUMBER" at beginning CoreLine name
                        SpectIndx <- as.integer(XPSComponent[1])
                        SpectName <- XPSComponent[2]
                        assign("activeFName", ActiveFName,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
                        assign("activeSpectName", SpectName,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
                        assign("activeSpectIndx", SpectIndx,.GlobalEnv) #set the activeSpectIndex to the actual CoreLine
                        Object <<- FName[[SpectIndx]]
                        plot(FName[[SpectIndx]])
                        enabled(F2group1) <- TRUE
                        enabled(F3group1) <- TRUE
                        enabled(F4group1) <- TRUE
                        enabled(F5group1) <- TRUE
                        enabled(F6group1) <- TRUE
                        enabled(F7group1) <- TRUE
                        enabled(F2filter) <- FALSE
                        enabled(F3filter) <- FALSE
                        enabled(F4filter) <- FALSE
                        enabled(F5filter) <- FALSE
                        enabled(F6filter) <- FALSE
                        enabled(F7filter) <- FALSE
                     }, container=F1frame2)
      plot(FName)
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


#======== Variabili ========
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   ActiveFName <- get("activeFName", envir=.GlobalEnv)  #cload the XPSSample name (string)
   FName <- get(ActiveFName, envir=.GlobalEnv)   #load the active XPSSample (data)
   FNameList <- XPSFNameList()                   #list of all XPSSamples loaded in .GlobalEnv
   FNameIdx <- grep(ActiveFName,FNameList)
   SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)#load the index of the active CoreLine
   Object <- FName[[SpectIndx]]
   SpectList <- XPSSpectList(ActiveFName)

   RawData <- NULL
   Filtered <- NULL
   coeff <- NULL
   CutOFF <- NULL
   FiltResp <- NULL
   BackGnd <- NULL
   BkgSubtr <- FALSE
   SigAmpli <- NULL
   SaveAmpli <- FALSE
   filterType <- c("AutoRegressive", "MovingAverage")
   waveletType <- list("Daubechies", "LeastAsymmetric", "BestLocalized", "Coiflet")
   waveletType[["Daubechies"]] <- c(2,4,6,8,10,12,14,16,18,20)
   waveletType[["LeastAsymmetric"]] <- c(8,10,12,14,16,18,20)
   waveletType[["BestLocalized"]] <- c(14,18,20)
   waveletType[["Coiflet"]] <- c(6,12,18,24,30)
   waveNumber <- "  "
   FiltInfo <- NULL


   mainFwin <- gwindow("DATA FILTERING", parent=c(200, 5), visible=FALSE)
   mainGroup <- ggroup(spacing=1, horizontal=FALSE, container=mainFwin)

   mainFrame1 <- gframe(spacing=1, horizontal=FALSE, container=mainGroup)

#   F1frame1 <- gframe(label=" CORELINE SELECTION ", spacing=1, horizontal=FALSE, container=mainGroup)
   F1group2 <- ggroup(label="  ",horizontal=TRUE, container=mainFrame1)

   F1frame1 <- gframe(" Select XPS Sample ", spacing=5, container=F1group2)
   F1XpsSpect <- gcombobox(FNameList, selected=FNameIdx, editable=FALSE, handler=function(h,...){
                            makeCombo()
                 }, container=F1frame1)

   F1frame2 <- gframe(" Select Coreline ", spacing=5, container=F1group2)
   F1CoreLine <- gcombobox(SpectList, selected=-1, editable=FALSE, handler=function(h,...){
                           XPSComponent <- svalue(F1CoreLine)
                           XPSComponent <- unlist(strsplit(XPSComponent, "\\."))   #skip the ".NUMBER" at beginning CoreLine name
                           SpectIndx <- as.integer(XPSComponent[1])
                           SpectName <- XPSComponent[2]
                           assign("activeSpectName", SpectName,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
                           assign("activeSpectIndx", SpectIndx,.GlobalEnv) #set the activeSpectIndex to the actual CoreLine
                           Object <<- FName[[SpectIndx]]
                           plot(FName[[SpectIndx]])
                           enabled(F2group1) <- TRUE
                           enabled(F3group1) <- TRUE
                           enabled(F4group1) <- TRUE
                           enabled(F5group1) <- TRUE
                           enabled(F6group1) <- TRUE
                           enabled(F7group1) <- TRUE
                           enabled(F2filter) <- FALSE
                           enabled(F3filter) <- FALSE
                           enabled(F4filter) <- FALSE
                           enabled(F5filter) <- FALSE
                           enabled(F6filter) <- FALSE
                           enabled(F7filter) <- FALSE
                 }, container = F1frame2)

   F1frame3 <- gframe(   "Update XPS Sample List", spacing=5, container=F1group2)
   UpdateButt <- gbutton("            UPDATE            ", spacing=30, handler=function(h,...){
                          FNameList <<- XPSFNameList()                   #list of all XPSSamples loaded in .GlobalEnv
                          delete(F1frame1, F1XpsSpect)
                          F1XpsSpect <<- gcombobox(FNameList, selected=FNameIdx, editable=FALSE, handler=function(h,...){
                                                   makeCombo()
                                         }, container=F1frame1)
                          makeCombo()
                 }, container=F1frame3)
   addSpring(F1frame3)
   glabel(" ", container=mainFrame1)   #add line space


#===== NoteBook =====
   nb1 <- gnotebook(expand=FALSE, spacing=10, container = mainFrame1)

# --- Tab1 ---
   F2group1 <- ggroup(label=" SAVITZKY GOLAY ", spacing=1, horizontal=FALSE, container=nb1)
   F2group2 <- ggroup(label="  ", horizontal=TRUE, container=F2group1)
   F2frame1 <- gframe(text="Degree of Noise Rejection", spacing=5, container=F2group2)
   F2FiltDeg <- gcombobox(c(1:20), selected=-1, editable=FALSE, handler=function(h,...){
                    enabled(F2filter) <- TRUE
                 }, container = F2frame1)

   BkgSubtr2 <- gcheckbox("BKG Subtraction", checked=FALSE, container=F2group2)

   F2group3 <- ggroup(label="  ",horizontal=TRUE, container=F2group1)
   F2filter <- gbutton(" FILTER ", handler=function(h,...){
                     SavGolay()
                     enabled(SaveNewButt) <- TRUE
                     enabled(SaveButt) <- TRUE
                     enabled(SaveTestButt) <- TRUE
                 }, container = F2group3)

   F2Response <- gbutton(" FREQ. RESPONSE ", handler=function(h,...){
                     FiltResp <- freqz(filt=coeff, Fs=1)
                     plot(FiltResp)
                 }, container = F2group3)

# --- Tab2 ---
   F3group1 <- ggroup(label=" AutoRegressive : Moving Average ", spacing=1, horizontal=FALSE, container=nb1)
   enabled(F3group1) <- FALSE
   F3group2 <- ggroup(label="  ", horizontal=TRUE, container=F3group1)
   F3frame1 <- gframe(text=" Select Filter Type ", spacing=5, container=F3group2)
   F3FiltType <- gcombobox(filterType, selected=-1, editable=FALSE, handler=function(h,...){
                    enabled(F3FiltDeg) <- TRUE
                 }, container = F3frame1)

   F3frame2 <- gframe(text="Degree of Noise Rejection", spacing=5, container=F3group2)
   F3FiltDeg <- gcombobox(c(2:15), selected=-1, editable=FALSE, handler=function(h,...){
                     enabled(F3filter) <- TRUE
                 }, container = F3frame2)

   BkgSubtr3 <- gcheckbox("BKG Subtraction", checked=FALSE, container=F3group2)
   MatchEdges3 <- gcheckbox("Match Edges", checked=FALSE, container=F3group2)

   F3group3 <- ggroup(label="  ",horizontal=TRUE, container=F3group1)
   F3filter <- gbutton(" FILTER ", handler=function(h,...){
                     AutoRegMedMob()
                     enabled(SaveNewButt) <- TRUE
                     enabled(SaveButt) <- TRUE
                     enabled(SaveTestButt) <- TRUE
                 }, container = F3group3)

   F3Response <- gbutton(" FREQ. RESPONSE ", handler=function(h,...){
                     if(svalue(F3FiltType) == "AutoRegressive"){
                        FiltLgth <- as.numeric(svalue(F3FiltDeg))                     
                        FiltResp <- freqz(filt=coeff, a=1, region="half", Fs=1) #signal::impz(filt=1, a=coeff)
                     } else {

                        FiltResp <- freqz(filt=coeff, a=1, region="half", Fs=1)
                     }
                     plot(FiltResp)
                 }, container = F3group3)


# --- Tab3 ---
   F4group1 <- ggroup(label=" FFT FILTER ", spacing=1, horizontal=FALSE, container=nb1)
   F4group2 <- ggroup(label="  ", horizontal=TRUE, container=F4group1)
   F4frame1 <- gframe(text="Degree of Noise Rejection", spacing=5, container=F4group2)
   F4FiltDeg <- gcombobox(c(1:15), selected=-1, editable=FALSE, handler=function(h,...){
                    enabled(F4filter) <- TRUE
                 }, container = F4frame1)

   BkgSubtr4 <- gcheckbox("BKG Subtraction", checked=FALSE, container=F4group2)

   F4group3 <- ggroup(label="  ",horizontal=TRUE, container=F4group1)
   F4filter <- gbutton(" FILTER ", handler=function(h,...){
                     FFTfilt()
                     enabled(SaveNewButt) <- TRUE
                     enabled(SaveButt) <- TRUE
                     enabled(SaveTestButt) <- TRUE
                 }, container = F4group3)

   F4Response <- gbutton(" FREQ. RESPONSE ", handler=function(h,...){
                     FiltResp <- freqz(filt=coeff, a=1, whole=FALSE, Fs=1)
                     plot(FiltResp)
                 }, container = F4group3)


# --- Tab4 ---
   F5group1 <- ggroup(label=" WAVELETS FILTERING ", spacing=1, horizontal=FALSE, container=nb1)
   F5group2 <- ggroup(label="  ",horizontal=TRUE, container=F5group1)
   F5frame1 <- gframe(text=" Select N. Wavelets ", spacing=5, container=F5group2)
   F5WTnum <- gcombobox(c(1:10), selected=5, editable=FALSE, container = F5frame1)

   F5frame2 <- gframe(text=" Degree of Noise Rejection ", spacing=5, container=F5group2)
   F5WTLevel <- gcombobox(c(1:10), selected=-1, editable=FALSE, handler=function(h,...){
                     enabled(F5filter) <- TRUE
                 }, container = F5frame2)

   BkgSubtr5 <- gcheckbox("BKG Subtraction", checked=FALSE, container=F5group2)

   F5group2 <- ggroup(label="  ",horizontal=TRUE, container=F5group1)
   F5filter <- gbutton(" FILTER ", handler=function(h,...){
                     if( is.na(match("wavelets", Pkgs)) == TRUE ){    #check if the package 'wavelets' is installed
                        txt <- "Package 'wavelets' not installed. \nOption 'Wavelets filtering' cannot be done"
                        gmessage(msg=txt, title="WARNING", icon="error")
                        return()
                     }
                     msSmoothMRA()
                     enabled(SaveNewButt) <- TRUE
                     enabled(SaveButt) <- TRUE
                     enabled(SaveTestButt) <- TRUE
                 }, container = F5group2)

   F5Response <- gbutton(" FREQ. RESPONSE ", handler=function(h,...){
                     FiltResp <- freqz(filt=coeff, a=1, region="half", Fs=1)  #this is the LOW-PASS filter frequency response
                     plot(FiltResp)                                            #these high frequencies are filtered-out from the spectrum
                 }, container = F5group2)


# --- Tab5 ---
   F6group1 <- ggroup(label=" FIR FILTER ", spacing=1, horizontal=FALSE, container=nb1)
   F6group2 <- ggroup(label="  ",horizontal=TRUE, container=F6group1)
   F6frame1 <- gframe(text=" Select Filter Order ", spacing=5, container=F6group2)
   F6FiltOrder <- gcombobox(c(10,20,40,60,80, 120), selected=2, editable=FALSE, handler=function(h,...){
                     enabled(F6CutOFF) <- TRUE
                 }, container = F6frame1)

   F6frame2 <- gframe(text="Degree of Noise Rejection", spacing=5, container=F6group2)
   F6CutOFF <- gcombobox(c(c(1:20), "Custom"), selected=-1, editable=FALSE, handler=function(h,...){
                     enabled(F6filter) <- TRUE
                     CutOFF <<- svalue(F6CutOFF)
                     if (CutOFF == "Custom") {
                         delete(F6frame2, F6CutOFF)
                         F6CutOFF <<- gedit(initial.msg ="Cut-Off: ?", container=F6frame2)
                     }
                 }, container = F6frame2)

   BkgSubtr6 <- gcheckbox("BKG Subtraction", checked=FALSE, container=F6group2)

   F6group3 <- ggroup(label="  ",horizontal=TRUE, container=F6group1)
   F6filter <- gbutton(" FILTER ", handler=function(h,...){
                     CutOFF <<- as.numeric(svalue(F6CutOFF))
                     delete(F6frame2, F6CutOFF)
                     F6CutOFF <<- gcombobox(c(c(1:20), "Custom"), selected=-1, editable=FALSE, handler=function(h,...){
                                        enabled(F6filter) <- TRUE
                                        CutOFF <<- svalue(F6CutOFF)
                                        if (CutOFF == "Custom") {
                                           delete(F6frame2, F6CutOFF)
                                           F6CutOFF <<- gedit(initial.msg ="Cut-Off: ?", container=F6frame2)
                                        }
                                    }, container = F6frame2)

                     FirFilt()
                     enabled(SaveNewButt) <- TRUE
                     enabled(SaveButt) <- TRUE
                     enabled(SaveTestButt) <- TRUE
                 }, container = F6group3)

   F6Response <- gbutton(" FREQ. RESPONSE ", handler=function(h,...){
                     FiltResp <- freqz(filt=coeff, a=1, n=512, region="half", Fs=1)
                     plot(FiltResp)
                 }, container = F6group3)

# --- Tab6 ---
   F7group1 <- ggroup(label=" BUTTERWORTH FILTER ", spacing=1, horizontal=FALSE, container=nb1)
   F7group2 <- ggroup(label="  ",horizontal=TRUE, container=F7group1)
   F7frame1 <- gframe(text=" Select Filter Order ", spacing=5, container=F7group2)
   F7FiltOrder <- gcombobox(c(6,8,12,16), selected=1, editable=FALSE, handler=function(h,...){
                     enabled(F7CutOFF) <- TRUE
                 }, container = F7frame1)
   F7frame2 <- gframe(text="Degree of Noise Rejection", spacing=5, container=F7group2)
   F7CutOFF <- gcombobox(c(c(1:20), "Custom"), selected=-1, editable=FALSE, handler=function(h,...){
                     enabled(F7filter) <- TRUE
                     CutOFF <<- svalue(F7CutOFF)
                     if (CutOFF == "Custom") {
                         delete(F7frame2, F7CutOFF)
                         F7CutOFF <<- gedit(initial.msg ="Cut-Off: ?", container=F7frame2)
                     }
                 }, container = F7frame2)

   BkgSubtr7 <- gcheckbox("BKG Subtraction", checked=FALSE, container=F7group2)

   F7group3 <- ggroup(label="  ",horizontal=TRUE, container=F7group1)
   F7filter <- gbutton(" FILTER ", handler=function(h,...){
                     CutOFF <<- as.numeric(svalue(F7CutOFF))
                     delete(F7frame2, F7CutOFF)
                     F7CutOFF <<- gcombobox(c(c(1:20), "Custom"), selected=-1, editable=FALSE, handler=function(h,...){
                                        enabled(F7filter) <- TRUE
                                        CutOFF <<- svalue(F7CutOFF)
                                        if (CutOFF == "Custom") {
                                           delete(F7frame2, F7CutOFF)
                                           F7CutOFF <<- gedit(initial.msg ="Cut-Off: ?", container=F7frame2)
                                        }
                                    }, container = F7frame2)

                     ButtFilt()
                     enabled(SaveNewButt) <- TRUE
                     enabled(SaveButt) <- TRUE
                     enabled(SaveTestButt) <- TRUE
                 }, container = F7group3)

   F6Response <- gbutton(" FREQ. RESPONSE ", handler=function(h,...){
                     FiltResp <- freqz(filt=coeff$b, a=coeff$a, n=512, region="half", Fs=1)
                     plot(FiltResp)
                 }, container = F7group3)


#--- Common buttons

   FButtgroup1 <- ggroup(horizontal=TRUE, spacing=1, container= mainGroup)
   FButtgroup2 <- ggroup(horizontal=TRUE, spacing=1, container= mainGroup)


   SaveNewButt <- gbutton(" SAVE AS A NEW CORE LINE ", spacing=1, handler=function(h,...){ #Filtered data saved in a new coreline
                    SpectIndx <- get("activeSpectIndx",.GlobalEnv)
                    Symbol <- get("activeSpectName",.GlobalEnv)
                    NCL <- length(FName)  #number of XPSSample corelines
                    CLNames <- names(FName)
                    FName[[NCL+1]] <<- FName[[SpectIndx]]
                    FName[[NCL+1]]@Symbol <<- Symbol
                    chrPos <- FindPattern(Info, "   ::: Smoothing")
                    if (length(chrPos[1]) > 0){
                        Info <- FName[[NCL+1]]@Info
                        nI <- length(Info)
                        Info[nI+1] <- paste("   ::: Smoothing: ", FiltInfo, sep="")
                        FName[[NCL+1]]@Info <<- Info
                    }
                    FName@names <<- c(CLNames,Symbol)
                    FName[[NCL+1]]@.Data[[2]] <<- Filtered
                    if (length(FName[[SpectIndx]]@RegionToFit$y>0)){ #RegionToFit defined
                       rng <- range(FName[[SpectIndx]]@RegionToFit$x)
                       if (Object@Flags[1]==TRUE) {rng <- rev(rng)}    #Binding energy set
                       idx1 <- which(FName[[TestIdx]]@.Data[[1]] == rng[1])
                       idx2 <- which(FName[[TestIdx]]@.Data[[1]] == rng[2])
                       FName[[TestIdx]]@RegionToFit$y <<- Filtered[idx1:idx2]
                    }
                    assign(ActiveFName, FName,envir=.GlobalEnv)   #save XPSSample with additional smoothed coreline
#                    assign("activeSpectIndx", (NCL+1), envir=.GlobalEnv)      #set the activeSpectIndx be the smoothed core line
                    RawData <<- NULL
                    Filtered <<- NULL
                    BackGnd <<- NULL
                    plot(FName)
                    XPSSaveRetrieveBkp("save")
                 }, container = FButtgroup1)

   SaveButt <- gbutton(" SAVE IN THE ORIGINAL CORE LINE", spacing=1, handler=function(h,...){  #Original data replaced by Filterd data
                    SpectIndx <- get("activeSpectIndx",.GlobalEnv)
                    FName[[SpectIndx]]@.Data[[2]] <<- Filtered
                    if (length(FName[[SpectIndx]]@RegionToFit$y>0)){ #RegionToFit defined
                       rng <- range(FName[[SpectIndx]]@RegionToFit$x)
                       if (Object@Flags[1]==TRUE) {rng <- rev(rng)}    #Binding energy set
                       idx1 <- which(FName[[TestIdx]]@.Data[[1]] == rng[1])
                       idx2 <- which(FName[[TestIdx]]@.Data[[1]] == rng[2])
                       FName[[TestIdx]]@RegionToFit$y <<- Filtered[idx1:idx2]
                    }
                    Info <- FName[[SpectIndx]]@Info
#                    xxx <- grep("Smoothing:", Info)     #is the Test Filtering already present?
                    chrPos <- FindPattern(Info, "   ::: Smoothing")
                    if (length(chrPos[1]) > 0) {         #Filtering Comments are present?
                         nI <- length(Info)
                    } else {
                         nI <- length(Info)+1
                    }
                    Info[nI] <- paste("   ::: Smoothing: ", FiltInfo, sep="")  #Add or Change filter information
                    FName[[SpectIndx]]@Info <<- Info
                    assign(ActiveFName, FName,envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                    Object <<- FName[[SpectIndx]]       #update Object used for filtering
                    plot(FName)
                    XPSSaveRetrieveBkp("save")
                 }, container = FButtgroup1)

   SaveTestButt <- gbutton(" SAVE AS A SMOOTHING-TEST ", spacing=1, handler=function(h,...){  #in Test Coreline new filtering is overwitten
                    SpectIndx <- get("activeSpectIndx",.GlobalEnv)
                    Symbol <- get("activeSpectName",.GlobalEnv)
                    CLNames <- names(FName)
                    LL <- length(FName)
#                    chrPos <- regexpr("ST.", CLNames[SpectIndx])    #Are we working on a Smoothing-Test core line?
                    chrPos <- FindPattern(CLNames[SpectIndx], "ST.")
                    if (length(chrPos[2]) > 0) {                               #Smoothing-Test Coreline IS PRESENT
                       TestIdx <- SpectIndx
                       Info <- FName[[TestIdx]]@Info                #update filter information
                       LL <- length(Info)
                       Info[LL] <- paste("   ::: Smoothing Test: ", FiltInfo, sep="")
                       FName[[TestIdx]]@Info <<- Info
                    } else {
#                       chrPos <- which(regexpr("ST.", CLNames) > 0)  #Find the index if there is a "ST." identifying the Smoothing-Test coreline
                       chrPos <- FindPattern(CLNames, "ST.")
                       if (length(chrPos[2])==0) {                   #Smoothing-Test coreline is NOT present
                          TestIdx <- LL+1                            #Smoothing-Test coreline is added to FName as a new coreline
                          FName[[TestIdx]] <<- FName[[SpectIndx]]    #We are testing a filter on a coreline and save results in the Smoothing-Test coreline
                          FName@names <<- c(CLNames,paste("ST.", Symbol, sep=""))  #modify the names and info of the new Smoothing-Test coreline
                          Info <- FName[[TestIdx]]@Info
                          LL <- length(Info)
                          chrPos <- FindPattern(Info[LL], "   ::: Smoothing Test")
                          if (length(chrPos[1]) == 0){ LL <- LL+1 }    #Smoothing-Test Info still not present
                          Info[LL] <- paste("   ::: Smoothing Test: ", FiltInfo, sep="")
                          FName[[TestIdx]]@Info <<- Info
                          FName[[TestIdx]]@Symbol <<- paste("ST.", Symbol, sep="")
                       } else {
                          TestIdx <- chrPos[2]                            #Smoothing-Test coreline is present
                          Info <- FName[[TestIdx]]@Info
                          LL <- length(Info)
                          Info[LL] <- paste("   ::: Smoothing Test: ", FiltInfo, sep="")
                          FName[[TestIdx]]@Info <<- Info
                       }
                    }
                    FName[[TestIdx]]@.Data[[2]] <<- Filtered         #change the original data with the filtered data
                    if (length(FName[[SpectIndx]]@RegionToFit$y>0)){ #RegionToFit defined
                       rng <- range(FName[[SpectIndx]]@RegionToFit$x)
                       if (Object@Flags[1]==TRUE) {rng <- rev(rng)}    #Binding energy set
                       idx1 <- which(FName[[TestIdx]]@.Data[[1]] == rng[1])
                       idx2 <- which(FName[[TestIdx]]@.Data[[1]] == rng[2])
                       FName[[TestIdx]]@RegionToFit$y <<- Filtered[idx1:idx2]
                    }
                    assign(ActiveFName, FName,envir=.GlobalEnv)      #Save the modified XPSSample in the globalEnv
#                    assign("activeSpectIndx", TestIdx, envir=.GlobalEnv)  #set the activeSpectIndx be the smoothed core line
                    RawData <<- NULL
                    Filtered <<- NULL
                    BackGnd <<- NULL
                    plot(FName)
                    XPSSaveRetrieveBkp("save")
                 }, container = FButtgroup1)

   gbutton(" RESET ", spacing=1, handler=function(h,...){
                    RawData <<- NULL
                    Filtered <<- NULL
                    BackGnd <<- NULL
                    plot(FName)
                 }, container = FButtgroup1)

   gbutton(" EXIT ", spacing=1, handler=function(h,...){
                    dispose(mainFwin)
                    XPSSaveRetrieveBkp("save")
                 }, container = FButtgroup1)


   enabled(F2filter) <- FALSE
   enabled(F3filter) <- FALSE
   enabled(F4filter) <- FALSE
   enabled(F5filter) <- FALSE
   enabled(F6filter) <- FALSE
   enabled(F7filter) <- FALSE
   enabled(SaveNewButt) <- FALSE
   enabled(SaveButt) <- FALSE
   enabled(SaveTestButt) <- FALSE

   for (ii in 6:1){
      svalue(nb1) <- ii #update notebook pages
   }
   visible(mainFwin) <- TRUE
}

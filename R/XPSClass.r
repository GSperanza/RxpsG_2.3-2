## --------------------------------------------------------------------------
## RxpsG - R package for processing XPS data from Scienta and Kratos Instruments
## --------------------------------------------------------------------------
##  Copyright (C) 2012 -- Roberto Canteri , Giorgio Speranza
##
## Note on CoreLine@Flags in XPSSamples:
## Flag[1] == Binding Energy --> If TRUE then x scale is Binding Energy
## Flag[2] == cps  --> If TRUE then y scale is cps. NB!! With vamas and scienta files is always TRUE!!
## Flag[3] == Scienta --> If TRUE then sample is Scienta sample
## Flag[4] == Vamas correction for transmission factor
##==============================================================================


##==============================================================================
#
# Class XPSCoreLine  methods and functions
#
##==============================================================================

#' @title Class "XPSCoreLine"
#' @description The package provides a class for XPS spectra (class \code{XPSCoreLine}) and
#'  lists of such objects (class \code{XPSSample}). \code{XPSCoreLine} are values
#'  pairs stored in a \code{list} and several additional parameters stored in
#'  slots.
#'
#' @slot .Data contains the X, Y spectral data
#' @slot RegionToFit the portion of the spectrum to fit
#' @slot Baseline the Baseline applied to subtract background
#' @slot Components the fitting components
#' @slot Fit the best fit
#' @slot Boundaries the values of the RegionToFit edges
#' @slot RSF the relative seisitivity factor associated to the element spectrum
#' @slot Shift the energy correction shift if charging present
#' @slot units the adopted units: kinetic/binding energy, counts/counts_per_second
#' @slot Flags logical 
#' @slot Info information regarding the spectrum acquisition
#' @slot Symbol symbol of the element associated to the spectrum
#' @examples
#' \dontrun{
#' test <- new("XPSCoreLine", Info="test", units=c("Binding [eV]", "Counts"))
#' }
#' @export
#'

setClass("XPSCoreLine",
         representation(
                     RegionToFit="list",
                     Baseline="list",
                     Components="list",
                     Fit="list",
                     Boundaries="list",
                     RSF="numeric",
                     Shift="numeric",
                     units="character",
                     Flags="logical",
                     Info="character",
                     Symbol="character"
                     ),
         contains="list",
         prototype(
                     RegionToFit=list(),
                     Baseline=list(),
                     Components=list(),
                     Fit=list(),
                     RSF=0,
                     Shift=0,
                     Boundaries=list(),
                     units=c("Binding Energy [eV]","Intensity [cps]"),
                     Flags=c(TRUE, FALSE, FALSE),
                     Info="",
                     Symbol=""
                   )
)

##==============================================================================
# Methods  concerning Class=Coreline
##==============================================================================
### Accessory functions:

#' @title hasBoundaries
#' @description S4method 'hasBoundaries' method for objects of class XPSCoreLine.
#' @details 'hasBoundaries' checks if Boundaries are defined for XPSCoreLine objects.
#' @param object an object of class \code{XPSCoreLine}
#' @return 'hasBoundaries' returns a logical vector with TRUE if the object has defined boundaries FALSE otherwise
setGeneric("hasBoundaries", function(object) standardGeneric("hasBoundaries"))

#' @title hasBoundaries
#' @description method to verify an object of class 'XPSCoreLine'
#' @param object a Core_Line object of class \code{XPSCoreLine}
#' @examples
#' \dontrun{
#'  if( hasBoundaries(test[["C1s"]]) ) print(test[["C1s"]]@Boundaries)
#' }
#' @export
setMethod("hasBoundaries", "XPSCoreLine", function(object) as.logical(length(slot(object, "Boundaries")) !=0) )

###
#' @title hasRegionToFit
#' @description S4method 'hasRegionToFit' method for objects of class XPSCoreLine.
#' @details 'hasRegionToFit' checks if the RegionToFit is defined for XPSCoreLine objects.
#' @param object an object of class \code{XPSCoreLine}
#' @return 'hasRegionToFit' returns a logical vector with TRUE if the object has defined RegionToFit FALSE otherwise
setGeneric("hasRegionToFit", function(object) standardGeneric("hasRegionToFit"))

#' @title hasRegionToFit
#' @description method to verify an object of class 'XPSCoreLine'
#' @param object a Core_Line object of class \code{XPSCoreLine}
#' @examples
#' \dontrun{
#'  if (hasRegionToFit(test[["C1s"]])) print(length(test[["C1s"]]@RegionToFit$x))
#' }
#' @export
setMethod("hasRegionToFit", "XPSCoreLine", function(object) as.logical(length(slot(object, "RegionToFit")) !=0) )

###
#' @title hasBaseline
#' @description S4method 'hasBaseline' method for objects of class XPSCoreLine.
#' @details 'hasBaseline' checks if Baseline is defined for an XPSCoreLine objects.
#' @param object an object of class \code{XPSCoreLine}
#' @return 'hasBaseline' returns a logical vector with TRUE if the object has a Baseline FALSE otherwise
setGeneric("hasBaseline", function(object) standardGeneric("hasBaseline"))

#' @title hasBaseline
#' @description method to verify an object of class 'XPSCoreLine'
#' @param object a Core_Line object of class \code{XPSCoreLine}
#' @examples
#' \dontrun{
#'  if( hasBaseline(test[["C1s"]]) ) print(length(test[["C1s"]]@Baseline$y))
#' }
#' @export
setMethod("hasBaseline", "XPSCoreLine", function(object) as.logical(length(slot(object, "Baseline")) !=0) )

###
#' @title hasComponents
#' @description S4method 'hasBaseline' method for objects of class XPSCoreLine.
#' @details 'hasComponents' checks if Fit Components are defined for an XPSCoreLine objects.
#' @param object an object of class \code{XPSCoreLine}
#' @return 'hasComponents' returns a logical vector with TRUE if the object has defined Fit Components FALSE otherwise
setGeneric("hasComponents", function(object) standardGeneric("hasComponents"))

#' @title hasComponents
#' @description method to verify an object of class 'XPSCoreLine'
#' @param object a Core_Line object of class \code{XPSCoreLine}
#' @examples
#' \dontrun{
#'  if (hasComponents(test[["C1s"]])) print(length(test[["C1s"]]@Components[[1]]))
#' }
#' @export
setMethod("hasComponents", "XPSCoreLine", function(object) as.logical(length(slot(object, "Components")) !=0) )

###
#' @title hasFit
#' @description S4method 'hasFit' method for objects of class XPSCoreLine.
#' @details 'hasFit' checks if  Best Fit is defined for an XPSCoreLine objects.
#' @param object an object of class \code{XPSCoreLine}
#' @return 'hasFit' returns a logical vector with TRUE if the object has a Best Fit FALSE otherwise
setGeneric("hasFit", function(object) standardGeneric("hasFit"))

#' @title hasFit
#' @description method to verify an object of class 'XPSCoreLine'
#' @param object a Core_Line object of class \code{XPSCoreLine}
#' @examples
#' \dontrun{
#'  if (hasFit(test[["C1s"]])) print(length(test[["C1s"]]@Fit$y))
#' }
#' @export
setMethod("hasFit", "XPSCoreLine", function(object) as.logical(length(slot(object, "Fit")) !=0) )

##==============================================================================
# show
##==============================================================================
#' @title show
#' @description Method to show XPSCoreLine elements
#' @param object XPSCoreLine
#' @examples
#' \dontrun{
#'  show(test.RData)
#' }
#' @export
#'

setMethod("show", signature(object="XPSCoreLine"),
    function(object) {
       cat(rep("-",30),"\n",sep="")
       cat("Core Line : ",slot(object,"Symbol"),"\n")
       cat("baseline  : ",ifelse(hasBaseline(object),"YES", "NO"),"\n")
       cat("fit       : ",ifelse(hasFit(object),"YES", "NO"),"\n")
       cat("n. comp.  : ",ifelse(hasComponents(object),length(object@Components), "NONE"),"\n")
       cat(" Info\n")
       print(slot(object,"Info"))
    }
)

##==============================================================================
# Conversions for XPSCoreLine  data.frame
##==============================================================================
###
#' @title setAsData.Frame function for XPSCoreLine
#' @description setAsData.Frame attempts to coerce an XPSCoreline to a data.frame type. 
#'  Mainly used for plot function.
#' @param from = an XPSCoreLine object
#' @param to = a data.frame object
#' @return 'setAsData.Frame' returns a data.frame object
setGeneric("setAsData.Frame", function(from, to="data.frame") standardGeneric("setAsData.Frame"))
#' @title setAsData.Frame
#' @description method to coerce an object of class 'XPSCoreLine' 
#'  in an object of class 'data.frame'
#' @param from = an XPSCoreLine object
#' @param to = a data.frame object
#' @return 'setAsData.Frame' returns a data.frame object
#' @examples
#' \dontrun{
#'  MyDataFrame <- setAsData.Frame(test[["C1s"]], to="data.frame")
#' }
#' @export
#'
setMethod("setAsData.Frame", signature(from = "XPSCoreLine"), 
     function (from, to="data.frame") {

     if ( ! hasRegionToFit(from) ) { X <- data.frame(x=from[[1]], y=from[[2]]) }
     else { X <- data.frame(x=from@RegionToFit$x, y=from@RegionToFit$y) }
     if (slot(from,"Symbol") != "") names(X)[2] <- slot(from,"Symbol")

     # Baseline: length(x@Baseline$x) == length(x@RegionToFit$x)
     if ( hasBaseline(from) ) { X$Baseline <- from@Baseline$y }
     # Components
     if ( hasComponents(from) ) {
       Y <- do.call("data.frame", lapply(from@Components, function(jk) { jk@ycoor }) )
       X <- cbind(X,Y)
     }
     # Fit
     if ( hasFit(from) ) { X$Fit <- as.vector(from@Baseline$y + from@Fit$y) }
     return(X)
  }
)


##==============================================================================
# Conversions for XPSCoreLine matrix
##==============================================================================
###
#' @title setAsMatrix function for XPSCoreLine
#' @description setAsMatrix attempts to coerce an XPSCoreline to a matrix type. 
#'  Mainly used for plot function.
#' @param from = an XPSCoreLine object
#' @param to = a matrix object
#' @return 'setAsMatrix' returns a matrix object
setGeneric("setAsMatrix", function(from, to="matrix") standardGeneric("setAsMatrix"))
#' @title setAsMatrix
#' @description method to coerce an object of class 'XPSCoreLine' in an object of class 'matrix'
#' @param from = an XPSCoreLine object
#' @param to = matrix object
#' @return 'setAsMatrix' returns a matrix object
#' @examples
#' \dontrun{
#'  MyMatix <- setAsMatrix(test[["C1s"]], to="matrix")
#' }
#' @export
#'
setMethod("setAsMatrix", signature(from = "XPSCoreLine"), function (from, to="matrix") {

     return( as.matrix(setAsData.Frame(from, "data.frame") ) )
  }
)


##==============================================================================
# Conversions for XPSCoreLine "list"
##==============================================================================
###
#' @title asList function for XPSCoreLine
#' @description asList attempts to coerce its argument to an object of class list. 
#'  Mainly used for plot function.
#' @param from = an XPSCoreLine object
#' @param select one or few or \code{"all"} (default) of \code{c("MAIN", "RTF", "BASE", "COMPONENTS", "FIT")}
#' @return 'asList' returns the selected slots of a XPSCoreLine coerced in list format
setGeneric("asList", function(from, select="all") standardGeneric("asList"))
#' @title asList
#' @description method to coerce an object of class 'XPSCoreLine' in an object of class 'list'
#' @param from = an XPSCoreLine object
#' @param select one or few or \code{"all"} (default) of \code{c("MAIN", "RTF", "BASE", "COMPONENTS", "FIT")}
#' @return 'asList' returns the selected slots of a XPSCoreLine coerced in list format
#' @examples
#' \dontrun{
#'  MyList <- asList(test[["C1s"]], selected="all")
#' }
#' @export
#'
setMethod("asList", signature(from = "XPSCoreLine"), function (from, select="all") {
     if (select[1] == "all") select <- c("MAIN", "RTF", "BASE", "COMPONENTS", "FIT")
       X <- Y <- list()
     ## main curve
     if ("MAIN" %in% select) {
        X$MAIN <- from[[1]]
        Y$MAIN <- from[[2]]
     }
     ## RegionToFit
     if (("RTF" %in% select) && hasRegionToFit(from) ) {
        X$RTF <- from@RegionToFit$x
        Y$RTF <- from@RegionToFit$y
     }
     ## Baseline
     if ( ("BASE" %in% select) && hasBaseline(from) ) {
        X$BASE <- from@Baseline$x
        Y$BASE <- from@Baseline$y
     }
     ## Components
     if (("COMPONENTS" %in% select) && hasComponents(from) ) {
        for (idx in seq_along(from@Components)) {
           X[[length(X)+1]] <- from@RegionToFit$x
           Y[[length(Y)+1]] <- from@Components[[idx]]@ycoor
           names(X)[length(X)] <- names(Y)[length(Y)] <- "COMPONENTS"
        }
     }
     ## Fit
     if (("FIT" %in% select) && hasFit(from) ) {
        X$FIT <- from@RegionToFit$x
        Y$FIT <- as.vector(from@Baseline$y + from@Fit$y)
     }   
     return(list(x=X,y=Y))
  }
)


##==============================================================================
# sortComponents for XPSCoreLine
# sort the components on the base of xcenter
##==============================================================================
#' @title sortComponents Sort the XPSCoreline Fit Components
#' @description Sort Components Method for XPSCoreLine
#' @param object XPSCoreLine
setGeneric("sortComponents", function(object) standardGeneric("sortComponents"))
#' @title sortComponents
#' @description method to sort Fit Components in ascending energy (binding or kinetic)
#' @param object a Core_Line object of class \code{XPSCoreLine}
#' @examples
#' \dontrun{
#'  sortComponents(test[["C1s"]])
#' }
#' @export
setMethod("sortComponents", signature(object="XPSCoreLine"),
 function(object) {

 ## sort xcenter in ascending order
    xcenter <- sapply(object@Components, getParam, parameter="start", variable="mu")
    idx <- order(xcenter)  #sequence of component indexes whose BE position (xcenter)is in ascending order
    CompIndx <- idx
    slot(object,"Components") <- slot(object,"Components")[idx]
 ## names redefinition
    compnum <- length(slot(object,"Components"))
    names(slot(object,"Components")) <- paste("C", seq_len(compnum),sep="")
    for (idx in seq_len(compnum) ) {
      slot(object@Components[[idx]], "label") <- paste("#", as.character(idx),sep="")
    }

## If component sequence is changed control also the link on sigma be OK (Revised Giorgio2019)
      for (ii in 1:compnum){
          if (length(object@Components[[ii]]@link)>0) { # there is a link  As an example let us suppose sigma4 linked to sigma1 (but this holds for any other fit param)
              LnkC1<-object@Components[[ii]]@link[[1]]$variable  #sigma4  or other linked variable
              Indx1 <- gsub("[^0-9]", "", LnkC1)   #LnkC1=="sigma4" only the alphanumerical part "4" is taken: now 4
              ParName1 <- strsplit(LnkC1, Indx1)   #now split "sigma4" in "sigma" and "4"
              Indx1 <- as.integer(Indx1)
              jj <- which(CompIndx == Indx1)         #search the index 4 inside the list of fit Component indexes (components are not ordered => index does not correspond with the component position in the list!)
              LnkC1 <- paste(ParName1, jj, sep="")   #build the correct param1 name
              object@Components[[ii]]@link[[1]]$variable <- LnkC1
              LnkC2 <- object@Components[[ii]]@link[[1]]$expr  #sigma1 (Or other variable) to which sigma1 is linked
              Indx2 <- gsub("[^0-9]", "", LnkC2)   #LnkC2==sigma1, gsub extracts Indx2==1  form string LnkC2
              ParName2 <- strsplit(LnkC2, Indx2)   #The parameter name (not necessarily sigma)
              Indx2 <- as.integer(Indx2)
              jj <- which(CompIndx == Indx2)         #search the index of Sigma Comp2 all'interno di CompIndx
              LnkC2 <- paste(ParName2, jj, sep="")   #build the correct param2 name
              object@Components[[ii]]@link[[1]]$expr <- LnkC2
          }
      }
    return(object)
  }
)

##==============================================================================
# getMaxOfComponents: get max of each components. return a list with x,y values
##==============================================================================
#' @title getMaxOfComponents Get Max of Components
#' @description Get Max of Components Method for XPSCoreLine
#' @param object XPSCoreLine
#' @return list with \code{x,y} value
#'
setGeneric("getMaxOfComponents", function(object)  standardGeneric("getMaxOfComponents"))
#' @title getMaxOfComponents
#' @description method to define the maximum of Fit Components
#' @param object XPSCoreLine
#' @examples
#' \dontrun{
#'  print(getMaxOfComponents(test[["C1s"]]))
#' }
#' @export
#'
setMethod("getMaxOfComponents", signature(object="XPSCoreLine"),
  function(object) {
    ans <- NA
    if ( hasComponents(object) ) {
        tmp.position <- sapply(object@Components, function(z,k) {
                              idxatymax <- which.max((z@ycoor-k@Baseline$y))
                              return(c(k@RegionToFit$x[idxatymax], z@ycoor[idxatymax]))
                            }, k = object )
        ans <- list(x = tmp.position[1,], y = tmp.position[2,])
    }
    return(ans)
  }
)



##==============================================================================
# Set RegionToFit: portion of Data delimited by boundaries
##==============================================================================
###
#' @title XPSsetRegionToFit Definition of the Region to Fit
#' @description Definition of the portion (usually on the x-axis) of the original curve for
#'   further processing. XPSsetRegionToFit limits usually will be set with the cursor on the 
#'   curve plot where a \code{Baseline} is needed. Then the \code{RegionToFit} is
#'   defined as a list of (x,y) data equal to the selected portion of the curve.
#' @param object XPSCoreLine object
#' @param limits list with x,y values to limit the region.
#' @param ...  further parameters to the XPSsetRegionToFit function
#' @return returns a portion of the CoreLine for a backgrouns subtraction
#' @seealso \link{XPSbaseline}
setGeneric("XPSsetRegionToFit", function(object, limits, ...) standardGeneric("XPSsetRegionToFit"))
#' @title getMaxOfComponents
#' @description method to define the maximum of Fit Components
#' @param object XPSCoreLine object
#' @param limits list with x,y values to limit the region.
#' @param ...  further parameters to the XPSsetRegionToFit function
#' @examples
#' \dontrun{
#'  test[["C1s"]] <- XPSsetRegionToFit(test[["C1s"]], limits=pos)   
    #pos is a list composed by pos$x1, pos$x2, pos$y1, pos$y2 provided by locator(n=2)
#' }                                                                
#' @export
#'
setMethod("XPSsetRegionToFit", signature(object="XPSCoreLine"),
  function(object, limits, ...) {

     if ( missing(limits) ) {
        if ( ! hasBoundaries(object) ) {
            plot(object, ...)
            slot(object,"Boundaries") <- locator(n=2, type="p")
        }
        limits <- slot(object,"Boundaries")
     } else {
        slot(object,"Boundaries") <- limits
     }
     index <- which( unlist(object@.Data[1]) >= min(limits$x) & unlist(object@.Data[1]) <= max(limits$x) )
     slot(object, "RegionToFit") <- list(x=object[[1]][index], y=object[[2]][index])
     return(object)
  }
)

# =======================================================
# Baseline: class definition
# =======================================================
#' @title Class "baseline"
#' @description The RxpsG package provides a class for baselines (class \code{baseline})
#'   \code{baseline} are values pairs stored in a \code{list}
#'   and additional parameters are stored in slots.
#'   Objects from the Class: Objects can be created by calls of the form \code{new("baseline", ...)}.
#' @examples
#' \dontrun{
#' test <- new("baseline", ...))
#' }
#' @export
#'
setClass("baseline",
         representation(
                     baseline="matrix",
                     corrected="matrix",
                     spectra="matrix",
                     call="language"
                     )
        )
# =======================================================
# Baseline: definition
# =======================================================
###
#' @title XPSbaseline Baseline definition for XPSCoreLine object.
#' @description Calculates the baseline for XPSCoreLine. There is a list of baseline shapes implemented:
#'   linear, polynomial, spline, Shirley, 2P.Shirley, 3P.Shirley, LP.Shirley,
#'   2P.Tougaard, 3p.Tougaard, 4P.Tougaard.
#'   Selection of the appropriate baseline have to be made upon spectral-data properties.
#' @param object XPSCoreLine object
#' @param bgtype the baseline type. For example linear type \code{"2P.Tougaard"}.
#' @param deg degree of the polynomial background
#' @param Wgt LinearPolynomial weigth in LPShirley, required parma in 3P. 4P.Tougaard
#' @param splinePoints numeric vector containing the points which will be connected by the spline
#' @param ...  other parameters.
#' @return The Object slot \code{Baseline} will be filled with the selected Bsseline
#'   and it will be displyed. The baseline function returns an object of class
#'   \code{baseline}. The x is the same as \code{RegionToFit} x coord. The y coord
#'   are the baseline values.
setGeneric("XPSbaseline", function(object, bgtype=c("linear","shirley","polynomial","spline"),
            deg=NULL, Wgt=NULL, splinePoints=list(x=NULL, y=NULL), ...)  standardGeneric("XPSbaseline"))
#' @title XPSbaseline
#' @description method to generate a Baseline for an object of class 'XPSCoreLine'
#' @param object XPSCoreLine object
#' @param bgtype the baseline type. For example linear type \code{"2P.Tougaard"}.
#' @param deg degree of the polynomial background
#' @param Wgt LinearPolynomial weigth in LPShirley, required parma in 3P. 4P.Tougaard
#' @param splinePoints numeric vector containing the points which will be connected by the spline
#' @param ...  other parameters.
#' @return The Object slot \code{Baseline}
#' @examples
#' \dontrun{
#'   test[["C1s"]] <- XPSbaseline(test[["C1s"]], "linear")
#' }
#' @export
#'
setMethod("XPSbaseline", signature(object="XPSCoreLine"),
  function(object, bgtype, deg=NULL, Wgt=NULL, splinePoints=list(x=NULL, y=NULL), ...){

    ## if RegionToFit not yet defined
    if ( ! hasRegionToFit(object) ) {
       object <- XPSsetRegionToFit(object)
    }

#-----  modificato Giorgio2018
    info<-NULL
    tmp<-NULL
    spectra <- matrix(data = object@RegionToFit$y, nrow = 1)
    ## wrapper for linear, polynomial, shirley, spline, tougaard bgnds
    bgtype <- tolower(bgtype) #converts upper to lower case characters then baseline names can be written in both capital an normal letters

    switch(bgtype,
           "linear" = {
              X <- object@Boundaries$x
              bgnd <- lm(formula=object@Boundaries$y ~ X)
              newx <- data.frame(X=object@RegionToFit$x)
              bgnd <- matrix(data = predict(bgnd, newx), nrow=1)
              tmp <- new("baseline",
                          baseline = bgnd,
                          corrected = spectra - bgnd,
                          spectra = spectra,
                          call = match.call()
                        )
           },
           "polynomial" = { #tmp structure defined by baseline(modpolyfit) function
#              tmp <- baseline(spectra, method="modpolyfit", t=object@RegionToFit$x, degree = deg, tol = 0.01, rep = 100)
#substraction of linear fit of object@RegionToFit$y
              LL <- length(object@RegionToFit$x)
              av1 <- mean(object@RegionToFit$y[1:3])  #average1 at beginning
              av2 <- mean(object@RegionToFit$y[(LL-3): LL])  #average2 at end
              X <- object@Boundaries$x
              Linbgnd <- lm(formula=c(av1, av2) ~ X)
              newx <- data.frame(X=object@RegionToFit$x)
              Linbgnd <- matrix(data = predict(Linbgnd, newx), nrow=1)
              newy <- as.vector(object@RegionToFit$y-Linbgnd)

#definition of nearly flat regions: peak suppression
              avy <- mean(newy)
              indx <- which(newy < avy)
              kkk <- rep(avy, LL)
              yyy <- rep(0, LL)  #in the regions where object@RTF$y > avy  yyy==0
              yyy[indx] <- newy[indx] #in the regions where object@RTF$y < avy  yyy==newy

#polynomial fit of the selected regions
              X <- object@RegionToFit$x
              wgts <- rep(1, LL)
              wgts[1:5] <- wgts[(LL-5):LL] <- 1000     #the first and least 5 points have weight 1000 to force the baseline be overlapped to them
              bgnd <- lm(formula=yyy ~ poly(X, deg), weights=wgts)
              bgnd <- matrix(data = predict(bgnd, newx), nrow=1)+Linbgnd
              tmp <- new("baseline",
                          baseline = bgnd,
                          corrected = spectra - bgnd,
                          spectra = spectra,
                          call = match.call()
                        )
              info <- as.character(deg)
           },
           "spline" = {
              limits<-object@Boundaries
              bgnd <- baseSpline(spectra, splinePoints, limits)    #customBaseline Spline call
              tmp <- new("baseline",        #definition of a structure of type "baseline"
                         baseline = bgnd[[1]],   #parameters obtained form custom Baseline
                         corrected = bgnd[[2]],
                         spectra = bgnd[[3]],
                         call = match.call()
                        )
           },
           "shirley" = {  #tmp structure defined by baseline(shirley) function
              limits <- object@Boundaries
              bgnd <- matrix(data=Shirley0(object, limits), nrow=1) #Shirley0 classical Shirley background
              tmp <- new("baseline",
                         baseline = bgnd,
                         corrected = spectra - bgnd,
                         spectra = spectra,
                         call = match.call()
                        )
           },
           "2p.shirley" = {
              limits<-object@Boundaries
              bgnd <- matrix(data=Shirley2P(object, limits), nrow=1)
              tmp <- new("baseline",
                         baseline = bgnd,
                         corrected = spectra - bgnd,
                         spectra = spectra,
                         call = match.call()
                        )
          },
          "3p.shirley" = {
             limits<-object@Boundaries
             bgnd <- matrix(data=Shirley3P(object, Wgt, limits), nrow=1)
             tmp <- new("baseline",
                         baseline = bgnd,
                         corrected = spectra - bgnd,
                         spectra = spectra,
                         call = match.call()
                       )
             info <- as.character(Wgt)
          },
          "lp.shirley" = {
             limits <- object@Boundaries
             bgnd <- matrix(data=LPShirley(object, Wgt, limits), nrow=1)
             tmp <- new("baseline",
                         baseline = bgnd,
                         corrected = spectra - bgnd,
                         spectra = spectra,
                         call = match.call()
                       )
             info <- as.character(Wgt)
          },
          "2p.tougaard" = {
             limits<-object@Boundaries
             bgnd <- matrix(data=Tougaard2P(object, limits), nrow=1)
             tmp <- new("baseline",
                         baseline = bgnd,
                         corrected = spectra - bgnd,
                         spectra = spectra,
                         call = match.call()
                       )
          },
          "3p.tougaard" = {
             limits<-object@Boundaries
             bgnd <- matrix(data=Tougaard3P(object, limits), nrow=1)
             tmp <- new("baseline",
                         baseline = bgnd,
                         corrected = spectra - bgnd,
                         spectra = spectra,
                         call = match.call()
                       )
          },
          "4p.tougaard" = {
             limits<-object@Boundaries
             bgnd <- matrix(data=Tougaard4P(object, Wgt, limits), nrow=1)
             tmp <- new("baseline",
                         baseline = bgnd,
                         corrected = spectra - bgnd,
                         spectra = spectra,
                         call = match.call()
                       )
             info <- as.character(Wgt)
          }

    )
    slot(object,"Baseline") <- list(x=object@RegionToFit$x, y=as.vector(tmp@baseline), baseline=tmp)
    object@Baseline$type <- c(bgtype, info)
    return(object)
  }
)


##==============================================================================
# x axis shift: class XPSCoreLine
##==============================================================================
###
#' @title XPSapplyshift
#' @description Apply the shift value to the X axis of a XPSCoreLine.
#'   If \code{shift} is NULL the x-axis is set to the original values.
#' @param object XPSCoreLine
#' @param shift X-shift value
#' @return 'XPSapplyshift' returns an XPSCoreLine energy shifted in any of its components 
#'   (RegionToFit, BaseLine, Fit Components, Best fit)
setGeneric("XPSapplyshift", function(object, shift=NULL) standardGeneric("XPSapplyshift"))
#' @title XPSapplyshift
#' @description method to energy shift an object of class 'XPSCoreLine'
#' @param object XPSCoreLine
#' @param shift X-shift value
#' @examples
#' \dontrun{
#'  test[["C1s"]] <- XPSapplyshift(test[["C1s"]], shift=0.3)
#' }
#' @export
#'
setMethod("XPSapplyshift", signature(object = "XPSCoreLine"),
          def=function(object, shift) {

     # If shift == NULL no shift applied to the coreline
     if ( is.null(shift) || !is.numeric(shift) ) {
     newshift <- -slot(object,"Shift")
     slot(object,"Shift") <- 0
     } else newshift <- shift

     # shift for original data
     object[[1]] <- object[[1]] + newshift

     # RegionToFit & Baseline
     if ( hasRegionToFit(object) ) object@RegionToFit$x <- object@RegionToFit$x + newshift
     if ( hasBaseline(object) ) object@Baseline$x <- object@Baseline$x + newshift

     # Fit Components
     for ( idx in seq_along(object@Components) ) {
         startMu <- getParam(object@Components[[idx]], variable = "mu")
         newstart <- startMu + newshift
         object@Components[[idx]] <- setParam(object@Components[[idx]],
                                              variable="mu",
                                              value = newstart
                                             )
     }

     if ( ! is.null(shift) ) slot(object,"Shift") <- newshift + slot(object,"Shift")

     return(object)
  }
)


##==============================================================================
# XPSremove: remove single step of processing
##==============================================================================
###
#' @title XPSremove Rests selected processing elements from XPSCoreLine
#' @description The function is designed to erase the content of selected slots: \cr \code{"all"} =
#'   it resets all the \code{slots}. This is the default value if \code{what} is
#'   missing. See examples. \cr \code{"fit"} = it resets the \code{Fit slot}. \cr
#'   \code{"components"} = it resets the \code{Components slot} indicated by the
#'   \code{number}. If \code{number} is missing then all the components will be
#'   reset.\cr \code{"baseline"} = it resets the \code{Baseline slot}. \cr
#'   \code{"regionToFit"} = it removes only and any link. \cr
#' @param object XPSCoreLine
#' @param what one of "all", "fit", "components", "baseline", "regionToFit"
#' @param number in case of \code{what='components'}: the component number will
#'   be reset,\cr if it is missing then all the components will be removed.
#' @return 'XPSremove' returns the object XPSCoreLine with erased slots.
#' @seealso \linkS4class{XPSCoreLine}
setGeneric("XPSremove",
     function(object, what=c("all", "fit", "components", "baseline", "regionToFit"), number)
     standardGeneric("XPSremove"))
#' @title XPSremove
#' @description method to erase the content of selected slots an object of class 'XPSCoreLine'
#' @param object XPSCoreLine
#' @param what one of "all", "fit", "components", "baseline", "regionToFit"
#' @param number in case of \code{what='components'}: the component number will
#'   be reset,\cr if it is missing then all the components will be removed.
#' @examples
#' \dontrun{
#'  test[["C1s"]] <- XPSremove(test[["C1s"]], "baseline")
#'  test[["C1s"]] <- XPSremove(test[["C1s"]], "all") # this reset everything
#' }
#' @export
#'
setMethod("XPSremove", signature(object="XPSCoreLine"),
  function(object, what=c("all", "fit", "components", "baseline", "regionToFit"), number) {

    ## Be careful to the sequence of removing for components: first the .Data slot then others
    type <- match.arg(what)
    switch(type,
       "all" = {
           slot(object, "Fit") <- list()
           slot(object, "Components") <- list()
           slot(object, "Baseline") <- list()
           slot(object, "RegionToFit") <- list()
           slot(object, "Boundaries") <- list()
       },
          "fit" = { 
           slot(object, "Fit") <- list()
       },

       "components" = {
           # remove fit
           slot(object, "Fit") <- list()
           if ( missing(number) ) {
               slot(object, "Components") <- list() # remove all components
               slot(object, "Fit") <- list()        # remove fit

           } else {                                  # remove only the selected component
               if ( number %in% seq_along(object@Components) ) {
                   slot(object,"Components") <- object@Components[-c(number)]
                   ## order the new components list
                   if (length(object@Components) >= 1) { #if Fit Component are still present...
                      object <- sortComponents(object)
                      LL<-length(object@Components)  #lenght of object after component substraction
                      for(ii in 1:LL){               #if single or all fit components are removed all links have to be removed
                         object@Components[[ii]]@link<-list()
                      }
                   }
               }
           }
       },
       "baseline" = {
           ## only x,y,RegionToFit
           slot(object, "Fit") <- list()
           slot(object, "Components") <- list()
           slot(object, "Baseline") <- list()
       },
           "regionToFit" = {
           ## only x,y
           slot(object, "Fit") <- list()
           slot(object, "Components") <- list()
           slot(object, "Baseline") <- list()
           slot(object, "RegionToFit") <- list()
       }
    )
    return(object)
  }
)

##==============================================================================
## XPSsetRSF: set the RSF for the core line only
##==============================================================================
###
#' @title XPSsetRSF set the value of the RSF
#' @description XPSsetRSF Set the RSF value of XPSCoreLine for quantification.
#' @param object XPSCoreLine
#' @param rsf RSF value
#'
setGeneric("XPSsetRSF", function(object, rsf=NULL) standardGeneric("XPSsetRSF"))
#' @title XPSsetRSF
#' @description method to set the RSF of an object of class 'XPSCoreLine'
#' @param object XPSCoreLine
#' @param rsf RSF value
#' @examples
#' \dontrun{
#'  test[["C1s"]] <- XPSsetRSF(test[["C1s"]], rsf=0.278)
#' }
#' @export
#'
setMethod("XPSsetRSF", signature(object="XPSCoreLine"),
  function(object, rsf=NULL){
    if (!is.null(rsf) && rsf != 0) {
       slot(object,"RSF") <- rsf
    } else {
    if ( slot(object,"RSF") == 0 ) {
       # with slot(object,"Symbol") get Symbol and Orbitals
       pattern <- c("[[:alpha:]]{1,2}")  # pattern composed only by letters
       if (object@Symbol=="survey" || object@Symbol=="Survey" || object@Symbol=="VB") { return(object) }  #definition of a baseline on the survey
       mpat <- regexpr(pattern, object@Symbol)
       # symbol element
       element <- regmatches(object@Symbol, mpat)
       # if element is ok ...
       if ( ElementCheck(element) ) {    #see XPSelement.r
           # orbital from Symbol
           orbital <- regmatches(object@Symbol, mpat, invert=TRUE)[[1]][2]
           OrbitalOK <- grep(orbital, c("1s", "2s", "3s", "4s", "5s", "6s", "2p", "3p", "4p", "5p", "6p", "3d", "4d", "5d", "4f")) #is "orbital" present in the list of possible orbitals?
           if (length(OrbitalOK)==0){
               cat("\n Unknown Element Orbital: RSF not set. Please check the Core Line Symbol")
               return(object)
           } else {
               # if Scienta: Flag[3] == TRUE modification 18/9/2012
               # but for older saved data length(Flags) = 2, then
               if (length(object@Flags) == 2) { slot(object,"Flags")[3] <- TRUE }

               # get RSF
               if (slot(object,"Flags")[3]){
                 analyzer <- "scienta"    # if Scienta
               } else {
                 analyzer <- "kratos"     # if Kratos
               }
               rsf <- getElementValue(element, orbital, analyzer, what="RSF") #vedi XPSElement.r
               LL <- length(unique(rsf)) # removes equal values: if LL > 1 multiple RSF value associated to the same coreline

#------ Check  
               if ( is.null(rsf) || is.na(prod(rsf)) || prod(rsf)==0 || LL > 1 ) { #prod() needed when length(rsf) > 1
                  rsf <- NA
                  RSFwin <- gwindow("SET RSF", visible=FALSE)
                  size(RSFwin)<- c(300,200)
                  RSFframe <- gframe("Display RSF table", container=RSFwin)
                  RSFgroup <- ggroup(container = RSFwin, horizontal=FALSE, label = "SET RSF")
                  glabel("WARNING: ZERO, UNDEFINED OR DIFFERENT RSF VALUES!", container=RSFgroup)
                  glabel("Please chose the RSF for the selected element orbital: ", container=RSFgroup)
                  Table <- showTableElement(element, analyzer)

                  Table[[1]] <- c(Table[[1]]," ")  #Number of Table rows is unknown. If Table has just 1 row
                  Table[[2]] <- c(Table[[2]]," ")  #formatting has no effect on GTABLE
                  Table[[3]] <- c(Table[[3]]," ")  #Then a row made just of spaces is added
                  Table[[4]] <- c(Table[[4]]," ")
                  Table[[5]] <- c(Table[[5]]," ")
                  Table[[1]] <- encodeString(Table[[1]], width=9)
                  Table[[2]] <- encodeString(Table[[2]], width=9)
                  Table[[3]] <- encodeString(Table[[3]], width=9)
                  Table[[4]] <- encodeString(Table[[4]], width=9)
                  Table[[5]] <- encodeString(Table[[5]], width=9)
                  names(Table) <- c("Element", "Orbital", "BE", "KE", "RSF")
                  Table <- as.data.frame(Table)
                  RSFTab <- gtable(Table,container=RSFgroup)
                  NewRSF <- gedit( initial.msg = "RSF = ?", container=RSFgroup)
                  gbutton("Save and Close", handler=function(h, ...){
                              NewRSF <- as.numeric(svalue(NewRSF))
                              if (is.na(NewRSF)) { 
                                gmessage("Give the RSF value please.", title="New RSF", icon="warning")
                              } else {
                                 rsf <<- NewRSF
                                 dispose(RSFwin)
                                 XPSSaveRetrieveBkp("save")
                              }
                         }, container=RSFgroup)
                  visible(RSFwin)<-TRUE
                  RSFwin$set_modal(TRUE)   # the program wait for an user answer

              } else {
                  rsf <- unique(rsf)
              } # end if ( is.null(rsf)
              object@RSF <- rsf       # set the RSF in the coreline slot
              N_comp=length(object@Components)
              if (N_comp > 0){
                 for(ii in 1:N_comp){  #coreline fit is present
                    object@Components[[ii]]@rsf <- rsf   #set the RSF of the fit components
                 }
              }
           } # if (length(orbital))
        } else {
           cat("Element not recognized! Please check Element Symbol")
        }    #end if ( ElementCheck(element)
      }      #end if ( slot(object,"RSF")
    }        #end if (!is.null(rsf)
    return(object)
  }
)


## =============================================================
#  XPScalc : computes the Element Concentrations in %
## =============================================================
###
#' @title XPScalc function to calculate the element concentrations
#' @description XPScalc Function to compute the integral intensity for a given XPS-CoreLine.
#'  Calculation of Integral Intensity of fitting components of a XPSCoreLine.
#' @param object XPSCoreLine
#' @param table Print table Logic
#' @return An ASCII file for each XPSCoreLine.
#' @seealso \link{write.table}, \link{write.csv}, \link{write.csv2}
setGeneric("XPScalc", function(object, table=TRUE) standardGeneric("XPScalc"))
#' @title XPScalc
#' @description method to compute the quantification of an object of class 'XPSCoreLine'
#' @param object XPSCoreLine
#' @param table Print table Logic
#' @examples
#' \dontrun{
#'  test[["C1s"]] <- XPScalc(test[["C1s"]], table=TRUE) #TRUE=prints the quantification
#' }
#' @export
#'
setMethod("XPScalc", signature(object="XPSCoreLine"),
  def=function(object, table=TRUE) {
    CPS <- object@Flags[2]
    E_stp<-abs(object@.Data[[1]][2]-object@.Data[[1]][1]) #energy step
    if ( CPS ) {
      SumofRTF <- sum(object@RegionToFit$y)*E_stp
      SumofBaseline <- sum(object@Baseline$y)*E_stp
    } else {
      SumofRTF <- sum(object@RegionToFit$y)*E_stp   #/abs(%NCore%_Param(6))
      SumofBaseline <- sum(object@Baseline$y)*E_stp #/abs(%NCore%_Param(6))
    }
    ## at least return sum of RTF and Baseline
    tmp <- list(RTF=SumofRTF, Baseline=SumofBaseline, Components=list())

#--- quantification

    if ( slot(object,"RSF") != 0 ) {
    ## if have components then sum of components
       if ( hasComponents(object) ) {
          tmp$lbl <- lapply( object@Components , slot, "label")  #component labels
          tmp$Components <- lapply(object@Components , function(x) {
                                     if ( CPS ) { sumcomp <- sum(x@ycoor)*E_stp-SumofBaseline }
                                     else { sumcomp <- sum(x@ycoor)*E_stp-SumofBaseline }
                                   }
                                  )

          tmp$RSF <- sapply(object@Components, slot, "rsf")
          tmp$BE <- sapply(object@Components, function(x) x@param["mu", "start"])
          ## if we have components then each component has its own RSF
          tmp$quant <- mapply( function(j, k) j/k, tmp$Components, tmp$RSF, SIMPLIFY=FALSE )
       }
       ## if we have only baseline take the object RSF
       else {
          tmp$RSF <- slot(object,"RSF")
          tmp$quant <- (tmp$RTF - tmp$Baseline)/tmp$RSF  # correction for the sensitivity factor
       }
       ## set symbol
       tmp$Symbol <- slot(object,"Symbol")
    }
    ## end of quantitative section

    ## cat ("\n | Components |Area(cps) | BE(eV) | Height | Fwhm | MixGL | Asym | % TOT.|")
    ## TABLE
    if ( table ) {
       cat ("\n   Core Line : ",object@Symbol )
       cat ("\n ==============================================")
       if ( hasComponents(object) ) {
            cat ("\n | Components |  Area(cps) | BE(eV) |  % TOT. |")
            cat ("\n ==============================================\n")
            sumComp<-sum(unlist(tmp$quant))

            bho <- sapply(seq_along(tmp$Components) , function(j) {
                           cname <- sprintf(" | %-11s|", tmp$lbl[j])
                           carea <- sprintf("%10.2f |", tmp$Components[j])
                           Xcenter <- sprintf("%7.2f|", tmp$BE[j]) # mu == secondo valore sempre!!
                           conc <- sprintf("%6.2f", as.numeric(tmp$quant[j])*100/sumComp)
                           cat(cname,carea,Xcenter,conc,"%|\n")
                         }
                       )

            cat (" ==============================================")
            csum <- sprintf ("%11.2f",sum(unlist(tmp$Components)))
            percsum <- sprintf (" %16.2f", sum(unlist(tmp$quant)*100/sumComp))
            cat("\n | Sum of Comp", csum, percsum,"%|")
       }
       cat(sprintf ("\n | SumRSFcorr  %11.2f%21s",tmp$RTF,"|"))
       cat(sprintf ("\n | Bgnd        %11.2f%21s",tmp$Baseline,"|"))
       cat(sprintf ("\n | Sum-Bgnd    %11.2f%21s",tmp$RTF-tmp$Baseline,"|"))
       cat ("\n ==============================================\n")
    }
    invisible(tmp)
  }
)


# ----- Utilities for XPSCoreLine--------


##==============================================================================
## Control on Package to process old Rxps.Rdata files
##==============================================================================
###
#' @title XPSpkgCtrl
#' @description XPSpkgCtrl Function to control the attributes of an XPSCoreLine.
#' @param object XPSCoreLine object
#' @return 'XPSpkgCtrl' returns an object of class 'XPSCoreLine' with attribute '.GlobalEnv'
setGeneric("XPSpkgCtrl", function(object) standardGeneric("XPSpkgCtrl"))
#' @title XPSpkgCtrl
#' @description method to control the attribute 'package' of an object of class 'XPSCoreLine'
#' @param object a Core_Line object of class \code{XPSCoreLine}
#' @examples
#' \dontrun{
#'  test[["C1s"]] <- XPSpkgCtrl(test[["C1s"]])
#' }
#' @export
#'
setMethod("XPSpkgCtrl", signature(object = "XPSCoreLine"),
   def=function(object){
#set the Package attribute of coreline to .GlobalEnv
      attr(class(object), "package") <- ".GlobalEnv"
#check if Baseline is present set the Formal class 'baseline' to [package ".GlobalEnv"]
      if(hasBaseline(object)){
         if (! is.null( attr(class(object@Baseline[[3]]), "package") )){
             if (attr(class(object@Baseline[[3]]), "package") == "baseline"){
                 attr(class(object@Baseline[[3]]), "package") <- ".GlobalEnv"
             }
         }
         CompNames <- names(object@Components)  #It may happen that FitComponents attribute(package)== Rxps
#set the Package attribute of the FIT Components to .GlobalEnv
         for (ii in seq_along(CompNames)){
              attr(class(object@Components[[ii]]), "package") <- ".GlobalEnv"
         }
      }
      return(object)
   }
)


# ----- Graphics for XPSCoreLine--------

##==============================================================================
# Application of function PLOT to objects of class XPSCoreLine
##==============================================================================
###
#' @title 'plot' function to plot XPSCoreLine or XPSSample objects
#' @description S4method 'plot' function to plot objects of class XPSCoreLine or XPSSample objects.
#'   The normal way to plot \code{XPS} objects is to use the \code{plot} method.
#'   In the case of \code{XPSCoreLine} object it is a wrapper for
#'   \code{\link{matplot}}. The data are transformed to \code{matrix} for \code{\link{matplot}}
#'   unprocessed CoreLine. For processed one it plots the RegionToFit defined by
#'   the Boundaries extremes, the baseline, any FitComponent and Fit if they are present.
#' @param x The \code{XPS} numeric matrix containing a XPS CoreLine to be plotted
#' @param y Xdata, Ydata are contained in the XPSCoreLine object
#' @seealso \code{\link{matplot}}, \code{\link{par}}
#'
setGeneric("plot", function(x="XPSCoreLine", y="missing") standardGeneric("plot")) #variables must be x, y, ... coherently with plot defined in R
#' @title plot
#' @description method to plot objects of class XPSCoreLine or XPSSample
#' @param x The \code{XPS} numeric matrix containing a XPS CoreLine to be plotted
#' @param y (not used) Xdata, Ydata are contained in the XPSCoreLine object
#' @param type character "l", "p", "b" for line, points or both
#' @param ltype character, "solid", "dashed"... pattern of the line 
#' @param color character, "black", "red", "green"... color for the data to be plotted
#' @param main character, title of the plot
#' @param xlim numeric, range(X) limits of the X-data
#' @param ylim numeric, range(Y) limits of the Y-data
#' @param labels logical TRUE to plot axis numbers
#' @param xlab character "X axis label"
#' @param ylab character "Y axis label"
#' @param ... additional parameters for plot function, see par()
#' @aliases missing
#' @examples
#' \dontrun{
#'  plot(test[["C1s"]]) #plot the XPSCoreline
#' }
#' @export
#'
setMethod("plot", signature(x="XPSCoreLine", y="missing"),

  function(x,
           type = "l",
           ltype = "solid",
           color = "black",
           main = x@Symbol,
           xlim = NULL,
           ylim = NULL,
           labels = TRUE,
           xlab=x@units[1],
           ylab=x@units[2],
           ...
           ){
       TestName <- x@Symbol
       assign("MatPlotMode", TRUE, envir=.GlobalEnv)  #basic matplot function used to plot data
       X <- NULL
       X <- setAsMatrix(from=x, to="matrix")  
       if ( is.null(ylim) ) {
          ylim <- sort(range(X[,2]))
       }
       if ( is.null(xlim) ) {
          xlim <- sort(range(X[,1]))
       } else {
          xlim <- sort(xlim) #xlim is the minimum X range
          X <- subset(X, subset = ( X[,1] >= xlim[1] & X[,1] <= xlim[2] ))
       }
       XX <- X[,1]  ## x-axis vector first column
       YY <- X[,-1] ## y values matrix: it is the X matrix without the abscissas

       if ( x@Flags[1] ) { xlim <- rev(xlim) } ## reverse x-axis

       patt <- paste("\U0394.D.", 1, sep="")  #patt == Delta.D.1.

#---- instructions to plot generic fitted corelines
#---- colors of Baseline, Fit Components and Fit  (modified Giorgio2013)
       NcolY <- dim(YY)[2]  #The second component of DIM = N columns
       if (length(NcolY) > 0 && NcolY == 2) {  #NcolY == 2, YY represents Baseline only
           color <- c("black", "sienna")
       }
       NComp <- length(x@Components)
       color <- c("black", "sienna", rep("blue", NComp), "red")   #Spectrum in black, background in sienna, FitComponents in blue, EnvelopComponents in red
       #------------------------------
       matplot(x=XX,
       y=YY,
       type=type,
       lty=ltype,
       col=color,
       xlim=xlim,
       ylim=ylim,
       main=main,
       xlab=xlab,
       ylab=ylab,
       ... )
       ## components label
       if ( hasComponents(x) && labels) {
          #  positions <- getMaxOfComponents(x)  #works only for Fit Components but not in the case of VBTop
          position <- list(x=NULL, y=NULL)
          LL <- length(x@Components)
          RngX <- range(x@RegionToFit$x)
          for(ii in 1:LL){                    #Control mu != NA  (see linear fit in VBTop
             position$x <- x@Components[[ii]]@param["mu", "start"]
             if (is.na(position$x)==FALSE){   #in VBtop Lin Fit there is not a value for mu
                if (position$x <= max(RngX) && position$x >= min(RngX)){   #Lab only if inside X-range
                   position$y <- findY(x@RegionToFit, position$x)
                   #labformula defined in XPSUtilities.r
                   FitCompLbl(position.list=position, label=x@Components[[ii]]@label, cex=1.0, pos=4, offset=0.1 ) #draws component labels
                }
             }
          }
       }
#---- check if the CoreLine is of type 'VBtop', 'VBFermi' or 'Derivative'
#     and plots marker points instead of the best fit
       # if it is a SPECIAL CoreLine print Markers (VBt, VBf, MaxMinD)
       if(TestName == "VBt" || TestName == "VBf" || grepl(patt, TestName)==TRUE){
          pos <- list(x=NULL, y=NULL)
          TestName <- NULL
          TestName <- sapply(x@Components, function(z) c(TestName, z@funcName))
          if(length(idx <- grep("VBtop", TestName)) > 0){
#--- plot VB Top position
             pos$x <- x@Components[[idx]]@param["mu", "start"]
             pos$y <- x@Components[[idx]]@param["h", "start"]
          } else if (length(idx <- grep("VBFermi", TestName)) > 0){
#--- plot VB Fermi position
             pos$x <- x@Components[[idx]]@param["mu", "start"]
             pos$y <- x@Components[[idx]]@param["h", "start"]
          } else if (length(idx <- grep("Derivative", TestName)) > 0){
#--- plot derivative Max,Min positions
             pos$x <- c(pos$x, x@Components[[idx]]@param["mu", "min"],
                       x@Components[[idx]]@param["mu", "max"])
             pos$y <- c(pos$y, x@Components[[idx]]@param["h", "min"],
                       x@Components[[idx]]@param["h", "max"])
          }
          points(pos$x, pos$y, col="orange", cex=3, lwd=2, pch=3)
       }
    }
)

## =====================================================
## Residuals plot with layout & plot
## =====================================================
###
#' @title 'XPSresidualPlot' function for objects of class 'XPSCoreLine'
#' @description XPSresidualPlot Function to plot data and the fit together with the residual difference.
#' @param object #XPSCoreLine object
#'
setGeneric("XPSresidualPlot", function(object)  standardGeneric("XPSresidualPlot"))
#' @title 'XPSresidualPlot' method for objects of class 'XPSCoreLine'
#' @description method to plot best fit and data of an object of class 'XPSCoreLine adding residuals
#' @param object #XPSCoreLine object
#' @examples
#' \dontrun{
#'  XPSresidualPlot(test[["C1s"]]) #add residuals when plotting the XPSCoreline Fit
#' }
#' @export
#'
setMethod("XPSresidualPlot", signature(object="XPSCoreLine"),
  function(object) {

     if ( length(object@Fit$fit) == 0 )
     stop("\n No fit available for residual plot\n")
     def.par <- par(no.readonly = TRUE)
     #cannot use mfrow because the panel of FitResiduals has dimension different from coreline panel
     layout(matrix(c(1,2),nrow=2,ncol=1,byrow=TRUE), heights=c(0.9,4), TRUE)
     par(mar=c(3,3,1,1), las=1) #margins dimension
     #the two plot() calls will display two windows with height 0.9 e 4
     plot(object@RegionToFit$x,
          residuals(object@Fit$fit),
          type="l",
          xlim=rev(range(object@RegionToFit$x)),
          xlab="", ylab="",
          cex.axis=0.8,
          panel.first=grid(),
         )
     mtext("Residuals", 3, 0)

     plot(object)
     grid()
     par(mfrow=c(1,1))   #reset the panel to one graphic window
     par(mar=c(5,4,4,2), las=1) 
  }
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##==============================================================================
# Definition of class XPSSample
##==============================================================================
###
#' @title Class "XPSSample"
#' @description The package provides classes for XPS spectra (class \code{XPSSample}) and
#'   collections of such lists (class \code{XPSCoreLine}). \code{XPSSample} are
#'   energy-intensity value pairs stored in a two column \code{list} and several
#'   additional parameters stored in slots.
#'   Objects from the Class: Objects can be created by calls of the form \code{new("XPSSample", data, Project, Sample, Comments, User, Filename)}.
#' @examples
#' \dontrun{
#' SiOx <- new("XPSSample",info="test",experiment="Si-Ti Waveguides")
#' }
#' @export
#'
setClass("XPSSample",
            representation(
                     Project="character",
                     Sample="character",
                     Comments="character",
                     User="character",
                     Filename="character",
                     names="character"),
            contains="list",
            prototype(
                     Project="",
                     Sample="", 
                     Comments="", 
                     User="",
                     Filename="")
        )

##==============================================================================
## Methods  concerning Class=XPSSample
##==============================================================================
#' @title initialize
#' @description define a method initialize for objects of class 'XPSSample'
#' @param .Object an object of class 'XPSSample'
#' @param data a list containing x, y original data
#' @param Project a character string containing information about the experiment
#' @param Sample a character string containing information about the analyzed sample
#' @param Comments a character string containing information about the experiment
#' @param User a character string containing information about the operator
#' @param Filename name of the data_file 
#' @examples
#' \dontrun{
#' SiOx <- initialize(.Object)
#' }
#' @export
#'
setMethod("initialize", signature(.Object="XPSSample"),
   function(.Object, data, Project, Sample, Comments, User, Filename){
     if( missing(data) ) {
        .Object@.Data <- vector("list",0)
     } else {
        .Object@.Data <- data
     }
     if(!missing(Project)) {
         .Object@Project <- Project
     }
     if(!missing(Sample)) {
         .Object@Sample <- Sample
     }
     if(!missing(Comments)) {
         .Object@Comments <- Comments
     }
     if(!missing(User)) {
         .Object@User <- User
     }
     if(!missing(Filename)) {
         .Object@Filename <- Filename
     }
     return(.Object)
  }
)

##==============================================================================
## Extract a subset from an object of class XPSSample
##==============================================================================
#' @title get subset of XPSSample
#' @description get subset of XPSSample
#' @param x the \code{XPS} object of class XPSSample to be generated
#' @param i numeric parameter corresponding to the number of spectra
#' @param j dummy numeric parameter
#' @param ... additional parameters
#' @param drop logical by default FALSE
#' @return XPSSample
#' @export
#'

setMethod("[", "XPSSample",
  def = function(x, i, j, ..., drop = FALSE) {

     newlist <- as(x,"list")[i]      #definition of the XPSSample structure
     y <- new("XPSSample",           #new() generates a new object of class XPSSample
              data=newlist,
              Project=x@Project,
              Sample=x@Sample,
              Comments=x@Comments,
              User=x@User,
              Filename=x@Filename
             )

     y@names <- names(x)[i]
     return(y)
  }
)

##=========================================================
## combines an object of class XPSSample
## with another XPSSample dataset
##=========================================================
#' @title combine
#' @description define a method to combine two XPSSamples
#' @param x object of class XPSSample to be combined
#' @param ... additional parameters
#' @return XPSSample
#' @examples
#' \dontrun{
#'  XPSSampTot <- c(XPSSample1, XPSsample2)
#' }
#' @export
#'
setMethod("c","XPSSample",
   def = function(x, ... ) {

      A <- as(x,"list")
      if (inherits(..., "XPSSample")) {
         B <- as(...,"list") }
      else B <- list(...)

      y <- new("XPSSample",            #new("XPSSample") generates a new structure of class XPSSample
                data=c(A,B),
                Project=x@Project,
                Sample=x@Sample,
                Comments=x@Comments,
                User=x@User,
                Filename=x@Filename
              )

      y@names <- c(names(x), names(...))
      return(y)
   }
)

##=========================================================
# shows the XPS-Sample content
##=========================================================
#' @title show
#' @description show method to print details of objects of class XPSSample
#' @param object XPSSample
#' @examples
#' \dontrun{
#'  show(XPSSample.RData)
#' }
#' @export
#'
setMethod("show", signature=("XPSSample"),
   def=function(object) {

     ## header
     main <- paste(" An object of class", class(object))
     cat(rep("-",30),"\n",sep="")
     cat(main,"\n")
     cat(rep("-",30),"\n",sep="")

     categories <- c(" Filename",
                    " Project",
                    " Sample",
                    " Comments",
                    " User",
                    " Core lines"
                   )

     CorelineNames <- names(object)
     if (nlevels(as.factor(CorelineNames)) != length(CorelineNames)) {
         All_names <- paste(CorelineNames, seq_along(CorelineNames), sep="#")
     }
     else { All_names <- CorelineNames }

     values <- list(
                   object@Filename,
                   object@Project,
                   object@Sample,
                   object@Comments,
                   object@User,
                   All_names
                  )
     ## print info
     categories <- format(categories, justify="left")
     for (indx in seq_along(categories)) {
         cat(categories[indx], ":", values[[indx]], "\n", sep=" ")
     }
     ## loop on CoreLines
     temp <- sapply(object, show)
   }
)

##==============================================================================
## Energy shift: Method to apply energy shift to XPSSample corelines
##==============================================================================
#' @title 'XPSapplyshift'
#' @description definition of method to apply Energy Shifts to objects of class XPSSample
#'   Apply the shift value to the Binding axis. If \code{shift} is NULL then
#'   the x-axis will be reset to the original values.
#' @param object XPSSample
#' @param shift shift value
#'
#' @examples
#' \dontrun{
#' test <- XPSapplyshift(test, shift=0.3 ) #apply shift =0.3 to the  test XPSSample
#' }
#' @export

setMethod("XPSapplyshift", signature(object = "XPSSample"),
   def = function(object, shift) {

      object@.Data <- lapply(object, XPSapplyshift, shift)  #here the object if of class CoreLine and then applies
      return(object)                                        #XPSapplyShift defined for objects of class CoreLine
   }
)


##=============================================================
##  XPSquantify : manage the output in the R consolle
##=============================================================
#' @title 'XPSquantify'
#' @description compute the element quantification for the selected XPSSample
#' @param object XPSSample
#' @param without character vector of coreline names which will be not used in the quantification
#' @param verbose if TRUE prints the quantification table
#'
setGeneric("XPSquantify", function(object, without=NULL, verbose=TRUE)  standardGeneric("XPSquantify"))
#' @title 'XPSquantify'
#' @description method compute the element quantification of objects of class 'XPSSample'
#' @param object XPSSample
#' @param without character vector of coreline names which will be not used in the quantification
#' @param verbose if TRUE prints the quantification table
#' @examples
#' \dontrun{
#'  QuantData <- XPSquantify(test, without=NULL, verbose=TRUE ) #computes the  quantification for the  test XPSSample
#' }
#' @export

setMethod("XPSquantify", signature(object = "XPSSample"),
  def=function(object, without=NULL, verbose=TRUE) {

     ## loop on corelines possessing Baseline and RSF >0
     idx <- sapply( object, function(x) {
                      return( hasBaseline(x) & ifelse(slot(x,"RSF") !=0, TRUE, FALSE) )
                    }
                  )

     ## without a sequence of lines
     if (!is.null(without)){ for (cl in without) {idx[cl] <- FALSE} }

     # Quantitative with RSFs
     QuantData <- lapply(object[idx], function(x) {
                            return(XPScalc(x, table=FALSE))
                          }
                        ) # end loop
     # Total of quatitative

     TotQuant <- sum(unlist(sapply(QuantData, function(x) x$quant)))
     if (verbose) {
         cat ("\n   Sample name: ",slot(object,"Filename"))
         cat ("\n -----------------------------------------------------")
         cat ("\n | Core lines |  RSF | Area (cps) | BE(eV) |  Conc.  | ")
         cat ("\n -----------------------------------------------------\n")
         ## loop on QuantData
         bho <- sapply(QuantData, function(x) {
             if ( ! length(x$Components) ) {
                cat(sprintf(" | %-10s |%5.2f | %10.2f |%8s|%6.2f %1s |", x$Symbol, x$RSF[1], (x$RTF-x$Baseline), "      ",x$quant/TotQuant*100, "%", "|"), "\n")
             } else {
                cat(sprintf(   " | %-10s                                %5.2f %1s|", x$Symbol, sum(unlist(x$quant))/TotQuant*100, "% ") ,"\n")
                sapply( seq_along(x$Components) , function(j) {
                        cat(sprintf(" |     %-7s|%5.2f | %10.2f |%7.2f |%6.2f %1s |", paste("#",j,sep=""), x$RSF[j], x$Components[j], x$BE[j], x$quant[[j]]/TotQuant*100,"%"), "\n")
                      } )
             }
         })
         cat (" =====================================================\n")
     }
     return(invisible(QuantData))
  }
)


##==============================================================================
# Application of 'Control on Package attributes' to the class XPSSample
##==============================================================================
#' @title 'XPSpkgCtrl'
#' @description method to controls the XPSSamples attributes
#' @param object XPSSample
#' @examples
#' \dontrun{
#'  test <- XPSpkgCtrl(test) #controls the  attributes of the  test XPSSample
#' }
#' @export
#'
setMethod("XPSpkgCtrl", signature(object = "XPSSample"),
   def = function(object) {
      attr(class(object), "package")<-".GlobalEnv"  #also the class XPSSample could have an attribute package=Rxps
      object@.Data <- lapply(object, XPSpkgCtrl)    #apply XPSpkgCtrl() to all the corelines of the XPSSample
      cat("\n XPSpkgCtrl DONE! \n\n")
      return(object)
  }
)


# ----- Graphics for XPSCoreLine--------

##==============================================================================
# Application of 'plot' to the class XPSSample
##==============================================================================
#' @title 'plot'
#' @description function plot for objects of class 'XPSSample'
#' @param x XPSSample
#' @param y (not used) Xdata, Ydata are contained in the XPSSample object
#' @param ...  further parameters to the plot function

setGeneric("plot", function(x="XPSSample", y="missing") standardGeneric("plot"))  #variables must be x, y, ... coherently with plot defined in R
#' @title 'plot'
#' @description definition of method 'plot' for objects of class 'XPSSample'
#' @param x XPSSample
#' @param y (not used) Xdata, Ydata are contained in the XPSSample object
#' @param ...  further parameters to the plot function
#' @aliases missing
#' @examples
#' \dontrun{
#' plot(test)
#' }
#' @export
#'
setMethod("plot", signature(x="XPSSample", y="missing"),
   function(x, ... ) {
      assign("MatPlotMode", TRUE, envir=.GlobalEnv)  #basic matplot function used to plot data

      ### set the title for the graphic window
      WinSize <- as.numeric(XPSSettings$General[4])
      Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
      Gdev <- strsplit(Gdev, "title")
      Gdev <- paste(Gdev[[1]][1], ", title='",activeFName,"', width=", WinSize, ", height=", WinSize, ")", sep="")  #add the correct window title and Win-Size
      ### nreset graphic window
      graphics.off() #switch off the graphic window
      eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON

      ###now prepare to plot a max number of 12 spectra
      rowXcol <- list(product= c(1, 2, 4, 6, 9, 12),
                  nrow=c(1,1,2,2,3,3),
                 ncol=c(1,2,2,3,3,4))
      idx <- min(which (rowXcol$product >= min(length(x),12)))
      op <- par(mfrow=c(rowXcol$nrow[idx], rowXcol$ncol[idx]))   #set the plot with N panels orgaized in $nrow rows and $ncol columns
      if (length(x) > 12) {
            x <- x[1:12]
            warning("Only the first 12 XPSCoreLines are shown.")
      }
      ### plot(XPSSample) calls plot(CoreLine)  via lapply()     
      par(type="l", lty=1)
      tmp <- lapply(x, plot, ...)
      par(mfrow=c(1,1))  #set single panel figure for CoreLine or generic data plots
   }
)






#XPSImport.Ascii GUI to make reading/loading ascii file easy

#' @title XPSImport.Ascii function
#' @description Function allowing import of textual (ascii) file data
#'   Options are available to account for header and data separator
#'   Options are also provided to store data in an object of class 'XPSSample'
#' @return XPSImport.Ascii returns and object of class 'XPSSample' with
#'   data loaded into a XPSCoreLine slot
#' @examples
#' \dontrun{
#'  XPSImport.Ascii()
#' }
#' @export
#'

XPSImport.Ascii <- function() {
    options(guiToolkit = "tcltk")

#--- warnings if required selections are lacking
    check_selection <- function(){
         if (nchar(svalue(ColX)) * nchar(svalue(ColY))==0) {  #X and Y columns must be indicated
              gmessage(msg="Please give ColX and ColY to be imported", title="WARNING: column lacking", icon="warning")
              return(FALSE)
         }
         if ( ! (svalue(reverseX) || svalue(NOreverseX))) {   #one of reverseX or NOreverseX should be SELECTED
              gmessage(msg="Reverse X axis? Plsease check!", title="WARNING X axis!", icon="warning")
              return(FALSE)
         }
         if ( nchar(svalue(CLname))==0) {   #Column name must be indicated
              gmessage(msg="Core Line Name please!", title="WARNING Core Line Name", icon="warning")
              return(FALSE)
         }
         return(TRUE)
      }

#--- read data from file
      Read_Data <- function(...) {
          opt <- svalue(OptLayout)
          Nrws <- as.numeric(svalue(NRowHeader))
          scf <- svalue(scanFile)
          if (scf==FALSE){
             Data <- read.table(file=FNameIN,sep=seps[opt$Separator],dec=decs[opt$Decimal],
                          skip=Nrws, colClasses="numeric", fill=TRUE )
          } else {
             fp <- file(FNameIN, open="r")
             Ncol <- as.numeric(svalue(dataCol))
             tmp <- "###" #just to make length(tmp) > 0
             Data <- NULL   #ora leggo i dati
             while (length(tmp)>0) {
                 tmp <- scan(fp, what="character", n=Ncol, quiet=TRUE)
                 tmp <- sub(", ", "  ", tmp)   #changes separation "," with " ": for data  1, 2,143, 5,723  generates  1  2,143  5,723
                 tmp <- sub(",", ".", tmp)     #changes decimal "," with ".": for data  1  2,143  5,723  generates  1  2.143  5.723
                 if (is.na(as.numeric(tmp))) break #stop reading if there are characters which cannot translated in numbers
                 Data <-  rbind(Data,as.numeric(tmp))
             }

          }
          DataIN[] <- Data
          invisible(Data)
      }

#--- Add a new XY data in a New CoreLine in an existing XPSSample
      addCoreLine <- function(){

              Xidx <- as.numeric(svalue(ColX))
              Yidx <- as.numeric(svalue(ColY))
              DF <- as.list(Read_Data())     #transform data.frame in a list
              DF[[Xidx]] <- na.omit(DF[[Xidx]])  #if present remove NA from read data
              DF[[Yidx]] <- na.omit(DF[[Yidx]])
              LL <- length(DF[[Xidx]])
              if (svalue(reverseX) && DF[[Xidx]][1] < DF[[Xidx]][LL]) { #reverse X axis selected but X is ascending ordered
                 answ <- gconfirm(msg="X is in ascending order. Do you want to reverse X axis? ", title="CONFIRM REVERSE AXIS", AICON="WARNING")
                 if (answ == TRUE ){
                    DF[[Xidx]] <- rev(DF[[Xidx]]) #reverse X in descending order
                    DF[[Yidx]] <- rev(DF[[Yidx]]) #reverse Y in descending order
                 } else {
                    svalue(reverseX) <- FALSE
                    svalue(NOreverseX) <- TRUE
                 }
              }
              LL <- length(XPSSample)+1
              NewCL <- new("XPSCoreLine",
                    .Data = list(x = DF[[Xidx]], y = DF[[Yidx]], t=NULL, err=NULL),   #err is dedicated to standard errors on Y data
                    units = c(svalue(unitX), svalue(unitY)),
                    Flags = c(svalue(reverseX), TRUE, FALSE, FALSE),
                    Symbol= svalue(CLname)
                   )
              CLnames <- names(XPSSample)
              XPSSample[[LL]] <<- NewCL
              names(XPSSample) <<- c(CLnames, as.character(svalue(CLname)))
              plot(XPSSample)
       }

#--- Add a new XY data in a New CoreLine in an existing XPSSample
      addErrors <- function(){
              if (length(svalue(ErrY)) == 0) {
                  gmessage(msg="PLEASE SELECT THE ERR-Y COLUMN", title="Err-Y column Lacking", icon="warning")
                  return()
              }
              Xidx <- as.numeric(svalue(ColX))
              Yidx <- as.numeric(svalue(ColY))
              Erridx <- as.numeric(svalue(ErrY)) 
              DF <- Read_Data()
              LL <- length(DF[[Xidx]])
              if (svalue(reverseX) && DF[[Xidx]][1] < DF[[Xidx]][LL]) { #reverse X axis selected but X is ascending ordered
                 answ <- gconfirm(msg="X is in ascending order. Do you want to reverse X axis? ", title="CONFIRM REVERSE AXIS", AICON="WARNING")
                 if (answ == TRUE ){
                    DF[[Xidx]] <- rev(DF[[Xidx]]) #reverse X in descending order
                    DF[[Yidx]] <- rev(DF[[Yidx]]) #reverse X in descending order
                 } else {
                    svalue(reverseX) <- FALSE
                    svalue(NOreverseX) <- TRUE
                 }
              }
              LL <- length(XPSSample)
              XPSSample[[LL]]@.Data[[4]] <<- DF[[Erridx]]
              gmessage("Y-ERRORS LOADED. USE CUSTOMPLOT TO DRAW DATA+ERRORS", title="Plot data", icon="warning")
#              svalue(ErrY) <- ""
              plot(XPSSample, col="blue")
       }


#--- Variables ---

       FNameIN <- NULL
       FName <- NULL
       XPSSample <- NULL
       activeFName <- NULL

#--- Widget definition ---
       ImportWin <- gwindow("Import Ascii Data", parent(50, 10), visible=FALSE)
       size(ImportWin) <- c(300, 400)
       MainGroup <- ggroup(horizontal=FALSE, spacing = 1, container = ImportWin)
       ImportGroup <- ggroup(horizontal=TRUE, container = MainGroup)
       OptGroup <- ggroup(horizontal=FALSE, spacing = 1, container = ImportGroup)
       WoutGroup <- ggroup(horizontal=FALSE, spacing = 1, container = ImportGroup)


#--- Import options ---
       read.opt <- gframe(text=" Import Options ", horizontal = FALSE, spacing=5,  container = OptGroup)
       LoadButt <- gbutton("Open Data File", handler=function(h,...){
                           FNameIN <<- gfile(text = "Select a file ...", type = "open",
                                             filter = list("Ascii files" = list(patterns = c(".asc",".txt", ".prn", ".dat"))),
                                             multi = FALSE)
                           activeFName <<- basename(FNameIN)
                           pathFile <- dirname(FNameIN)
                           XPSSample <<- new("XPSSample",
                                            Project = " ",
                                            Comments = " ",
                                            User=Sys.getenv('USER'),
                                            Filename=activeFName )
                           setwd(pathFile)
                           svalue(raw_input) <-  paste(readLines(FNameIN), collapse="\n")
                           enabled(import_btn) <- TRUE
#                           enabled(addErr_btn) <- FALSE
                           enabled(save_btn) <- FALSE
                           enabled(AddToXPSSamp) <- FALSE
                           enabled(exit_btn) <- FALSE
                        }, container=read.opt)

#--- read options
       HeaderLayout <- gformlayout(container = read.opt)

       heading <- gradio(c("Yes", "No"), horizontal=TRUE, label="Header", selected = 2,  handler=function(h, ...){
                             answ <- svalue(heading)
                             if (answ == "Yes") {
                                svalue(NRowHeader) <- "1"
                                enabled(NRowHeader)  <-  TRUE
                              } else if (answ == "No") {
                                svalue(NRowHeader) <- "0"
                                enabled(NRowHeader) <- FALSE
                             }
                        }, container = HeaderLayout)
       NRowHeader <- gedit(text="0", label="Header Rows", container=HeaderLayout)
       enabled(NRowHeader) <- FALSE
       gseparator(container = read.opt) # separator

       OptLayout <- gformlayout(container = read.opt)

       seps <- c("Tab"="\t", "Whitespace"=" ", "Comma" = ",", Semicolon=";", "Unspecified"="")
       gcombobox(names(seps), selected=1, label="Separator", container = OptLayout)

       decs <- c("Period"=".", "Comma"=",")
       gcombobox(names(decs), label="Decimal", container = OptLayout)

       quotes  <- c("No quote" = "", "Double quote (\")" = '"', "Single quote (')" = "'")
       gcombobox(names(quotes), label="Quote", container = OptLayout)

       gseparator(container = read.opt) # separator
       FmtGroup <- gpanedgroup(expand=TRUE, horizontal=TRUE,container = read.opt)
       scanFile <- gcheckbox(text="Scan DataFile", checked = FALSE, handler=function(h, ...){
                            scf <- svalue(scanFile)
                            if (scf==TRUE) {
                               enabled(dataCol) <- TRUE
                            } else {
                               enabled(dataCol) <- FALSE
                            }
                        }, container = FmtGroup)
       dataCol <- gedit(initial.msg="Data Ncol", container=FmtGroup)
       enabled(dataCol) <- FALSE

#--- Try to read data with selected options and unpdate OUTPUT data gtable to see if reading options are correct
       gbutton("Try to Read Data", spacing=5, handler=function(h, ...) Read_Data(), container = read.opt)

#---  Asci data Information to store Ascii_Data in a XPSSample DataFrame
       Elementframe <- gframe(text=" XPS Core Line ", spacing=5, horizontal = FALSE, container = OptGroup)
       CLparam <- gformlayout(container = Elementframe)
       CLname <- gedit("", initial.msg=" Core Line Name", container = CLparam, label="Core Line Name")
       unitX <- gedit("Binding Energy [eV]", container = CLparam, label="X Scale")
       unitY <- gedit("Intensity [counts]", container = CLparam, label="Y Scale")
       CLyesno <- ggroup( horizontal=TRUE, container = Elementframe)
       glabel(text="Reverse X Axis?", container = CLyesno)
       reverseX <- gcheckbox("Yes", selected=FALSE, handler=function(h, ...){
                    svalue(NOreverseX) <- FALSE
                    enabled(import_btn) <- TRUE
                 }, container = CLyesno)
       NOreverseX <- gcheckbox("No", selected=FALSE, handler=function(h, ...){
                    svalue(reverseX) <- FALSE
                    enabled(import_btn) <- TRUE
                 }, container = CLyesno)


#--- define INPUT Window
       InputGroup <- ggroup(horizontal=FALSE, container = WoutGroup)
       WinIn <- gvbox(expand=TRUE, fill=TRUE, spacing=5, container = InputGroup)
       labtxt <- glabel(gettext("Input data: "), container = WinIn, anchor=c(-1,0))
       font(labtxt) <- list(weight="bold") #list(family = "helvetica", size="12", weight="bold", style="italic")
       raw_input <- gtext("", wrap=FALSE,  
                   font.attr=list(family="monospace"),
                   container=WinIn, expand=TRUE, fill=TRUE)
       size(raw_input) <- c(230,150)

#--- define LOAD Window
       WinOUT <- ggroup(horizontal=FALSE, spacing=5, container =InputGroup )
       labtxt <- glabel("Loaded data: ", container = WinOUT)
       font(labtxt) <- list(weight="bold")
       DataIN <- gtable("", container = WinOUT)
       size(DataIN) <- c(230,150)

#--- Which column to read?
       Colframe <- gformlayout(container = WoutGroup)
       ColX <- gedit(initial.msg="1", label="  X-Col to read", spacing=2, container = Colframe)
       ColY <- gedit(initial.msg="2", label="  Y-Col to read", spacing=2, container = Colframe)
       ErrY <- gedit(initial.msg="?", label="Err-Col to read", spacing=2, container = Colframe)

#--- Import info
       ImportInfo <- glabel("Import X, Y and Errors before SAVE data",  spacing=2, container =  WoutGroup)
       font(ImportInfo) <- list(weight="normal", family="sans", size=11)

##--- BUTTONS
       ButtLT <- glayout(spacing = 10, container = MainGroup)
       import_btn <- ButtLT[1,1] <- gbutton("     IMPORT    ", handler=function(h,...) {
                         if (! check_selection()){return()}  #controls all the needed information are given
                         addCoreLine()    #add a new coreline
                         LL <- length(XPSSample)
                         cat("\n ----- Data Info -----")
                         cat("\n ===> Data File: ", activeFName, ", Core Line: ", XPSSample[[LL]]@Symbol)
                         cat("\n ===> Xmin= ", min(XPSSample[[LL]]@.Data[[1]]), "Xmax: ", max(XPSSample[[1]]@.Data[[1]]))
                         cat("\n ===> Ymin= ", min(XPSSample[[LL]]@.Data[[2]]), "Ymax: ", max(XPSSample[[1]]@.Data[[2]]))
                         cat("\n")
                         ErrCol <- svalue(ErrY)
                         if (ErrCol != ""){
                             addErrors()    #add a new coreline
                             LL <- length(XPSSample)
                             cat("\n ----- Data Info -----")
                             cat("\n ===> Data File: ", activeFName, ", Core Line: ", XPSSample[[LL]]@Symbol)
                             cat("\n ===> Standard Deviation Error Added to Last Saved Data" )
                             cat("\n")
                         }
#                         enabled(addErr_btn) <- TRUE
                         enabled(save_btn) <- TRUE
                         enabled(AddToXPSSamp) <- TRUE
                         enabled(exit_btn) <- TRUE
       }, container = ButtLT)

#       addErr_btn <- ButtLT[1,2] <- gbutton(" ADD Y-ERRORS ", handler=function(h, ...){
#                         if (! check_selection()){return()}  #controls all the needed information are given
#                         gmessage(msg="Add errors to the last imported data", title="WARNING", icon="warning")
#                         enabled(ErrY) <- TRUE
#                         addErrors()    #add a new coreline
#                         LL <- length(XPSSample)
#                         cat("\n ----- Data Info -----")
#                         cat("\n ===> Data File: ", activeFName, ", Core Line: ", XPSSample[[LL]]@Symbol)
#                         cat("\n ===> Standard Deviation Error Added to Last Saved Data" )
#                         cat("\n")
#                     }, container = ButtLT)    #save without exiting ImportAscii(): possibility to import other data from ascii file

       save_btn <- ButtLT[1,3] <- gbutton("    SAVE    ", handler=function(h, ...){
                         LL <- length(XPSSample) #number of Corelines of the source XPSSample
                         assign(activeFName, XPSSample, envir=.GlobalEnv)  #save the XPSSample in the .GlobalEnv
                         assign("activeFName", activeFName, envir=.GlobalEnv)  #Set the activeSpectName to the last name of imported data
                         assign("activeSpectName", XPSSample[[LL]]@Symbol, envir=.GlobalEnv)
                         assign("activeSpectIndx", LL, envir=.GlobalEnv)   #set the activeSpectIndx to the last imported data
                         XPSSaveRetrieveBkp("save")   
#                         enabled(addErr_btn) <- FALSE
                         enabled(save_btn) <- FALSE
                         enabled(AddToXPSSamp) <- FALSE
                     }, container = ButtLT)    #save without exiting ImportAscii(): possibility to import other data from ascii file

       AddToXPSSamp <- ButtLT[1,4] <- gbutton("   Save in an existing XPS Sample   ", label = " ", checked = FALSE, handler = function(h, ...) {
                         XPSSamplesList <- XPSFNameList()
                         if (length(XPSSamplesList) > 0 ) {
                             SelectWin <- gwindow(" SELECT SAMPLE ", visible=FALSE)
                             size(SelectWin) <- c(150,250)
                             gwinsave <- ggroup(container = SelectWin)
                                       samplesfr <- gframe(" XPS Samples ", spacing=5, horizontal=FALSE, container= gwinsave)
                                       SampleName <- gtable(items=XPSSamplesList, container=samplesfr)
                                       size(SampleName) <- c(120,200)
                                       gbutton("Select", handler = function(...) {
                                            if (length(svalue(SampleName)) > 0 ) {
                                                activeFName <<- svalue(SampleName)
                                                dispose(SelectWin)
                                                FName <<- get(activeFName, envir=.GlobalEnv)
                                                LL <- length(FName)      #Number of corelines in the destination XPSSample
                                                CLnames <- names(FName)
                                                LLL <- length(XPSSample) #Number of CoreLines in the source XPSSample
                                                FName[[LL+1]] <<- XPSSample[[LLL]] #save last imported Corelines in the destinaton XPSSample
                                                FName@names <<- c(CLnames, svalue(CLname)) #add names of new CoreLines
                                                XPSSample<<-FName      #set the source XPSSample == destination file with all spectra
                                                assign(activeFName, FName, envir=.GlobalEnv)  #Save the destination XPSSample in GlobalEnv
                                                assign("activeFName", activeFName, envir=.GlobalEnv)
                                                assign("activeSpectName", FName[[LL+1]]@Symbol, envir=.GlobalEnv)
                                                assign("activeSpectIndx", 1, envir=.GlobalEnv)
                                                plot(FName)
                                                cat("\n Data saved in ", activeFName)
                                                XPSSaveRetrieveBkp("save")
#                                                enabled(addErr_btn) <- FALSE
                                                enabled(save_btn) <- FALSE
                                                enabled(AddToXPSSamp) <- FALSE
                                            }
                                       }, container = samplesfr)
                                       visible(SelectWin) <- TRUE
                                   }
                     }, container = ButtLT)

       exit_btn <- ButtLT[1,5] <- gbutton("   SAVE and EXIT   ", handler=function(h, ...){
                         dispose(ImportWin)
                         LL <- length(XPSSample)
                         assign(activeFName, XPSSample, envir=.GlobalEnv)  #save XPSSample in GlobalEnv
                         assign("activeFName", activeFName, envir=.GlobalEnv)  #Set the activeSpectName to the last name of imported data
                         assign("activeSpectName", XPSSample[[LL]]@Symbol, envir=.GlobalEnv)
                         assign("activeSpectIndx", LL, envir=.GlobalEnv)   #set the activeSpectIndx to the last imported data
                         XPSSaveRetrieveBkp("save")
                         return(XPSSample)
                     }, container = ButtLT)




       enabled(import_btn) <- FALSE
#       enabled(addErr_btn) <- FALSE
       enabled(save_btn) <- FALSE
       enabled(AddToXPSSamp) <- FALSE
       enabled(exit_btn) <- FALSE

       visible(ImportWin) <- TRUE
       ImportWin$set_modal(TRUE)

       return(XPSSample)
}     





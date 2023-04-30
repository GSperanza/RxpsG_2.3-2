#This routine allows selection of the spectra to save in ASCII text file
# and the output Format (header, separators...)

#' @title XPSExportAscii
#' @description XPSExportAscii GUI for the selection of the more convenient
#'   style to export single and all the XPSCoreLines in a XPSSample in anASCII 
#'   text file.
#' @examples
#' \dontrun{
#'	 XPSexportASCII()
#' }
#' @export
#'


XPSExportAscii <- function(){

   writeData <- function(Fdata, filename, fmt) {   #write data following the format fmt
		                   switch(fmt,
                             "Raw" = { write.table(Fdata, file = filename, sep=" ", eol="\n",
                                                     dec=".", row.names=FALSE, col.names=TRUE)},
                             "x.x  x.x" = { write.table(Fdata, file = filename, row.names=FALSE, col.names=TRUE)},
                             "x.x, x.x" = { write.csv(Fdata, file = filename, col.names=TRUE)},
                             "x,x; x,x" = { write.csv2(Fdata, file = filename, col.names=TRUE)}
                         )
                     }


#--- variabili ---
   if (is.na(activeFName)){
       gmessage("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList()   #get the list of all XPSSamples in .GlobalEnv
   OutFormat <- c("Raw", "x.x  x.x", "x.x, x.x", "x,x; x,x")
   FName <- NULL
   SpectList <- " "
   FData <- NULL
   filename <- NULL
   fmt <- NULL


#--- GUI ---
   EAwin <- gwindow("EXPORT ASCII", visible=FALSE)
   EAgroup1 <- ggroup(container = EAwin, horizontal=FALSE, label = "Export Ascii")

   EAframe1 <- gframe(" Select the XPSSample ", horizontal=FALSE, spacing=5, container=EAgroup1)
   EAobj1 <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
                         ActiveFName <- svalue(EAobj1)
                         FName <<- get(ActiveFName, envir = .GlobalEnv)
                         delete(EAframe2, EAobj2)
                         SpectList <<- XPSSpectList(ActiveFName)
                         SpectList <<- c("All", SpectList) #all option added to save all the XPS Corelines
                         delete(EAframe2,EAobj2) 
                         EAobj2 <<- gcheckboxgroup(SpectList, selected=-1, horizontal=TRUE, spacing=5, handler=function(h,...){
                                              SpectName <- svalue(EAobj2, index=TRUE)
                                              if (SpectName == 1){
                                                  plot(FName)
                                              } else { 
                                                  tmp <- new("XPSSample")
                                                  for(ii in 1:length(SpectName)){
                                                      idx <- SpectName[ii]-1
                                                      tmp[[ii]] <- FName[[idx]] #ii-1 because the first element is "all"
                                                  }
                                                  plot(tmp)
                                              }
                                              enabled(EAobj3) <- TRUE
                                              enabled(EAobj4) <- TRUE
                                   }, container=EAframe2)
                         LL <- length(SpectList)
                         NRow <- ceiling(LL/5)   #gradio will be split in solumns of 5 elements
                         for(ii in 1:LL){
                             tkpack.forget(EAobj2$widgets[[ii]]$button)  # unparent widgets (uses library call)
                         }
                         for(kk in 1:NRow){
                             NN <- (kk-1)*5
                             for(ii in 1:5) {
                                 if((ii+NN) > LL) {break}
                                 CoreLineLyt[kk,ii] <<- EAobj2$widgets[[(ii+NN)]]$button
                             }
                         }
                         plot(FName)
                         }, container=EAframe1)


   EAframe2 <- gframe(" Select CoreLines ", horizontal=FALSE, spacing=5, container=EAgroup1)
   CoreLineLyt <- glayout(spacing=5, container=EAframe2)
   EAobj2 <- glabel("   ", selected=-1, container=EAframe2)
   
   EAframe3 <- gframe(" Data to Save ", horizontal=FALSE, spacing=5, container=EAgroup1)
   EAobj3 <- gradio(c("Spectrum+Fit", "Spectrum Only"), horizontal=TRUE, selected=1, container=EAframe3)

   EAframe4 <- gframe(" Select the OUTPUT format ", horizontal=FALSE, spacing=5, container=EAgroup1)
   EAobj4 <- gcombobox(OutFormat, selected=-1, editable=FALSE, handler=NULL, container=EAframe4)


   gbutton("SELECT DIR & EXPORT DATA", handler=function(...){
                         fmt <- svalue(EAobj4)
                         if (length(fmt)==0) {
                            gmessage("Please select output format before saving" , title = "OUTPUT FORMAT LACKING", icon = "warning")
                            return()
                         }
                         filename <- gfile(type="save", initial.filename=NULL, initial.dir=getwd() ) #select the filename
                         filename <- unlist(strsplit(filename, "\\."))
                         if( is.na(filename[2])) {        #if extension not given, .txt by default
                            filename[2] <- ".txt"
                         } else {
                            filename[2] <- paste(".", filename[2], sep="")
                         }
                         SpectName <- svalue(EAobj2)
                         FitYesNo <- svalue(EAobj3, index=TRUE)
                         if (SpectName == "All"){
                            LL <- length(SpectList) # -1: "All" is not a core-line
                            for (ii in 2:LL){
                                 SpectName <- unlist(strsplit(SpectList[ii], "\\."))
                                 idx <- SpectName[1]
                                 filenameOUT <- paste(filename[1], "_", SpectName[2],filename[2], sep="")
                                 if (FitYesNo==1) {
                                    data <- as(FName[[idx]], "matrix")  #export spectrum and fit
                                 } else if (FitYesNo==2){
                                    data <- data.frame(x=FName[[idx]]@.Data[1], y=FName[[idx]]@.Data[2]) #export spectrum only
                                    names(data)[1] <- "x"
                                    if (slot(FName[[idx]],"Symbol") != "") names(data)[2] <- slot(FName[[idx]],"Symbol")
                                 }
                                 data <- round(data,digits=4) #round to 4 decimal digits
                                 writeData(data, filenameOUT, fmt)
                                 cat("\n Core line: ", SpectName[2], "   written in file: ", filenameOUT)
                            }
                            XPSSaveRetrieveBkp("save")
                         } else {
                            idx <- svalue(EAobj2, index=TRUE)-1  #the first element is "all"
                            for (ii in 1:length(idx)){
                                 SpectName <- FName[[idx[ii]]]@Symbol
                                 filenameOUT <- paste(filename[1], "_", SpectName, filename[2], sep="")
                                 if (FitYesNo==1) {
                                    data <- as(FName[[idx[ii]]], "matrix")  #export spectrum and fit
                                 } else if (FitYesNo==2){
                                    data <- data.frame(x=FName[[idx[ii]]]@.Data[[1]], y=FName[[idx[ii]]]@.Data[[2]]) #export spectrum only
                                    names(data)[2] <- FName[[idx[ii]]]@Symbol
                                 }
                                 data <- round(data,digits=3)  #round to 4 decimal digits
                                 writeData(data, filenameOUT, fmt)
                                 cat("\n Core line: ", SpectName, "   written in file: ", filenameOUT)
                            }
                            XPSSaveRetrieveBkp("save")
                         }
                  }, container=EAgroup1)

   gbutton("EXIT", container=EAgroup1, handler=function(...){
    	                   dispose(EAwin)
                  })

   enabled(EAobj2) <- FALSE
   enabled(EAobj3) <- FALSE
   enabled(EAobj4) <- FALSE
   visible(EAwin) <- TRUE
}

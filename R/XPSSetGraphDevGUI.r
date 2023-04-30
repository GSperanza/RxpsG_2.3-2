#Setting the Graphic device for different Operating Systems

#' @title XPSSetGraphDev
#' @description function to select the kind of graphic device compatible
#'   with the operating system in use.
#'   Also a list of graphic formats is provided to save the content 
#'   of the current graphic device.
#' @examples
#' \dontrun{
#' 	XPSSetGraphDev()
#' }
#' @export
#'


XPSSetGraphDev <- function() {

   ChDir <- function(){
          workingDir <- getwd()
          PathName <- tk_choose.dir(default=workingDir)
          PathName <- paste(dirname(PathName), "/", basename(PathName), sep="") #changes from \\ to /
          return(PathName)
   }

   CutPathName <- function(PathName){  #taglia il pathname ad una lunghezza determinata
             splitPathName <- strsplit(PathName, "/")
             LL <- length(splitPathName[[1]])    
             headPathName <- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="") 
             ShortPathName <- paste(headPathName, splitPathName[[1]][LL], sep="")
             ii=0
             tailPathName <- ""
             while (nchar(ShortPathName) < 25) {
                   tailPathName <- paste("/",splitPathName[[1]][LL-ii],tailPathName, sep="")
                   ShortPathName <- paste(headPathName, tailPathName, sep="") 
                   ii <- ii+1
             }       
             return(ShortPathName)
   }



#--- variables
   OSList <- c("Windows", "MacOS-X", "Linux")
   FormatList <- c("jpeg", "png", "bmp", "tiff", "eps", "pdf")
   pathName <- getwd()
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)

# --- Widget ---
   GDwin <- gwindow("GRAPHIC DEVICE ", parent=c(200, 5), visible=FALSE)
   GDgroup <- ggroup(label="GRAPHIC DEVICE OPTIONS", horizontal=FALSE, container=GDwin)

   frame1 <- gframe(" SELECT your Operating System ", horizontal=FALSE, spacing=5, container=GDgroup)

   obj1 <- gradio(OSList, selected=1, horizontal=TRUE, handler=function(h,...){
                      OS <- svalue(obj1)
                      switch (OS,
                          "Linux"   = {Gdev <- "X11(type='cairo', xpos=700, ypos=20, title=' ')" },
                          "Windows" = {Gdev <- "windows(xpos=700, ypos=20, title=' ')"},
                          "MacOS-X" = {Gdev <- "quartz(title=' ')" },  #quartz() does allow setting the opening position
                          "Mac OS"  = {Gdev <- "quartz(title=' ')" },
                          "macOS"   = {Gdev <- "quartz(title=' ')" },
                          "Darwin"  = {Gdev <- "quartz(title=' ')" })
                      XPSSettings$General[6] <<- Gdev
           }, container = frame1)
           
   gbutton("RESET THE GRAPHIC WINDOW", handler=function(h,...){
                      Gdev <- XPSSettings$General[6]
                      graphics.off()
                      eval(parse(text=Gdev),envir=.GlobalEnv)
           }, container = frame1)
 

   frame2 <- gframe("FILE-FORMAT TO SAVE IMAGE ", horizontal=FALSE, spacing=5, container=GDgroup)

   obj2 <- gradio(FormatList, selected=1, horizontal=FALSE, container=frame2)
   glabel(text="File Name (No extension):", container=frame2)
   obj3 <- gedit(text="", container=frame2)

   txt <- paste("Current dir: ", pathName, sep="")
   obj4 <- glabel(text=txt, container=frame2)

   gbutton("CHANGE DIRECTORY", handler=function(h,...){
                      pathName<<- ChDir()
                      pathName <- CutPathName(pathName)
                      txt <- paste("Current dir: ", pathName, sep="")
                      svalue(obj4)<<- txt
                   },container=frame2)

   gbutton("  EXPORT TO FILE   ", handler=function(h,...){
                      Format <- svalue(obj2)
                      PlotFileName <- svalue(obj3)
                      PlotFileName <- paste(pathName,"/",PlotFileName,".",Format,sep="")
                      if (Format == "png") dev.print(file=PlotFileName, device=png, bg="white", width=1024)
                      if (Format == "jpeg") dev.print(file=PlotFileName, device=jpeg,  bg="white", width=1024)
                      if (Format == "bmp") dev.print(file=PlotFileName, device=bmp,  bg="white",width=1024)
                      if (Format == "tiff") dev.print(file=PlotFileName, device=tiff, bg="white", width=1024)
                      if (Format == "eps") dev.print(file=PlotFileName, device=postscript, horizontal=FALSE, pointsize=1)
                      if (Format == "pdf") dev.print(file=PlotFileName,device=pdf)
                      cat("\n Graphic Device Exported to .", Format, " File")
                      Gdev <- dev.cur()
          }, container=frame2)

   gbutton(" SAVE&EXIT ", handler=function(h,...){
   #--- get System info and apply correspondent XPS Settings ---
                      Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG")
                      if (file.exists(Ini.pthName)) {
                          ColNames <- names(XPSSettings)
                          write.table(XPSSettings, file = Ini.pthName, sep=" ", eol="\n", row.names=FALSE, col.names=ColNames)
                          assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                          dispose(GDwin)
                      } else {
                          gmessage(msg="ATTENTION: XPSSettings.ini file is lacking. Check the RxpsG package", title = "WARNING",icon = "warning" )
                          dispose(GDwin)
                          return()
                      }
                      assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                      dispose(GDwin)
                 }, container = frame2)

   visible(GDwin) <- TRUE
}

RxpsG_2.3-2

Processing tool for X-ray Photoelectron Spectroscopy Data

R, RStudio installation: (see also: https://cloud.r-project.org/ and https://www.rstudio.com/)

1. Install R and Rstudio on your computer 

2. Run Rstudio and open the Tools menu -> Global Options;

3. Verify all is working by typing:
   version
   you should get something like:
   
     platform       x86_64-pc-linux-gnu         
     arch           x86_64                      
     os             linux-gnu                   
     system         x86_64, linux-gnu           
     status                                     
     major          4                           
     minor          3.1                         
     year           2023                        
     month          06                          
     day            16                          
     svn rev        84548                       
     language       R                           
     version.string R version 4.3.1 (2023-06-16)
     nickname       Beagle Scouts   

RxpsG installation:

1. Click on the RxpsG_xx.xx.tar.gz package and download. Exit the unzipping procedure if it starts automatically.
 
2. Control in the Dowloads folder the RxpsG_xx.xx.tar.gz package is present (it could be the .gz extension is lacking do not         worry).

3. Run RStudio 

4. Under RStudio copy and paste the following command to INSTALL THE REQUIRED LIBRARIES: 
                        
   install.packages(c("digest", "gWidgets2", "gWidgets2tcltk", "import", 
               "latticeExtra", "memoise", "minpack.lm", "signal"), 
               repos = "https://cloud.r-project.org", dependencies=TRUE)                                    

   
   Control that installation proceed correctly without errors;

5. To INSTALL RxpsG copy and paste the following command:

   install.packages("C:/Path-To-Tar.Gz/RxpsG_2.3-2.tar.gz", type = "source", dependencies=TRUE)

   where Path-To-Tar.Gz is the path to the dowloaded RxpsG_2.3-2.tar.gz file.
   
6. To run RxpsG, in RStudio select the PACKAGE pain (generally on the right of the RStudio console) 
   and select the RxpsG package then type:
 
   xps()

   ------------------------------------------------------------

   This program is free software. You can use it under the
   terms of the  GNU Affero General Public License.
   http://www.gnu.org/licenses/
   and licenses linked to the use of the R, Rstudio platforms.

   - Authors decline any responsibility deriving from
     the use of the software or from software bugs.

   - Users are kindly requested to cite the software
     authors in their publications:
   
     Giorgio Speranza, Roberto Canteri
     Fondazione Bruno Kessler, Sommarive str. 18
     38123 Trento Italy.  "

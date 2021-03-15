![banner image](logo/logo.jpeg)

# TreeTalkersCheck

**An alert system to daily monitoring of TreeTalkers devices**

<br><br>

## [Manuscript](https://www.topolino.it/character/pluto/) 

## Citation

> Ilaria Zorzi, Saverio Francini*, Gherardo Chirici, Claudia Cocozza (in review). The TreeTalkersCheck R package: an automatic daily routine to check the functionality of TreeTalker devices. Ecologial Informatics

<br><br>

## Install and Test the package

#### Install the TreeTalkersCheck package. 
To do it, paste the code below in the R Console and press enter

`install.packages("devtools", repos = "http://cran.us.r-project.org")`
`devtools::install_github("saveriofrancini/TreeTalkersCheck")`

#### Execute the package
To do it, paste the code below in the R Console and press enter

`source(system.file("run", "run.R", package = "TreeTalkersCheck"))`

#### Congratulations!
If everything went fine, outputs are now in *C:\TreeTalkerDB*. 
Note that obtained TreeTalkerCheck outputs refer to server links and TT serial numbers distributed within the package for testing purpose. If you want to run the package to check the status of your own devices see the next section.

<br><br>

## Define input parameters

#### Go to the *parameters* folder. 
The *parameters* folder is in your TreeTalkersCheck installation folder. To find the path, paste the code below in the R Console and press enter.

`system.file("parameters", package = "TreeTalkersCheck")`

#### Define your own input parameters
Just edit the *serverLinks.txt* and *TTid.txt* files with your own server links, sites, TT serial numbers and the id you want to give to each TT. 

#### The game is made!
If you want to use the package with new parameters you defined, paste the code below in the R Console and press enter

`system(paste0("R CMD BATCH ", system.file("run", "run.R", package = "TreeTalkersCheck")))`

<br><br>

## Schedule a daily execution of the TreeTalkersPackage:
Note that the procedure below is for windows operating system. If you have a different operating system, for example a mac, you can use its [specific scheduler](https://www.macscheduler.net/)

- Open the scheduler: START -> All Programs -> Accesories -> System Tools -> Scheduler

- Create a new Task

- under tab Action, create a new action

- choose Start Program

- browse to Rscript.exe which should be placed e.g. here: "C:\Program Files\R\R-x.x.x\bin\x64\Rscript.exe". Note that quotation marks are mandatory to specify the path.

- input *run.R* in the parameters field

- input the path where the script is to be found in the Start in field (e.g. *C:/Users/userName/Documenti/R/win-library/4.0/TreeTalkersCheck/run/*). If you don't find the path, paste the code below in the R Console and press enter.

`system.file("run", package = "TreeTalkersCheck")`

- go to the Triggers tab

- create new trigger

- schedule a daily execution. 

<br><br>

## TreeTalkersCheck help

For more information on the package and on functions it includes, use the help.

`?TreeTalkersCheck::checkCloud`

`?TreeTalkersCheck::checkRequirements`

`?TreeTalkersCheck::checkTT`

`?TreeTalkersCheck::DownloadNaturetalkers`

`?TreeTalkersCheck::Plot`

`?TreeTalkersCheck::ReadServerData`

`?TreeTalkersCheck::TreeTalkersCheck`

<br><br>

###### Thanks for stopping by and have a great day!

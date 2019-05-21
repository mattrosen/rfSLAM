# rfSLAM
## Random Forests for Survival, Longitudinal, and Multivariate Data
We introduce a novel dynamic approach to clinical risk prediction for survival, longitudinal, and multivariate (SLAM) outcomes, called random forest for SLAM data analysis (RF-SLAM). RF-SLAM (an extension of Ishwaran and Kogalur's randomForestSRC package) is a continuous-time, random forest method for survival analysis that combines the strengths of existing statistical and machine learning methods to produce individualized Bayes estimates of piecewise-constant hazard rates.


In RF-SLAM, a large number of trees (e.g. 1000) are grown to create the random forest. Unlike the random survival forest approach in randomForestSRC where the individual is the unit of analysis, RF-SLAM builds trees using data binned according to user-specified lengths of time in a format we call counting process information units (CPIUs). Each individual can have many CPIUs during the period of follow-up. In the example sudden cardiac arrest (SCA) risk prediction problem, we consider follow-up time of 8 years and specify the time intervals to be 6 months long so that each individual has a CPIU representing each half-year of observation. We assume that a person's event hazard is constant within each CPIU. This strategy allows predictor variables to change from one interval to the next. Given the partition of the follow-up time into CPIUs, we use a Poisson regression splitting criterion that does not impose the proportional hazards assumption that the predictors have a common effect across the entire follow-up time.



## Installation

### Requirements:
+ R version >= 3.5.1
+ R packages: dplyr, Rcpp, pracma

### General instructions
1. *Clone this repository*, e.g.

   + run `git clone https://github.com/mattrosen/rfSLAM.git` in a command line interface;
   + clone the repository through a GitHub GUI;
   + download the package as a .zip and unpack it

2. *Install the package*. rfSLAM isn't yet an official CRAN-listed package, so the best way to do this is probably by running `R CMD INSTALL --clean --preclean rfSLAM/`.

### A note on parallelization
Random forests are particularly amenable to parallelization (the trees comprising a forest can be built independently of one another, or in an *embarrassingly parallel* way). As such, the authors of randomForestSRC, the R package upon which rfSLAM builds, wrote their code to take advantage of some common shared memory processing libraries when possible. This capability is machine-dependent, however, and requires slight modification to how the package is compiled and installed. We've included some instructions for getting this to work on Mac OSX; further instructions for this can be found at https://kogalur.github.io/randomForestSRC/building.html. 

### Mac OSX
1. Install Homebrew (a package utility for Mac) from <https://brew.sh/>.
2. On the Terminal, run `brew install llvm`; note that the final output of this command should show something like

> For compilers to find llvm you may need to set:
>
>   export LDFLAGS="-L/usr/local/opt/llvm/lib"  
>   export CPPFLAGS="-I/usr/local/opt/llvm/include"
3. From your home directory (e.g. `/users/USERNAME`), do the following: 

   + Create a `.R` directory, if one doesn't exist already: `mkdir .R`
   + Using a text editor (e.g. vim or emacs), open the file `.R/Makevars`
   + Make sure that file contains the contents below, where the values for CC and CXX are equivalent to where Homebrew installed llvm on your machine (as denoted by the output lines copied in item 2). Note that if llvm installation specifies LDFLAGS as being in directory `/dirX/dirY/dirZ/llvm/lib`, you should use `dirX/dirY/dirZ/llvm/bin/clang` for CC and `dirX/dirY/dirZ/llvm/bin/clang++` for CXX.  

    
> CC=/usr/local/opt/llvm/bin/clang  
> CXX=/usr/local/opt/llvm/bin/clang++  
> LDFLAGS=-L/usr/local/opt/llvm/lib

## High-level overview
This release of rfSLAM encompasses two distinct foci: 
1. Reformulation of the standard random forest (as described originally by Breiman and as implemented by Ishwaran and Kogalur) to deal specifically with the challenges of survival, longitudinal, and multivariate data; and
2. Conversion of standard (e.g. implicitly longitudinal) data to work with these adjusted random forests.

All changes relevant to focus 1 occur within the rfsrc.R and randomForestSRC.c/randomForestSRC.h files. Changes to rfsrc.R take the form of added arguments to support functionality that we have built on top of randomForestSRC (or changes we have made to the default operation of randomForestSRC). As rfsrc.R calls randomForestSRC.c sitting underneath, all additions or changes that these arguments support are actually implemented in randomForestSRC.c. Because the original source code from which we worked was scrubbed of all comments/documentation, we find it easiest to refer interested users to particular functions within randomForestSRC.c to find the relevant changes:
+ rfsrcGrow()
+ selectRandomCovariates()
+ rfsrcPredict()

Our changes implementing variants of Poisson split rules can be found in [splitCustom.c](./src/splitCustom.c). We have also documented the specifics of all changes to rfsrc.R in a manually annotated diff file, accessible [here](rfsrc_diff.txt).

All code relevant to focus 2 is our own, and can be found (with explanatory comments) in [cpiu.R](./utilities/cpiu.R) and [cpiu.cpp](./utilities/cpiu.cpp); examples of how to use these functions can be found in [make_into_cpiu.R](./utilities/make_into_cpiu.R).

## Examples
Example code for using the rfSLAM package can be found in the [examples](./examples/) folder.

## Contact
For questions about package maintenance, email <mattrosen@uchicago.edu>.

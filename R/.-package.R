

#' Wisconsin Prognostic Breast Cancer Data
#' 
#' Recurrence of breast cancer from 198 breast cancer patients, all of which
#' exhibited no evidence of distant metastases at the time of diagnosis.  The
#' first 30 features of the data describe characteristics of the cell nuclei
#' present in the digitized image of a fine needle aspirate (FNA) of the breast
#' mass.
#' 
#' 
#' @name breast
#' @docType data
#' @source The data were obtained from the UCI machine learning repository, see
#' \url{http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Prognostic)}.
#' @keywords datasets
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## Standard analysis
#' ## ------------------------------------------------------------
#' 
#' data(breast, package = "randomForestSRC")
#' breast <- na.omit(breast)
#' o <- rfsrc(status ~ ., data = breast, nsplit = 10)
#' print(o)
#' 
#' ## ------------------------------------------------------------
#' ## The data is imbalanced so we use balanced random forests
#' ## with undersampling of the majority class
#' ##
#' ## Specifically let n0, n1 be sample sizes for majority, minority
#' ## cases.  We sample 2 x n1 cases with majority, minority cases chosen
#' ## with probabilities n1/n, n0/n where n=n0+n1
#' ## ------------------------------------------------------------
#' 
#' y <- breast$status
#' o <- rfsrc(status ~ ., data = breast, nsplit = 10,
#'             case.wt = randomForestSRC:::make.wt(y),
#'             sampsize = randomForestSRC:::make.size(y))
#' print(o)
#' 
#' }
NULL





#' Follicular Cell Lymphoma
#' 
#' Competing risk data set involving follicular cell lymphoma.
#' 
#' 
#' @name follic
#' @docType data
#' @format A data frame containing: \tabular{ll}{ age \tab age\cr hgb \tab
#' hemoglobin (g/l)\cr clinstg \tab clinical stage: 1=stage I, 2=stage II\cr ch
#' \tab chemotherapy\cr rt \tab radiotherapy\cr time \tab first failure time\cr
#' status \tab censoring status: 0=censored, 1=relapse, 2=death }
#' @references Pintilie M., (2006) \emph{Competing Risks: A Practical
#' Perspective.} West Sussex: John Wiley and Sons.
#' @source Table 1.4b, \emph{Competing Risks: A Practical Perspective}.
#' @keywords datasets
#' @examples
#' 
#' \donttest{
#' data(follic, package = "randomForestSRC")
#' follic.obj <- rfsrc(Surv(time, status) ~ ., follic, nsplit = 3, ntree = 100)
#' }
#' 
NULL





#' Hodgkin's Disease
#' 
#' Competing risk data set involving Hodgkin's disease.
#' 
#' 
#' @name hd
#' @docType data
#' @format A data frame containing: \tabular{ll}{ age \tab age\cr sex \tab
#' gender\cr trtgiven \tab treatment: RT=radition, CMT=Chemotherapy and
#' radiation\cr medwidsi \tab mediastinum involvement: N=no, S=small,
#' L=Large\cr extranod \tab extranodal disease: Y=extranodal disease, N=nodal
#' disease\cr clinstg \tab clinical stage: 1=stage I, 2=stage II\cr time \tab
#' first failure time\cr status \tab censoring status: 0=censored, 1=relapse,
#' 2=death }
#' @references Pintilie M., (2006) \emph{Competing Risks: A Practical
#' Perspective.} West Sussex: John Wiley and Sons.
#' @source Table 1.6b, \emph{Competing Risks: A Practical Perspective}.
#' @keywords datasets
#' @examples
#' data(hd, package = "randomForestSRC")
NULL





#' Nutrigenomic Study
#' 
#' Study the effects of five diet treatments on 21 liver lipids and 120 hepatic
#' gene expression in wild-type and PPAR-alpha deficient mice.  We use a
#' multivariate mixed random forest analysis by regressing gene expression,
#' diet and genotype (the x-variables) on lipid expressions (the multivariate
#' y-responses).
#' 
#' 
#' @name nutrigenomic
#' @docType data
#' @references Martin P.G. et al. (2007). Novel aspects of PPAR-alpha-mediated
#' regulation of lipid and xenobiotic metabolism revealed through a
#' nutrigenomic study. \emph{Hepatology}, 45(3), 767--777.
#' @keywords datasets
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## multivariate mixed forests
#' ## lipids used as the multivariate y-responses
#' ## ------------------------------------------------------------
#' 
#' ## load the data
#' data(nutrigenomic, package = "randomForestSRC")
#' 
#' ## multivariate mixed forest call
#' mv.obj <- rfsrc(get.mv.formula(colnames(nutrigenomic$lipids)),
#'             data.frame(do.call(cbind, nutrigenomic)),
#'             importance=TRUE, nsplit = 10)
#' 
#' ## ------------------------------------------------------------
#' ## plot the standarized performance and VIMP values
#' ## ------------------------------------------------------------
#' 
#' ## acquire the error rate for each of the 21-coordinates 
#' ## standardize to allow for comparison across coordinates
#' serr <- get.mv.error(mv.obj, std = TRUE)
#' 
#' ## acquire standardized VIMP 
#' svimp <- get.mv.vimp(mv.obj, std = TRUE)
#' 
#' par(mfrow = c(1,2))
#' plot(serr, xlab = "Lipids", ylab = "Standardized Performance")
#' matplot(svimp, xlab = "Genes/Diet/Genotype", ylab = "Standardized VIMP")
#' 
#' 
#' }
NULL





#' Primary Biliary Cirrhosis (PBC) Data
#' 
#' Data from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of the
#' liver conducted between 1974 and 1984.  A total of 424 PBC patients,
#' referred to Mayo Clinic during that ten-year interval, met eligibility
#' criteria for the randomized placebo controlled trial of the drug
#' D-penicillamine.  The first 312 cases in the data set participated in the
#' randomized trial and contain largely complete data.
#' 
#' 
#' @name pbc
#' @docType data
#' @references Flemming T.R and Harrington D.P., (1991) \emph{Counting
#' Processes and Survival Analysis.} New York: Wiley.
#' @source Flemming and Harrington, 1991, Appendix D.1.
#' @keywords datasets
#' @examples
#' 
#' \donttest{
#' data(pbc, package = "randomForestSRC")
#' pbc.obj <- rfsrc(Surv(days, status) ~ ., pbc, nsplit = 3)
#' }
#' 
NULL





#' Random Forests for Survival, Regression, and Classification (RF-SRC)
#' 
#' This package provides a unified treatment of Breiman's random forests
#' (Breiman 2001) for a variety of data settings.  Regression and
#' classification forests are grown when the response is numeric or categorical
#' (factor), while survival and competing risk forests (Ishwaran et al. 2008,
#' 2012) are grown for right-censored survival data.  Multivariate regression
#' and classification responses as well as mixed outcomes
#' (regression/classification responses) are also handled.  Also includes
#' unsupervised forests and quantile regression forests.  Different splitting
#' rules invoked under deterministic or random splitting are available for all
#' families.  Variable predictiveness can be assessed using variable importance
#' (VIMP) measures for single, as well as grouped variables.  Variable
#' selection is implemented using minimal depth variable selection (Ishwaran et
#' al. 2010). Missing data (for x-variables and y-outcomes) can be imputed on
#' both training and test data.  The underlying code is based on Ishwaran and
#' Kogalur's now retired \pkg{randomSurvivalForest} package (Ishwaran and
#' Kogalur 2007), and has been significantly refactored for improved
#' computational speed.
#' 
#' 
#' @name randomForestSRC-package
#' @aliases randomForestSRC-package cindex extract.bootsample extract.subsample
#' get.mv.error get.mv.formula get.mv.predicted get.mv.vimp perf.metric
#' @docType package
#' @section Package Overview:
#' 
#' This package contains many useful functions and users should read the help
#' file in its entirety for details.  However, we briefly mention several key
#' functions that may make it easier to navigate and understand the layout of
#' the package.
#' 
#' \enumerate{ \item \code{\link{rfsrc}}
#' 
#' This is the main entry point to the package.  It grows a random forest using
#' user supplied training data.  We refer to the resulting object as a RF-SRC
#' grow object.  Formally, the resulting object has class \code{(rfsrc, grow)}.
#' 
#' \item \code{\link{predict.rfsrc}}, \code{predict}
#' 
#' Used for prediction.  Predicted values are obtained by dropping the user
#' supplied test data down the grow forest.  The resulting object has class
#' \code{(rfsrc, predict)}.
#' 
#' \item \code{\link{max.subtree}}, \code{\link{var.select}}
#' 
#' Used for variable selection.  The function \code{max.subtree} extracts
#' maximal subtree information from a RF-SRC object which is used for selecting
#' variables by making use of minimal depth variable selection.  The function
#' \code{var.select} provides an extensive set of variable selection options
#' and is a wrapper to \code{max.subtree}.
#' 
#' \item \code{\link{impute.rfsrc}}
#' 
#' Fast imputation mode for RF-SRC.  Both \code{rfsrc} and \code{predict.rfsrc}
#' are capable of imputing missing data.  However, for users whose only
#' interest is imputing data, this function provides an efficient and fast
#' interface for doing so.
#' 
#' \item \code{\link{partial.rfsrc}}
#' 
#' Used to extract the partial effects of a variable or variables on the
#' ensembles.
#' 
#' }
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{find.interaction}},
#' 
#' \command{\link{impute}}, \command{\link{max.subtree}},
#' 
#' \command{\link{plot.competing.risk}}, \command{\link{plot.rfsrc}},
#' \command{\link{plot.survival}}, \command{\link{plot.variable}},
#' \command{\link{predict.rfsrc}}, \command{\link{print.rfsrc}},
#' \command{\link{quantileReg}}, \command{\link{rfsrc}},
#' \command{\link{rfsrcSyn}},
#' 
#' \command{\link{stat.split}}, \command{\link{var.select}},
#' \command{\link{vimp}}
#' @references Breiman L. (2001). Random forests, \emph{Machine Learning},
#' 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007).  Random survival forests for R,
#' \emph{Rnews}, 7(2):25-31.
#' 
#' Ishwaran H. (2007).  Variable importance in binary regression trees and
#' forests, \emph{Electronic J. Statist.}, 1:519-537.
#' 
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S.  (2008).  Random
#' survival forests, \emph{Ann. App.  Statist.}, 2:841-860.
#' 
#' Ishwaran H., Kogalur U.B., Gorodeski E.Z, Minn A.J. and Lauer M.S. (2010).
#' High-dimensional variable selection for survival data.  \emph{J. Amer.
#' Statist. Assoc.}, 105:205-217.
#' 
#' Ishwaran H., Kogalur U.B., Chen X. and Minn A.J. (2011). Random survival
#' forests for high-dimensional data. \emph{Stat. Anal. Data Mining}, 4:115-132
#' 
#' Ishwaran H., Gerds T.A., Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M.
#' (2014). Random survival forests for competing risks.  \emph{Biostatistics},
#' 15(4):757-773.
#' 
#' Ishwaran H. and Malley J.D. (2014). Synthetic learning machines.
#' \emph{BioData Mining}, 7:28.
#' 
#' Ishwaran H. (2015).  The effect of splitting on random forests.
#' \emph{Machine Learning}, 99:75-118.
#' 
#' Ishwaran H. and Lu M.  (2017).  Standard errors and confidence intervals for
#' variable importance in random forest regression, classification, and
#' survival.
#' 
#' Mantero A. and Ishwaran H. (2017). Unsupervised random forests.
#' 
#' O'Brien R. and Ishwaran H. (2017).  A random forests quantile classifier for
#' class imbalanced data.
#' 
#' Tang F. and Ishwaran H. (2017).  Random forest missing data algorithms.
#' \emph{Statistical Analysis and Data Mining}, 10, 363-377.
#' @keywords package
NULL





#' van de Vijver Microarray Breast Cancer
#' 
#' Gene expression profiling for predicting clinical outcome of breast cancer
#' (van't Veer et al., 2002).  Microarray breast cancer data set of 4707
#' expression values on 78 patients with survival information.
#' 
#' 
#' @name vdv
#' @docType data
#' @references van't Veer L.J. et al. (2002).  Gene expression profiling
#' predicts clinical outcome of breast cancer.  \emph{Nature}, \bold{12},
#' 530--536.
#' @keywords datasets
#' @examples
#' data(vdv, package = "randomForestSRC")
NULL





#' Veteran's Administration Lung Cancer Trial
#' 
#' Randomized trial of two treatment regimens for lung cancer.  This is a
#' standard survival analysis data set.
#' 
#' 
#' @name veteran
#' @docType data
#' @references Kalbfleisch J. and Prentice R, (1980) \emph{The Statistical
#' Analysis of Failure Time Data.} New York: Wiley.
#' @source Kalbfleisch and Prentice, \emph{The Statistical Analysis of Failure
#' Time Data.}
#' @keywords datasets
#' @examples
#' data(veteran, package = "randomForestSRC")
NULL





#' Women's Interagency HIV Study (WIHS)
#' 
#' Competing risk data set involving AIDS in women.
#' 
#' 
#' @name wihs
#' @docType data
#' @format A data frame containing: \tabular{ll}{ time \tab time to event\cr
#' status \tab censoring status: 0=censoring, 1=HAART initiation, 2=AIDS/Death
#' before HAART\cr ageatfda \tab age in years at time of FDA approval of first
#' protease inhibitor\cr idu \tab history of IDU: 0=no history, 1=history\cr
#' black \tab race: 0=not African-American; 1=African-American\cr cd4nadir \tab
#' CD4 count (per 100 cells/ul) }
#' @references Bacon M.C, von Wyl V., Alden C., et al. (2005). The Women's
#' Interagency HIV Study: an observational cohort brings clinical sciences to
#' the bench, \emph{Clin Diagn Lab Immunol}, 12(9):1013-1019.
#' @source Study included 1164 women enrolled in WIHS, who were alive, infected
#' with HIV, and free of clinical AIDS on December, 1995, when the first
#' protease inhibitor (saquinavir mesylate) was approved by the Federal Drug
#' Administration. Women were followed until the first of the following
#' occurred: treatment initiation, AIDS diagnosis, death, or administrative
#' censoring (September, 2006). Variables included history of injection drug
#' use at WIHS enrollment, whether an individual was African American, age, and
#' CD4 nadir prior to baseline.
#' @keywords datasets
#' @examples
#' 
#' \donttest{
#' data(wihs, package = "randomForestSRC")
#' wihs.obj <- rfsrc(Surv(time, status) ~ ., wihs, nsplit = 3, ntree = 100)
#' }
#' 
NULL




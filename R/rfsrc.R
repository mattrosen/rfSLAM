#' Random Forests for Survival, Regression, and Classification (RF-SRC)
#' 
#' \emph{Note:} This documentation was generated automatically using \code{roxygen}
#' from Kogalur and Ishwaran's original package \code{randomForestSRC}, 
#' distributed under the GNU GPL v3.0. The vast majority of the material here 
#' is theirs. Additions have been made to this documentation to reflect the
#' changes made to support CPIU data for this package (\code{rfSLAM}). The 
#' extent and nature of these changes, per the GNU license, are enumerated in 
#' the \code{deltas/} directory.
#' 
#' A random forest (Breiman, 2001) is grown using user supplied training data.
#' Applies when the response (outcome) is numeric, categorical (factor), or
#' right-censored (including competing risk), and yields regression,
#' classification, and survival forests, respectively.  The resulting forest,
#' informally referred to as a RF-SRC object, contains many useful values which
#' can be directly extracted by the user and/or parsed using additional
#' functions (see the examples below).  This is the main entry point to the
#' \pkg{randomForestSRC} package.
#' 
#' Note that the package now handles multivariate regression and classification
#' responses as well as mixed outcomes (regression/classification responses).
#' In such cases, a multivariate forest is grown, informally referred to as an
#' mRF-SRC object.  Unsupervised forests and quantile regression forests are
#' also available.
#' 
#' The package implements OpenMP shared-memory parallel programming.  However,
#' the default installation will only execute serially.  Users should consult
#' the randomForestSRC-package help file for details on installing the OpenMP
#' version of the package.  The help file is readily accessible via the command
#' \code{package?randomForestSRC}.
#' 
#' \enumerate{
#' 
#' \item \emph{Families}
#' 
#' Users do *not* set this value as the package automagically determines the
#' underlying random forest family from the type of response and the formula
#' supplied.  There are eight possible scenarios:
#' 
#' \code{regr}, \code{regr+}, \code{class}, \code{class+}, \code{mix+},
#' \code{unsupv}, \code{surv}, and \code{surv-CR}.
#' 
#' \enumerate{ \item Regression forests (\code{regr}) for continuous responses.
#' \item Multivariate regression forests (\code{regr+}) for multivariate
#' continuous responses.  \item Classification forests (\code{class}) for
#' factor responses.  \item Multivariate classification forests (\code{class+})
#' for multivariate factor responses.  \item Multivariate mixed forests
#' (\code{mix+}) for mixed continuous and factor responses.  \item Unsupervised
#' forests (\code{unsupv}) when there is no response.  \item Survival forest
#' (\code{surv}) for right-censored survival settings.  \item Competing risk
#' survival forests (\code{surv-CR}) for competing risk scenarios.  }
#' 
#' See below for how to code the response in the two different survival
#' scenarios and for specifying formula for multivariate forests.
#' 
#' \item \emph{Splitrules}
#' 
#' Splitrules are set according to the option \code{splitrule} as follows:
#' \itemize{
#' 
#' \item Regression analysis: \enumerate{ \item The default rule is weighted
#' mean-squared error splitting \code{mse} (Breiman et al. 1984, Chapter 8.4).
#' \item Unweighted and heavy weighted mean-squared error splitting rules can
#' be invoked using splitrules \code{mse.unwt} and \code{mse.hvwt}.  Generally
#' \code{mse} works best, but see Ishwaran (2015) for details.  }
#' 
#' \item Multivariate regression analysis: For multivariate regression
#' responses, a composite normalized mean-squared error splitting rule is used.
#' 
#' \item Classification analysis: \enumerate{ \item The default rule is Gini
#' index splitting \code{gini} (Breiman et al. 1984, Chapter 4.3).  \item
#' Unweighted and heavy weighted Gini index splitting rules can be invoked
#' using splitrules \code{gini.unwt} and \code{gini.hvwt}.  Generally
#' \code{gini} works best, but see Ishwaran (2015) for details.  }
#' 
#' \item Multivariate classification analysis: For multivariate classification
#' responses, a composite normalized Gini index splitting rule is used.
#' 
#' \item Mixed outcomes analysis: When both regression and classification
#' responses are detected, a multivariate normalized composite split rule of
#' mean-squared error and Gini index splitting is invoked.  See Tang and
#' Ishwaran (2017) for details.
#' 
#' \item Unsupervised analysis: In settings where there is no outcome,
#' unsupervised splitting is invoked.  In this case, the mixed outcome
#' splitting rule (above) is applied.  See Mantero and Ishwaran (2017) for
#' details.
#' 
#' \item Survival analysis: \enumerate{ \item The default rule is
#' \code{logrank} which implements log-rank splitting (Segal, 1988; Leblanc and
#' Crowley, 1993).  \item \code{logrankscore} implements log-rank score
#' splitting (Hothorn and Lausen, 2003).  }
#' 
#' \item Competing risk analysis: \enumerate{
#' 
#' \item The default rule is \code{logrankCR} which implements a modified
#' weighted log-rank splitting rule modeled after Gray's test (Gray, 1988).
#' 
#' \item \code{logrank} implements weighted log-rank splitting where each event
#' type is treated as the event of interest and all other events are treated as
#' censored.  The split rule is the weighted value of each of log-rank
#' statistics, standardized by the variance.  For more details see Ishwaran et
#' al. (2014).  }
#' 
#' \item Custom splitting: All families except unsupervised are available for
#' user defined custom splitting.  Some basic C-programming skills are
#' required.  The harness for defining these rules is in \code{splitCustom.c}.
#' In this file we give examples of how to code rules for regression,
#' classification, survival, and competing risk.  Each family can support up to
#' sixteen custom split rules.  Specifying \code{splitrule="custom"} or
#' \code{splitrule="custom1"} will trigger the first split rule for the family
#' defined by the training data set. Multivariate families will need a custom
#' split rule for both regression and classification.  In the examples, we
#' demonstrate how the user is presented with the node specific membership.
#' The task is then to define a split statistic based on that membership.  Take
#' note of the instructions in \code{splitCustom.c} on how to \emph{register}
#' the custom split rules.  It is suggested that the existing custom split
#' rules be kept in place for reference and that the user proceed to develop
#' \code{splitrule="custom2"} and so on. The package must be recompiled and
#' installed for the custom split rules to become available.
#' 
#' \item Random splitting.  For all families, pure random splitting can be
#' invoked by setting \code{splitrule="random"}.  See below for more details
#' regarding randomized splitting rules.  }
#' 
#' \item \emph{Allowable data types}
#' 
#' Data types must be real valued, integer, factor or logical -- however all
#' except factors are coerced and treated as if real valued.  For ordered
#' x-variable factors, splits are similar to real valued variables.  If the
#' x-variable factor is unordered, a split will move a subset of the levels in
#' the parent node to the left daughter, and the complementary subset to the
#' right daughter.  All possible complementary pairs are considered and apply
#' to factors with an unlimited number of levels.  However, there is an
#' optimization check to ensure that the number of splits attempted is not
#' greater than the number of cases in a node (this internal check will
#' override the \code{nsplit} value in random splitting mode if \code{nsplit}
#' is large enough; see below for information about \code{nsplit}).
#' 
#' \item \emph{Improving computational speed}
#' 
#' \itemize{
#' 
#' \item \emph{Randomized Splitting Rules}
#' 
#' Trees tend to favor splits on continuous variables and factors with large
#' numbers of levels (Loh and Shih, 1997).  To mitigate this bias, and
#' considerably improve computational speed, a randomize version of a splitting
#' rule can be invoked using \code{nsplit}.  If \code{nsplit} is set to a
#' non-zero positive integer, then a maximum of \code{nsplit} split points are
#' chosen randomly for each of the \code{mtry} variables within a node.  The
#' splitting rule is applied to the random split points and the node is split
#' on that variable and random split point yielding the best value (as measured
#' by the splitting rule).  Pure random splitting can be invoked by setting
#' \code{splitrule="random"}.  For each node, a variable is randomly selected
#' and the node is split using a random split point (Cutler and Zhao, 2001; Lin
#' and Jeon, 2006).
#' 
#' The value of \code{nsplit} has a significant impact on the time taken to
#' grow a forest.  This is because non-random splitting (i.e. the default
#' option \code{nsplit=0}), considers all possible split points for each of the
#' \code{mtry} variables, and iterating over each split point can be CPU
#' intensive, especially in large sample size settings.
#' 
#' In general, regardless of computational speed, it is good practice to use
#' the \code{nsplit} when the data contains a mix of continuous and discrete
#' variables.  Using a reasonably small value mitigates bias and may not
#' compromise accuracy.
#' 
#' \item \emph{Large sample size}
#' 
#' For increased efficiency for survival families, users should consider
#' setting \code{ntime} to a relatively small value when the sample size
#' (number of rows of the data) is large.  This constrains ensemble
#' calculations such as survival functions to a restricted grid of time points
#' of length no more than \code{ntime} which can considerably reduce
#' computational times.
#' 
#' \item \emph{Large number of variables}
#' 
#' For increased efficiency when the number of variables are large, use the
#' defalut setting of \code{importance="none"} which turns off variable
#' importance (VIMP) calculations and can considerably reduce computational
#' times (see below for more details about variable importance).  Note that
#' variable importance calculations can always be recovered using functions
#' \command{vimp} or \command{predict}.  Finally, the function
#' \command{max.subtree} calculates minimal depth, a measure of the depth that
#' a variable splits, and can be used for fast variable selection (Ishwaran,
#' 2010).
#' 
#' \item \emph{Factors}
#' 
#' For coherence, an immutable map is applied to each factor that ensures that
#' factor levels in the training data set are consistent with the factor levels
#' in any subsequent test data set.  This map is applied to each factor before
#' and after the native C library is executed.  Because of this, if x-variables
#' are all factors, then computational times may become very long in high
#' dimensional problems.
#' 
#' }
#' 
#' \item \emph{Prediction Error}
#' 
#' Prediction error is calculated using OOB data.  Performance is measured in
#' terms of mean-squared-error for regression, and misclassification error for
#' classification.  A normalized Brier score (relative to a coin-toss) is also
#' provided upon printing a classification forest.
#' 
#' For survival, prediction error is measured by 1-C, where C is Harrell's
#' (Harrell et al., 1982) concordance index.  Prediction error is between 0 and
#' 1, and measures how well the predictor correctly ranks (classifies) two
#' random individuals in terms of survival.  A value of 0.5 is no better than
#' random guessing. A value of 0 is perfect.
#' 
#' When bootstrapping is \code{by.node} or \code{none}, a coherent OOB subset
#' is not available to assess prediction error.  Thus, all outputs dependent on
#' this are suppressed.  In such cases, prediction error is only available via
#' classical cross-validation (the user will need to use the
#' \command{predict.rfsrc} function).
#' 
#' \item \emph{Variable Importance (VIMP)}
#' 
#' The option \code{importance} allows several ways to calculate VIMP.  When
#' the option is \code{permute} or \code{TRUE}, the action is to return
#' Breiman-Cutler permutation VIMP as described in Breiman (2001).  For each
#' tree, the prediction error on the out-of-bag (OOB) data is recorded.  Then
#' for a given variable \emph{x}, OOB cases are randomly permuted in \emph{x}
#' and the prediction error is recorded.  The VIMP for \emph{x} is defined as
#' the difference between the perturbed and unperturbed error rate, averaged
#' over all trees.  If \code{random} is specified, then \emph{x} is not
#' permuted, but rather an OOB case is assigned a daughter node randomly
#' whenever a split on \emph{x} is encountered in the in-bag tree.  If
#' \code{anti} is specified, then \emph{x} is assigned to the opposite node
#' whenever a split on \emph{x} is encountered in the in-bag tree.
#' 
#' If one of the \code{ensemble} options is selected, VIMP is calculated by
#' comparing the error rate for the perturbed OOB forest ensemble to the
#' unperturbed OOB forest ensemble, where the perturbed ensemble is obtained by
#' either permuting \emph{x}, or by random daughter node assignment, or by
#' anti-splitting on \emph{x}.  Thus, unlike Breiman-Cutler VIMP, ensemble VIMP
#' does not measure the tree average effect of \emph{x}, but rather its overall
#' forest effect.  This is called Ishwaran-Kogalur VIMP (Ishwaran et al. 2008).
#' 
#' A useful compromise between Breiman-Cutler and Ishwaran-Kogalur VIMP can be
#' obtained using \code{err.block} when Breiman-Cutler permutation VIMP is
#' requested (not yet implemented).  Setting this value to an integer between
#' \code{1} and \code{ntree} will calculate ensemble VIMP in blocks of size
#' \code{err.block}.  The average of these values are returned.  Ensemble
#' blocked VIMP is faster to calculate than Breiman-Cutler VIMP and is
#' preferable in large scale problems (this is especially true for survival).
#' 
#' Note that the option \code{none} turns VIMP off entirely.
#' 
#' Note that the function \command{vimp} provides a friendly user interface for
#' extracting VIMP.
#' 
#' \item \emph{Multivariate Forests}
#' 
#' Multivariate forests are specified by using the multivariate formula
#' interface.  Such a call takes one of two forms:
#' 
#' rfsrc(Multivar(y1, y2, ..., yd) ~ . , my.data, ...)
#' 
#' rfsrc(cbind(y1, y2, ..., yd) ~ . , my.data, ...)
#' 
#' A multivariate normalized composite splitting rule is used to split nodes.
#' The nature of the outcomes will inform the code as to the type of
#' multivariate forest to be grown; i.e. whether it is real-valued,
#' categorical, or a combination of both (mixed). Note that performance
#' measures (when requested) are returned for all outcomes.
#' 
#' \item \emph{Unsupervised Forests}
#' 
#' In the case where no y-outcomes are present, unsupervised forests can be
#' invoked by one of two means:
#' 
#' rfsrc(data = my.data)
#' 
#' rfsrc(Unsupervised() ~ ., data = my.data)
#' 
#' To split a tree node, a random subset of \code{ytry} variables are selected
#' from the available features, and these variables function as
#' "pseudo-responses" to be split on.  Thus, in unsupervised mode, the features
#' take turns acting as both target y-outcomes and x-variables for splitting.
#' 
#' More precisely, as in supervised forests, \code{mtry} x-variables are
#' randomly selected from the set of \code{p} features for splitting the node.
#' Then on each \code{mtry} loop, \code{ytry} variables are selected from the
#' \code{p}-1 remaining features to act as the target pseduo-responses to be
#' split on (there are \code{p}-1 possibilities because we exclude the
#' currently selected x-variable for the current \code{mtry} loop --- also,
#' only pseudo-responses that pass purity checks are used).  The
#' split-statistic for splitting the node on the pseudo-responses using the
#' x-variable is calculated.  The best split over the \code{mtry} pairs is used
#' to split the node.
#' 
#' The default value of \code{ytry} is 1 but can be increased by the
#' \code{ytry} option.  A value larger than 1 initiates multivariate splitting.
#' As illustration, consider the call:
#' 
#' rfsrc(data = my.data, ytry = 5, mtry = 10)
#' 
#' This is equivalent to the call:
#' 
#' rfsrc(Unsupervised(5) ~ ., my.data, mtry = 10)
#' 
#' In the above, a node will be split by selecting \code{mtry=10} x-variables,
#' and for each of these a random subset of 5 features will be selected as the
#' multivariate pseudo-responses.  The split-statistic is a multivariate
#' normalized composite splitting rule which is applied to each of the 10
#' multivariate regression problems.  The node is split on the variable leading
#' to the best split.
#' 
#' Note that all performance values (error rates, VIMP, prediction) are turned
#' off in unsupervised mode.
#' 
#' \item \emph{Survival, Competing Risks}
#' 
#' \enumerate{ \item Survival settings require a time and censoring variable
#' which should be identifed in the formula as the response using the standard
#' \code{Surv} formula specification.  A typical formula call looks like:
#' 
#' Surv(my.time, my.status) ~ .
#' 
#' where \code{my.time} and \code{my.status} are the variables names for the
#' event time and status variable in the users data set.
#' 
#' \item For survival forests (Ishwaran et al. 2008), the censoring variable
#' must be coded as a non-negative integer with 0 reserved for censoring and
#' (usually) 1=death (event).  The event time must be non-negative.
#' 
#' \item For competing risk forests (Ishwaran et al., 2013), the implementation
#' is similar to survival, but with the following caveats:
#' 
#' \itemize{ \item Censoring must be coded as a non-negative integer, where 0
#' indicates right-censoring, and non-zero values indicate different event
#' types.  While 0,1,2,..,J is standard, and recommended, events can be coded
#' non-sequentially, although 0 must always be used for censoring.
#' 
#' \item Setting the splitting rule to \code{logrankscore} will result in a
#' survival analysis in which all events are treated as if they are the same
#' type (indeed, they will coerced as such).
#' 
#' \item Generally, competing risks requires a larger \code{nodesize} than
#' survival settings.  } }
#' 
#' \item \emph{Missing data imputation}
#' 
#' Setting \code{na.action="na.impute"} imputes missing data (both x and
#' y-variables) using a modification of the missing data algorithm of Ishwaran
#' et al. (2008).  See also Tang and Ishwaran (2017).  Split statistics are
#' calculated using non-misssing data only.  If a node splits on a variable
#' with missing data, the variable's missing data is imputed by randomly
#' drawing values from non-missing in-bag data.  The purpose of this is to make
#' it possible to assign cases to daughter nodes based on the split.  Following
#' a node split, imputed data are reset to missing and the process is repeated
#' until terminal nodes are reached.  Missing data in terminal nodes are
#' imputed using in-bag non-missing terminal node data.  For integer valued
#' variables and censoring indicators, imputation uses a maximal class rule,
#' whereas continuous variables and survival time use a mean rule.
#' 
#' The missing data algorithm can be iterated by setting \code{nimpute} to a
#' positive integer greater than 1.  Using only a few iterations are needed to
#' improve accuracy.  When the algorithm is iterated, at the completion of each
#' iteration, missing data is imputed using OOB non-missing terminal node data
#' which is then used as input to grow a new forest.  Note that when the
#' algorithm is iterated, a side effect is that missing values in the returned
#' objects \code{xvar}, \code{yvar} are replaced by imputed values.  Further,
#' imputed objects such as \code{imputed.data} are set to \code{NULL}.  Also,
#' keep in mind that if the algorithm is iterated, performance measures such as
#' error rates and VIMP become optimistically biased.
#' 
#' Finally, records in which all outcome and x-variable information are missing
#' are removed from the forest analysis.  Variables having all missing values
#' are also removed.
#' 
#' See the function \command{impute.rfsrc} for a fast impute interface.
#' 
#' }
#' 
#' @aliases rfsrc rfSLAM
#' @param formula A symbolic description of the model to be fit.  If missing,
#' unsupervised splitting is implemented.
#' @param data Data frame containing the y-outcome and x-variables.
#' @param ntree Number of trees in the forest.
#' @param bootstrap Bootstrap protocol.  The default is \code{by.root} which
#' bootstraps the data by sampling with replacement at the root node before
#' growing the tree.  If \code{by.node} is choosen, the data is bootstrapped at
#' each node during the grow process.  If \code{none} is chosen, the data is
#' not bootstrapped at all.  If \code{by.user} is choosen, the bootstrap
#' specified by \code{samp} is used. Note the details below on prediction error
#' when the default choice is not in effect.
#' @param mtry Number of variables randomly selected as candidates for
#' splitting a node.  The default is \code{p}/3 for regression families, where
#' \code{p} equals the number of variables.  For all other families (including
#' unsupervised settings), the default is sqrt(\code{p}).  Values are always
#' rounded up.
#' @param ytry For unsupervised forests, sets the number of randomly selected
#' pseudo-responses (see below for more details).  The default is
#' \code{ytry}=1, which selects one pseudo-response.
#' @param yvar.wt NOT YET IMPLEMENTED: Vector of non-negative weights where
#' entry \code{k}, after normalizing, is the probability of selecting response
#' \code{k} as a candidate for inclusion in the split statistic in unsupervised
#' settings.  Default is to use uniform weights.  Vector must be of the same
#' length as the number of respones in the data set.
#' @param nodesize Forest average number of unique cases (data points) in a
#' terminal node.  The defaults are: survival (3), competing risk (6),
#' regression (5), classification (1), mixed outcomes (3), unsupervised (3).
#' It is recommended to experiment with different \code{nodesize} values.
#' @param nodedepth Maximum depth to which a tree should be grown.  The default
#' behaviour is that this parameter is ignored.
#' @param splitrule Splitting rule used to grow trees.  See below for details.
#' @param nsplit Non-negative integer value.  When zero, deterministic
#' splitting for an x-variable is in effect.  When non-zero, a maximum of
#' nsplit split points are randomly chosen among the possible split points for
#' the x-variable. This can significantly increase speed over deterministic
#' splitting.  In the case of pure random splitting, a value of one is used as
#' the default, since deterministic splitting is not a compatible concept in
#' that scenario.  However, values greater than one are accepted, as with the
#' other split rules.
#' @param importance Method for computing variable importance (VIMP).  Because
#' VIMP is computationally expensive, the default action is
#' \code{importance="none"} (VIMP can always be recovered later using the
#' functions \command{vimp} or \command{predict}). Setting
#' \code{importance=TRUE} implements Breiman-Cutler permutation VIMP.  See
#' below for more details.
#' @param err.block Should the error rate be calculated on every tree?  When
#' \code{NULL}, it will only be calculated on the last tree.  In such
#' situations, plot of the error rate will result in a flat line.  To view the
#' error rate on every nth tree, set the value to an integer between \code{1}
#' and \code{ntree}.  Restore the model with these values on demand to
#' accomplish the same.  As an intentended side effect, if importance is
#' requested, VIMP is calculated in "blocks" of size equal to \code{err.block},
#' thus resulting in a useful compromise between ensemble and permutation VIMP
#' (not yet implemented). See below for more details.
#' @param na.action Action taken if the data contains \code{NA}'s.  Possible
#' values are \code{na.omit} or \code{na.impute}.  The default \code{na.omit}
#' removes the entire record if even one of its entries is \code{NA} (for
#' x-variables this applies only to those specifically listed in 'formula').
#' Selecting \code{na.impute} imputes the data.  See below for more details
#' regarding missing data imputation.
#' @param nimpute Number of iterations of the missing data algorithm.
#' Performance measures such as out-of-bag (OOB) error rates tend to become
#' optimistic if \code{nimpute} is greater than 1.
#' @param ntime Integer value used for survival families to constrain ensemble
#' calculations to a grid of time values of no more than \code{ntime} time
#' points.  Alternatively if a vector of values of length greater than one is
#' supplied, it is assumed these are the time points to be used to constrain
#' the calculations (note that the constrained time points used will be the
#' observed event times closest to the user supplied time points).  If no value
#' is specified, the default action is to use all observed event times.
#' @param cause Integer value between 1 and \code{J} indicating the event of
#' interest for competing risks, where \code{J} is the number of event types
#' (this option applies only to competing risks and is ignored otherwise).
#' While growing a tree, the splitting of a node is restricted to the event
#' type specified by \code{cause}.  If not specified, the default is to use a
#' composite splitting rule which is an average over the entire set of event
#' types (a democratic approach).  Users can also pass a vector of non-negative
#' weights of length \code{J} if they wish to use a customized composite split
#' statistic (for example, passing a vector of ones reverts to the default
#' composite split statistic).  In all instances when \code{cause} is set
#' incorrectly, splitting reverts to the default.  Finally, note that
#' regardless of how \code{cause} is specified, the returned forest object
#' always provides estimates for all event types.
#' @param proximity Proximity of cases as measured by the frequency of sharing
#' the same terminal node.  This is an \code{n}x\code{n} matrix, which can be
#' large.  Choices are \code{"inbag"}, \code{"oob"}, \code{"all"}, \code{TRUE},
#' or \code{FALSE}.  Setting \code{proximity = TRUE} is equivalent to
#' \code{proximity = "inbag"}.
#' @param sampsize Requested size of bootstrap when \code{"by.root"} is in
#' effect (if missing the default action is the usual bootstrap).
#' @param samptype Type of bootstrap when \code{"by.root"} is in effect.
#' Choices are \code{swr} (sampling with replacement, the default action) and
#' \code{swor} (sampling without replacement).
#' @param samp Bootstrap specification when \code{"by.user"} is in effect.
#' This is a array of dim \code{n x ntree} specifying how many times each
#' record appears inbag in the bootstrap for each tree. Note that the
#' requirement in Kogalur and Ishwaran's original implementation of 
#' randomForestSRC -- that the sample matrix have the same number of samples
#' per tree -- is relaxed here.
#' @param case.wt Vector of non-negative weights where entry \code{k}, after
#' normalizing, is the probability of selecting case \code{k} as a candidate
#' for the bootstrap.  Default is to use uniform weights.  Vector must be of
#' dimension \code{n}, where \code{n} equals the number of cases in the
#' processed data set (missing values may be removed, thus altering the
#' original sample size).  It is generally better to use real weights rather
#' than integers.  With larger sizes of \code{n}, the slightly different
#' sampling algorithms used in the two scenarios can result in dramatically
#' different execution times.  See the example below for the breast data set
#' for an illustration of its use for class imbalanced data.
#' @param split.wt Vector of non-negative weights where entry \code{k}, after
#' normalizing, is the multiplier by which the split statistic for a variable
#' is adjusted.  A large value encourages the node to split on the variable.
#' Default is to use uniform weights.  Vector must be of dimension \code{p},
#' where \code{p} equals the number of variables, otherwise the default is
#' invoked.
#' @param forest.wt Should the forest weight matrix be calculated?  Creates an
#' \code{n}x\code{n} matrix which can be used for prediction and constructing
#' customized estimators.  Choices are similar to proximity: \code{"inbag"},
#' \code{"oob"}, \code{"all"}, \code{TRUE}, or \code{FALSE}.  The default is
#' \code{TRUE} which is equivalent to \code{"inbag"}.
#' @param k_for_alpha The value of \code{k} to use for the Bayesian estimate
#' in computing the split rule; this only applies when choosing between split 
#' rules named \code{"poisson.split1"}, \code{"poisson.split2"}, and 
#' \code{"poisson.split3"}. \code{k} is used specifically to compute \eqn{\alpha}.
#' @param stratifier Stratifying variable (e.g. interval number) to use for 
#' random forests with CPIUs.
#' @param risk.time Variable to use for risk time (CPIU-specific).
#' @param categories When provided, enables the assignment of variables during
#' splitting to different categories (e.g. to allow a more structured process
#' of sampling variables). Expects a list with 2 elements: \code{var.assignments}, 
#' which gives the category to which each variable belongs, and \code{categories.weights},
#' which specifies the weight for each category during sampling for each split.
#' @param fixed_during_bootstrap Specifies which variables (if any) should be 
#' considered as a candidate for splitting at every node.
#' @param xvar.wt Vector of non-negative weights where entry \code{k}, after
#' normalizing, is the probability of selecting variable \code{k} as a
#' candidate for splitting a node.  Default is to use uniform weights.  Vector
#' must be of dimension \code{p}, where \code{p} equals the number of
#' variables, otherwise the default is invoked. It is generally better to use
#' real weights rather than integers.  With larger sizes of \code{p}, the
#' slightly different sampling algorithms used in the two scenarios can result
#' in dramatically different execution times.
#' @param forest Should the forest object be returned?  Used for prediction on
#' new data and required by many of the functions used to parse the RF-SRC
#' object.  It is recommended not to change the default setting.
#' @param var.used Return variables used for splitting?  Default is
#' \code{FALSE}.  Possible values are \code{all.trees} and \code{by.tree}.
#' @param split.depth Records the minimal depth for each variable.  Default is
#' \code{FALSE}.  Possible values are \code{all.trees} and \code{by.tree}. Used
#' for variable selection.
#' @param seed Negative integer specifying seed for the random number
#' generator.
#' @param do.trace Number of seconds between updates to the user on approximate
#' time to completion.
#' @param membership Should terminal node membership and inbag information be
#' returned?
#' @param statistics Should split statistics be returned?  Values can be parsed
#' using \command{stat.split}.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of class \code{(rfsrc, grow)} with the following
#' components:
#' 
#' \item{call}{The original call to \command{rfsrc}.} \item{family}{The family
#' used in the analysis.} \item{n}{Sample size of the data (depends upon
#' \code{NA}'s, see \code{na.action}).} \item{ntree}{Number of trees grown.}
#' \item{mtry}{Number of variables randomly selected for splitting at each
#' node.} \item{nodesize}{Minimum size of terminal nodes.}
#' \item{nodedepth}{Maximum depth allowed for a tree.}
#' \item{splitrule}{Splitting rule used.} \item{nsplit}{Number of randomly
#' selected split points.} \item{yvar}{y-outcome values.} \item{yvar.names}{A
#' character vector of the y-outcome names.} \item{xvar}{Data frame of
#' x-variables.} \item{xvar.names}{A character vector of the x-variable names.}
#' \item{xvar.wt}{Vector of non-negative weights specifying the probability
#' used to select a variable for splitting a node.} \item{split.wt}{Vector of
#' non-negative weights where entry \code{k}, after normalizing, is the
#' multiplier by which the split statistic for a covariate is adjusted.}
#' \item{cause.wt}{Vector of weights used for the composite competing risk
#' splitting rule.} \item{leaf.count}{Number of terminal nodes for each tree in
#' the forest. Vector of length \code{ntree}.  A value of zero indicates a
#' rejected tree (can occur when imputing missing data).  Values of one
#' indicate tree stumps.} \item{proximity}{Proximity matrix recording the
#' frequency of pairs of data points occur within the same terminal node.}
#' \item{forest}{If \code{forest=TRUE}, the forest object is returned.  This
#' object is used for prediction with new test data sets and is required for
#' other R-wrappers.} \item{forest.wt}{Forest weight matrix.}
#' \item{membership}{Matrix recording terminal node membership where each
#' column contains the node number that a case falls in for that tree.}
#' \item{splitrule}{Splitting rule used.} \item{inbag}{Matrix recording inbag
#' membership where each column contains the number of times that a case
#' appears in the bootstrap sample for that tree.} \item{var.used}{Count of the
#' number of times a variable is used in growing the forest.}
#' \item{imputed.indv}{Vector of indices for cases with missing values.}
#' \item{imputed.data}{Data frame of the imputed data. The first column(s) are
#' reserved for the y-responses, after which the x-variables are listed.}
#' \item{split.depth}{Matrix [i][j] or array [i][j][k] recording the minimal
#' depth for variable [j] for case [i], either averaged over the forest, or by
#' tree [k].} \item{node.stats}{Split statistics returned when
#' \code{statistics=TRUE} which can be parsed using \command{stat.split}.}
#' \item{err.rate}{Tree cumulative OOB error rate.} \item{importance}{Variable
#' importance (VIMP) for each x-variable.} \item{predicted}{In-bag predicted
#' value.} \item{predicted.oob}{OOB predicted value.}\cr
#' 
#' \item{++++++++}{for classification settings, additionally ++++++++} \cr
#' \item{class}{In-bag predicted class labels.} \item{class.oob}{OOB predicted
#' class labels.}\cr
#' 
#' \item{++++++++}{for multivariate settings, additionally ++++++++} \cr
#' 
#' \item{regrOutput}{List containing performance values for multivariate
#' regression responses (applies only in multivariate settings).}
#' \item{clasOutput}{List containing performance values for multivariate
#' categorical (factor) responses (applies only in multivariate settings).}
#' 
#' \item{++++++++}{for survival settings, additionally ++++++++} \cr
#' 
#' \item{survival}{In-bag survival function.} \item{survival.oob}{OOB survival
#' function.} \item{chf}{In-bag cumulative hazard function (CHF).}
#' \item{chf.oob}{OOB CHF.} \item{time.interest}{Ordered unique death times.}
#' \item{ndead}{Number of deaths.}\cr
#' 
#' \item{++++++++}{for competing risks, additionally ++++++++} \cr
#' 
#' \item{chf}{In-bag cause-specific cumulative hazard function (CSCHF) for each
#' event.} \item{chf.oob}{OOB CSCHF.} \item{cif}{In-bag cumulative incidence
#' function (CIF) for each event.} \item{cif.oob}{OOB CIF.}
#' \item{time.interest}{Ordered unique event times.} \item{ndead}{Number of
#' events.}
#' @note \enumerate{ \item Values returned depend heavily on the family.  In
#' particular, \code{predicted} and \code{predicted.oob} are the following
#' values calculated using in-bag and OOB data:
#' 
#' \enumerate{ \item For regression, a vector of predicted y-responses.
#' 
#' \item For classification, a matrix with columns containing the estimated
#' class probability for each class.
#' 
#' \item For survival, a vector of mortality values (Ishwaran et al., 2008)
#' representing estimated risk for each individual calibrated to the scale of
#' the number of events (as a specific example, if \emph{i} has a mortality
#' value of 100, then if all individuals had the same x-values as \emph{i}, we
#' would expect an average of 100 events).  Also included in the grow object
#' are matrices containing the CHF and survival function.  Each row corresponds
#' to an individual's ensemble CHF or survival function evaluated at each time
#' point in \code{time.interest}.
#' 
#' \item For competing risks, a matrix with one column for each event recording
#' the expected number of life years lost due to the event specific cause up to
#' the maximum follow up (Ishwaran et al., 2013).  The grow object also
#' contains the cause-specific cumulative hazard function (CSCHF) and the
#' cumulative incidence function (CIF) for each event type.  These are encoded
#' as a three-dimensional array, with the third dimension used for the event
#' type, each time point in \code{time.interest} making up the second dimension
#' (columns), and the case (individual) being the first dimension (rows).
#' 
#' \item For multivariate families, predicted values (and other performance
#' values such as VIMP and error rates) are stored in the lists
#' \code{regrOutput} and \code{clasOutput}.
#' 
#' }
#' 
#' \item Different R-wrappers are provided to aid in parsing the grow object.
#' 
#' \item Setting \code{var.used="all.trees"} returns a vector of size \code{p}
#' where each element is a count of the number of times a split occurred on a
#' variable.  If \code{var.used="by.tree"}, a matrix of size
#' \code{ntree}x\code{p} is returned.  Each element [i][j] is the count of the
#' number of times a split occurred on variable [j] in tree [i].
#' 
#' \item Setting \code{split.depth="all.trees"} returns a matrix of size
#' \code{n}x\code{p} where entry [i][j] is the minimal depth for variable [j]
#' for case [i] averaged over the forest.  That is, for case [i], the entry
#' [i][j] records the first time case [i] splits on variable [j] averaged over
#' the forest.  If \code{split.depth="by.tree"}, a three-dimensional array is
#' returned where the third dimension [k] records the tree and the first two
#' coordinates [i][j] record the case and the variable.  Thus entry [i][j][k]
#' is the minimal depth for case [i] for variable [j] for tree [k].  }
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{find.interaction}},
#' 
#' \command{\link{impute}}, \command{\link{max.subtree}},
#' 
#' \command{\link{plot.competing.risk}}, \command{\link{plot.rfsrc}},
#' \command{\link{plot.survival}}, \command{\link{plot.variable}},
#' \command{\link{predict.rfsrc}}, \command{\link{print.rfsrc}},
#' \command{\link{quantileReg}}, \command{\link{rfsrcSyn}},
#' 
#' \command{\link{stat.split}}, \command{\link{var.select}},
#' \command{\link{vimp}}
#' @references Breiman L., Friedman J.H., Olshen R.A. and Stone C.J.
#' \emph{Classification and Regression Trees}, Belmont, California, 1984.
#' 
#' Breiman L. (2001). Random forests, \emph{Machine Learning}, 45:5-32.
#' 
#' Cutler A. and Zhao G. (2001). Pert-Perfect random tree ensembles.
#' \emph{Comp. Sci. Statist.}, 33: 490-497.
#' 
#' Gray R.J. (1988).  A class of k-sample tests for comparing the cumulative
#' incidence of a competing risk, \emph{Ann. Statist.}, 16: 1141-1154.
#' 
#' Harrell et al. F.E. (1982).  Evaluating the yield of medical tests, \emph{J.
#' Amer. Med. Assoc.}, 247:2543-2546.
#' 
#' Hothorn T. and Lausen B. (2003). On the exact distribution of maximally
#' selected rank statistics, \emph{Comp. Statist. Data Anal.}, 43:121-137.
#' 
#' Ishwaran H. (2007).  Variable importance in binary regression trees and
#' forests, \emph{Electronic J. Statist.}, 1:519-537.
#' 
#' Ishwaran H. and Kogalur U.B. (2007).  Random survival forests for R,
#' \emph{Rnews}, 7(2):25-31.
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
#' survival. Statistics in Medicine (in press).
#' 
#' Lin Y. and Jeon Y. (2006).  Random forests and adaptive nearest neighbors,
#' \emph{J. Amer. Statist. Assoc.}, 101:578-590.
#' 
#' LeBlanc M. and Crowley J. (1993).  Survival trees by goodness of split,
#' \emph{J. Amer. Statist. Assoc.}, 88:457-467.
#' 
#' Loh W.-Y and Shih Y.-S (1997).  Split selection methods for classification
#' trees, \emph{Statist. Sinica}, 7:815-840.
#' 
#' Mantero A. and Ishwaran H. (2017). Unsupervised random forests.
#' 
#' Mogensen, U.B, Ishwaran H. and Gerds T.A. (2012). Evaluating random forests
#' for survival analysis using prediction error curves, \emph{J.  Statist.
#' Software}, 50(11): 1-23.
#' 
#' O'Brien R. and Ishwaran H. (2017).  A random forests quantile classifier for
#' class imbalanced data.
#' 
#' Segal M.R. (1988).  Regression trees for censored data, \emph{Biometrics},
#' 44:35-47.
#' 
#' Tang F. and Ishwaran H. (2017).  Random forest missing data algorithms.
#' \emph{Statistical Analysis and Data Mining}, 10, 363-377.
#' @keywords forest
#' @examples
#' 
#' ##------------------------------------------------------------
#' ## Survival analysis
#' ##------------------------------------------------------------
#' 
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100, tree.err=TRUE)
#' 
#' # print and plot the grow object
#' print(v.obj)
#' plot(v.obj)
#' 
#' # plot survival curves for first 10 individuals: direct way
#' matplot(v.obj$time.interest, 100 * t(v.obj$survival[1:10, ]),
#'     xlab = "Time", ylab = "Survival", type = "l", lty = 1)
#' 
#' # plot survival curves for first 10 individuals
#' # indirect way: using plot.survival (also generates hazard plots)
#' plot.survival(v.obj, subset = 1:10, haz.model = "ggamma")
#' 
#' \donttest{
#' ## Primary biliary cirrhosis (PBC) of the liver
#' 
#' data(pbc, package = "randomForestSRC")
#' pbc.obj <- rfsrc(Surv(days, status) ~ ., pbc, nsplit = 10)
#' print(pbc.obj)
#' 
#' 
#' ##------------------------------------------------------------
#' ## Example of imputation in survival analysis
#' ##------------------------------------------------------------
#' 
#' data(pbc, package = "randomForestSRC")
#' pbc.obj2 <- rfsrc(Surv(days, status) ~ ., pbc,
#'            nsplit = 10, na.action = "na.impute")
#' 
#' 
#' # here's a nice wrapper to combine original data + imputed data
#' combine.impute <- function(object) {
#'  impData <- cbind(object$yvar, object$xvar)
#'  if (!is.null(object$imputed.indv)) {
#'    impData[object$imputed.indv, ] <- object$imputed.data
#'  }
#'  impData
#' }
#' 
#' # combine original data + imputed data
#' pbc.imp.data <- combine.impute(pbc.obj2)
#' 
#' # same as above but we iterate the missing data algorithm
#' pbc.obj3 <- rfsrc(Surv(days, status) ~ ., pbc, nsplit=10,
#'          na.action = "na.impute", nimpute = 3)
#' pbc.iterate.imp.data <- combine.impute(pbc.obj3)
#' 
#' # fast way to impute the data (no inference is done)
#' # see impute.rfsc for more details
#' pbc.fast.imp.data <- impute.rfsrc(data = pbc, nsplit = 10, nimpute = 5)
#' 
#' ##------------------------------------------------------------
#' ## Compare RF-SRC to Cox regression
#' ## Illustrates C-index and Brier score measures of performance
#' ## assumes "pec" and "survival" libraries are loaded
#' ##------------------------------------------------------------
#' 
#' if (library("survival", logical.return = TRUE)
#'     & library("pec", logical.return = TRUE)
#'     & library("prodlim", logical.return = TRUE))
#' 
#' {
#'   ##prediction function required for pec
#'   predictSurvProb.rfsrc <- function(object, newdata, times, ...){
#'     ptemp <- predict(object,newdata=newdata,...)$survival
#'     pos <- sindex(jump.times = object$time.interest, eval.times = times)
#'     p <- cbind(1,ptemp)[, pos + 1]
#'     if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
#'       stop("Prediction failed")
#'     p
#'   }
#' 
#'   ## data, formula specifications
#'   data(pbc, package = "randomForestSRC")
#'   pbc.na <- na.omit(pbc)  ##remove NA's
#'   surv.f <- as.formula(Surv(days, status) ~ .)
#'   pec.f <- as.formula(Hist(days,status) ~ 1)
#' 
#'   ## run cox/rfsrc models
#'   ## for illustration we use a small number of trees
#'   cox.obj <- coxph(surv.f, data = pbc.na)
#'   rfsrc.obj <- rfsrc(surv.f, pbc.na, nsplit = 10, ntree = 150)
#' 
#'   ## compute bootstrap cross-validation estimate of expected Brier score
#'   ## see Mogensen, Ishwaran and Gerds (2012) Journal of Statistical Software
#'   set.seed(17743)
#'   prederror.pbc <- pec(list(cox.obj,rfsrc.obj), data = pbc.na, formula = pec.f,
#'                         splitMethod = "bootcv", B = 50)
#'   print(prederror.pbc)
#'   plot(prederror.pbc)
#' 
#'   ## compute out-of-bag C-index for cox regression and compare to rfsrc
#'   rfsrc.obj <- rfsrc(surv.f, pbc.na, nsplit = 10)
#'   cat("out-of-bag Cox Analysis ...", "\n")
#'   cox.err <- sapply(1:100, function(b) {
#'     if (b%%10 == 0) cat("cox bootstrap:", b, "\n")
#'     train <- sample(1:nrow(pbc.na), nrow(pbc.na), replace = TRUE)
#'     cox.obj <- tryCatch({coxph(surv.f, pbc.na[train, ])}, error=function(ex){NULL})
#'     if (is.list(cox.obj)) {
#'       randomForestSRC:::cindex(pbc.na$days[-train],
#'                                       pbc.na$status[-train],
#'                                       predict(cox.obj, pbc.na[-train, ]))
#'     } else NA
#'   })
#'   cat("\n\tOOB error rates\n\n")
#'   cat("\tRSF            : ", rfsrc.obj$err.rate[rfsrc.obj$ntree], "\n")
#'   cat("\tCox regression : ", mean(cox.err, na.rm = TRUE), "\n")
#' }
#' 
#' ##------------------------------------------------------------
#' ## Competing risks
#' ##------------------------------------------------------------
#' 
#' ## WIHS analysis
#' ## cumulative incidence function (CIF) for HAART and AIDS stratified by IDU
#' 
#' data(wihs, package = "randomForestSRC")
#' wihs.obj <- rfsrc(Surv(time, status) ~ ., wihs, nsplit = 3, ntree = 100)
#' plot.competing.risk(wihs.obj)
#' cif <- wihs.obj$cif
#' Time <- wihs.obj$time.interest
#' idu <- wihs$idu
#' cif.haart <- cbind(apply(cif[,,1][idu == 0,], 2, mean),
#'                    apply(cif[,,1][idu == 1,], 2, mean))
#' cif.aids  <- cbind(apply(cif[,,2][idu == 0,], 2, mean),
#'                    apply(cif[,,2][idu == 1,], 2, mean))
#' matplot(Time, cbind(cif.haart, cif.aids), type = "l",
#'         lty = c(1,2,1,2), col = c(4, 4, 2, 2), lwd = 3,
#'         ylab = "Cumulative Incidence")
#' legend("topleft",
#'        legend = c("HAART (Non-IDU)", "HAART (IDU)", "AIDS (Non-IDU)", "AIDS (IDU)"),
#'        lty = c(1,2,1,2), col = c(4, 4, 2, 2), lwd = 3, cex = 1.5)
#' 
#' 
#' ## illustrates the various splitting rules
#' ## illustrates event specific and non-event specific variable selection
#' if (library("survival", logical.return = TRUE)) {
#' 
#'   ## use the pbc data from the survival package
#'   ## events are transplant (1) and death (2)
#'   data(pbc, package = "survival")
#'   pbc$id <- NULL
#' 
#'   ## modified Gray's weighted log-rank splitting
#'   pbc.cr <- rfsrc(Surv(time, status) ~ ., pbc, nsplit = 10)
#' 
#'   ## log-rank event-one specific splitting
#'   pbc.log1 <- rfsrc(Surv(time, status) ~ ., pbc, nsplit = 10,
#'               splitrule = "logrank", cause = c(1,0), importance="permute")
#' 
#'   ## log-rank event-two specific splitting
#'   pbc.log2 <- rfsrc(Surv(time, status) ~ ., pbc, nsplit = 10,
#'               splitrule = "logrank", cause = c(0,1), importance="permute")
#' 
#'   ## extract VIMP from the log-rank forests: event-specific
#'   ## extract minimal depth from the Gray log-rank forest: non-event specific
#'   var.perf <- data.frame(md = max.subtree(pbc.cr)$order[, 1],
#'                          vimp1 = 100 * pbc.log1$importance[ ,1],
#'                          vimp2 = 100 * pbc.log2$importance[ ,2])
#'   print(var.perf[order(var.perf$md), ])
#' 
#' }
#' 
#' 
#' 
#' ## ------------------------------------------------------------
#' ## Regression analysis
#' ## ------------------------------------------------------------
#' 
#' ## New York air quality measurements
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' 
#' # partial plot of variables (see plot.variable for more details)
#' plot.variable(airq.obj, partial = TRUE, smooth.lines = TRUE)
#' 
#' ## motor trend cars
#' mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' 
#' # minimal depth variable selection via max.subtree
#' md.obj <- max.subtree(mtcars.obj)
#' cat("top variables:\n")
#' print(md.obj$topvars)
#' 
#' # equivalent way to select variables
#' # see var.select for more details
#' vs.obj <- var.select(object = mtcars.obj)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## Classification analysis
#' ## ------------------------------------------------------------
#' 
#' ## Edgar Anderson's iris data
#' iris.obj <- rfsrc(Species ~., data = iris)
#' 
#' ## Wisconsin prognostic breast cancer data
#' data(breast, package = "randomForestSRC")
#' breast.obj <- rfsrc(status ~ ., data = breast, nsplit = 10, tree.err=TRUE)
#' plot(breast.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Classification analysis with class imbalanced data
#' ## ------------------------------------------------------------
#' 
#' data(breast, package = "randomForestSRC")
#' breast <- na.omit(breast)
#' o <- rfsrc(status ~ ., data = breast, nsplit = 10)
#' print(o)
#' 
#' ## The data is imbalanced so we use balanced random forests
#' ## with undersampling of the majority class
#' ##
#' ## Specifically let n0, n1 be sample sizes for majority, minority
#' ## cases.  We sample 2 x n1 cases with majority, minority cases chosen
#' ## with probabilities n1/n, n0/n where n=n0+n1
#' 
#' y <- breast$status
#' o <- rfsrc(status ~ ., data = breast, nsplit = 10,
#'            case.wt = randomForestSRC:::make.wt(y),
#'            sampsize = randomForestSRC:::make.size(y))
#' print(o)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## Unsupervised analysis
#' ## ------------------------------------------------------------
#' 
#' # two equivalent ways to implement unsupervised forests
#' mtcars.unspv <- rfsrc(Unsupervised() ~., data = mtcars)
#' mtcars2.unspv <- rfsrc(data = mtcars)
#' 
#' #minimal depth variable selection applies!
#' var.select(mtcars2.unspv)
#' 
#' ## ------------------------------------------------------------
#' ## Multivariate regression analysis
#' ## ------------------------------------------------------------
#' mtcars.mreg <- rfsrc(Multivar(mpg, cyl) ~., data = mtcars,
#'            tree.err=TRUE, importance = TRUE)
#' 
#' ## extract error rates, vimp, and predicted values for all targets
#' err <- get.mv.error(mtcars.mreg)
#' vmp <- get.mv.vimp(mtcars.mreg)
#' pred <- get.mv.predicted(mtcars.mreg)
#' 
#' ## standardized error and vimp and OOB prediction
#' err.std <- get.mv.error(mtcars.mreg, std = TRUE)
#' vmp.std <- get.mv.vimp(mtcars.mreg, std = TRUE)
#' pred.oob <- get.mv.predicted(mtcars.mreg, oob = TRUE)
#' 
#' ## ------------------------------------------------------------
#' ## Mixed outcomes analysis
#' ## ------------------------------------------------------------
#' mtcars.new <- mtcars
#' mtcars.new$cyl <- factor(mtcars.new$cyl)
#' mtcars.new$carb <- factor(mtcars.new$carb, ordered = TRUE)
#' mtcars.mix <- rfsrc(cbind(carb, mpg, cyl) ~., data = mtcars.new, tree.err=TRUE)
#' print(mtcars.mix, outcome.target = "mpg")
#' print(mtcars.mix, outcome.target = "cyl")
#' plot(mtcars.mix, outcome.target = "mpg")
#' plot(mtcars.mix, outcome.target = "cyl")
#' 
#' 
#' ## ------------------------------------------------------------
#' ## Custom splitting using the pre-coded examples.
#' ## ------------------------------------------------------------
#' ## motor trend cars
#' mtcars.obj <- rfsrc(mpg ~ ., data = mtcars, splitrule="custom")
#' ## Edgar Anderson's iris data
#' iris.obj <- rfsrc(Species ~., data = iris, splitrule="custom1")
#' ## WIHS analysis
#' wihs.obj <- rfsrc(Surv(time, status) ~ ., wihs, nsplit = 3,
#'                   ntree = 100, splitrule="custom1")
#' }
#' 
rfsrc <- function(formula,
                  data,
                  ntree = 1000,
                  bootstrap = c("by.root", "by.node", "none", "by.user"),
                  mtry = NULL,
                  ytry = NULL,
                  yvar.wt = NULL,
                  nodesize = NULL,
                  nodedepth = NULL,
                  splitrule = NULL,
                  nsplit = NULL,
                  importance = c(FALSE, TRUE, "none", "permute", "random", "anti", "permute.ensemble", "random.ensemble", "anti.ensemble"),
                  err.block = NULL,
                  na.action = c("na.omit", "na.impute"),
                  nimpute = 1,
                  ntime,
                  cause,
                  proximity = FALSE,
                  sampsize = NULL,
                  samptype = c("swr", "swor"),
                  samp = NULL,
                  case.wt = NULL,
                  split.wt = NULL,
                  forest.wt = FALSE,
                  k_for_alpha = 2,
                  stratifier = NULL, ## stratifying variable (e.g. interval number)
                  risk.time = NULL, 
                  categories = list(),
                  fixed_during_bootstrap = NULL,
                  #var.categories = NULL, ## vector of length n.xvar to indicate variable class
                  #category.weights = NULL, ## vector of length number of variable categories

                  xvar.wt = NULL,
                  forest = TRUE,
                  var.used = c(FALSE, "all.trees", "by.tree"),
                  split.depth = c(FALSE, "all.trees", "by.tree"),
                  seed = NULL,
                  do.trace = FALSE,
                  membership = FALSE,
                  statistics = FALSE,
                  verbose = FALSE,
                   ...)
{
  univariate.nomenclature = TRUE

  ## convert to data frame to avoid indexing problems
  data <- as.data.frame(data)

  ## get any hidden options
  user.option     <- list(...)
  impute.only     <- is.hidden.impute.only(user.option)
  terminal.qualts <- is.hidden.terminal.qualts(user.option)
  terminal.quants <- is.hidden.terminal.quants(user.option)

  ## TBD TBD make perf.type visible TBD TBD
  perf.type <- is.hidden.perf.type(user.option)
  rfq       <- is.hidden.rfq(user.option)
  htry      <- is.hidden.htry(user.option)

  ## verify key options
  bootstrap   <- match.arg(bootstrap, c("by.root", "by.node", "none", "by.user"))
  importance  <- match.arg(as.character(importance), c(FALSE, TRUE, "none", "permute", "random", "anti", "permute.ensemble", "random.ensemble", "anti.ensemble"))
  
  na.action   <- match.arg(na.action, c("na.omit", "na.impute"))
  
   
  proximity   <- match.arg(as.character(proximity), c(FALSE, TRUE, "inbag", "oob", "all"))
     
  var.used    <- match.arg(as.character(var.used), c("FALSE", "all.trees", "by.tree"))
  split.depth <- match.arg(as.character(split.depth),  c("FALSE", "all.trees", "by.tree"))
  if (var.used    == "FALSE") var.used    <- FALSE
  if (split.depth == "FALSE") split.depth <- FALSE

  ## data cannot be missing
  if (missing(data)) stop("data is missing")

  ## conduct preliminary formula validation
  if (missing(formula) | (!missing(formula) && is.null(formula))) {
    if (is.null(ytry)) {
      formula <- as.formula("Unsupervised() ~ .")
    }
    else {
      formula <- as.formula(paste("Unsupervised(", ytry, ")~."))
    }
  } 

  ## parse the formula
  formulaPrelim <- parseFormula(formula, data, ytry)

  ## save the call/formula for the return object
  my.call <- match.call()
  my.call$formula <- eval(formula)

  ## conduct preliminary processing of missing data
  ## record whether there's no missing data: efficiency step
  if (any(is.na(data))) {
    data <- parseMissingData(formulaPrelim, data)
    miss.flag <- TRUE
  }
    else {
      miss.flag <- FALSE
    }

  ## finalize the formula based on the pre-processed data
  formulaDetail <- finalizeFormula(formulaPrelim, data)

  ## coherence checks on option parameters
  ntree <- round(ntree)
  if (ntree < 1) stop("Invalid choice of 'ntree'.  Cannot be less than 1.")
  if (!is.null(nodesize) && nodesize < 1) stop("Invalid choice of 'nodesize'. Cannot be less than 1.")
  if (!is.null(nodedepth)) nodedepth = round(nodedepth) else nodedepth = -1
  nimpute <- round(nimpute)
  if (nimpute < 1) stop("Invalid choice of 'nimpute'.  Cannot be less than 1.")

  ## initialize the seed
  seed <- get.seed(seed)

  ## save the family for convenient access
  family <- formulaDetail$family

  ## save the names for convenient access
  xvar.names <- formulaDetail$xvar.names
  yvar.names <- formulaDetail$yvar.names

  ## reality check on x and y
  ## are there any x-variables?  (can happen when for multivariate formula)
  if (length(xvar.names) == 0) {
    stop("something seems wrong: your formula did not define any x-variables")
  }

  ## .. are there any y-variables?  (do not test for the unsupervised case)
  if (family != "unsupv" && length(yvar.names) == 0) {
    stop("something seems wrong: your formula did not define any y-variables")
  }

  ## missing levels are allowed.
  if (family == "class") {
    if (length(setdiff(levels(data[, yvar.names]), unique(data[, yvar.names]))) > 0) {
      warning("empty classes found when implementing classification\n")
    }
  }

  ## mark missing factor levels as NA.
  # final check: make sure yvar is a factor; if not, see if it can be coerced into one
  factor_cols <- sapply(data, is.factor)
  for (yname in yvar.names) {
    if (!factor_cols[yname]) { 
      cat(yname)
      n_levels <- length(unique(data[,yname]))
      if (n_levels > 2) {
        stop(paste("y-variable", yname, "improperly specified; should be a factor with 2 levels."))
      }

      # if it can be coerced into factor, re-parse the formula, too
      else {
        data[,yname] <- as.factor(data[,yname])
        ## parse the formula
        formulaPrelim <- parseFormula(formula, data, ytry)

        ## save the call/formula for the return object
        my.call <- match.call()
        my.call$formula <- eval(formula)
        formulaDetail <- finalizeFormula(formulaPrelim, data)

        ## save the family for convenient access
        family <- formulaDetail$family

        ## save the names for convenient access
        xvar.names <- formulaDetail$xvar.names
        yvar.names <- formulaDetail$yvar.names
        warning(paste("y-variable", yname, "coerced into factor."))
      }
    }
  }

  data <- rm.na.levels(data, xvar.names)
  data <- rm.na.levels(data, yvar.names)


  ## Determine the immutable yvar factor map.
  yfactor <- extract.factor(data, yvar.names)

  ## Determine the immutable xvar factor map.
  xfactor <- extract.factor(data, xvar.names)

  ## get the y-outcome type and nlevels
  yvar.types    <- get.yvar.type(family, yfactor$generic.types, yvar.names)
  yvar.nlevels  <- get.yvar.nlevels(family, yfactor$nlevels, yvar.names, data)

  ## get the x-variable type and nlevels
  xvar.types    <- get.xvar.type(xfactor$generic.types, xvar.names)
  xvar.nlevels  <- get.xvar.nlevels(xfactor$nlevels, xvar.names, data)

  ## Convert the data to numeric mode, apply the na.action protocol.
  data <- finalizeData(c(yvar.names, xvar.names), data, na.action, miss.flag)

  ## Save the row and column names for later overlay
  data.row.names <- rownames(data)
  data.col.names <- colnames(data)

  ## Finalize the xvar matrix.
  xvar <- as.matrix(data[, xvar.names, drop = FALSE])
  rownames(xvar) <- colnames(xvar) <- NULL

  ## Initialize sample size
  ## Set mtry
  ## Set the weight matrix for xvar, and the split
  n <- nrow(xvar)
  n.xvar <- ncol(xvar)
  mtry <- get.grow.mtry(mtry, n.xvar, family)
  samptype <- match.arg(samptype, c("swr", "swor"))

  if (bootstrap == "by.root") {

    ## Set the default value for the size of the bootstrap
    if(missing(sampsize) | is.null(sampsize)) {
      if (samptype == "swr") {
        sampsize <- nrow(xvar)
      }
      if (samptype == "swor") {
        sampsize <- round(nrow(xvar) * (1 - exp(-1)))
      }
    }

    else {
      sampsize <- round(sampsize)
      if (sampsize < 1) {
        stop("sampsize must be greater than zero")
      }
      if (samptype == "swr") {
        ## Size is not limited.
      }
      if (samptype == "swor") {
        ## Size is limited by the number of cases.
        sampsize <- min(sampsize, nrow(xvar))
      }
    }
    maxsampsize <- sampsize
    sampsize <- rep(sampsize, ntree)
    samp = NULL
  }

    else if (bootstrap == "by.user") {

      ## Override sample type when bootstrapping by user.
      samptype <- "swr"

      ## Check for coherent sample.  It will of be dim [n] x [ntree].
      if (is.null(samp)) {
        stop("samp must not be NULL when bootstrapping by user")
      }

      sampsize <- apply(samp, 2, sum)
      #if (sum(sampsize == sampsize[1]) != ntree) {
      #  stop("sampsize must identical for each tree")
      #}
      #sampsize <- sampsize[1]
      maxsampsize <- max(sampsize)
    }

      else {

        ## Override sample size when not bootstrapping by root or user.
        sampsize <- rep(nrow(xvar), ntree)

        ## Override sample type when not bootstrapping by root or user.
        samptype <- "swr"
        maxsampsize <- nrow(xvar)
      }

  case.wt   <- get.weight(case.wt, n)
  forest.wt <- match.arg(as.character(forest.wt), c(FALSE, TRUE, "inbag", "oob", "all"))
  split.wt  <- get.weight(split.wt, n.xvar)

  ## Note that yvar.types is based on the formula:
  ## If the family is unsupervised, yvar.types is NULL
  ## If the family is regression, classification, or
  ## multivariate/mixed, it is of length greater than zero (0).  In the case
  ## of surival and competing risk, it is specifically of length two (2) for
  ## coherency, though it is ignored in the native-code.
  ## Note that ytry is set by the formula.
  ## If the family is unsupervised, ytry assumes the value specified
  ## via the formula.  If the family is regression, classification, or
  ## multivariate/mixed, it defaults to length(yvar.types).  In the case
  ## of survival and competing risk, it assumes the value of two (2) for
  ## coherency, though it is ignored in the native-code.
  if (family == "unspv") {

    ## Override any incoming weights.
    yvar.wt <- NULL
  }
  else {
    yvar.wt <- get.weight(yvar.wt, length(yvar.types))
  }
  xvar.wt  <- get.weight(xvar.wt, n.xvar)

  ## Get the y-outcome
  yvar <- as.matrix(data[, yvar.names, drop = FALSE])
  if(dim(yvar)[2] == 0) {

    ## Override yvar if it is not present.
    yvar <- NULL
  }

  ## Determine the number of missing values
  if (miss.flag) {
    n.miss <- get.nmiss(xvar, yvar)
  }
    else {
      n.miss <- 0
    }

  ## In impute.only, if there is no missing data,
  ## return the data and do nothing else.
  if (impute.only && n.miss == 0) {
    return(data)
  }

  ## Don't need the data anymore.
  remove(data)

  ## Memory management option
  ## (TBD) Not currently implemented.
  big.data <- FALSE

  ## TBD I think we need to separate event related data and dimensioning
  ## of the response which this function combines. TBD 
  ## Get event information and dimensioning for families
  event.info <- get.grow.event.info(yvar, family, ntime = ntime)

  ## Initialize nsplit, noting that it may be been overridden;
  ## also, check if splitrule="poisson.split1" (change to custom2),
  ## or splitrule="poisson.split2" (change to custom3), or 
  ## splitrule="poisson.split3" (change to custom4)
  if (!is.null(splitrule)) {
    if (strcmp(splitrule, "poisson.split1")) {
      splitrule <- "custom2"
    }
    else if (strcmp(splitrule, "poisson.split2")) {
      splitrule <- "custom3"
    }
    else if (strcmp(splitrule, "poisson.split3")) {
      splitrule <- "custom4"
    }
    else if (strcmp(splitrule, "poisson.split4")) {
      splitrule <- "custom5"
    }
    else if (strcmp(splitrule, "poisson.split5")) {
      splitrule <- "custom6"
    }
    else if (strcmp(splitrule, "poisson.split6")) {
      splitrule <- "custom7"
    }
    else if (strcmp(splitrule, "multinomial.split")) {
      splitrule <- "custom8"
    }
  }

  splitinfo <- get.grow.splitinfo(formulaDetail, splitrule, htry, nsplit, event.info$event.type)

  ## Set the cause weights for the split statistic calculation.
  if (family == "surv" || family == "surv-CR") {
    if (length(event.info$event.type) > 1) {

      ## This may or may not be CR, but we do have multiple events.
      ## This requires associated weights.
      if (missing(cause) || is.null(cause)) {
        cause <- NULL
        cause.wt <- rep(1, length(event.info$event.type))
      }

      else {
        if (length(cause) == 1) {

          ## Event specific selection
          if (cause >= 1 && cause <= length(event.info$event.type)) {
            cause.wt <- rep(0, length(event.info$event.type))
            cause.wt[cause] <- 1
          }
          else {
            cause.wt <- rep(1, length(event.info$event.type))
          }
        }
        else {

          ## The user has specified event specific weights
          if (length(cause) == length(event.info$event.type) && all(cause >= 0) && !all(cause == 0)) {
            cause.wt <- cause / sum(cause)
          }
          else {
            cause.wt <- rep(1, length(event.info$event.type))
          }
        }
      }

    }
    else {

      ## We are conducting non-CR, without multiple events.  Send in a dummy value.
      cause <- NULL
      cause.wt = 1
    }

    ## Set the coerced family.
    family <- get.coerced.survival.fmly(family, event.info$event.type, splitinfo$name)
  }
  else {

    ## We pass in NULL for the cause weight for all other families
    cause <- cause.wt <- NULL
  }

  ## Nodesize determination
  nodesize <- get.grow.nodesize(family, nodesize)

  ## Turn ensemble outputs off unless bootstrapping by root or user.
  if ((bootstrap != "by.root") && (bootstrap != "by.user")) {
    importance <- "none"
    perf.type <- "none"
  }

  ## Turn ensemble outputs off for unsupervised mode only
  if (family == "unsupv") {
    importance <- "none"
    perf.type <- "none"
  }

  ## Impute only mode
  ## Some of this may be redundant
  if (impute.only) {
    forest       <- FALSE
    proximity    <- FALSE
       
    var.used     <- FALSE
    split.depth  <- FALSE
    membership   <- FALSE
    perf.type    <- "none"
    importance   <- "none"
    terminal.qualts <- FALSE
    terminal.quants <- FALSE
    ## We have deleted the following line because it was incorrectly
    ## overriding the user specified option na.action in 
    ## impute.rfsrc( generic.impute.rfsrc( rfsrc() ) ).  Now, the user specified option
    ## is respected.  Note that calls from impute.rfsrc()
    ## set the impute.only bit which results in turning of all outputs
    ## other than imputed data.
    ## na.action    <- "na.impute"
  }

  ## Bit dependencies:
  if (terminal.qualts | terminal.quants) {
    forest <- TRUE
  }

  ## Assign low bits for the native code
  impute.only.bits  <- get.impute.only(impute.only, n.miss)
  var.used.bits     <- get.var.used(var.used)
  split.depth.bits  <- get.split.depth(split.depth)
  importance.bits   <- get.importance(importance)
  bootstrap.bits    <- get.bootstrap(bootstrap)
  forest.bits       <- get.forest(forest)
  proximity.bits    <- get.proximity(TRUE, proximity)
     
  membership.bits <-  get.membership(membership)
  statistics.bits <- get.statistics(statistics)
  split.cust.bits <- get.split.cust(splitinfo$cust)

  ## get performance and rfq bits
  perf.type <- get.perf(perf.type, impute.only, family)
  perf.bits <- get.perf.bits(perf.type)
  rfq       <- get.rfq(rfq)
  rfq.bits  <- get.rfq.bits(rfq, family)

  ## Assign high bits for the native code
  samptype.bits   <- get.samptype(samptype)
  forest.wt.bits  <- get.forest.wt(TRUE, bootstrap, forest.wt)
  na.action.bits  <- get.na.action(na.action)
  err.block       <- get.err.block(err.block, ntree)
  terminal.qualts.bits <- get.terminal.qualts(terminal.qualts, FALSE)
  terminal.quants.bits <- get.terminal.quants(terminal.quants, FALSE)

  ## Set the trace
  do.trace <- get.trace(do.trace)

  ## create default values for risk.time/stratifier: 1 for all,
  ## if not supplied
  if (is.null(risk.time)) risk.time   <- as.numeric(rep.int(1, n))
  if (is.null(stratifier)) stratifier <- rep.int(1, n)


  ## ensure risk.time is of correct length
  if (length(risk.time) != n) 
    stop("Make sure argument 'risk.time' is of correct length.")


  ## if stratifier argument is not NULL, ensure it is of correct length
  if (length(stratifier) != n)
    stop("Make sure argument 'stratifier' is of correct length.")

  # pass default max-stratifier
  max_strat <- max(stratifier)

  ################################################################
  ## verify categories argument
  ################################################################

  if (!is.list(categories)) {
    stop("Make sure argument 'categories' is of class 'list', with elements 'var.assignments' and 'categories.weights'.")
  }

  var.categories    <- categories[["var.assignments"]]
  category.weights  <- categories[["categories.weights"]]

  ## verify var.categories
  if (length(var.categories) != n.xvar && !is.null(var.categories))
    stop("Make sure argument 'var.categories' is of correct length.")

  # default (if not specified)
  n.categories <- 1

  ## perform default behavior for this, and category.weights, if not
  ## specified
  if (is.null(var.categories)) {
    var.categories <- as.integer(rep(1, n.xvar))
    if (!is.null(category.weights)) {
      if (verbose)
        warning("var.categories not specified, so ignoring value of category.weights.")
    }
    category.weights <- 1
  }

  ## if var.categories is specified
  else {
      if (class(var.categories) != "factor") {
        if (verbose) warning("Did not receive argument 'var.categories' of class 'factor'.")
        if (class(var.categories) != "integer")
          stop("...also not of class integer. Pass as integer or factor vector.")
        var.categories <- as.factor(var.categories)
      }

      # take care of category.weights
      n.categories <- length(unique(var.categories))

      # if null
      if (is.null(category.weights)) {
        if (verbose) warning("category.weights not specified. Setting default (uniform) weights.")
        category.weights <- rep(1, n.categories)
      }

      # if is a list, extract 
      #if (is.list(category.weights)) {
      #  category.weights <- 
      #}

      if (length(category.weights) != n.categories) 
        stop("Incorrect # of category weights supplied. Ensure length(category.weights) == length(unique(var.categories)).")

      # ensure sums to 1
      if (sum(category.weights) != 1.0) {
        if (verbose) warning("Sum of category.weights not specified to sum to 1. Normalizing by default.")
        category.weights <- category.weights / sum(category.weights)
      }
  }

  # manipulate fixed_during_bootstrap to obtain indices of specified variables
  to_fix <- which(fixed_during_bootstrap %in% xvar.names)

  nativeOutput <- tryCatch({.Call("rfsrcGrow",
                                  as.integer(do.trace),
                                  as.integer(seed),
                                  as.integer(impute.only.bits +
                                             var.used.bits +
                                             split.depth.bits +
                                             importance.bits +
                                             bootstrap.bits +
                                             forest.bits +
                                             proximity.bits +
                                             perf.bits +
                                             rfq.bits +
                                             statistics.bits),
                                  as.integer(samptype.bits +
                                             forest.wt.bits +
                                              
                                             na.action.bits +
                                             split.cust.bits +
                                             membership.bits +
                                             terminal.qualts.bits +
                                             terminal.quants.bits),
                                  as.integer(splitinfo$index),
                                  as.integer(splitinfo$nsplit),
                                  as.integer(mtry),
                                  as.integer(htry),
                                  as.integer(formulaDetail$ytry),
                                  as.integer(nodesize),
                                  as.integer(nodedepth),
                                  as.integer(length(cause.wt)),
                                  as.double(cause.wt),
                                  as.integer(ntree),
                                  as.integer(n),
                                  as.integer(length(yvar.types)),
                                  as.character(yvar.types),
                                  as.integer(yvar.nlevels),
                                  as.double(as.vector(yvar)),
                                  as.integer(n.xvar),
                                  as.character(xvar.types),
                                  as.integer(xvar.nlevels),
                                  as.double(risk.time),
                                  as.integer(stratifier),
                                  as.integer(max_strat),
                                  as.integer(var.categories),
                                  as.double(category.weights),
                                  as.integer(n.categories),
                                  as.integer(to_fix),
                                  as.integer(length(to_fix)),
                                  as.double(k_for_alpha),
                                  as.integer(maxsampsize),
                                  as.integer(sampsize),
                                  as.integer(samp),
                                  as.double(case.wt),
                                  as.double(split.wt),
                                  as.double(yvar.wt),
                                  as.double(xvar.wt),
                                  as.double(xvar),
                                  as.integer(length(event.info$time.interest)),
                                  as.double(event.info$time.interest),
                                  as.integer(nimpute),
                                  as.integer(err.block),
                                  as.integer(get.rf.cores()))}, error = function(e) {
                                    print(e)
                                    NULL})
  
  ## check for error return condition in the native code
  if (is.null(nativeOutput)) {
    if (impute.only) {
      ## in impute mode we proceed and return NULL
      return(NULL)
    }
      else {
        ## currently all other modes will error out with the RFSRC error message
        stop("An error has occurred in the grow algorithm.  Please turn trace on for further analysis.")
      }
  }
  ## check if there was missing data, and assign imputed data if so.
  if (n.miss > 0) {
    imputed.data <- matrix(nativeOutput$imputation, nrow = n.miss, byrow = FALSE)
    imputed.indv <- imputed.data[, 1]
    imputed.data <- as.matrix(imputed.data[, -1, drop = FALSE])
    ##if (n.miss == 1) {
    ##  imputed.data <- t(imputed.data)
    ##}
    nativeOutput$imputation <- NULL
    ## fill NA's in original GROW data with multiply imputed values.
    ## This will now serve as the forest data set and will enable
    ## recovery of terminal node membership with the head of the
    ## seed chain
    if (nimpute > 1) {
      ## the forest was grown using overlaid full (all) summary values
      if (grepl("surv", family)) {
        yvar[imputed.indv, 1] <- imputed.data[, 1]
        yvar[imputed.indv, 2] <- imputed.data[, 2]
        xvar[imputed.indv, ] <- imputed.data[, -c(1:2), drop = FALSE]
      }
        else {
          if (!is.null(yvar.types)) {
            yvar[imputed.indv, ] <- imputed.data[, 1:length(yvar.types), drop = FALSE]
            xvar[imputed.indv, ] <- imputed.data[, -c(1:length(yvar.types)), drop = FALSE]
          }
            else {
              xvar[imputed.indv, ] <- imputed.data
            }
        }
      ## remove the imputed data outputs
      imputed.indv <- NULL
      imputed.data <- NULL
      imputedOOBData <- NULL
      ## the missingness action for the training data is formally safed.  This is saved as part of the
      ## forest object, but the setting is now irrelevant as the training data has no missingness.
      na.action = "na.omit"
    }
      else {
        ## add column names to the imputed data outputs in the absence
        ## of multiple imputation.
        colnames(imputed.data) <- c(yvar.names, xvar.names)
        imputed.data <- as.data.frame(imputed.data)
      }
    ## now map the imputed.data columns back to their original order
    ## commented out because of difficulty in maintaining order across different modes
    ## imputed.data <- imputed.data[, data.col.names]
  }
  ## add row and column names to xvar matrix
  xvar <- as.data.frame(xvar)
  rownames(xvar) <- data.row.names
  colnames(xvar) <- xvar.names
  ## map xvar factors back to original values
  xvar <- map.factor(xvar, xfactor)
  ## add column names to response matrix
  ## does not apply for unsupervised mode
  if (family != "unsupv") {
    yvar <- as.data.frame(yvar)
    colnames(yvar) <- yvar.names
  }
  else {
    yvar <- NULL
  }
  ## map response factors back to original values
  ## does not apply for unsupervised mode
  if (family != "unsupv") {
    if (family == "regr+" | family == "class+" | family == "mix+") {
      yvar <- map.factor(yvar, yfactor)
    }
    else {
      yvar <- amatrix.remove.names(map.factor(yvar, yfactor))
    }
  }
  ## class imbalanced processing
  pi.hat <- NULL
  if (family == "class" && rfq) {
    pi.hat <- table(yvar) / length(yvar)
  }
  ## map imputed data factors back to original values
  ## does NOT apply for y under unsupervised mode
  if ((n.miss > 0) & (nimpute < 2)) {
    imputed.data <- map.factor(imputed.data, xfactor)
    if (family != "unsupv") {
      imputed.data <- map.factor(imputed.data, yfactor)
    }
  }
  ## Define the forest.
  if (forest) {
      ## We allocate, in the native code, the native array to its
      ## theoretical maximum.  This is trimmed below to the actual
      ## size.
      nativeArraySize = 0
      if (htry == 0) {
          mwcpCountSummary = rep (0, 1)
          nativeFactorArray <- vector("list", 1)
      }
      else {
          mwcpCountSummary = rep (0, htry)
          nativeFactorArray <- vector("list", htry)
      }
    ## Marker for start of native forest topology.  This can change with the outputs requested.
    ## For the arithmetic related to the pivot point, you need to refer to stackOutput.c and in
    ## particular, stackForestOutputObjects().
    pivot <- which(names(nativeOutput) == "treeID")
    nullO <- lapply(1:ntree, function(b) {
      if (nativeOutput$leafCount[b] > 0) {
        ## The tree was not rejected.  Count the number of internal
        ## and external (terminal) nodes in the forest.
        nativeArraySize <<- nativeArraySize + (2 * nativeOutput$leafCount[b]) - 1
        mwcpCountSummary[1] <<- mwcpCountSummary[1] + nativeOutput$mwcpCT[b]
        if (htry > 1) {
          for (i in 0:(htry-2)) {
            mwcpCountSummary[i] <<- mwcpCountSummary[i] + nativeOutput[[pivot + 9 + (5 * htry) + i]][b]
          }
        }
      }
      else {
        ## The tree was rejected.  However, it acts as a
        ## placeholder, being a stump topologically and thus
        ## adds to the total node count.
        nativeArraySize <<- nativeArraySize + 1
      }
      NULL
    })
    rm(nullO)##memory saving device
    nativeArray <- as.data.frame(cbind(nativeOutput$treeID[1:nativeArraySize],
                                       nativeOutput$nodeID[1:nativeArraySize]))
    nativeArrayHeader <- c("treeID", "nodeID")
    nativeArray <- as.data.frame(cbind(nativeArray,
                                       nativeOutput$parmID[1:nativeArraySize],
                                       nativeOutput$contPT[1:nativeArraySize],
                                       nativeOutput$mwcpSZ[1:nativeArraySize]))
    nativeArrayHeader <- c(nativeArrayHeader, "parmID", "contPT", "mwcpSZ") 
    if (mwcpCountSummary[1] > 0) {
      ## This can be NULL if there are no factor splits along this dimension.
      nativeFactorArray[[1]] <- nativeOutput$mwcpPT[1:mwcpCountSummary[1]]
    }
    nativeFactorArrayHeader <- "mwcpPT"
    if (htry > 0) {
      nativeArray <- as.data.frame(cbind(nativeArray,
                                         nativeOutput[[pivot + 7]][1:nativeArraySize]))
      nativeArrayHeader <- c(nativeArrayHeader, "hcDim")
      nativeArray <- as.data.frame(cbind(nativeArray,
                                         nativeOutput[[pivot + 8]][1:nativeArraySize]))
      nativeArrayHeader <- c(nativeArrayHeader, "contPTR1")
    }
    if (htry > 1) {
      for (i in 0:(htry-2)) {
        nativeArray <- as.data.frame(cbind(nativeArray,
                                           nativeOutput[[pivot + 9 + (0 * htry) + i]][1:nativeArraySize]))
        nativeArrayHeader <- c(nativeArrayHeader, paste("hcDim", i+2, sep=""))
        nativeArray <- as.data.frame(cbind(nativeArray,
                                           nativeOutput[[pivot + 9 + (1 * htry) + i]][1:nativeArraySize]))
        nativeArrayHeader <- c(nativeArrayHeader, paste("parmID", i+2, sep=""))
        nativeArray <- as.data.frame(cbind(nativeArray,
                                           nativeOutput[[pivot + 9 + (2 * htry) + i]][1:nativeArraySize]))
        nativeArrayHeader <- c(nativeArrayHeader, paste("contPT", i+2, sep=""))
        nativeArray <- as.data.frame(cbind(nativeArray,
                                           nativeOutput[[pivot + 9 + (3 * htry) + i]][1:nativeArraySize]))
        nativeArrayHeader <- c(nativeArrayHeader, paste("contPTR", i+2, sep=""))
        nativeArray <- as.data.frame(cbind(nativeArray,
                                           nativeOutput[[pivot + 9 + (4 * htry) + i]][1:nativeArraySize]))
        nativeArrayHeader <- c(nativeArrayHeader, paste("mwcpSZ", i+2, sep=""))
        nativeArray <- as.data.frame(cbind(nativeArray,
                                           nativeOutput[[pivot + 9 + (5 * htry) + i]][1:nativeArraySize]))
        nativeArrayHeader <- c(nativeArrayHeader, paste("mwcpPT", i+2, sep=""))
        if (mwcpCountSummary[i+2] > 0) {
          ## This can be NULL if there are no factor splits along this dimension.
          nativeFactorArray[[i+2]] <- nativeOutput[[pivot + 9 + (6 * htry) + i]][1:mwcpCountSummary[i+2]]
        }
        nativeFactorArrayHeader <- c(nativeFactorArrayHeader, paste("mwcpPT", i+2, sep=""))
      }
    }
    names(nativeArray) <- nativeArrayHeader
    names(nativeFactorArray) <- nativeFactorArrayHeader

    if (terminal.qualts | terminal.quants) {
      ## The terminal node qualitative and quantitative outputs
      ## are allocated to their theoretical maximum.
      ## Each tree-specific segment has an initialized and uninitialized
      ## partition. It will have [1:leafCount] validly populated, and
      ## [leafCount+1 : treeTheoreticalMaximum] uninitialized.  We parse the
      ## valid segments here and concatenate them.
      temp <- 2 * (nodesize - 1)

      if (maxsampsize  > temp) { 
        treeTheoreticalMaximum <- maxsampsize - temp;
      }
        else {
          treeTheoreticalMaximum <- 1;
        }
      forestTheoreticalMaximum <- treeTheoreticalMaximum * ntree
      offset <- 0
      valid.mcnt.indices <- unlist(lapply(1:ntree, function(b) {
        if (b > 1) {
          offset <<- offset + treeTheoreticalMaximum
        }
        (offset + 1):(offset + nativeOutput$leafCount[b])
      }))
      if (terminal.quants) {
        ## family specific additions to the grow object
        if (grepl("surv", family)) {
          offset <- 0
          valid.2D.surv.indices <- unlist(lapply(1:ntree, function(b) {
            if (b > 1) {
              offset <<- offset + (treeTheoreticalMaximum * length(event.info$event.type) * length(event.info$time.interest))
            }
            (offset + 1):(offset + nativeOutput$leafCount[b] * length(event.info$event.type) * length(event.info$time.interest))
          }))
          offset <- 0
          valid.1D.surv.indices <- unlist(lapply(1:ntree, function(b) {
            if (b > 1) {
              offset <<- offset + (treeTheoreticalMaximum * length(event.info$time.interest))
            }
            (offset + 1):(offset + nativeOutput$leafCount[b] * length(event.info$time.interest))
          }))
          offset <- 0
          valid.mort.indices <- unlist(lapply(1:ntree, function(b) {
            if (b > 1) {
              offset <<- offset + (treeTheoreticalMaximum * length(event.info$event.type))
            }
            (offset + 1):(offset + nativeOutput$leafCount[b] * length(event.info$event.type))
          }))
        }
        else {
          ## This will pick up all "C" and "I".
          class.index <- which(yvar.types != "R")
          class.count <- length(class.index)
          regr.index <- which(yvar.types == "R")
          regr.count <- length(regr.index)
          if (class.count > 0) {
            ## Vector to hold the number of levels in each factor response. 
            levels.count <- array(0, class.count)
            counter <- 0
            for (i in class.index) {
              counter <- counter + 1
              ## Note that [i] is the actual index of the y-variables and not a sequential iterator.
              ## The sequential iteratior is [counter]
              levels.count[counter] <- yvar.nlevels[i]
            }
            offset <- 0
            valid.clas.indices <- unlist(lapply(1:ntree, function(b) {
              if (b > 1) {
                offset <<- offset + (treeTheoreticalMaximum * sum(levels.count))
              }
              (offset + 1):(offset + nativeOutput$leafCount[b] * sum(levels.count))
            }))
          }
          if (regr.count > 0) {
            offset <- 0
            valid.regr.indices <- unlist(lapply(1:ntree, function(b) {
              if (b > 1) {
                offset <<- offset + (treeTheoreticalMaximum * regr.count)
              }
              (offset + 1):(offset + nativeOutput$leafCount[b] * regr.count)
            }))
          }
        }
      }
      nativeArrayTNDS <- list(if(!is.null(nativeOutput$tnSURV)) nativeOutput$tnSURV[valid.1D.surv.indices] else NULL,
                              if(!is.null(nativeOutput$tnMORT)) nativeOutput$tnMORT[valid.mort.indices] else NULL,
                              if(!is.null(nativeOutput$tnNLSN)) nativeOutput$tnNLSN[valid.1D.surv.indices] else NULL,
                              if(!is.null(nativeOutput$tnCSHZ)) nativeOutput$tnCSHZ[valid.2D.surv.indices] else NULL,
                              if(!is.null(nativeOutput$tnCIFN)) nativeOutput$tnCIFN[valid.2D.surv.indices] else NULL,
                              if(!is.null(nativeOutput$tnREGR)) nativeOutput$tnREGR[valid.regr.indices] else NULL,
                              if(!is.null(nativeOutput$tnCLAS)) nativeOutput$tnCLAS[valid.clas.indices] else NULL,
                              nativeOutput$rmbrMembership,
                              nativeOutput$ambrMembership,
                              nativeOutput$tnRCNT[valid.mcnt.indices],
                              nativeOutput$tnACNT[valid.mcnt.indices]);
      names(nativeArrayTNDS) <- c("tnSURV","tnMORT","tnNLSN","tnCSHZ","tnCIFN","tnREGR","tnCLAS", "tnRMBR", "tnAMBR", "tnRCNT", "tnACNT")
    }
    else {
      nativeArrayTNDS <- NULL
    }
    forest.out <- list(htry = htry,
                       nativeArray = nativeArray,
                       nativeFactorArray = nativeFactorArray,
                       totalNodeCount = dim(nativeArray)[1],
                       nodesize = nodesize,
                       nodedepth = nodedepth,
                       ntree = ntree,
                       family = family,
                       splitrule = splitinfo$name,
                       yvar = yvar,
                       yvar.names = yvar.names,
                       xvar = xvar,
                       xvar.names = xvar.names,
                       seed = nativeOutput$seed,
                       bootstrap = bootstrap,
                       sampsize = sampsize,
                       samptype = samptype,
                       samp     = samp,
                       case.wt  = case.wt,
                       terminal.qualts = terminal.qualts,
                       terminal.quants = terminal.quants,
                       nativeArrayTNDS = nativeArrayTNDS,
                       version = "2.6.1",
                       na.action = na.action,
                       perf.type = perf.type)
    ## family specific additions to the forest object
    if (grepl("surv", family)) {
      forest.out$time.interest <- event.info$time.interest
    }
    ## Initialize the default class of the forest.
    class(forest.out) <- c("rfsrc", "forest", family)
    if (big.data) {
      class(forest.out) <- c(class(forest.out), "bigdata")
    }
  }
  ## the forest is NULL (the user has requested not to save the forest)
  else {
    forest.out <- NULL
  }
  ## process the proximity matrix
  if (proximity != FALSE) {
    proximity.out <- matrix(0, n, n)
    count <- 0
    for (k in 1:n) {
      proximity.out[k,1:k] <- nativeOutput$proximity[(count+1):(count+k)]
      proximity.out[1:k,k] <- proximity.out[k,1:k]
      count <- count + k
    }
    nativeOutput$proximity <- NULL
  }
  else {
    proximity.out <- NULL
  }
    
  ## process weight matrix
  if (forest.wt != FALSE) {
    forest.wt.out <- matrix(nativeOutput$weight, c(n, n), byrow = TRUE)
    nativeOutput$weight <- NULL
  }
  else {
    forest.wt.out <- NULL
  }
  ## membership
  if (membership) {
    membership.out <- matrix(nativeOutput$nodeMembership, c(n, ntree))
    inbag.out <- matrix(nativeOutput$bootMembership, c(n, ntree))
    nativeOutput$nodeMembership <- NULL
    nativeOutput$bootMembership <- NULL
  }
    else {
      membership.out <- NULL
      inbag.out <- NULL
    }
  ## variables used
  if (var.used != FALSE) {
    if (var.used == "all.trees") {
      var.used.out <- nativeOutput$varUsed
      names(var.used.out) <- xvar.names
    }
      else {
        var.used.out <- matrix(nativeOutput$varUsed, nrow = ntree, byrow = TRUE)
        colnames(var.used.out) <- xvar.names
      }
    nativeOutput$varUsed <- NULL
  }
    else {
      var.used.out <-  NULL
    }
  ## split depth
  if (split.depth != FALSE) {
    if (split.depth == "all.trees") {
      split.depth.out <- array(nativeOutput$splitDepth, c(n, n.xvar))
    }
    else {
      split.depth.out <- array(nativeOutput$splitDepth, c(n, n.xvar, ntree))
    }
    nativeOutput$splitDepth <- NULL
  }
  else {
    split.depth.out <-  NULL
  }
  ## node statistics
  if (statistics) {
    node.stats <- as.data.frame(cbind(nativeOutput$spltST[1:nativeArraySize]))
    colnames(node.stats) <- "spltST"
    node.mtry.stats <- t(array(nativeOutput$mtryST[1:nativeArraySize], c(mtry, forest.out$totalNodeCount)))
    node.mtry.index <- t(array(nativeOutput$mtryID[1:nativeArraySize], c(mtry, forest.out$totalNodeCount)))
    ## In the case of unsupervised splitting, output additional
    ## statistics.
    ## (TBD TBD) We need to output ytry related statistics in
    ## the supervised case as well, following the introduction of ytry in
    ## supervised settings.
    if (family == "unsupv") {
      node.ytry.index <- t(array(nativeOutput$uspvST[1:nativeArraySize], c(formulaDetail$ytry, forest.out$totalNodeCount)))
    }
      else {
        node.ytry.index <- NULL
      }
  }
    else {
      node.stats      <- NULL
      node.mtry.stats <- NULL
      node.mtry.index <- NULL
      node.ytry.index <- NULL
    }
  ## make the output object
  rfsrcOutput <- list(
    call = my.call,
    family = family,
    n = n,
    ntree = ntree,
    nimpute = nimpute,
    mtry = mtry,
    nodesize = nodesize,
    nodedepth = nodedepth,
    nsplit = splitinfo$nsplit,
    yvar = yvar,
    yvar.names = yvar.names,
    xvar = xvar,
    xvar.names = xvar.names,
    xvar.wt = xvar.wt,
    split.wt = split.wt,
    cause.wt = cause.wt,
    leaf.count = nativeOutput$leafCount,
    proximity = proximity.out,
    forest = forest.out,
    forest.wt = forest.wt.out,    
     
    membership = membership.out,
    splitrule = splitinfo$name,
    inbag = inbag.out,
    var.used = var.used.out,
    imputed.indv = (if (n.miss > 0) imputed.indv else NULL),
    imputed.data = (if (n.miss > 0) imputed.data else NULL),
    split.depth  = split.depth.out,
    node.stats      = node.stats,
    node.mtry.stats = node.mtry.stats,
    node.mtry.index = node.mtry.index,
    node.ytry.index = node.ytry.index,
    err.block = err.block
  )
  ## memory management
  remove(yvar)
  remove(xvar)
  nativeOutput$leafCount <- NULL
  remove(proximity.out)
  remove(forest.out)
  remove(forest.wt.out)
    
  remove(membership.out)
  remove(inbag.out)
  remove(var.used.out)
  if (n.miss > 0) remove(imputed.indv)
  if (n.miss > 0) remove(imputed.data)
  remove(split.depth.out)
  ## save the outputs
  survOutput <- NULL
  classOutput <- NULL
  regrOutput <- NULL
  ## EFFICIENCY EFFICIENCY EFFICIENCY
  ## for efficiency - the following does not need to be executed in impute.only mode
  if (!impute.only) {
    ## family specific additions to the grow object
    if (grepl("surv", family)) {
        if ((length(event.info$event.type) > 1) &&
            (splitinfo$name != "l2.impute") &&
            (splitinfo$name != "logrankscore")) {
        coerced.event.count <- length(event.info$event.type)
      }
        else {
          coerced.event.count <- 1
        }
      if (family == "surv") {
        ## Right Censored names.
        ens.names <- list(NULL, NULL)
        mortality.names <- list(NULL, NULL)
        err.names <- list(NULL, NULL)
        vimp.names <- list(NULL, xvar.names)
      }
        else {
          ## Competing Risk names.
          ens.names <- list(NULL, NULL, c(paste("condCHF.", 1:length(event.info$event.type), sep = "")))
          mortality.names <- list(NULL, paste("event.", 1:length(event.info$event.type), sep = ""))
          cif.names <- list(NULL, NULL, c(paste("CIF.", 1:length(event.info$event.type), sep = "")))
          err.names <- list(c(paste("event.", 1:length(event.info$event.type), sep = "")), NULL)
          vimp.names <- list(paste("event.", 1:length(event.info$event.type), sep = ""), xvar.names)
        }
      ## From the native code:
      ##   "allEnsbCHF"
      ##   "oobEnsbCHF"
      ## -> of dim [length(event.info$event.type)] x [RF_sortedTimeInterestSize] x [n]
      ##    where [length(event.info$event.type)] may be equal to [1].
      ## To the R code:
      ## -> of dim [n] x [RF_sortedTimeInterestSize] x [length(event.info$event.type)]  
      chf <- (if (!is.null(nativeOutput$allEnsbCHF))
                adrop3d.last(array(nativeOutput$allEnsbCHF,
                                   c(n, length(event.info$time.interest), length(event.info$event.type)),
                                   dimnames=ens.names), coerced.event.count) else NULL)
      nativeOutput$allEnsbCHF <- NULL
      survOutput <- list(chf = chf)
      remove(chf)
      chf.oob <- (if (!is.null(nativeOutput$oobEnsbCHF))
                    adrop3d.last(array(nativeOutput$oobEnsbCHF,
                                       c(n, length(event.info$time.interest), length(event.info$event.type)),
                                       dimnames=ens.names), coerced.event.count) else NULL)
      nativeOutput$oobEnsbCHF <- NULL
      survOutput = c(survOutput, chf.oob = list(chf.oob))
      remove(chf.oob)
      ## From the native code:
      ##   "allEnsbMRT"
      ##   "oobEnsbMRT"
      ## -> of dim [length(event.info$event.type)] x [n]
      ## To the R code:
      ## -> of dim [n] x [length(event.info$event.type)] 
      predicted <- (if (!is.null(nativeOutput$allEnsbMRT))
                      adrop2d.last(array(nativeOutput$allEnsbMRT,
                                         c(n, length(event.info$event.type)), dimnames=mortality.names), coerced.event.count) else NULL)
      nativeOutput$allEnsbMRT <- NULL
      survOutput = c(survOutput, predicted = list(predicted))
      remove(predicted)
      predicted.oob <- (if (!is.null(nativeOutput$oobEnsbMRT))
                          adrop2d.last(array(nativeOutput$oobEnsbMRT,
                                             c(n, length(event.info$event.type)), dimnames=mortality.names), coerced.event.count) else NULL)
      nativeOutput$oobEnsbMRT <- NULL
      survOutput <- c(survOutput, predicted.oob = list(predicted.oob))
      remove(predicted.oob)
      ## From the native code:
      ##   "allEnsbSRV"
      ##   "oobEnsbSRV"
      ## -> of dim [RF_sortedTimeInterestSize] x [n]
      ## To the R code:
      ## -> of dim [n] x [RF_sortedTimeInterestSize]
      survival <-  (if (!is.null(nativeOutput$allEnsbSRV))
                      matrix(nativeOutput$allEnsbSRV,
                             c(n, length(event.info$time.interest))) else NULL)
      nativeOutput$allEnsbSRV <- NULL
      survOutput <- c(survOutput, survival = list(survival))
      remove(survival)
      survival.oob <-  (if (!is.null(nativeOutput$oobEnsbSRV))
                          matrix(nativeOutput$oobEnsbSRV,
                                 c(n, length(event.info$time.interest))) else NULL)
      nativeOutput$oobEnsbSRV <- NULL
      survOutput <- c(survOutput, survival.oob = list(survival.oob))
      remove(survival.oob)
      ## From the native code:
      ##   "allEnsbCIF"
      ##   "oobEnsbCIF"
      ##   -> of dim [length(event.info$event.type)] x [RF_sortedTimeInterestSize] x [n]
      ## To the native code:
      ##   -> of dim  [n] x [RF_sortedTimeInterestSize] x [length(event.info$event.type)]
      cif <- (if (!is.null(nativeOutput$allEnsbCIF))
                array(nativeOutput$allEnsbCIF,
                      c(n, length(event.info$time.interest), length(event.info$event.type)),
                      dimnames=cif.names) else NULL)
      nativeOutput$allEnsbCIF <- NULL
      survOutput <- c(survOutput, cif = list(cif))
      remove(cif)
      cif.oob <- (if (!is.null(nativeOutput$oobEnsbCIF))
                    array(nativeOutput$oobEnsbCIF,
                          c(n, length(event.info$time.interest), length(event.info$event.type)),
                          dimnames=cif.names) else NULL)
      nativeOutput$oobEnsbCIF <- NULL
      survOutput = c(survOutput, cif.oob = list(cif.oob))
      remove(cif.oob)
      ## From the native code:
      ##   "perfSurv"
      ##   -> of dim [ntree] x length(event.info$event.type)]
      ## To the R code:
      ##   -> of dim [ntree] x length(event.info$event.type)]
      if (!is.null(nativeOutput$perfSurv)) {
        err.rate <- adrop2d.first(array(nativeOutput$perfSurv,
                                        c(length(event.info$event.type), ntree),
                                        dimnames=err.names),
                                  coerced.event.count)
        nativeOutput$perfSurv <- NULL
        if (family == "surv-CR") {
          survOutput = c(survOutput, err.rate = list(t(err.rate)))
        }
          else {
            survOutput = c(survOutput, err.rate = list(err.rate))
          }
        remove(err.rate)
      }
      ## From the native code:
      ##   "vimpSurv"
      ##   -> of dim [n.xvar] x length(event.info$event.type)]
      ## To the R code:
      ##   -> of dim length(event.info$event.type)] x [n.xvar]
      if (!is.null(nativeOutput$vimpSurv)) {
        importance <- adrop2d.first(array(nativeOutput$vimpSurv,
                                          c(length(event.info$event.type), n.xvar),
                                          dimnames = vimp.names),
                                    coerced.event.count)
        nativeOutput$vimpSurv <- NULL
        if (family == "surv-CR") {
          survOutput = c(survOutput, importance = list(t(importance)))
        }
          else {
            survOutput = c(survOutput, importance = list(importance))
          }
        remove(importance)
      }
      survOutput = c(
        survOutput, list(
          time.interest = event.info$time.interest,
          ndead = sum(na.omit(event.info$cens) != 0))
      )
      ## When TRUE we revert to univariate nomenclature for all the outputs.
      if(univariate.nomenclature) {
        rfsrcOutput <- c(rfsrcOutput, survOutput)
      }
        else {
          rfsrcOutput <- c(rfsrcOutput, survOutput = list(survOutput))
        }
    }
    else {
      ## We consider "R", "I", and "C" outcomes.  The outcomes are grouped
      ## by type and sequential.  That is, the first "C" encountered in the
      ## response type vector is in position [[1]] in the classification output
      ## list, the second "C" encountered is in position [[2]] in the
      ## classification output list, and so on.  The same applies to the
      ## regression outputs.  We also have a mapping from the outcome slot back
      ## to the original response vector type, given by the following:
      ## Given yvar.types = c("R", "C", "R", "C", "R" , "I")
      ## regr.index[1] -> 1
      ## regr.index[2] -> 3
      ## regr.index[3] -> 5
      ## clas.index[1] -> 2
      ## clas.index[2] -> 4
      ## clas.index[3] -> 6
      ## This will pick up all "C" and "I".
      class.index <- which(yvar.types != "R")
      class.count <- length(class.index)
      regr.index <- which(yvar.types == "R")
      regr.count <- length(regr.index)
      if (class.count > 0) {
        classOutput <- vector("list", class.count)
        ## Names of the classification outputs.
        names(classOutput) <- yvar.names[class.index]
        ## Vector to hold the number of levels in each factor response. 
        levels.count <- array(0, class.count)
        ## List to hold the names of levels in each factor response. 
        levels.names <- vector("list", class.count)
        counter <- 0
        for (i in class.index) {
            counter <- counter + 1
            ## Note that [i] is the actual index of the y-variables and not a sequential iterator.
            ## The sequential iteratior is [counter]
            levels.count[counter] <- yvar.nlevels[i]
            if (yvar.types[i] == "C") {
              ## This an unordered factor.
              ## Here, we don't know the sequence of the unordered factor list, so we identify the factor by name.
              levels.names[[counter]] <- yfactor$levels[[which(yfactor$factor == yvar.names[i])]]
            }
              else {
                ## This in an ordered factor.
                ## Here, we don't know the sequence of the ordered factor list, so we identify the factor by name.
                levels.names[[counter]] <- yfactor$order.levels[[which(yfactor$order == yvar.names[i])]]
              }
        }
        ## Incoming error rates: T=tree R=response L=level
        ## T1R1L0 T1R1L1 T1R1L2 T1R1L3 T1R2L0 T1R2L1 T1R2L2, T2R1L0 T2R1L1 T2R1L2 T2R1L3 T2R2L0 T2R2L1 T2R2L2, ... 
        ## In GROW mode, all class objects are represented in the tree offset calculation.
        ## In PRED mode, the offsets are dependent on the only those targets that are requested!
        ## Yields tree.offset = c(1, 8, ...) 
        tree.offset <- array(1, ntree)
        if (ntree > 1) {
          tree.offset[2:ntree] <- sum(1 + levels.count)
        }
        tree.offset <-  cumsum(tree.offset)
        ## Incoming vimp rates: V=xvar R=response L=level
        ## V1R1L0 V1R1L1 V1R1L2 V1R1L3 V1R1L0 V1R2L1 V1R2L2, V2R1L0 V2R1L1 V2R1L2 V2R1L3 V2R2L0 V2R2L1 V2R2L2, ... 
        ## Yields vimp.offset = c(1, 8, ...) 
        vimp.offset <- array(1, n.xvar)
        if (n.xvar > 1) {
          vimp.offset[2:n.xvar] <- sum(1 + levels.count)
        }
        vimp.offset <-  cumsum(vimp.offset)
        iter.ensb.start <- 0
        iter.ensb.end   <- 0
        ## From the native code:
        ##   "allEnsbCLS"
        ##   "oobEnsbCLS"
        ## -> of dim [class.count] x [levels.count[]] x [n]
        ##    where this is a ragged array.
        ## From the native code:
        ##   "perfClas"
        ## -> of dim [ntree] x [class.count] x [1 + levels.count[]]
        ## where the slot [.] x [.] x [1] holds the unconditional error rate.
        ## Note that this is a ragged array.
        ## To the R code:
        ## -> of dim [[class.count]] x [ntree] x [1 + levels.count[]] 
        ## From the native code:
        ##   "vimpClas"
        ## -> of dim [n.xvar] x [class.count] x [1 + levels.count[]]
        ## where the slot [.] x [.] x [1] holds the unconditional vimp.
        ## Note that this is a ragged array.
        ## To the R code:
        ## -> of dim [[class.count]] x [1 + levels.count[]] x [n.xvar] 
        for (i in 1:class.count) {
          iter.ensb.start <- iter.ensb.end
          iter.ensb.end <- iter.ensb.end + (levels.count[i] * n)
          ens.names <- list(NULL, levels.names[[i]])
          err.names <- c("all", levels.names[[i]])
          vimp.names <- list(c("all", levels.names[[i]]), xvar.names)
          predicted <- (if (!is.null(nativeOutput$allEnsbCLS))
                        array(nativeOutput$allEnsbCLS[(iter.ensb.start + 1):iter.ensb.end],
                              c(n, levels.count[i]), dimnames=ens.names) else NULL)
          classOutput[[i]] <- list(predicted = predicted)
          response <- (if (!is.null(predicted)) bayes.rule(predicted, pi.hat) else NULL)
          classOutput[[i]] <- c(classOutput[[i]], class = list(response))
          remove(predicted)
          remove(response)
          predicted.oob <- (if (!is.null(nativeOutput$oobEnsbCLS))
                            array(nativeOutput$oobEnsbCLS[(iter.ensb.start + 1):iter.ensb.end],
                                  c(n, levels.count[i]), dimnames=ens.names) else NULL)
          classOutput[[i]] <- c(classOutput[[i]], predicted.oob = list(predicted.oob))
          response.oob <- (if (!is.null(predicted.oob)) bayes.rule(predicted.oob, pi.hat) else NULL)
          classOutput[[i]] <- c(classOutput[[i]], class.oob = list(response.oob))
          remove(predicted.oob)
          remove(response.oob)
          if (!is.null(nativeOutput$perfClas)) {
            err.rate <- array(0, c(1 + levels.count[i], ntree))
            for (j in 1: (1 + levels.count[i])) {
              err.rate[j, ]  <- nativeOutput$perfClas[tree.offset]
              tree.offset <- tree.offset + 1
            }
            row.names(err.rate) <- err.names
            classOutput[[i]] <- c(classOutput[[i]], err.rate = list(t(err.rate)))
            remove(err.rate)
          }
          if (!is.null(nativeOutput$vimpClas)) {
            importance <- array(0, c(1 + levels.count[i], n.xvar), dimnames=vimp.names)
            for (j in 1: (1 + levels.count[i])) {
              importance[j, ]  <- nativeOutput$vimpClas[vimp.offset]
              vimp.offset <- vimp.offset + 1
            }
            classOutput[[i]] <- c(classOutput[[i]], importance = list(t(importance)))
            remove(importance)
          }
        }
        nativeOutput$allEnsbCLS <- NULL
        nativeOutput$oobEnsbCLS <- NULL
        nativeOutput$perfClas <- NULL
        nativeOutput$vimpClas <- NULL
        ## When TRUE we revert to univariate nomenclature for all the outputs.
        if(univariate.nomenclature) {
          if ((class.count == 1) & (regr.count == 0)) {
            names(classOutput) <- NULL
            rfsrcOutput <- c(rfsrcOutput, unlist(classOutput, recursive=FALSE))
          }
          else {
            rfsrcOutput <- c(rfsrcOutput, classOutput = list(classOutput))
          }
        }
        else {
          rfsrcOutput <- c(rfsrcOutput, classOutput = list(classOutput))
        }
      }
      if (regr.count > 0) {
        regrOutput <- vector("list", regr.count)
        names(regrOutput) <- yvar.names[regr.index]
        ## Incoming: T=tree R=response
        ## T1R1 T1R2, T2R1 T2R2, T3R1 T3R2, ... 
        ## Yields tree.offset = c(1, 3, 5, ...) 
        tree.offset <- array(1, ntree)
        if (ntree > 1) {
          tree.offset[2:ntree] <- length(regr.index)
        }
        tree.offset <-  cumsum(tree.offset)
        ## Incoming vimp rates: V=xvar R=response L=level
        ## V1R1 V1R2, V2R1 V2R2, V3R1 V3R2, ... 
        ## Yields vimp.offset = c(1, 3, 5, ...) 
        vimp.offset <- array(1, n.xvar)
        if (n.xvar > 1) {
          vimp.offset[2:n.xvar] <- length(regr.index)
        }
        vimp.offset <-  cumsum(vimp.offset)
        iter.ensb.start <- 0
        iter.ensb.end   <- 0
        ## From the native code:
        ##   "allEnsbRGR"
        ##   "oobEnsbRGR"
        ## -> of dim [regr.count] x [obsSize]
        ## From the native code:
        ##   "perfRegr"
        ## -> of dim [ntree] x [regr.count]
        ## To the R code:
        ## -> of dim [ntree] x [[regr.count]]
        ## From the native code:
        ##   "vimpRegr"
        ## -> of dim [n.vxar] x [regr.count]
        ## To the R code:
        ## -> of dim  [[regr.count]] x [n.xvar]
        for (i in 1:regr.count) {
          iter.ensb.start <- iter.ensb.end
          iter.ensb.end <- iter.ensb.end + n
          vimp.names <- xvar.names
          predicted <- (if (!is.null(nativeOutput$allEnsbRGR))
                        array(nativeOutput$allEnsbRGR[(iter.ensb.start + 1):iter.ensb.end], n) else NULL)
          regrOutput[[i]] <- list(predicted = predicted)
          remove(predicted)
          predicted.oob <- (if (!is.null(nativeOutput$oobEnsbRGR))
                            array(nativeOutput$oobEnsbRGR[(iter.ensb.start + 1):iter.ensb.end], n) else NULL)
          regrOutput[[i]] <- c(regrOutput[[i]], predicted.oob = list(predicted.oob))
          remove(predicted.oob)
          if (!is.null(nativeOutput$perfRegr)) {
            err.rate <- nativeOutput$perfRegr[tree.offset]
            tree.offset <- tree.offset + 1
            regrOutput[[i]] <- c(regrOutput[[i]], err.rate = list(err.rate))
            remove(err.rate)
          }
          if (!is.null(nativeOutput$vimpRegr)) {
            importance <- nativeOutput$vimpRegr[vimp.offset]
            names(importance) <- xvar.names
            vimp.offset <- vimp.offset + 1
            regrOutput[[i]] <- c(regrOutput[[i]], importance = list(importance))
            remove(importance)
          }
        }
        nativeOutput$allEnsbRGR <- NULL
        nativeOutput$oobEnsbRGR <- NULL
        nativeOutput$perfRegr <- NULL
        nativeOutput$vimpRegr <- NULL
        ## When TRUE we revert to univariate nomenclature for all the outputs.
        if(univariate.nomenclature) {
          if ((class.count == 0) & (regr.count == 1)) {
            names(regrOutput) <- NULL
            rfsrcOutput <- c(rfsrcOutput, unlist(regrOutput, recursive=FALSE))
          }
          else {
            rfsrcOutput <- c(rfsrcOutput, regrOutput = list(regrOutput))
          }
        }
        else {
          rfsrcOutput <- c(rfsrcOutput, regrOutput = list(regrOutput))
        }
      }
    }
  }## BLOCK OF CODE IS NOT EXECUTED IN IMPUTE.ONLY
  class(rfsrcOutput) <- c("rfsrc", "grow", family)
  if (big.data) {
    class(rfsrcOutput) <- c(class(rfsrcOutput), "bigdata")
  }
  return(rfsrcOutput)
}

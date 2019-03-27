################################################################################
# example.R
# 
# Authors: Matt Rosen & Shannon Wongvibulsin
#
# Description: Example usage of modified rfsrc function.
################################################################################

# Load packages (second to generate CPIU data)
library(rfSLAM)

# Read in data
df_cpiu_sca1 <- readRDS("../../Desktop/RF-SLAM_data/interval_by_sca1.rds")

# Generate sample matrix (for specifying how many samples per tree);
# make it so that one tree has 10x as many samples as all others
# (to test functionality)
ntree    <- 500
samp     <- matrix(1, dim(df_cpiu_sca1)[1], ntree)
samp[,1] <- samp[,1] * 10

# Example outcome variable of interest
df_cpiu_sca1$i.sca.i <- as.factor(df_cpiu_sca1$i.sca)

# Lay out the variables
x_vars <- c("age",           "gender",         "ethnic",          "bsa", 
			"etio",          "durationofcm",   "chfclass",        "htn", 
			"hyperchol",     "dm",             "smoker",          "aceiorarb", 
			"betablocker",   "lipidlowering",  "antiarrhythmics", "diuretic", 
			"dig",           "spironolactone", "asa",             "afib", 
			"heartrate",     "qrsdur",         "lbbb",            "biv", 
			"serumna",       "serumk",         "serumcr",         "egfr", 
			"lab_bun",       "lab_gluc",       "hct",             "l.crpprose", 
			"ntprobnp",      "hs_il6",         "il10",            "tnfa_rec2", 
			"ctnt",          "tropin_i",       "ckmb",            "myoglobin", 
			"otheref",       "lvef",           "edvindex",        "esvindex", 
			"Cinemassindex", "LAvolmini",      "LAvolmaxi",       "LAvolpreai", 
			"LAtotalef",     "LApassiveef",    "LAactiveef",      "l.truegzmass", 
			"l.truecorem",   "l.trueism",      "fibrosis",        "int.n", 
			"n.hf")

y_var <-   "i.sca.i"

# Assemble the formula.
formula_xs <- strsplit(paste(x_vars, collapse='+'), ' ')[[1]]
formula_ys <- paste(y_var, "~")

formula    <- as.formula(paste(formula_ys, formula_xs))


# Variable Sampling
# ----------------------------------------
# first 2 lines: example variable assignment to categories 
# (weighted equally here, but they don't need to be)
# cats should be of length (# of variables used in training the model)
#
# last 3 lines: package together in the correct format: categories is a list,
# with elements that specify variable category assignments and
# the respective category weights
cats        <- as.integer(c(rep(1, length(x_vars))))
cat_weights <- c(1.0) # arbitrary here, so it doesn't impact training

categories <- list()
categories$categories.weights <- cat_weights
categories$var.assignments    <- cats

# Train a random forest.
#
# Note 1: 
# -------
# The performance will probably be bad, because this is mainly a proof-of-
# concept (to make sure things talk to each other as expected).
#
# Note 2: 
# -----
# Default behavior, if no argument 'stratifier' is passed, is to not 
# stratify at all. (Likewise re: risk time; if no variable is passed
# explicitly to specify risk time, risk time is not taken into account).
rf.c.ppt <- rfsrc(formula, 
				  data       = df_cpiu_sca1, 
				  nodesize   = 5, 
				  ntree      = ntree, 
				  na.action  = "na.impute", 
				  splitrule  = "poisson.split4", 
				  stratifier = df_cpiu_sca1$int.n, 
				  risk.time  = df_cpiu_sca1$rt, 
				  do.trace   = TRUE, # argument used for debugging
				  categories = categories,
				  bootstrap  = "by.user",
				  samp       = samp)

rf.c.ppt

# Use this random forest to make predictions given new data.
#
# Note:
# -----
# This is a toy example; if you were actually testing predictions,
# you would do it on out-of-bag samples, or some other held-out data.
new.pred <- predict.rfsrc(rf.c.ppt, 
						  newdata    = df_cpiu_sca1, 
						  na.action  = "na.impute", 
						  membership = TRUE)
new.pred

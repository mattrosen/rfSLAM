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
df_cpiu_sca1 <- readRDS("../data/RF-SLAM_data/interval_by_sca1.rds")
df_cpiu_hf1 <- readRDS("../data/RF-SLAM_data/interval_by_hf1.rds")
df_cpiu_death <- readRDS("../data/RF-SLAM_data/interval_by_death.rds")

# Generate sample matrix (for specifying how many samples per tree);
# make it so that one tree has 10x as many samples as all others
# (to test functionality)
ntree <- 500
samp <- matrix(1, dim(df_cpiu_sca1)[1], ntree)
samp[,1] <- samp[,1] * 10

# example outcome variable of interest
df_cpiu_sca1$i.sca.i <- as.factor(df_cpiu_sca1$i.sca)

# Variable Sampling
# ----------------------------------------
# first 2 lines: example variable assignment to categories 
# (weighted equally here, but they don't need to be)
# cats should be of length (# of variables used in training the model)
#
# last 3 lines: package together in the correct format: categories is a list,
# with elements that specify variable category assignments and
# the respective category weights
cats <- as.integer(c(rep(1, 30), rep(2, 31)))
cat_weights <- c(0.5, 0.5) # arbitrary here, so it doesn't impact training

categories <- list()
categories$categories.weights <- cat_weights
categories$var.assignments <- cats

# train a random forest; note: the performance will probably be bad,
# because 1 sample specified per tree; this is mainly a proof-of-concept
# (make sure things talk to each other as expected)
rf.c.ppt <- rfsrc(i.sca.i ~ age + gender + ethnic + bsa + etio + 
							durationofcm + chfclass + htn + hyperchol + 
							dm + smoker + aceiorarb + betablocker + 
							lipidlowering + antiarrhythmics + diuretic + 
							dig + spironolactone + asa + afib + heartrate + 
							qrsdur + lbbb + biv + serumna + serumk + 
							serumcr + egfr + lab_bun + lab_gluc + hct + 
							l.crpprose + ntprobnp + hs_il6 + il10 + 
							tnfa_rec2 + ctnt + tropin_i + ckmb + myoglobin + 
							otheref + lvef + edvindex + esvindex + 
							Cinemassindex + LAvolmini + LAvolmaxi + 
							LAvolpreai + LAtotalef + LApassiveef + LAactiveef + 
							l.truegzmass + l.truecorem + l.trueism + fibrosis + 
							int.n + n.hf + n.sca + i.hf + ni.hf + rt, 
							data = df_cpiu_sca1, 
							nodesize = 5, 
							ntree = ntree, 
							na.action = "na.impute", 
							splitrule="poisson.split1", 
							stratifier=df_cpiu_sca1$int.n, 
							risk.time=df_cpiu_sca1$rt, 
							do.trace=TRUE, # argument used for debugging
							categories=categories,
							bootstrap="by.user",
							samp=samp)
rf.c.ppt
new.pred <- predict.rfsrc(rf.c.ppt, newdata = sca1,df, na.action = "na.impute", membership = TRUE)

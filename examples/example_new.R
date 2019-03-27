library(rfSLAM)
<<<<<<< HEAD
sca1.df<- readRDS("../Desktop/RF-SLAM_data/interval_by_sca1.rds")
=======
sca1.df<- readRDS("data/RF-SLAM_data/interval_by_sca1.rds")
>>>>>>> 2a6b2f43cb6f6bbf2caacad221216bbf016e1526

sca1.df$i.sca <- as.factor(sca1.df$i.sca)

sel.var1 <- c("pid","scd", "age", "gender", 
			  "ethnic", "bsa", "etio", "durationofcm", 
			  "chfclass", "htn", "hyperchol", "dm", 
			  "smoker", "aceiorarb", "betablocker", 
			  "lipidlowering", "antiarrhythmics", 
			  "diuretic", "dig", "spironolactone", 
			  "asa", "afib", "heartrate", "qrsdur", 
			  "lbbb", "biv", "serumna", "serumk", 
			  "serumcr", "egfr", "lab_bun", "lab_gluc", 
			  "hct", "crpprose", "ntprobnp", "hs_il6", 
			  "il10", "tnfa_rec2", "ctnt", "tropin_i", 
			  "ckmb", "myoglobin", "otheref", "lvef", 
			  "edvindex", "esvindex", "Cinemassindex", 
			  "LAvolmini", "LAvolmaxi", "LAvolpreai", 
			  "LAtotalef", "LApassiveef", "LAactiveef", 
			  "truegzmass", "truecorem", "trueism", 
			  "fibrosis", "int.n", "t.start", "t.end", 
			  "rt", "i.death", "i.hf", "i.sca", "n.hf", 
			  "n.sca", "ni.hf", "ni.sca", "t.hf", "t.sca")

# reduce data frame to only include predictors of interest
#r.sca1.df <- sca1.df[,sel.var1]


### 

set.seed(321)

# all predictors
rf.sca <- rfsrc(i.sca ~ age + gender + ethnic + bsa + etio + durationofcm + 

						chfclass + htn + hyperchol + dm + smoker + aceiorarb + 
						betablocker + lipidlowering + antiarrhythmics + diuretic + 
						dig + spironolactone + asa + afib + heartrate + qrsdur + 
						lbbb + biv + serumna + serumk + serumcr + egfr + lab_bun + 
						lab_gluc + hct + crpprose + ntprobnp + hs_il6 + il10 + 
						tnfa_rec2 + ctnt + tropin_i + ckmb + myoglobin + otheref + 
						lvef + edvindex + esvindex + Cinemassindex + LAvolmini + 
						LAvolmaxi + LAvolpreai + LAtotalef + LApassiveef + 
						LAactiveef + truegzmass + truecorem + trueism + fibrosis + 
						int.n + n.hf, 
						data = sca1.df, 
						nodesize = 500, 
						ntree = 100, 
						na.action = "na.impute", 
						splitrule="poisson.split1", 
						risk.time = sca1.df$rt, 
						membership = TRUE,
						do.trace = TRUE)

rf.sca

new.pred <- predict.rfsrc(rf.sca, newdata = sca1.df, na.action = "na.impute", membership = TRUE)



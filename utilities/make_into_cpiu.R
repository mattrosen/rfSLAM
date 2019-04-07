# load cpiu file
source("cpiu.R")
library("readstata13")
library("tidyverse")
library("lubridate")

# load updated_corrected
corrected <- readRDS("../../Desktop/RF-SLAM_data/updated.corrected.rds")

# load LVEF data
lvef_data <- read.dta13("../../Desktop/RF-SLAM_data/LVEFdata_022619.dta")

# merge row-wise
corrected <- merge(corrected, lvef_data, all.x=TRUE)

# compute start time of study
corrected$start.time1 <- as.Date(as.character(corrected$dateofmri),format = "%m/%d/%Y")

# add timehf1psca, etc.
corrected$timehf1psca <- ifelse(corrected$timehf1 < corrected$timescd1, corrected$timehf1, NA)
corrected$timehf2psca <- ifelse(corrected$timehf2 < corrected$timescd1, corrected$timehf2, NA)
corrected$timehf3psca <- ifelse(corrected$timehf3 < corrected$timescd1, corrected$timehf3, NA)
corrected$timehf4psca <- ifelse(corrected$timehf4 < corrected$timescd1, corrected$timehf4, NA)
corrected$timehf5psca <- ifelse(corrected$timehf5 < corrected$timescd1, corrected$timehf5, NA)
corrected$timehf6psca <- ifelse(corrected$timehf6 < corrected$timescd1, corrected$timehf6, NA)
corrected$timehf7psca <- ifelse(corrected$timehf7 < corrected$timescd1, corrected$timehf7, NA)
corrected$timehf8psca <- ifelse(corrected$timehf8 < corrected$timescd1, corrected$timehf8, NA)
corrected$timehf9psca <- ifelse(corrected$timehf9 < corrected$timescd1, corrected$timehf9, NA)
corrected$timehf10psca <- ifelse(corrected$timehf10 < corrected$timescd1, corrected$timehf10, NA)

# add for lvef
corrected$nuc1date <- corrected$nucdate
corrected$nuc1ef   <- corrected$nucef
corrected$ef0time  <- 0
corrected$ef0value <- corrected$otheref

# add merge ef times for given patient
lvef_prep <- function(df) {

	# set initial values
	cur_nuc <- 1
	v_nuc <- ifelse(is.na(df[1,sprintf("nuc%ddate",cur_nuc)]) || nchar(df[1,sprintf("nuc%ddate",cur_nuc)]) < 7, 
			NA, as.numeric(as.Date(as.character(df[[1,sprintf("nuc%ddate",cur_nuc)]]),format = "%m/%d/%Y") - df$start.time))

	for (i in 1:11) {

		v_ef    <- ifelse(is.na(df[1,sprintf("echo%ddate",i)]) || nchar(df[1,sprintf("echo%ddate",i)]) < 7, 
			NA, as.numeric(as.Date(as.character(df[[1,sprintf("echo%ddate",i)]]),format = "%m/%d/%Y") - df$start.time))
		vals    <- c(v_ef, v_nuc)
		time_ef <- min(vals, na.rm=TRUE)
		which   <- which.min(vals)


		if (time_ef == Inf) {
			time_ef <- NA
		}

		if (length(which) == 0) {
			which <- 3
		}

		# update the time for this var + update the actual ef value
		val <- ifelse(which == 1, as.numeric(df[[1,sprintf("echo%def", i)]]), 
					  ifelse(which == 2, as.numeric(df[[1,sprintf("nuc%def", cur_nuc)]]), NA))

		# first, add cols
		df <- df %>% mutate(!!sprintf("ef%dtime", i):=time_ef, !!sprintf("ef%dvalue", i) := val)

		# if the nuclear ejection fraction used, update which one to evaluate next
		if (which == 2) {
			cur_nuc <- cur_nuc + 1
			if (cur_nuc > 3) {
				v_nuc <- NA
			}
			else {
				v_nuc <- ifelse(is.na(df[1,sprintf("nuc%ddate",cur_nuc)]) || nchar(df[1,sprintf("nuc%ddate",cur_nuc)]) < 7, 
				NA, as.numeric(as.Date(as.character(df[[1,sprintf("nuc%ddate",cur_nuc)]]),format = "%m/%d/%Y") - df$start.time))
			}	
		}
	}
	return(df)
}



#corrected <- to_time(corrected, sprintf("echo%ddate", 1:10), "start.time")#, sprintf("echo%def", 1:2)))



# column s.t. the i-th value is min(deathtime[i], timescd1[i])
corrected$min_death_sca <- pmin(corrected$deathtime, 
								corrected$timescd1, 
								na.rm=TRUE)

# ditto for above, but with min(deathtime, timehf1)
corrected$min_death_hf <- pmin(corrected$deathtime,
							   corrected$timehf1, 
							   na.rm=TRUE)

to_create_msc <- list(t.lvef=sprintf("ef%dtime", 1:11),
					  c.lvef=list(times=sprintf("ef%dtime", 0:11), values=sprintf("ef%dvalue", 0:11)))

# list of variables to create for CPIU'd data ending at hf1
to_create_mhf <- list(t.hf="timehf1",
					  t.sca="timescd1",
					  t.lvef=sprintf("ef%dtime", 0:11),
					  c.lvef=list(times=sprintf("ef%dtime", 0:11), values=sprintf("ef%dvalue", 0:11)),
					  n.hf="timehf1",
					  n.sca="timescd1",
					  i.hf="timehf1",
					  i.sca="timescd1",
					  ni.hf="timehf1",
					  ni.sca="timescd1",
					  i.death='deathtime')

# ditto for above, but for intervals ending in death
to_create_mdeath <- list(t.hf=sprintf("timehf%dpsca",seq(1:10)),
						 t.sca="timescd1",
						 t.lvef=sprintf("ef%dtime", 0:11),
						 c.lvef=list(times=sprintf("ef%dtime", 0:11), values=sprintf("ef%dvalue", 0:11)),
						 n.hf=sprintf("timehf%dpsca",seq(1:10)),
						 n.sca="timescd1",
						 i.hf=sprintf("timehf%dpsca",seq(1:10)),
						 i.sca="timescd1",
						 ni.hf=sprintf("timehf%dpsca",seq(1:10)),
						 ni.sca="timescd1")

for (k in c(0, 2)) {

	# new files
	interval_by_sca   <- cpiu.fc(corrected, t.outcome="min_death_sca", to_create=to_create_msc, k_to_persist=k)
	interval_by_hf    <- cpiu.fc(corrected, t.outcome="min_death_hf", to_create=to_create_mhf,  k_to_persist=k)
	interval_by_death <- cpiu.fc(corrected, t.outcome="deathtime", to_create=to_create_mdeath,  k_to_persist=k)


	# save
	#s0 <- saveRDS(corrected, "../corrected_debug.rds")
	s1 <- saveRDS(interval_by_sca, sprintf("../../Desktop/RF-SLAM_data/interval_by_sca1_new_m%d.rds", k))
	s2 <- saveRDS(interval_by_hf, sprintf("../../Desktop/RF-SLAM_data/interval_by_hf1_new_m%d.rds", k))
	s3 <- saveRDS(interval_by_death, sprintf("../../Desktop/RF-SLAM_data/interval_by_death_new_m%d.rds", k))
}



to_date <- function(x) { 
	x[nchar(x) < 7] <- NA
	tryCatch(as.Date(x, 
	                     tryFormats = c("%m-%d-%Y", 
	                                    "%m/%d/%Y")),
	            error = function(err) {NA})
}

corrected$start.time <- to_date(corrected$dateofmri)

##########################################


# convert dates to times
to_time <- function(df, cols, start) {

	# check which columns can be formatted as dates; try a bevy of options
    # re: formats (for generality)
	is_date <- function(mydate) {
		if (is.na(mydate)) {
			return(TRUE)
		}
	  	tryCatch(!is.na(as.Date(mydate, 
	  							"",
	  							tryFormats = c("%Y-%m-%d", 
                   							   "%Y/%m/%d",
                   							   "%d-%m-%Y",
                   							   "%d/%m/%Y",
                   							   "%m-%d-%Y", 
                   							   "%m/%d/%Y"))),  
	        error = function(err) {FALSE})  
	}

    current_data <- df[,cols]
    
    # if is only one column, beef up dimension
    current_data <- data.frame(current_data)
    dates <- sapply(current_data, 
                    function(x) all(sapply(x, is_date)))

    # for those columns, convert the dates to times from start
    elapsed <- function(msrmnt, t0) {

    	# take care of NAs
    	msrmnt <- to_date(msrmnt)
    	msrmnt[is.na(msrmnt)] <- t0[is.na(msrmnt)]
    	return(as.numeric(msrmnt - t0))
    	
    }


    to_convert <- current_data[,dates]
    ref <- df[,start]
    df[,cols[dates]] <- sapply(to_convert, elapsed, ref)

    return(df)
}

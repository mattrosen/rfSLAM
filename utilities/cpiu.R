################################################################################
## cpiu.R
## 
## Created : 7/2018
## Modified: 11/2018
##
## Authors: Shannon Wongvibulsin & Matt Rosen
## 
## Description: Function to reformat data into CPIUs.
##
################################################################################
##
##  Arguments:
## 		(dataframe)  df             : dataframe to be created into CPIU format
## 		(string)     id             : subject identifier
## 		(str. vect.) td.events      : vector column names corresponding to event 
##								      times for time-dependent events in df (NOT
##                                    a major focus of current uses)
## 		(string)     death.time     : column name corresponding to time of death  
##								      /censoring (i.e. last follow-up time) in df
## 		(string)     death          : column name corresponding 0/1 indicator 
##								      indicating whether death.time is the time
## 								      of death (1) or censoring (0) in df
## 		(string) 	 t.outcome      : column name corresponding to time of
##								      outcome of interest in df
##      (numeric)    interval       : floating point number, used in setting
##                                    interval length. should be the fraction of 
##                                    one desired unit per CPIU interval. if we 
##                                    want our cpiu intervals to have risk-time 
##                                    in terms of half years, for example, then
##                                    this parameter should be 0.5.
##      (integer)    unit_norm      : should take care of unit switches from
##                                    orig. data to cpiu format; for example, if
##                                    original data is in units of days, and you
##                                    want it to be in units of years, 
##                                    unit_norm should be 365; there are
##                                    365 days per year, or unit_norm 
##                                    current units of time per 1 desired unit
##                                    of time. If desired units of time = units 
##                                    of time used in the original data, this 
##                                    parameter should be 1.
##      (boolean)    to_partition   : boolean indicating whether CPIUs should be
##                                    allowed to be of variable length (TRUE) or
##                                    not (FALSE). If TRUE, any time an event 
##                                    specified by td.events occurs within an 
##                                    interval, that interval is split in two,
##                                    s.t. new interval 1 = orig interval up to 
##                                    the occurrence of the event, and new 
##                                    interval 2 = orig interval after the event
##      (list)      to_create       : list of variables to create, s.t. every
##                                    (key, value) pair of the list satisifies
##                                    the following criteria: 1) the key follows
##                                    the correct naming convention (must begin
##                                    with i, t, n, or ni); 2) the value 
##                                    specifies valid columns in df.
##      (int)       k_to_persist    : integer, optional; specifies, for continuous
##                                    variables, how long (in terms of intervals)
##                                    a measured value should be carried on for. If
##                                    specified as -1, denotes that a measurement
##                                    should be carried to min(time of censoring/
##                                    death, time of next measurement).
##
##  Returns:
##	    (partitioned) mod_df        : dataframe in CPIU format
##	
################################################################################
library(dplyr)
library(Rcpp)
sourceCpp("../utilities/cpiu.cpp")

################################################################################

# convert columns of date strings to Date objects (allowing for slight
# format flexibility)
to_date <- function(x) { 
    #print(x)
    x[nchar(x) < 7] <- NA
    tryCatch(as.Date(x, 
                         tryFormats = c(#"%Y-%m-%d", 
                                        #"%Y/%m/%d",
                                        #"%d-%m-%Y",
                                        #"%d/%m/%Y",
                                        "%m-%d-%Y", 
                                        "%m/%d/%Y")),
                error = function(err) {NA})
}

# check which columns can be formatted as dates; try a bevy of options
# re: formats (for generality)
is_date <- function(mydate) {

    # easy eliminations/inclusions
    if (is.na(mydate)) {
        return(FALSE)
    }
    else if (class(mydate) != "character") {
        return(FALSE)
    }
    else if (nchar(mydate) <= 1) {
        return(NA)
    }
    else if (nchar(mydate) < 7) {
        return(FALSE)
    }
    else {
        val <- tryCatch(!is.na(as.Date(mydate, 
                                tryFormats = c("%Y-%m-%d", 
                                               "%Y/%m/%d",
                                               "%d-%m-%Y",
                                               "%d/%m/%Y",
                                               "%m-%d-%Y", 
                                               "%m/%d/%Y"))),  
            error = function(err) {FALSE})  
            return(val)
    }
        
}

# set up log for errors
setup_error_log <- function(fn) {
    if (!file.exists(fn)) {
        f <- file(fn)
        writeLines("Potential typos in data:", f)
        close(f)
    }
}

# reset error log
reset_error_log <- function(fn) {
    f <- file(fn)
    writeLines("Potential typos in data:", f)
    close(f)
}

# when dealing with columns of dates, find entries that fail to conform to
# correct/expected format
catch_formatting_errors <- function(data, names) {

    ind <- sapply(data, function(x) sapply(x, is_date))

    # start a new error log for formatting errors
    reset_error_log("formatting_errors.txt")
    for (i in 1:dim(data)[2]) {

        ind_cur <- ind[,i]

        errors <- NA

        # if only 1 is a date, that's probably the error
        #print(sum(ind_cur, na.rm=TRUE) == 1)
        #print(sum(is.na(ind_cur)) < length(ind_cur))
        if ((sum(ind_cur, na.rm=TRUE) == 1) && (sum(is.na(ind_cur)) < length(ind_cur))) {
            errors <- which(ind_cur)
        }

        # if more than half are dates, spit out values for ones that 
        # (a) aren't dates, and 
        # (b) aren't NA
        else if (sum(ind_cur, na.rm=TRUE) > length(ind_cur) / 2) {
            ind_cur[is.na(ind_cur)] <- TRUE
            errors <- which(!ind_cur)
        }

        # write the putative errors
        if (!is.na(errors[1])) {
            write(c(names[i], errors), file="formatting_errors.txt", append=TRUE)
            write(c("\n", file="formatting_errors.txt", append=TRUE))
        }
        
    }

}

# when dealing with columns of dates, find entries that aren't valid
# re: start/end
catch_validity_errors <- function(data, names, lens) {

    # start a new error log for formatting errors
    reset_error_log("validity_errors.txt")

    for (i in 1:dim(data)[2]) {

        cur_data                  <- data[,i]
        cur_data[is.na(cur_data)] <- 0
        cur_name                  <- names[i]

        # make sure date > start date; do this by seeing if times are
        # all positive (they should be)
        errors_type1 <- which(cur_data < 0)

        # make sure date < end date; do this by seeing if times 
        # fall before end date
        errors_type2 <- which(cur_data > lens)

        # write the putative errors
        if (length(errors_type1) > 0) {
            write("Date before start:", file="validity_errors.txt", append=TRUE)
            write(c(cur_name, errors_type1), file="validity_errors.txt", append=TRUE)
            write("\n", file="validity_errors.txt", append=TRUE)
        }
        if (length(errors_type2) > 0) {
            write("Date after end:", file="validity_errors.txt", append=TRUE)
            write(c(cur_name, errors_type2), file="validity_errors.txt", append=TRUE)
            write("\n", file="validity_errors.txt", append=TRUE)
        }
        
    }

}

# correct errors found; default atm is to set to NA
correct_errors <- function(data, errs) {
    data[errs] <- NA
    return(data)
}

# compute time elapsed from start for columns of dates
to_time <- function(df, cols, start) {
    
    current_data <- df[,cols]
    
    # if is only one column, beef up dimension
    current_data <- data.frame(current_data)
    dates        <- sapply(current_data, 
                        function(x) any(sapply(x, is_date)))

    dates <- names(which(dates))

    # for those columns, convert the dates to times from start
    elapsed <- function(msrmnt, t0) {

        # take care of NAs
        msrmnt                <- to_date(msrmnt)
        msrmnt[is.na(msrmnt)] <- t0[is.na(msrmnt)]
        return(as.numeric(msrmnt - t0))
        
    }

    to_convert       <- current_data[,dates]
    ref              <- df[,start]
    df[,dates] <- sapply(to_convert, elapsed, ref)

    return(df)
}

################################################################################
# data quality assurance: check to see if things that look like dates are,
perform_quality_assurance <- function(data, check_type, date_cols, lens=NA) {

    # find date-like columns
    if (check_type == 'formatting') {
        catch_formatting_errors(data, date_cols)
    }
    else if (check_type == 'validity') {
        catch_validity_errors(data, date_cols, lens)
    }
}

################################################################################
# the main attraction: convert data into CPIU format
cpiu.fc <- function(df,
                   start.date       = "dateofmri",
				   id           	= "pid",
                   td.events 		= sprintf("timehf%dpsca",seq(1:10)), 
                   death.time 		= "deathtime",
                   death.date       = "datedeath",
                   death 			= "deaths", 
                   t.outcome 		= "timescd1",
				   interval     	= 0.5,
				   unit_norm        = 365,
				   to_partition     = FALSE,
				   to_create        = list(t.hf=sprintf("timehf%d",seq(1:10)),
										   t.sca="timescd1",
										   n.hf=sprintf("timehf%d",seq(1:10)),
										   n.sca="timescd1",
										   i.hf=sprintf("timehf%d",seq(1:10)),
										   i.sca="timescd1",
										   ni.hf=sprintf("timehf%d",seq(1:10)),
										   ni.sca="timescd1",
                                           i.death='deathtime'),
                   k_to_persist     = -1,
                   quality_check    = TRUE) {

    print("Working...")


    # before working on a subject-to-subject basis, convert start
    # dates to Date objects
    df$start.time <- to_date(df[,start.date])
    
    # convert all columns that look like dates to times elapsed since start of study
    date_cols      <- names(which(sapply(df, function(x) any(sapply(x, is_date)))))

    # check formatting first
    if (quality_check) {
        perform_quality_assurance(df[,date_cols], 'formatting', date_cols)
    }

    df <- to_time(df, names(df), "start.time")

    # check validity first
    if (quality_check) {
        perform_quality_assurance(df[,date_cols], 'validity', date_cols, df[,death.time])
    }

    # find continuous variables that pull from multiple columns,
    # and TODO: make sure that they all work together as they should
    # extract names of columns to add by type
    var_names       <- names(df)
    continuous_vars <- grep("^c\\.",  var_names)
    #print(continuous_vars)
    #stop()

    # split by subject, + let the magic happen
	partitioned <- df             %>% 
				   group_by_(id)  %>%
				   do(event(., 
				   			id, 
				   			td.events, 
				   			death.time, 
				   			death, 
				   			t.outcome, 
				   			interval, 
				   			unit_norm,
				   			to_partition, 
				   			to_create, 
                            k_to_persist))

  	return(partitioned)
}

# perform partitioning on dataframe, return
event <- function(df,           
				  id,              
				  td.events,   
				  death.time, 
				  death, 	    
				  t.outcome,       
				  interval,    
				  unit_norm, 
				  to_partition, 
				  to_create,
                  k_to_persist) {

    # extract specified columns
    td_events   <- df[,td.events]
    t_outcome 	<- df[,t.outcome]
    tdeath 		<- df[,death.time]
    death 		<- df[,death]
    cur_id 		<- df[,id]


    interval_mod <- interval * unit_norm
    

    # compute number of rows (e.g. number of intervals) to return
    n_intervals <- ceiling(t_outcome[[1]] / interval_mod)

    # extract names of columns to add by type
    var_names 		<- names(to_create)
    indicators 		<- grep("^i\\.",  var_names)
    count_total 	<- grep("^n\\.",  var_names)
    count_current 	<- grep("^ni\\.", var_names)
    time_vars 		<- grep("^t\\.",  var_names)
    continuous_vars <- grep("^c\\.",  var_names)

    # extract data specified by to_create; do for continuous variables before unlisting all
    c_vars  <- lapply(to_create[continuous_vars], 
                    function(x) {lapply(x, function(y) {as.matrix(select(df, !!y))})})
    vars 	<- lapply(to_create, unlist)
    
    ind 	<- lapply(vars[indicators], 	 function(x) {as.matrix(select(df, !!x))})
    ct_tot 	<- lapply(vars[count_total], 	 function(x) {as.matrix(select(df, !!x))})
    ct_cur 	<- lapply(vars[count_current], 	 function(x) {as.matrix(select(df, !!x))})
    t_vars 	<- lapply(vars[time_vars], 		 function(x) {as.matrix(select(df, !!x))})

    ##############################################################################
    ## Naming convention check
    ##
    ## Make sure the user is playing by the rules; if they're naming things 
    ## incorrectly, let 'em know! We don't want to break silently.
    ##############################################################################

    # info string for error, if naming convention not followed
    naming_err <- paste("Names supplied to to_create must take the following form:",
    					"    i.[var_name]  : indicator variables",
    					"    ni.[var_name] : count of event in current interval",
    					"    n.[var_name]  : count of event over previous intervals",
    					"    t.[var_name]  : time elapsed since event occurred",
    					"    c.[var_name]  : continuous or factor variable change", 
    					"",
    					sep = "\n",
    					collapse = NULL)

    # check to see if all supplied names covered;
    # throw error if not
    if (!all(union(indicators, 
    			union(count_total, 
    				union(count_current, 
    					union(time_vars, continuous_vars)))))) {
    	cat(naming_err)
    	stop("Variables do not conform to naming convention.")
    } 

    # warn user to make sure they name values of continuous_vars correctly
    c_var_warn <- paste("When passing continuous variables with [c.var_name],",
    					"make sure to have to_create[c.var_name] be a list with",
    					"two attributes: times (time of measurement) and values",
    					"(measured value).",
    					sep 	 = "\n",
    					collapse = NULL) 

    # FIGURE OUT HOW TO IMPLEMENT C VAR WARNING HERE

    ##############################################################################
    ## Compute time elapsed when needed
    ##
    ## When we have measurements that vary over time, pre-CPIU conversion, 
    ## different measurements are denoted by their dates. Convert these here to 
    ## time elapsed since their enrollment date, + make sure the CPIU demarcation
    ## handles the time units computed here correctly.
    ##############################################################################

    # create empty dataframe with correct shape
    mod_df 				 <- df %>% slice(rep(1:n(), each = n_intervals))

    # MODIFY TIMES FOR T ELAPSED VARS; MODIFY TIMES FOR C VARS; 


    ##############################################################################
    ## AREA TO EDIT (APPLICATION-SPECIFIC)
    ## 	The code below assumes that, if you want to put your data in CPIU format,
    ##  the following columns will be necessary. If you feel that your CPIUs don't
    ##  need some column to keep track of interval number, interval start time/end
    ##  time, or an indicator of death/censoring in an interval, feel free to 
    ##  change the block of code directly below (+ do a cmd-F to make sure other 
    ##  parts of the code won't be broken by that change).
    ############################################################################## 
    mod_df$"int.n" 		<- seq(1:n_intervals)
    mod_df$"t.start" 	<- interval_mod * (mod_df$int.n - 1) 
    mod_df$"t.end"      <- ifelse(mod_df$t.start + interval_mod < t_outcome[[1]], 
                                 mod_df$t.start + interval_mod, 
                                 t_outcome[[1]])
    mod_df$"rt"         <- ifelse(mod_df$t.start + interval_mod < t_outcome[[1]], 
                                  interval, 
                                  (t_outcome[[1]] - mod_df$t.start) / unit_norm)
    mod_df$"i.death"	<- ifelse(mod_df$t.start + interval_mod < tdeath[[1]],
    							  0,
    							  death[[1]])

    # pass to C++ function to facilitate the looping;
    # set td.events values to -1 if NA to ensure that they
    # won't satisfy inequality in cpiu.cpp
    td_events <- unlist(td_events, use.names = FALSE)
    td_events[is.na(td_events)] <- -1

    # handle different variable types differently
    # (functions defined in cpiu.cpp are doing the heavy lifting here)
    indicators_l 		<- lapply(ind, 
    					   		 assemble_indicator, 
    					   		 t_start = mod_df$t.start, 
    					   		 t_end   = mod_df$t.end)
    count_total_l 		<- lapply(ct_tot, 
    							 assemble_accumulator, 
    							 t_start = mod_df$t.start, 
    							 t_end   = mod_df$t.end,
    							 flag    = 1)
    count_current_l 	<- lapply(ct_cur, 
    							 assemble_accumulator, 
    							 t_start = mod_df$t.start, 
    							 t_end   = mod_df$t.end,
    							 flag    = 0)
    time_vars_l 		<- lapply(t_vars, 
    							 assemble_time_elapsed, 
    							 t_start = mod_df$t.start, 
    							 t_end   = mod_df$t.end)
    continuous_vars_l 	<- lapply(c_vars, 
    							 assemble_continuous, 
    							 t_start      = mod_df$t.start, 
    							 t_end        = mod_df$t.end,
                                 k_to_persist = k_to_persist)
    
    # add to dataframe
    if (length(indicators_l) > 0) {
    	indicators_df 				 <- matrix(unlist(indicators_l), 
    										   nrow=n_intervals, 
    										   byrow=F)

    	mod_df[,names(indicators_l)] <- indicators_df
    }

    if (length(count_total_l) > 0) {
    	count_total_df 				  <- matrix(unlist(count_total_l), 
    											nrow=n_intervals, 
    											byrow=F)
    	mod_df[,names(count_total_l)] <- count_total_df
    }

    if (length(count_current_l) > 0) {
    	count_current_df 	<- matrix(unlist(count_current_l), 
    								  nrow=n_intervals, 
    								  byrow=F)
    	mod_df[,names(count_current_l)] <- count_current_df
    }

    if (length(time_vars_l) > 0) {
    	time_vars_df 		<- matrix(unlist(time_vars_l), 
    								  nrow=n_intervals, 
    								  byrow=F)
    	mod_df[,names(time_vars_l)] <- time_vars_df
    }

	if (length(continuous_vars_l) > 0) {
		continuous_vars_df  <- matrix(unlist(continuous_vars_l), 
									  nrow=n_intervals, 
									  byrow=F)
		mod_df[,names(continuous_vars_l)] <- continuous_vars_df
	}

	mod_df <- as.data.frame(mod_df)

    # take care of missingness
    mod_df[mod_df == -1] <- NA


    # take care of variable partitioning on time-dependent events
    # (this is why that argument is still crucial)
    if (to_partition) {
    	td_events <- mod_df[,td.events]
    	split_times <- unique(td_events[!apply(td_events, 2, is.na)])
    	intervals_to_split <- ceiling(split_times / interval_mod) # interval numbers
    	variables_to_update <- names(to_create)[sapply(vars, function(x) {return(any(x %in% td.events))})]
    	mod_df <- mod_df %>%
    			 	group_by_("int.n") %>%
    			 	do(interval_split(., intervals_to_split, split_times, interval, unit_norm, variables_to_update))
    }
    
    return(mod_df)

}

#################################################
# TO UPDATE: 
# - support for changes to what we're calling 
#   'continuous' time-dependent variables
# split intervals, if to_partition == TRUE
#################################################
interval_split <- function(df, intervals_to_split, split_times, interval, unit_norm, variables_to_update) {

	# if not an interval to split, return unmodified
	if (!(df["int.n"] %in% intervals_to_split))
		return(df)

	# find time(s) of split(s)
	times <- sort(split_times[df$t.start < split_times & df$t.end >= split_times])

	# modify original row
	orig_end <- df$t.end
	df$t.end <- min(times)
	df$rt <- (df$t.end - df$t.start) / unit_norm

	# based on the different cases, update variables differently
	time_vars <- variables_to_update[sapply(variables_to_update, 
                                            function(x) { 
                                                return(substring(x, 1, 1) == "t")
                                            } 
                                     )]
	others <- variables_to_update[sapply(variables_to_update, 
                                         function(x) {
                                            return(!any(substring(x, 1, 1) %in% c("c", "t")))
                                         }
                                  )]
	df[,others] <- 0

	# create new rows (one per hf in the interval) by copying df
	mod_df <- df[rep(1:nrow(df),each=length(times) + 1),]

	## all intermediates
	if (length(times) >= 2) {
		for (i in 2:length(times)) {
			mod_df[i,]$t.start <- mod_df[i - 1,]$t.end
			mod_df[i,]$t.end <- times[i - 1]
			mod_df[i,]$rt <- (mod_df[i,]$t.end - mod_df[i,]$t.start) / unit_norm
			mod_df[i,others] <- 1
			mod_df[i,time_vars] <- 0 
		}
	}

	## last one
	end                   <- nrow(mod_df)

	mod_df[end,]$t.start  <- mod_df[end-1,]$t.end
	mod_df[end,]$t.end    <- orig_end
	mod_df[end,]$rt       <- (mod_df[end,]$t.end - mod_df[end,]$t.start) / unit_norm
	mod_df[end,others]    <- 1
	mod_df[end,time_vars] <- 0


	return(mod_df)

}


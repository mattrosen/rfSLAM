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
##      (integer)    unit.norm      : should take care of unit switches from
##                                    orig. data to cpiu format; for example, if
##                                    original data is in units of days, and you
##                                    want it to be in units of years, 
##                                    unit.norm should be 365; there are
##                                    365 days per year, or unit.norm 
##                                    current units of time per 1 desired unit
##                                    of time. If desired units of time = units 
##                                    of time used in the original data, this 
##                                    parameter should be 1.
##      (boolean)    to.partition   : boolean indicating whether CPIUs should be
##                                    allowed to be of variable length (TRUE) or
##                                    not (FALSE). If TRUE, any time an event 
##                                    specified by td.events occurs within an 
##                                    interval, that interval is split in two,
##                                    s.t. new interval 1 = orig interval up to 
##                                    the occurrence of the event, and new 
##                                    interval 2 = orig interval after the event
##      (list)      to.create       : list of variables to create, s.t. every
##                                    (key, value) pair of the list satisifies
##                                    the following criteria: 1) the key follows
##                                    the correct naming convention (must begin
##                                    with i, t, n, or ni); 2) the value 
##                                    specifies valid columns in df.
##      (int)       k.to.persist    : integer, optional; specifies, for continuous
##                                    variables, how long (in terms of intervals)
##                                    a measured value should be carried on for. If
##                                    specified as -1, denotes that a measurement
##                                    should be carried to min(time of censoring/
##                                    death, time of next measurement).
##
##  Returns:
##	    (partitioned) mod.df        : dataframe in CPIU format
##	
################################################################################
library(dplyr)
library(Rcpp)
sourceCpp("../utilities/cpiu.cpp")

################################################################################

# convert columns of date strings to Date objects (allowing for slight
# format flexibility)
to.date <- function(x) { 
    x[nchar(x) < 7] <- NA
    x <- tryCatch(as.Date(x, 
                         tryFormats = c(#"%Y-%m-%d", 
                                        #"%Y/%m/%d",
                                        #"%d-%m-%Y",
                                        #"%d/%m/%Y",
                                        "%m-%d-%Y", 
                                        "%m/%d/%Y")),
                error = function(err) {NA})
    return(x)
}

# check which columns can be formatted as dates; try a bevy of options
# re: formats (for generality)
is.date <- function(mydate) {

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
setup.error.log <- function(fn) {
    if (!file.exists(fn)) {
        f <- file(fn)
        writeLines("Potential typos in data:", f)
        close(f)
    }
}

# reset error log
reset.error.log <- function(fn) {
    f <- file(fn)
    writeLines("Potential typos in data:", f)
    close(f)
}

# when dealing with columns of dates, find entries that fail to conform to
# correct/expected format
catch.formatting.errors <- function(data, ids, to.correct=FALSE) {

    ind <- sapply(data, function(x) sapply(x, is.date))
    names <- colnames(data)

    # start a new error log for formatting errors
    reset.error.log("formatting_errors.txt")
    for (i in 1:dim(data)[2]) {

        ind.cur <- ind[,i]

        errors <- NA

        # if only 1 is a date, that's probably the error
        if ((sum(ind.cur, na.rm=TRUE) == 1) && (sum(is.na(ind.cur)) < length(ind.cur))) {
            errors <- which(ind.cur)
        }

        # if more than half are dates, spit out values for ones that 
        # (a) aren't dates, and 
        # (b) aren't NA
        # (c) aren't empty strings
        # (d) aren't whitespace
        else if (sum(ind.cur, na.rm=TRUE) > length(ind.cur) / 2) {
            ind.cur[is.na(ind.cur)] <- TRUE
            blanks <- sapply(data[,i], grepl, pattern="^\\s*$")
            ind.cur[which(blanks)] <- TRUE
            errors <- which(!ind.cur)
        }


        # write the putative errors
        if (!is.na(errors[1])) {
            err.ids <- ids[errors]

            # format row ids, error value
            to_print <- format(unname(data.frame(errors, err.ids, data[errors, i])), 
                               justify='right', 
                               width=8)

            to_print <- sprintf("%s: %s [error = %s]", 
                                to_print[,1], 
                                to_print[,2], 
                                to_print[,3])

            write(c(names[i], to_print), file="formatting_errors.txt", append=TRUE)
            write("\n", file="formatting_errors.txt", append=TRUE)
        }
        if (to.correct) {
            data[,i] <- correct.errors(data[,i], errors)
        }
        
    }
    return(data)
}

# when dealing with columns of dates, find entries that aren't valid
# re: start/end
catch.validity.errors <- function(data, lens, ids, start.dates, end.dates) {

    # start a new error log for formatting errors
    reset.error.log("validity_errors.txt")
    names <- colnames(data)

    for (i in 1:dim(data)[2]) {

        cur.data                  <- data[,i]
        cur.data[is.na(cur.data)] <- 0
        cur.name                  <- names[i]

        # make sure date >= start date; do this by seeing if times are
        # all positive (they should be)
        errors.type1 <- which(cur.data < 0)

        # make sure date <= end date; do this by seeing if times 
        # fall before end date
        errors.type2 <- which(cur.data > lens)

        # write the putative errors
        if (length(errors.type1) > 0) {

            err.ids <- ids[errors.type1]

            err_dates <- start.dates[errors.type1] + data[errors.type1, i]

            # format row ids, error value
            to_print <- format(unname(data.frame(errors.type1, err.ids, err_dates, start.dates[errors.type1])), 
                               justify='right', 
                               width=8)

            to_print <- sprintf("%s: %s [error = %s, should be after %s]", 
                                to_print[,1], 
                                to_print[,2], 
                                to_print[,3],
                                to_print[,4])
            write("Date before start:", file="validity_errors.txt", append=TRUE)
            write(c(cur.name, to_print), file="validity_errors.txt", append=TRUE)
            write("\n", file="validity_errors.txt", append=TRUE)
        }

        if (length(errors.type2) > 0) {

            # find ids
            err.ids <- ids[errors.type2]

            # convert errors to their associated dates
            err_dates <- start.dates[errors.type2] + data[errors.type2,i]
            

            # format row ids, error value
            to_print <- format(unname(data.frame(errors.type2, err.ids, err_dates, end.dates[errors.type2])), 
                               justify='right', 
                               width=8)

            to_print <- sprintf("%s: %s [error = %s, should be before %s]", 
                                to_print[,1], 
                                to_print[,2], 
                                to_print[,3],
                                to_print[,4])

            write("Date after end:",         file="validity_errors.txt", append=TRUE)
            write(c(cur.name, to_print), file="validity_errors.txt", append=TRUE)
            write("\n", file="validity_errors.txt", append=TRUE)
        }

    }
    return(data)
}

# correct errors found; default atm is to set to NA
correct.errors <- function(data, errs) {
    data[errs] <- NA
    return(data)
}

# compute time elapsed from start for columns of dates
to.time <- function(df, cols, start) {
    
    current.data <- df[,cols]
    
    # if is only one column, beef up dimension
    current.data <- data.frame(current.data)
    dates        <- sapply(current.data, 
                        function(x) any(sapply(x, is.date)))

    dates <- names(which(dates))

    # for those columns, convert the dates to times from start
    elapsed <- function(msrmnt, t0) {

        # take care of NAs
        msrmnt                <- to.date(msrmnt)
        msrmnt[is.na(msrmnt)] <- t0[is.na(msrmnt)]
        return(as.numeric(msrmnt - t0))
        
    }

    to.convert <- current.data[,dates]
    ref        <- df[,start]
    df[,dates] <- sapply(to.convert, elapsed, ref)

    return(df)
}

################################################################################
# data quality assurance: check to see if things that look like dates are
perform.quality.assurance <- function(data, 
                                      check.type, 
                                      date.cols=NA, 
                                      lens=NA, 
                                      ids=NA,
                                      start.dates=NA,
                                      end.dates=NA,
                                      to.correct=FALSE) {

    # guess at date.cols if not specified
    if (all(is.na(date.cols))) {
        date.cols <- names(which(sapply(data, function(x) any(sapply(x, is.date)))))
    }

    if (check.type == 'formatting') {
        catch.formatting.errors(data, ids, to.correct)
    }
    else if (check.type == 'validity') {
        catch.validity.errors(data, lens, ids, start.dates, end.dates)
    }
    else {
        stop(paste("Failed to specify argument `check.type` correctly.",
                   "Must be either 'validity' or 'formatting'."))
    }

}

# utility first
handle.row <- function(times, vals) {

    # find sorting based on time
    return(order(times, decreasing=FALSE))

}

################################################################################
# for continuous-valued, time-varying variables: pull together all columns
# used for specifying, name them so that later code can take advantage
prepare.c.vars <- function(df, to.create) {

    # find c.XXX variables
    cvs <- grep("^c\\.", names(to.create))
    c.vars  <- lapply(to.create[cvs], 
                    function(x) {lapply(x, function(y) {as.matrix(select(df, !!y))})})

    for (cv in c.vars) {
        time.cols <- names(cv$times)
        val.cols <- names(cv$values)

        # obtain all measures; for each row, sort them,
        orders <- apply(cv, 1, handle.row)
        df[,time.cols] <- df[,time.cols]
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
				   unit.norm        = 365,
				   to.partition     = FALSE,
				   to.create        = list(t.hf=sprintf("timehf%d",seq(1:10)),
										   t.sca="timescd1",
										   n.hf=sprintf("timehf%d",seq(1:10)),
										   n.sca="timescd1",
										   i.hf=sprintf("timehf%d",seq(1:10)),
										   i.sca="timescd1",
										   ni.hf=sprintf("timehf%d",seq(1:10)),
										   ni.sca="timescd1",
                                           i.death='deathtime'),
                   k.to.persist     = 0,
                   quality.check    = TRUE,
                   exempt.variables = "dateofcmdx") {

    print("Working...")


    # before working on a subject-to-subject basis, convert start
    # dates to Date objects; extract for future use
    df$start.time <- to.date(df[,start.date])
    df$death.date <- to.date(df[,death.date])
    start.dates <- df[,start.date]
    end.dates <- df[,death.date]

    
    # convert all columns that look like dates to times elapsed since start of study
    date.cols      <- names(which(sapply(df, function(x) any(sapply(x, is.date)))))

    # ids 
    ids <- df[,id]

    # check formatting first
    if (quality.check) {
        df[,date.cols] <- perform.quality.assurance(df[,date.cols], 
                                                    check.type='formatting', 
                                                    date.cols=date.cols, 
                                                    ids=ids)
    }

    df <- to.time(df, names(df), "start.time")

    # check validity
    if (quality.check) {

        exempted <- df[,date.cols[!(date.cols %in% exempt.variables)]]
        perform.quality.assurance(exempted, 
                                  check.type='validity', 
                                  date.cols=date.cols, 
                                  lens=to.date(end.dates) - to.date(start.dates), 
                                  ids=ids,
                                  start.dates=to.date(start.dates),
                                  end.dates=to.date(end.dates))
    }

    # find continuous variables that pull from multiple columns,
    # and TODO: make sure that they all work together as they should
    # extract names of columns to add by type
    #var.names       <- names(df)
    #continuous.vars <- grep("^c\\.",  var.names)
    #prepare.c.vars(df, to.create)
    #print(continuous.vars)
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
				   			unit.norm,
				   			to.partition, 
				   			to.create, 
                            k.to.persist))

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
				  unit.norm, 
				  to.partition, 
				  to.create,
                  k.to.persist) {

    # extract specified columns
    td.events   <- df[,td.events]
    t.outcome 	<- df[,t.outcome]
    tdeath 		<- df[,death.time]
    death 		<- df[,death]
    cur.id 		<- df[,id]


    interval.mod <- interval * unit.norm
    

    # compute number of rows (e.g. number of intervals) to return
    n.intervals <- ceiling(t.outcome[[1]] / interval.mod)

    # extract names of columns to add by type
    var.names 		<- names(to.create)
    indicators 		<- grep("^i\\.",  var.names)
    count.total 	<- grep("^n\\.",  var.names)
    count.current 	<- grep("^ni\\.", var.names)
    time.vars 		<- grep("^t\\.",  var.names)
    continuous.vars <- grep("^c\\.",  var.names)

    # extract data specified by to.create; do for continuous variables before unlisting all
    c.vars  <- lapply(to.create[continuous.vars], 
                    function(x) {lapply(x, function(y) {as.numeric(as.matrix(select(df, !!y)))})})
    
    cvars.sort <- function(cv) {
        sorted.times <- sort(cv$times, index.return=TRUE)
        sorted.values <- cv$values[sorted.times$ix]
        return(list(times=sorted.times$x, values=sorted.values))
    }

    c.vars <- lapply(c.vars, cvars.sort)
    vars 	<- lapply(to.create, unlist)
    
    ind 	<- lapply(vars[indicators], 	 function(x) {as.matrix(select(df, !!x))})
    ct.tot 	<- lapply(vars[count.total], 	 function(x) {as.matrix(select(df, !!x))})
    ct.cur 	<- lapply(vars[count.current], 	 function(x) {as.matrix(select(df, !!x))})
    t.vars 	<- lapply(vars[time.vars], 		 function(x) {as.matrix(select(df, !!x))})

    ##############################################################################
    ## Naming convention check
    ##
    ## Make sure the user is playing by the rules; if they're naming things 
    ## incorrectly, let 'em know! We don't want to break silently.
    ##############################################################################

    # info string for error, if naming convention not followed
    naming.err <- paste("Names supplied to to.create must take the following form:",
    					"    i.[var.name]  : indicator variables",
    					"    ni.[var.name] : count of event in current interval",
    					"    n.[var.name]  : count of event over previous intervals",
    					"    t.[var.name]  : time elapsed since event occurred",
    					"    c.[var.name]  : continuous or factor variable change", 
    					"",
    					sep = "\n",
    					collapse = NULL)

    # check to see if all supplied names covered;
    # throw error if not
    if (!all(union(indicators, 
    			union(count.total, 
    				union(count.current, 
    					union(time.vars, continuous.vars)))))) {
    	cat(naming.err)
    	stop("Variables do not conform to naming convention.")
    } 

    # warn user to make sure they name values of continuous.vars correctly
    c.var.warn <- paste("When passing continuous variables with [c.var.name],",
    					"make sure to have to.create[c.var.name] be a list with",
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
    mod.df 				 <- df %>% slice(rep(1:n(), each = n.intervals))

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
    mod.df$"int.n" 		<- seq(1:n.intervals)
    mod.df$"t.start" 	<- interval.mod * (mod.df$int.n - 1) 
    mod.df$"t.end"      <- ifelse(mod.df$t.start + interval.mod < t.outcome[[1]], 
                                 mod.df$t.start + interval.mod, 
                                 t.outcome[[1]])
    mod.df$"rt"         <- ifelse(mod.df$t.start + interval.mod < t.outcome[[1]], 
                                  interval, 
                                  (t.outcome[[1]] - mod.df$t.start) / unit.norm)
    mod.df$"i.death"	<- ifelse(mod.df$t.start + interval.mod < tdeath[[1]],
    							  0,
    							  death[[1]])

    # pass to C++ function to facilitate the looping;
    # set td.events values to -1 if NA to ensure that they
    # won't satisfy inequality in cpiu.cpp
    td.events                   <- unlist(td.events, use.names = FALSE)
    td.events[is.na(td.events)] <- -1

    # handle different variable types differently
    # (functions defined in cpiu.cpp are doing the heavy lifting here)
    indicators.l 		<- lapply(ind, 
    					   		 assemble_indicator, 
    					   		 t_start = mod.df$t.start, 
    					   		 t_end   = mod.df$t.end)
    count.total.l 		<- lapply(ct.tot, 
    							 assemble_accumulator, 
    							 t_start = mod.df$t.start, 
    							 t_end   = mod.df$t.end,
    							 flag    = 1)
    count.current.l 	<- lapply(ct.cur, 
    							 assemble_accumulator, 
    							 t_start = mod.df$t.start, 
    							 t_end   = mod.df$t.end,
    							 flag    = 0)
    time.vars.l 		<- lapply(t.vars, 
    							 assemble_time_elapsed, 
    							 t_start = mod.df$t.start, 
    							 t_end   = mod.df$t.end)
    continuous.vars.l 	<- lapply(c.vars, 
    							 assemble_continuous, 
    							 t_start      = mod.df$t.start, 
    							 t_end        = mod.df$t.end,
                                 k_to_persist = k.to.persist)
    
    # add to dataframe
    if (length(indicators.l) > 0) {
    	indicators.df 				 <- matrix(unlist(indicators.l), 
    										   nrow=n.intervals, 
    										   byrow=F)

    	mod.df[,names(indicators.l)] <- indicators.df
    }

    if (length(count.total.l) > 0) {
    	count.total.df 				  <- matrix(unlist(count.total.l), 
    											nrow=n.intervals, 
    											byrow=F)
    	mod.df[,names(count.total.l)] <- count.total.df
    }

    if (length(count.current.l) > 0) {
    	count.current.df 	<- matrix(unlist(count.current.l), 
    								  nrow=n.intervals, 
    								  byrow=F)
    	mod.df[,names(count.current.l)] <- count.current.df
    }

    if (length(time.vars.l) > 0) {
    	time.vars.df 		<- matrix(unlist(time.vars.l), 
    								  nrow=n.intervals, 
    								  byrow=F)
    	mod.df[,names(time.vars.l)] <- time.vars.df
    }

	if (length(continuous.vars.l) > 0) {
		continuous.vars.df  <- matrix(unlist(continuous.vars.l), 
									  nrow=n.intervals, 
									  byrow=F)
		mod.df[,names(continuous.vars.l)] <- continuous.vars.df
	}

	mod.df <- as.data.frame(mod.df)

    # take care of missingness
    mod.df[mod.df == -1] <- NA


    # take care of variable partitioning on time-dependent events
    # (this is why that argument is still crucial)
    if (to.partition) {
    	td.events <- mod.df[,td.events]
    	split.times <- unique(td.events[!apply(td.events, 2, is.na)])
    	intervals.to.split <- ceiling(split.times / interval.mod) # interval numbers
    	variables.to.update <- names(to.create)[sapply(vars, function(x) {return(any(x %in% td.events))})]
    	mod.df <- mod.df %>%
    			 	group.by.("int.n") %>%
    			 	do(interval.split(., intervals.to.split, split.times, interval, unit.norm, variables.to.update))
    }
    
    return(mod.df)

}

#################################################
# TO UPDATE: 
# - support for changes to what we're calling 
#   'continuous' time-dependent variables
# split intervals, if to.partition == TRUE
#################################################
interval.split <- function(df, intervals.to.split, split.times, interval, unit.norm, variables.to.update) {

	# if not an interval to split, return unmodified
	if (!(df["int.n"] %in% intervals.to.split))
		return(df)

	# find time(s) of split(s)
	times <- sort(split.times[df$t.start < split.times & df$t.end >= split.times])

	# modify original row
	orig.end <- df$t.end
	df$t.end <- min(times)
	df$rt <- (df$t.end - df$t.start) / unit.norm

	# based on the different cases, update variables differently
	time.vars <- variables.to.update[sapply(variables.to.update, 
                                            function(x) { 
                                                return(substring(x, 1, 1) == "t")
                                            } 
                                     )]
	others <- variables.to.update[sapply(variables.to.update, 
                                         function(x) {
                                            return(!any(substring(x, 1, 1) %in% c("c", "t")))
                                         }
                                  )]
	df[,others] <- 0

	# create new rows (one per hf in the interval) by copying df
	mod.df <- df[rep(1:nrow(df),each=length(times) + 1),]

	## all intermediates
	if (length(times) >= 2) {
		for (i in 2:length(times)) {
			mod.df[i,]$t.start <- mod.df[i - 1,]$t.end
			mod.df[i,]$t.end <- times[i - 1]
			mod.df[i,]$rt <- (mod.df[i,]$t.end - mod.df[i,]$t.start) / unit.norm
			mod.df[i,others] <- 1
			mod.df[i,time.vars] <- 0 
		}
	}

	## last one
	end                   <- nrow(mod.df)

	mod.df[end,]$t.start  <- mod.df[end-1,]$t.end
	mod.df[end,]$t.end    <- orig.end
	mod.df[end,]$rt       <- (mod.df[end,]$t.end - mod.df[end,]$t.start) / unit.norm
	mod.df[end,others]    <- 1
	mod.df[end,time.vars] <- 0


	return(mod.df)

}


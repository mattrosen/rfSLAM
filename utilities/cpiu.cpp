/******************************************************************************/
/* cpiu.cpp (7/2018)                                                          */
/*                                                                            */
/* Authors: Shannon Wongvibulsin & Matt Rosen                                 */
/* 																			  */
/* Description: Helper functions for reformatting data into CPIUs.            */
/******************************************************************************/
#include <Rcpp.h>
using namespace Rcpp;

/******************************************************************************/
/* assemble_indicator                                                         */
/*                                                                            */
/* Description: A function to assemble indicator variable (does event x occur */
/*              in this interval?, for example)                               */
/*                                                                            */
/* Arguments:                                                                 */
/* 			 NumericVector events                                             */
/*			 NumericVector t_start                                            */
/* 			 NumericVector t_end                                              */
/*                                                                            */
/* Returns:                                                                   */
/* 			 IntegerVector indicator                                          */
/*                                                                            */
/******************************************************************************/
// [[Rcpp::export]]
IntegerVector assemble_indicator(NumericVector events, NumericVector t_start, 
								 NumericVector t_end) {

	// assert that t_start and t_end are of equal length
	if (t_start.size() != t_end.size()) 
		stop("len(t_start) must equal len(t_end).");

	// vector of indicators; initializes to 0
	IntegerVector indicator(t_start.size());
	
	// set values appropriately
	for (int i = 0; i < t_start.size(); i++) {
		
		// does event occur in this interval?
		for (int j = 0; j < events.size(); j++) {
			if ((events[j] > t_start[i]) && (events[j] <= t_end[i])) {
				indicator[i] = 1;
			}
		}

	}

	return indicator;
}

/******************************************************************************/
/* assemble_accumulator                                                       */
/*                                                                            */
/* Description: A function to assemble accumulator variable (how many times   */
/*              has event x occurred?, for example)                           */
/*                                                                            */
/* Arguments:                                                                 */
/* 			 NumericVector events                                             */
/*			 NumericVector t_start                                            */
/* 			 NumericVector t_end                                              */
/*			 int           flag                                               */
/*                                                                            */
/* Returns:                                                                   */
/* 			 IntegerVector accumulated                                        */
/*                                                                            */
/* Notes:                                                                     */
/* 		- argument 'flag' specifies some options re: accumulation.            */
/*        	0: count # events occurring within EACH INTERVAL.                 */
/*          1: count # events that have occurred in PREVIOUS INTERVALS.       */
/*                                                                            */
/******************************************************************************/
// [[Rcpp::export]]
IntegerVector assemble_accumulator(NumericVector events, 
								   NumericVector t_start, 
								   NumericVector t_end,
								   int flag) {

	// count
	int count = 0;

	// assert that t_start and t_end are of equal length
	if (t_start.size() != t_end.size()) 
		stop("len(t_start) must equal len(t_end).");

	// vector of counts; initializes to 0
	IntegerVector accumulated(t_start.size());
	
	// set values appropriately
	for (int i = 0; i < t_start.size(); i++) {

		// if flag is 0, reset count value to 0
		if (flag == 0) count = 0;

		// if flag is 1, set value of count for current
		// interval to count
		if (flag == 1) accumulated[i] = count;
		
		// count events
		for (int j = 0; j < events.size(); j++) {
			if (events[j] > t_start[i] && (events[j] <= t_end[i]))
				count++;
		}

		// if counting events in this interval, assign value as needed
		if (flag == 0) accumulated[i] = count;

	}

	return accumulated;
}

/******************************************************************************/
/* assemble_time_elapsed                                                      */
/*                                                                            */
/* Description: A function to assemble 'elapsed-time' variables (e.g. 'how    */
/*              much time has elapsed since event x occurred?')               */
/*                                                                            */
/* Arguments:                                                                 */
/* 			 NumericVector events                                             */
/*			 NumericVector t_start                                            */
/* 			 NumericVector t_end                                              */
/*                                                                            */
/* Returns:                                                                   */
/* 			 NumericVector time_elapsed                                       */
/*                                                                            */
/******************************************************************************/
// [[Rcpp::export]]
NumericVector assemble_time_elapsed(NumericVector events, 
								    NumericVector t_start, 
								    NumericVector t_end) {

	// time of last event
	double last_event = 0;
	int event_toggle = 0;

	// assert that t_start and t_end are of equal length
	if (t_start.size() != t_end.size()) 
		stop("len(t_start) must equal len(t_end).");

	// vector of times; initializes to 0
	NumericVector time_elapsed(t_start.size());
	
	// set values appropriately
	for (int i = 0; i < t_start.size(); i++) {

		event_toggle = 0;
		
		// does event occur in this interval? if so, 
		// change time of last event to start time of this interval
		for (int j = 0; j < events.size(); j++) {
			if (events[j] > t_start[i] && (events[j] <= t_end[i])) {
				last_event = events[j];
				event_toggle = 1;
			}
		}

		// compute time since last event + store
		if (!event_toggle) time_elapsed[i] = t_end[i] - last_event;

	}

	return time_elapsed;
}

/******************************************************************************/
/* assemble_continuous                                                        */
/*                                                                            */
/* Description: A function to handle changes in continuous variable values    */
/*              (e.g. lab measures at multiple time-steps, change in HF       */
/*              class, etc.). Can also be used for factor variables that      */
/*              change.                                                       */
/*                                                                            */
/* Arguments:                                                                 */
/* 			 List          measures                                           */
/*			 NumericVector t_start                                            */
/* 			 NumericVector t_end                                              */
/*                                                                            */
/* Returns:                                                                   */
/* 			 NumericVector continuous                                         */
/*                                                                            */
/******************************************************************************/
// [[Rcpp::export]]
NumericVector assemble_continuous(List measures,
								  NumericVector t_start, 
								  NumericVector t_end) {

	// change flag
	int did_change = 0;
	int j_start = 0;

	NumericVector m_times  = as<NumericVector>(measures["times"]);
	NumericVector m_values = as<NumericVector>(measures["values"]);

	// assert that t_start and t_end are of equal length
	if (t_start.size() != t_end.size()) 
		stop("len(t_start) must equal len(t_end).");

	// assert that measure_times and measure_values are equal length
	if (m_times.size() != m_values.size())
		stop("len(m_times) must equal len(m_values");

	// vector of values; initializes to 0
	NumericVector continuous(t_start.size());
	
	// set values appropriately
	for (int i = 0; i < t_start.size(); i++) {

		did_change = 0;
		
		// does change in measure occur in this interval?
		for (int j = j_start; j < m_times.size(); j++) {
			if ((m_times[j] == 0 || m_times[j] > t_start[i]) && (m_times[j] <= t_end[i])) {
				continuous[i] = m_values[j];
				did_change = 1;
				j_start = j;
			}
		}

		// check for case of no measurement for first interval; this makes
		if (!did_change) {
			if (i == 0) {
				stop("data must include measurement for first interval.");
			}
			continuous[i] = continuous[i - 1];
		}

	}

	return continuous;
}


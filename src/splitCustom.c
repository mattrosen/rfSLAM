#include  <stdlib.h>
#include  <math.h>
#include  "splitCustom.h"
#include <stdio.h>

/*

  Before coding a custom split rule, you must register it with the
  family specific vector of custom split rules.  This allows the user
  to code from one (1) to sixteen (16) different split rules for each
  family.  In the following function, each sample split rule is
  registered in slot one (1) of the family specific vector of custom
  split rules.

  The specific rule for slot X in the vector, where 1 <= X <= 16, is
  activated by the R-code user parameter:  splitrule = "customX".
  
  Note that splitrule = "custom" is the equivalent of splitrule = "custom1".  

  
 */

void registerCustomFunctions() {

  // Register the custom classification split rule in the first slot.
  registerThis (&getCustomSplitStatisticMultivariateClassification, CLAS_FAM, 1);

  // Register the custom regression split rule in the first slot.
  registerThis (&getCustomSplitStatisticMultivariateRegression, REGR_FAM, 1);

  // Register the custom survival split rule in the first slot.
  registerThis (&getCustomSplitStatisticSurvival, SURV_FAM, 1);

  // Register the custom competing risk split rule in the first slot.
  registerThis (&getCustomSplitStatisticCompetingRisk, CRSK_FAM, 1);

  // If you have more than one function of each type, you would
  // uncomment one or more of the following statements and complete
  // the coding for the named functions.  Note that you can call these
  // functions anything you want.  In a multivariate scenario you must
  // be sure that the index of both the classification and regression
  // split rule are identical.  For example, in a multivariate
  // scenario, splitrule = "custom2", will call the classification
  // split rule in the second slot, along with the regression split
  // rule in the second slot if the respones contain factors and reals.

    registerThis (&poissonSplit1, CLAS_FAM, 2);
    registerThis (&getCustomSplitStatisticMultivariateRegressionTwo, REGR_FAM, 2);

    registerThis (&poissonSplit2, CLAS_FAM, 3);
    registerThis (&getCustomSplitStatisticMultivariateRegressionThree, REGR_FAM, 3);

    registerThis (&poissonSplit3, CLAS_FAM, 4);
    registerThis (&getCustomSplitStatisticMultivariateRegressionFour, REGR_FAM, 4);

    registerThis (&poissonSplit4, CLAS_FAM, 5);
    registerThis (&getCustomSplitStatisticMultivariateRegressionFive, REGR_FAM, 5);

    registerThis (&poissonSplit5, CLAS_FAM, 6);
    registerThis (&getCustomSplitStatisticMultivariateRegressionSix, REGR_FAM, 6);

    registerThis (&poissonSplit6, CLAS_FAM, 7);
    registerThis (&getCustomSplitStatisticMultivariateRegressionSeven, REGR_FAM, 7);
    
    registerThis (&multinomialSplit, CLAS_FAM, 8);
    registerThis (&getCustomSplitStatisticMultivariateRegressionEight, REGR_FAM, 8);
    
  //  registerThis (&getCustomSplitStatisticSurvivalTwo, SURV_FAM, 2);
  //  registerThis (&getCustomSplitStatisticCompetingRiskTwo, CRSK_FAM, 2);

}


/*
  Generic Custom Split Rule Harness

  FUNCTION INPUTS:
  n - number of replicates in the parent node to be split

  membership     - vector of length n representing daughter node
  membership (LEFT or RIGHT) as a result of the
  test split.

  time     - vector of length n of time variable for survival data sets.
  This will be NULL for non-survival data sets.
  event   - vector of length n of event/censoring variable for
  survival data sets.  This will be NULL for non-survival
  data sets.

  response - vector of length n of response variable (y-outcome) in the
  node. In the multivariate case this function will be called 
  once for each response.  The multivariate split statistic 
  is the sum of the individual response statistic.  It is 
  important to normalize the statistic for each response to 
  avoid one response overwhelming another's value.  This will 
  be NULL for survival data sets.

  mean     - convenience value representing the mean of the response vector
  variance - convenience value representing the variance of the response vector

  maxLevel - convenience value representing the maximum level for this factor respone
  in the data set as a whole.  This will be NULL for non-factor respones.

  feature - matrix of user specified features that can be sent into
  the split rule and acted on as desired.  The matrix is of dimension
  [featureCount] x [n].  Features are neither y-variables or
  x-variables. However, for expediency, they are specified by the user
  as y-variables, but are tagged as having zero weight via the
  y-variable weight vector.  Thus, they are never used in the the
  pre-defined split rules, and have no predicted value. The pointer will be NULL
  when features are absent.

  featureCount - count of features in the above matrix, specifically the
  number of rows.  The count will be zero when features are absent.


  FUNCTION OUTPUT:  returns a double value representing the custom split statistic


  Depending on the split statistic, the user may need to allocate and 
  de-allocate arrays of various dimensions.  A typical alloc/de-alloc is
  defined in this file:

  unsigned int *alloc_uivector(...)
  void        dealloc_uivector(...)

  Always remember to deallocate what has been allocated.

  Always remember to declare the function in the corresponding ".h" file.

*/



double getCustomSplitStatisticMultivariateRegression (unsigned int n,
                                                      double k_for_alpha,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount)
{

  // EXAMPLE:  Multivariate Regression

  // Local variables needed for this example:
  double sumLeft, sumRght;
  double sumLeftSqr, sumRghtSqr;
  double delta;

  // Left and right normalization sizes.
  unsigned int leftSize, rghtSize;

  unsigned int i;

  // Initialization of local variables:
  sumLeft = sumRght = 0.0;
  leftSize = rghtSize = 0;

  delta = 0.0;

  // In general, calculating a split statistic will require iterating
  // over all members in the parent node, and ascertaining daughter
  // membership, and performing a well defined calculation based on
  // membership.  In this example, the sum of the difference from the
  // mean for the y-outcome in each daughter node is calculated.

  for (i = 1; i <= n; i++) {
    // Membership will be either LEFT or RIGHT.  
    if (membership[i] == LEFT) {
      // Add the left member to the sum.
      sumLeft += response[i] - mean;
      leftSize ++;
    }
    else {
      // Add the right member to the sum.
      sumRght += response[i] - mean;
      rghtSize ++;
    }
  }

  // Finally, we calculate the composite mean square error for each daughter.
  sumLeftSqr = pow(sumLeft, 2.0) / ((double) leftSize * variance);
  sumRghtSqr = pow(sumRght, 2.0) / ((double) rghtSize * variance);

  delta = sumLeftSqr + sumRghtSqr;

  return delta;
}


double getCustomSplitStatisticMultivariateClassification (unsigned int n,
                                                          double k_for_alpha,
                                                          char        *membership,
                                                          double      *time,
                                                          double      *event,

                                                          unsigned int eventTypeSize,
                                                          unsigned int eventTimeSize,
                                                          double      *eventTime,
                                                          
                                                          double      *response,
                                                          double       mean,
                                                          double       variance,
                                                          unsigned int maxLevel,

                                                          double     **feature,
                                                          unsigned int featureCount)
{

  // EXAMPLE:  Multivariate Classification


  // Local variables needed for this example:
  unsigned int   *leftClassProp, *rghtClassProp;
  double sumLeftSqr, sumRghtSqr;
  double delta;

  // Arrays needed for multivariate regression:
  double sumLeft, sumRght;

  // Left and right normalization sizes.
  unsigned int leftSize, rghtSize;

  unsigned int i, p;

  // Initialization of local variables:
  sumLeft = sumRght = 0.0;
  leftSize = rghtSize = 0;

  delta = 0.0;

  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Vector for class counts.
  leftClassProp = alloc_uivector(maxLevel);
  rghtClassProp = alloc_uivector(maxLevel);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//

  // Initialize the class counts.
  for (p = 1; p <= maxLevel; p++) {
    leftClassProp[p] = rghtClassProp[p] = 0;
  }
  
  // In general, calculating a split statistic will require iterating
  // over all members in the parent node, and ascertaining daughter
  // membership, and performing a well defined calculation based on
  // membership.  In this example, the left and right class counts for
  // the y-outcome in each daughter node are calculated.
  for (i = 1; i <= n; i++) {
    // Membership will be either LEFT or RIGHT.  Package specific
    // constants are contained in global.h.
    if (membership[i] == LEFT) {

      // Add the left member to the left class proportion.
      leftClassProp[(unsigned int) response[i]] ++;
      leftSize ++;

    }
    else {

      // Add the right member to the left class proportion.
      rghtClassProp[(unsigned int) response[i]] ++;
      rghtSize ++;

    }
  }

  for (p = 1; p <= maxLevel; p++) {
    sumLeft += pow((double) leftClassProp[p], 2.0);
    sumRght += pow((double) rghtClassProp[p], 2.0);
  }

  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Vector for class counts.
  dealloc_uivector(leftClassProp, maxLevel);
  dealloc_uivector(rghtClassProp, maxLevel);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//

  sumLeftSqr = sumLeft / leftSize;
  sumRghtSqr = sumRght / rghtSize;
  
  
  delta = sumLeftSqr + sumRghtSqr;

  return delta;
}

double getCustomSplitStatisticSurvival (unsigned int n,
                                        double k_for_alpha,
                                        char        *membership,
                                        double      *time,
                                        double      *event,

                                        unsigned int eventTypeSize,
                                        unsigned int eventTimeSize,
                                        double      *eventTime,

                                        double      *response,
                                        double       mean,
                                        double       variance,
                                        unsigned int maxLevel,

                                        double     **feature,
                                        unsigned int featureCount)
{

  // EXAMPLE:  Survival (logrank)

  // Local variables needed for this example:
  unsigned int   *nodeLeftEvent,  *nodeParentEvent;
  unsigned int   *nodeLeftAtRisk, *nodeParentAtRisk;

  double delta, deltaNum, deltaDen;
  unsigned int i, k;

  // Initialization of local variables:
  deltaNum = deltaDen = 0.0;

  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Vector for event counts.
  nodeLeftEvent   = alloc_uivector(eventTimeSize);
  nodeParentEvent = alloc_uivector(eventTimeSize);

  // Vector for at risk counts.
  nodeLeftAtRisk    = alloc_uivector(eventTimeSize);
  nodeParentAtRisk  = alloc_uivector(eventTimeSize);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//

  // Reset the node specific counts needed for the statistic.
  for (k = 1; k <= eventTimeSize; k++) {
    nodeParentEvent[k]  = 0;
    nodeParentAtRisk[k] = 0;
    nodeLeftEvent[k]    = 0;
    nodeLeftAtRisk[k]   = 0;
  }

  k = eventTimeSize;
  i = n;

  // Iterate over all individuals.  Note that they arrive sorted by time
  // in increasing order.  We parse them in decreasing order.
  while ((i > 0) && (k > 0)) {

    // Initialize the parent event count.
    if (eventTime[k] <= time[i]) {

      // The member is still at risk!
      nodeParentAtRisk[k] ++;

      // Membership will be either LEFT or RIGHT.  
      if (membership[i] == LEFT) {
        nodeLeftAtRisk[k] ++;
      }
        
      // Did the member experience an event?
      if (eventTime[k] == time[i]) {
        if (event[i] > 0) {
          nodeParentEvent[k] ++;

          // Membership will be either LEFT or RIGHT.  
          if (membership[i] == LEFT) {
            nodeLeftEvent[k] ++;
          }
        }
      }

      // Examine the previous individual.
      i--;
    }
    else {

      // Examine the previous event time.
      k--;
    }
  }

  // Adjust the at risk counts to achieve the step function.
  for (k = eventTimeSize; k > 1; k--) {
    nodeParentAtRisk[k-1] = nodeParentAtRisk[k] + nodeParentAtRisk[k-1];
    nodeLeftAtRisk[k-1] = nodeLeftAtRisk[k] + nodeLeftAtRisk[k-1];
  }

  // Iterate over the distinct event times and acquire the numerator and denominator of the test.
  for (k = 1; k <= eventTimeSize; k++) {
    deltaNum = deltaNum + ((double) nodeLeftEvent[k] - ((double) ( nodeLeftAtRisk[k] * nodeParentEvent[k]) / nodeParentAtRisk[k]));

    // Log-Rank denominator requires that there be at least two at risk.
    if (nodeParentAtRisk[k] >= 2) {
      deltaDen = deltaDen + (
                             ((double) nodeLeftAtRisk[k] / nodeParentAtRisk[k]) *
                             (1.0 - ((double) nodeLeftAtRisk[k] / nodeParentAtRisk[k])) *
                             ((double) (nodeParentAtRisk[k] - nodeParentEvent[k]) / (nodeParentAtRisk[k] - 1)) * nodeParentEvent[k]
                             );

    }
  }


  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Vector for event counts.
  dealloc_uivector(nodeLeftEvent, eventTimeSize);
  dealloc_uivector(nodeParentEvent, eventTimeSize);

  // Vector for at risk counts.
  dealloc_uivector(nodeLeftAtRisk, eventTimeSize);
  dealloc_uivector(nodeParentAtRisk, eventTimeSize);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  
  deltaNum = fabs(deltaNum);
  deltaDen = sqrt(deltaDen);
  if (deltaDen <= 1.0e-9) {
    if (deltaNum <= 1.0e-9) {
      delta = 0.0;
    }
    else {
      delta = deltaNum / deltaDen;
    }
  }
  else {
    delta = deltaNum / deltaDen;
  }

  return delta;

}


double getCustomSplitStatisticCompetingRisk (unsigned int n,
                                             double k_for_alpha,
                                             char        *membership,
                                             double      *time,
                                             double      *event,

                                             unsigned int eventTypeSize,
                                             unsigned int eventTimeSize,
                                             double      *eventTime,

                                             double      *response,
                                             double       mean,
                                             double       variance,
                                             unsigned int maxLevel,
                                             
                                             double     **feature,
                                             unsigned int featureCount)
{

  // EXAMPLE:  Competing Risk (logrankCR)

  // Local variables needed for this example:
  unsigned int   *nodeLeftEvent,  *nodeParentEvent;
  unsigned int   *nodeLeftAtRisk, *nodeParentAtRisk;

  // Local CR variable needed for this example:
  unsigned int   **nodeLeftEventCR,  **nodeParentEventCR;
  unsigned int   **nodeLeftInclusiveAtRisk, **nodeParentInclusiveAtRisk;

  double delta, deltaNum, deltaSubNum, deltaDen, deltaSubDen;
  unsigned int i, j, k, r, s;

  // Initialization of local variables:
  deltaNum = deltaDen = 0.0;

  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Vector for event counts.
  nodeLeftEvent   = alloc_uivector(eventTimeSize);
  nodeParentEvent = alloc_uivector(eventTimeSize);

  // Vector for at risk counts.
  nodeLeftAtRisk    = alloc_uivector(eventTimeSize);
  nodeParentAtRisk  = alloc_uivector(eventTimeSize);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//

  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Matrix containing event counts at each event time.
  nodeParentEventCR = alloc_uimatrix(eventTypeSize, eventTimeSize);
  nodeLeftEventCR = alloc_uimatrix(eventTypeSize, eventTimeSize);

  // Matrix containing event inclusive at risk counts at each event time.
  nodeParentInclusiveAtRisk = alloc_uimatrix(eventTypeSize, eventTimeSize);
  nodeLeftInclusiveAtRisk = alloc_uimatrix(eventTypeSize, eventTimeSize);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//

  // Reset the node specific counts needed for the statistic.
  for (k = 1; k <= eventTimeSize; k++) {
    nodeParentEvent[k]  = 0;
    nodeParentAtRisk[k] = 0;
    nodeLeftEvent[k]    = 0;
    nodeLeftAtRisk[k]   = 0;

    for (j = 1; j <= eventTypeSize; j++) {
      nodeParentEventCR[j][k]         = 0;
      nodeLeftEventCR[j][k]           = 0;
      nodeParentInclusiveAtRisk[j][k] = 0;
      nodeLeftInclusiveAtRisk[j][k]   = 0;
    }
  }

  k = eventTimeSize;
  i = n;

  // Iterate over all individuals.  Note that they arrive sorted by time
  // in increasing order.  We parse them in decreasing order.
  while ((i > 0) && (k > 0)) {

    // Initialize the parent event count.
    if (eventTime[k] <= time[i]) {

      // The member is still at risk!
      nodeParentAtRisk[k] ++;

      // Membership will be either LEFT or RIGHT.  
      if (membership[i] == LEFT) {
        nodeLeftAtRisk[k] ++;
      }
        
      // Did the member experience an event?
      if (eventTime[k] == time[i]) {
        if (event[i] > 0) {

          nodeParentEventCR[(unsigned int) event[i]][k] ++;
          nodeParentEvent[k] ++;

          // Membership will be either LEFT or RIGHT.  
          if (membership[i] == LEFT) {
            nodeLeftEventCR[(unsigned int) event[i]][k] ++;
          }
        }
      }

      // Examine the previous individual.
      i--;
    }
    else {

      // Examine the previous event time.
      k--;
    }

  }

  // Adjust the at risk counts to achieve the step function.
  for (k = eventTimeSize; k > 1; k--) {
    nodeParentAtRisk[k-1] = nodeParentAtRisk[k] + nodeParentAtRisk[k-1];
    nodeLeftAtRisk[k-1] = nodeLeftAtRisk[k] + nodeLeftAtRisk[k-1];
  }

  // Finalize the left and right inclusive at risk counts.
  for (k = 1; k <= eventTimeSize; k++) {
    for (j = 1; j <= eventTypeSize; j++) {
      nodeParentInclusiveAtRisk[j][k] = nodeParentAtRisk[k];
      nodeLeftInclusiveAtRisk[j][k] = nodeLeftAtRisk[k];
      for (s = 1; s < k; s++) {
        for (r = 1; r <= eventTypeSize; r++) {
          if (j != r) {
            nodeParentInclusiveAtRisk[j][k]  += nodeParentEventCR[r][s];
            nodeLeftInclusiveAtRisk[j][k]  += nodeLeftEventCR[r][s];
          }
        }
      }
    }
  }

  // Iterate over the event types and distinct event times and acquire the numerator and denominator of the test.
  for (j = 1; j <= eventTypeSize; j++) {

    deltaSubNum = 0;
    for (k = 1; k <= eventTimeSize; k++) {
      deltaSubNum = deltaSubNum + (nodeLeftEventCR[j][k] - (nodeParentEventCR[j][k] * ((double) nodeLeftInclusiveAtRisk[j][k] / nodeParentInclusiveAtRisk[j][k])));
    }
    deltaNum = deltaNum + deltaSubNum;

    deltaSubDen = 0;
    for (k = 1; k <= eventTimeSize; k++) {

      // Log-Rank CR denominator requires that there be at least two at risk.
      if (nodeParentAtRisk[k] >= 2) {
        deltaSubDen = deltaSubDen  + (
                                      (nodeParentEventCR[j][k] * ((double) nodeLeftInclusiveAtRisk[j][k] / nodeParentInclusiveAtRisk[j][k])) *
                                      (1.0 - ((double) nodeLeftInclusiveAtRisk[j][k] / nodeParentInclusiveAtRisk[j][k])) *
                                      ((double) (nodeParentInclusiveAtRisk[j][k] - nodeParentEventCR[j][k]) / (nodeParentInclusiveAtRisk[j][k] - 1))
                                      );
      }
    }
    deltaDen = deltaDen + deltaSubDen;
  }


  
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Vector for event counts.
  dealloc_uivector(nodeLeftEvent, eventTimeSize);
  dealloc_uivector(nodeParentEvent, eventTimeSize);

  // Vector for at risk counts.
  dealloc_uivector(nodeLeftAtRisk, eventTimeSize);
  dealloc_uivector(nodeParentAtRisk, eventTimeSize);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//

  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//
  // Matrix containing event counts at each event time.
  dealloc_uimatrix(nodeParentEventCR, eventTypeSize, eventTimeSize);
  dealloc_uimatrix(nodeLeftEventCR, eventTypeSize, eventTimeSize);

  // Matrix containing event inclusive at risk counts at each event time.
  dealloc_uimatrix(nodeParentInclusiveAtRisk, eventTypeSize, eventTimeSize);
  dealloc_uimatrix(nodeLeftInclusiveAtRisk, eventTypeSize, eventTimeSize);
  //---------- *** WARNING MEMORY MANIPULATION WARNING *** -----------//

  deltaNum = fabs(deltaNum);
  deltaDen = sqrt(deltaDen);
  if (deltaDen <= 1.0e-9) {
    if (deltaNum <= 1.0e-9) {
      delta = 0.0;
    }
    else {
      delta = deltaNum / deltaDen;
    }
  }
  else {
    delta = deltaNum / deltaDen;
  }

  return delta;

}


/**
* Poisson split 1: with stratification by risk time and interval time
* ---------------------------------------------------------------
* for speed, we unroll computations for each of the daughter nodes.
*/
double poissonSplit1 (unsigned int n,
                      double k_for_alpha,
                      char        *membership,
                      double      *time,
                      double      *event,

                      unsigned int eventTypeSize,
                      unsigned int eventTimeSize,
                      double      *eventTime,

                      double      *response,
                      double       mean,
                      double       variance,
                      unsigned int maxLevel,

                      double     **feature,
                      unsigned int featureCount)
{
    /* necessary vars */
    //double k_for_alpha = 0.5;
    int i = 0, K = 0;
    double stat = 0.0, stat_L = 0.0, stat_R = 0.0;
    double y_sum = 0.0, rt_sum = 0.0;
    for (i = 1; i <= n; i++) {
      y_sum += response[i];
      rt_sum += feature[2][i];
    }
    double alpha = 1 / (k_for_alpha * k_for_alpha);
    double beta = alpha / (y_sum / rt_sum);

    /**
    * determine K; also, check that response conforms
    * with expectations (2 classes, 1.0 and 2.0)
    */
    for (i = 1; i <= n; i++) {
        if (feature[1][i] > K) 
            K = feature[1][i];
        if (response[i] > 2.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  does not yet support k-class classification for k > 2; your response\n \
                  took value %f.\n", response[i]);
          exit(1);
        }
        if (response[i] < 1.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  expects responses with values in {1.0, 2.0}. Yours assumed value %f.\n", 
                  response[i]);
          exit(1);
        }
    }
    

    /* arrays for results; pre-allocate everything, 
       for L and R, too, for speed */
    double *lambda_js = calloc((K + 1), sizeof(double));
    double *Y_pjs = calloc((K + 1), sizeof(double));
    double *t_js  = calloc((K + 1), sizeof(double));

    double *lambda_js_L = calloc((K + 1), sizeof(double));
    double *Y_pjs_L = calloc((K + 1), sizeof(double));
    double *t_js_L  = calloc((K + 1), sizeof(double));

    double *lambda_js_R = calloc((K + 1), sizeof(double));
    double *Y_pjs_R = calloc((K + 1), sizeof(double));
    double *t_js_R  = calloc((K + 1), sizeof(double));

    /* compute lambda_js, Y_ks! */
    /* make the pass through, count necessary quantities */
    int cur_j;
    double cur_y, cur_pt;

    for (i = 1; i <= n; i++) {

        cur_y  = response[i];
        cur_j  = (int) feature[1][i];
        cur_pt = feature[2][i];

        /****************/
        /**   parent   **/
        /****************/
        /* increment Y_pjs appropriately */
        if (cur_y == 2.0) Y_pjs[cur_j]++;

        /* increment t_j appropriately */
        t_js[cur_j] += cur_pt;

        /* increment numerator of lambda_j for this fraction */
        lambda_js[cur_j] += (cur_y - 1);

        /****************/
        /** L DAUGHTER **/
        /****************/

        if (membership[i] == LEFT) {
            if (cur_y == 2.0) Y_pjs_L[cur_j]++;
            t_js_L[cur_j]  += cur_pt;
            lambda_js_L[cur_j] += (cur_y - 1);
        }
        
        /****************/
        /** R DAUGHTER **/
        /****************/
        else {
            if (cur_y == 2.0) Y_pjs_R[cur_j]++;
            t_js_R[cur_j]  += cur_pt;
            lambda_js_R[cur_j] += (cur_y - 1);
        }
        
    }

    /* divide to find lambda_js, calc. split statistic for 
       each case */
    for (int j = 1; j <= K; j++) {
      lambda_js[j]   = (lambda_js[j] + alpha)   / (beta + t_js[j]);
      lambda_js_L[j] = (lambda_js_L[j] + alpha) / (beta + t_js_L[j]);
      lambda_js_R[j] = (lambda_js_R[j] + alpha) / (beta + t_js_R[j]);
    }

    /* calculate the split statistic for each case; ignore
       cases when the value of mu is 0 (else will result in 
       NaNs) */
    for (int j = 1; j <= K; j++) {
        stat   += (Y_pjs[j]   * log(lambda_js[j]));
        stat_L += (Y_pjs_L[j] * log(lambda_js_L[j]));
        stat_R += (Y_pjs_R[j] * log(lambda_js_R[j]));
    }

    /* free memory */
    free(lambda_js);
    free(Y_pjs);
    free(t_js);

    free(lambda_js_L);
    free(Y_pjs_L);
    free(t_js_L);

    free(lambda_js_R);
    free(Y_pjs_R);
    free(t_js_R);

    /* determine delta, return */
    return (stat_L + stat_R - stat);


}

double getCustomSplitStatisticMultivariateRegressionTwo (unsigned int n,
                                                      double k_for_alpha,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount)
{
  fprintf(stderr, "outcome must be a factor; custom regression rule not yet implemented.\n");
  exit(1);
}

/******************************************************************************/

/**
* Poisson split 2: with stratification by risk time but not interval time
* -----------------------------------------------------------------------
* for speed, we unroll computations for each of the daughter nodes.
*/
double poissonSplit2 (unsigned int n,
                      double k_for_alpha,
                      char        *membership,
                      double      *time,
                      double      *event,
                      
                      unsigned int eventTypeSize,
                      unsigned int eventTimeSize,
                      double      *eventTime,

                      double      *response,
                      double       mean,
                      double       variance,
                      unsigned int maxLevel,

                      double     **feature,
                      unsigned int featureCount)
{

    /* necessary vars */
    //double k_for_alpha = 0.5;
    int i = 0;
    double stat = 0.0, stat_L = 0.0, stat_R = 0.0;
    double y_sum = 0.0, rt_sum = 0.0;
    for (i = 1; i <= n; i++) {
      y_sum += response[i];
      rt_sum += feature[2][i];
    }
    double alpha = 1 / (k_for_alpha * k_for_alpha);
    double beta = alpha / (y_sum / rt_sum);

    /* K = 1, to denote 1 interval (no stratification by interval) */
    int K = 1;
    

    /* arrays for results; pre-allocate everything, 
       for L and R, too, for speed */
    double *lambda_js = calloc((K + 1), sizeof(double));
    double *Y_pjs = calloc((K + 1), sizeof(double));
    double *t_js  = calloc((K + 1), sizeof(double));

    double *lambda_js_L = calloc((K + 1), sizeof(double));
    double *Y_pjs_L = calloc((K + 1), sizeof(double));
    double *t_js_L  = calloc((K + 1), sizeof(double));

    double *lambda_js_R = calloc((K + 1), sizeof(double));
    double *Y_pjs_R = calloc((K + 1), sizeof(double));
    double *t_js_R  = calloc((K + 1), sizeof(double));

    /* compute lambda_js, Y_ks! */
    /* make the pass through, count necessary quantities */
    int cur_j;
    double cur_y, cur_pt;

    for (i = 1; i <= n; i++) {

        cur_y  = response[i];
        cur_j  = 1;
        cur_pt = feature[2][i];

        /****************/
        /**   parent   **/
        /****************/
        /* increment Y_pjs, Y_ks appropriately */
        if (cur_y == 2.0) Y_pjs[cur_j]++;

        /* increment t_j appropriately */
        t_js[cur_j] += cur_pt;

        /* increment numerator of lambda_j for this fraction */
        lambda_js[cur_j] += (cur_y - 1);

        /****************/
        /** L DAUGHTER **/
        /****************/

        if (membership[i] == LEFT) {
            if (cur_y == 2.0) Y_pjs_L[cur_j]++;
            t_js_L[cur_j]  += cur_pt;
            lambda_js_L[cur_j] += (cur_y - 1);
        }
        
        /****************/
        /** R DAUGHTER **/
        /****************/
        else {
            if (cur_y == 2.0) Y_pjs_R[cur_j]++;
            t_js_R[cur_j]  += cur_pt;
            lambda_js_R[cur_j] += (cur_y - 1);
        }
        
    }

    /* divide to find lambda_js, calc. split statistic for 
       each case */
    for (int j = 1; j <= K; j++) {
      lambda_js[j]   = (lambda_js[j] + alpha)   / (beta + t_js[j]);
      lambda_js_L[j] = (lambda_js_L[j] + alpha) / (beta + t_js_L[j]);
      lambda_js_R[j] = (lambda_js_R[j] + alpha) / (beta + t_js_R[j]);
    }

    /* calculate the split statistic for each case; ignore
       cases when the value of mu is 0 (else will result in 
       NaNs) */
    for (int j = 1; j <= K; j++) {
        stat   += (Y_pjs[j]   * log(lambda_js[j]));
        stat_L += (Y_pjs_L[j] * log(lambda_js_L[j]));
        stat_R += (Y_pjs_R[j] * log(lambda_js_R[j]));
    }

    /* free memory */
    free(lambda_js);
    free(Y_pjs);
    free(t_js);

    free(lambda_js_L);
    free(Y_pjs_L);
    free(t_js_L);

    free(lambda_js_R);
    free(Y_pjs_R);
    free(t_js_R);

    //printf("%f\n", stat_L + stat_R - stat);

    /* determine delta, return */
    return (stat_L + stat_R - stat);


}

double getCustomSplitStatisticMultivariateRegressionThree (unsigned int n,
                                                      double k_for_alpha,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount)
{
  fprintf(stderr, "outcome must be a factor; custom regression rule not yet implemented.\n");
  exit(1);

}

/******************************************************************************/

/**
* Poisson split 3: with stratification by interval time but not risk time.
* ------------------------------------------------------------------------
* for speed, we unroll computations for each of the daughter nodes.
*/
double poissonSplit3 (unsigned int n,
                      double k_for_alpha,
                      char        *membership,
                      double      *time,
                      double      *event,
                      
                      unsigned int eventTypeSize,
                      unsigned int eventTimeSize,
                      double      *eventTime,

                      double      *response,
                      double       mean,
                      double       variance,
                      unsigned int maxLevel,

                      double     **feature,
                      unsigned int featureCount)
{

    /* necessary vars */
    //double k_for_alpha = 0.5;
    int i = 0, K = 0;
    double stat = 0.0, stat_L = 0.0, stat_R = 0.0;
    double y_sum = 0.0, rt_sum = 0.0;
    for (i = 1; i <= n; i++) {
      y_sum += response[i];
      rt_sum += feature[2][i];
    }
    double alpha = 1 / (k_for_alpha * k_for_alpha);
    double beta = alpha / (y_sum / rt_sum);

    /**
    * determine K; also, check that response conforms
    * with expectations (2 classes, 1.0 and 2.0)
    */
    for (i = 1; i <= n; i++) {
        if (feature[1][i] > K) 
            K = feature[1][i];
        if (response[i] > 2.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  does not yet support k-class classification for k > 2; your response\n \
                  took value %f.\n", response[i]);
          exit(1);
        }
        if (response[i] < 1.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  expects responses with values in {1.0, 2.0}. Yours assumed value %f.\n", 
                  response[i]);
          exit(1);
        }
    }
    

    /* arrays for results; pre-allocate everything, 
       for L and R, too, for speed */
    double *lambda_js = calloc((K + 1), sizeof(double));
    double *Y_pjs = calloc((K + 1), sizeof(double));
    double *t_js  = calloc((K + 1), sizeof(double));

    double *lambda_js_L = calloc((K + 1), sizeof(double));
    double *Y_pjs_L = calloc((K + 1), sizeof(double));
    double *t_js_L  = calloc((K + 1), sizeof(double));

    double *lambda_js_R = calloc((K + 1), sizeof(double));
    double *Y_pjs_R = calloc((K + 1), sizeof(double));
    double *t_js_R  = calloc((K + 1), sizeof(double));

    /* compute lambda_js, Y_ks! */
    /* make the pass through, count necessary quantities */
    int cur_j;
    double cur_y, cur_pt;

    for (i = 1; i <= n; i++) {

        cur_y  = response[i];
        cur_j  = (int) feature[1][i];
        cur_pt = 1;//feature[2][i];

        /****************/
        /**   parent   **/
        /****************/
        /* increment Y_pjs, Y_ks appropriately */
        if (cur_y == 2.0) Y_pjs[cur_j]++;

        /* increment t_j appropriately */
        t_js[cur_j] += cur_pt;

        /* increment numerator of lambda_j for this fraction */
        lambda_js[cur_j] += (cur_y - 1);

        /****************/
        /** L DAUGHTER **/
        /****************/

        if (membership[i] == LEFT) {
            if (cur_y == 2.0) Y_pjs_L[cur_j]++;
            t_js_L[cur_j]  += cur_pt;
            lambda_js_L[cur_j] += (cur_y - 1);
        }
        
        /****************/
        /** R DAUGHTER **/
        /****************/
        else {
            if (cur_y == 2.0) Y_pjs_R[cur_j]++;
            t_js_R[cur_j]  += cur_pt;
            lambda_js_R[cur_j] += (cur_y - 1);
        }
        
    }

   /* divide to find lambda_js, calc. split statistic for 
       each case */
    for (int j = 1; j <= K; j++) {
      lambda_js[j]   = (lambda_js[j] + alpha)   / (beta + t_js[j]);
      lambda_js_L[j] = (lambda_js_L[j] + alpha) / (beta + t_js_L[j]);
      lambda_js_R[j] = (lambda_js_R[j] + alpha) / (beta + t_js_R[j]);
    }

    /* calculate the split statistic for each case; ignore
       cases when the value of mu is 0 (else will result in 
       NaNs) */
    for (int j = 1; j <= K; j++) {
        stat   += (Y_pjs[j]   * log(lambda_js[j]));
        stat_L += (Y_pjs_L[j] * log(lambda_js_L[j]));
        stat_R += (Y_pjs_R[j] * log(lambda_js_R[j]));
    }

    /* free memory */
    free(lambda_js);
    free(Y_pjs);
    free(t_js);

    free(lambda_js_L);
    free(Y_pjs_L);
    free(t_js_L);

    free(lambda_js_R);
    free(Y_pjs_R);
    free(t_js_R);

    /* determine delta, return */
    return (stat_L + stat_R - stat);


}

double getCustomSplitStatisticMultivariateRegressionFour (unsigned int n,
                                                      double k_for_alpha,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount)
{
  fprintf(stderr, "outcome must be a factor; custom regression rule not yet implemented.\n");
  exit(1);

}


/**
* Poisson split 1: with stratification by risk time and interval time
* ---------------------------------------------------------------
* for speed, we unroll computations for each of the daughter nodes.
*/
double poissonSplit4 (unsigned int n,
                      double k_for_alpha,
                      char        *membership,
                      double      *time,
                      double      *event,

                      unsigned int eventTypeSize,
                      unsigned int eventTimeSize,
                      double      *eventTime,

                      double      *response,
                      double       mean,
                      double       variance,
                      unsigned int maxLevel,

                      double     **feature,
                      unsigned int featureCount)
{

    /* necessary vars */
    int i, k, K = 0;
    double stat = 0.0, stat_L = 0.0, stat_R = 0.0;

    /**
    * determine K; also, check that response conforms
    * with expectations (2 classes, 1.0 and 2.0)
    */
    for (i = 1; i <= n; i++) {
        if (feature[1][i] > K) 
            K = feature[1][i];
        if (response[i] > 2.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  does not yet support k-class classification for k > 2; your response\n \
                  took value %f.\n", response[i]);
          exit(1);
        }
        if (response[i] < 1.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  expects responses with values in {1.0, 2.0}. Yours assumed value %f.\n", 
                  response[i]);
          exit(1);
        }
    }
    

    /* arrays for results; pre-allocate everything, 
       for L and R, too, for speed */
    double *mu_ks = calloc((K + 1), sizeof(double));
    double *Y_ks  = calloc((K + 1), sizeof(double));
    double *Y_pks = calloc((K + 1), sizeof(double));
    double *t_ks  = calloc((K + 1), sizeof(double));

    double *mu_ks_L = calloc((K + 1), sizeof(double));
    double *Y_ks_L  = calloc((K + 1), sizeof(double));
    double *Y_pks_L = calloc((K + 1), sizeof(double));
    double *t_ks_L  = calloc((K + 1), sizeof(double));

    double *mu_ks_R = calloc((K + 1), sizeof(double));
    double *Y_ks_R  = calloc((K + 1), sizeof(double));
    double *Y_pks_R = calloc((K + 1), sizeof(double));
    double *t_ks_R  = calloc((K + 1), sizeof(double));

    /* compute mu_ks, Y_ks! */
    /* make the pass through, count necessary quantities */
    int cur_k;
    double cur_y, cur_pt;

    for (i = 1; i <= n; i++) {

        cur_y  = response[i];
        cur_k  = (int) feature[1][i];
        cur_pt = feature[2][i];

        /****************/
        /**   parent   **/
        /****************/
        /* increment Y_pks, Y_ks appropriately */
        if (cur_y == 2.0) Y_pks[cur_k]++;
        Y_ks[cur_k]++;

        /* increment t_k appropriately */
        t_ks[cur_k] += cur_pt;

        /* increment numerator of mu_k for this fraction */
        mu_ks[cur_k] += (cur_y - 1) * cur_pt;

        /****************/
        /** L DAUGHTER **/
        /****************/

        if (membership[i] == LEFT) {
            if (cur_y == 2.0) Y_pks_L[cur_k]++;
            Y_ks_L[cur_k]++;
            t_ks_L[cur_k]  += cur_pt;
            mu_ks_L[cur_k] += (cur_y - 1) * cur_pt;
        }
        
        /****************/
        /** R DAUGHTER **/
        /****************/
        else {
            if (cur_y == 2.0) Y_pks_R[cur_k]++;
            Y_ks_R[cur_k]++;
            t_ks_R[cur_k]  += cur_pt;
            mu_ks_R[cur_k] += (cur_y - 1) * cur_pt;
        }
        
    }

    /* divide to find mu_ks, calc. split statistic for 
       each case */
    for (i = 1; i <= K; i++) {
 
        if (t_ks[i] == 0) mu_ks[i] = 0.0;
        else
          mu_ks[i]   = mu_ks[i]   / t_ks[i];
        
        if (t_ks_L[i] == 0) mu_ks_L[i] = 0.0;
        else
          mu_ks_L[i] = mu_ks_L[i] / t_ks_L[i];

        if (t_ks_R[i] == 0) mu_ks_R[i] = 0.0;
        else
          mu_ks_R[i] = mu_ks_R[i] / t_ks_R[i];
    }

    /* calculate the split statistic for each case; ignore
       cases when the value of mu is 0 (else will result in 
       NaNs) */
    for (k = 1; k <= K; k++) {

        if (mu_ks[k] != 0)
          stat   += (Y_pks[k]   * log(mu_ks[k]))   - (Y_ks[k]   * mu_ks[k]);

        if (mu_ks_L[k] != 0)
          stat_L += (Y_pks_L[k] * log(mu_ks_L[k])) - (Y_ks_L[k] * mu_ks_L[k]);

        if (mu_ks_R[k] != 0)
          stat_R += (Y_pks_R[k] * log(mu_ks_R[k])) - (Y_ks_R[k] * mu_ks_R[k]);
    }

    /* free memory */
    free(mu_ks);
    free(Y_ks);
    free(Y_pks);
    free(t_ks);

    free(mu_ks_L);
    free(Y_ks_L);
    free(Y_pks_L);
    free(t_ks_L);

    free(mu_ks_R);
    free(Y_ks_R);
    free(Y_pks_R);
    free(t_ks_R);

    //printf("%f\n", stat_L + stat_R - stat);

    /* determine delta, return */
    return (stat_L + stat_R - stat);


}

double getCustomSplitStatisticMultivariateRegressionFive (unsigned int n,
                                                      double k_for_alpha,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount)
{
  fprintf(stderr, "outcome must be a factor; custom regression rule not yet implemented.\n");
  exit(1);
}

/******************************************************************************/

/**
* Poisson split 5: with stratification by risk time but not interval time,
*                  no Bayes estimate.
* -----------------------------------------------------------------------
* for speed, we unroll computations for each of the daughter nodes.
*/
double poissonSplit5 (unsigned int n,
                      double k_for_alpha,
                      char        *membership,
                      double      *time,
                      double      *event,
                      
                      unsigned int eventTypeSize,
                      unsigned int eventTimeSize,
                      double      *eventTime,

                      double      *response,
                      double       mean,
                      double       variance,
                      unsigned int maxLevel,

                      double     **feature,
                      unsigned int featureCount)
{

    /* necessary vars */
    int i, k;
    double stat = 0.0, stat_L = 0.0, stat_R = 0.0;

    /* K = 1, to denote 1 interval (no stratification by interval) */
    int K = 1;
    

    /* arrays for results; pre-allocate everything, 
       for L and R, too, for speed */
    double *mu_ks = calloc((K + 1), sizeof(double));
    double *Y_ks  = calloc((K + 1), sizeof(double));
    double *Y_pks = calloc((K + 1), sizeof(double));
    double *t_ks  = calloc((K + 1), sizeof(double));

    double *mu_ks_L = calloc((K + 1), sizeof(double));
    double *Y_ks_L  = calloc((K + 1), sizeof(double));
    double *Y_pks_L = calloc((K + 1), sizeof(double));
    double *t_ks_L  = calloc((K + 1), sizeof(double));

    double *mu_ks_R = calloc((K + 1), sizeof(double));
    double *Y_ks_R  = calloc((K + 1), sizeof(double));
    double *Y_pks_R = calloc((K + 1), sizeof(double));
    double *t_ks_R  = calloc((K + 1), sizeof(double));

    /* compute mu_ks, Y_ks! */
    /* make the pass through, count necessary quantities */
    int cur_k;
    double cur_y, cur_pt;

    for (i = 1; i <= n; i++) {

        cur_y  = response[i];
        cur_k  = 1;//(int) feature[1][i];
        cur_pt = feature[2][i];

        /****************/
        /**   parent   **/
        /****************/
        /* increment Y_pks, Y_ks appropriately */
        if (cur_y == 2.0) Y_pks[cur_k]++;
        Y_ks[cur_k]++;

        /* increment t_k appropriately */
        t_ks[cur_k] += cur_pt;

        /* increment numerator of mu_k for this fraction */
        mu_ks[cur_k] += (cur_y - 1) * cur_pt;

        /****************/
        /** L DAUGHTER **/
        /****************/

        if (membership[i] == LEFT) {
            if (cur_y == 2.0) Y_pks_L[cur_k]++;
            Y_ks_L[cur_k]++;
            t_ks_L[cur_k]  += cur_pt;
            mu_ks_L[cur_k] += (cur_y - 1) * cur_pt;
        }
        
        /****************/
        /** R DAUGHTER **/
        /****************/
        else {
            if (cur_y == 2.0) Y_pks_R[cur_k]++;
            Y_ks_R[cur_k]++;
            t_ks_R[cur_k]  += cur_pt;
            mu_ks_R[cur_k] += (cur_y - 1) * cur_pt;
        }
        
    }

    /* divide to find mu_ks, calc. split statistic for 
       each case */
    for (i = 1; i <= K; i++) {
 
        if (t_ks[i] == 0) mu_ks[i] = 0.0;
        else
          mu_ks[i]   = mu_ks[i]   / t_ks[i];
        
        if (t_ks_L[i] == 0) mu_ks_L[i] = 0.0;
        else
          mu_ks_L[i] = mu_ks_L[i] / t_ks_L[i];

        if (t_ks_R[i] == 0) mu_ks_R[i] = 0.0;
        else
          mu_ks_R[i] = mu_ks_R[i] / t_ks_R[i];
    }

    /* calculate the split statistic for each case; ignore
       cases when the value of mu is 0 (else will result in 
       NaNs) */
    for (k = 1; k <= K; k++) {

        if (mu_ks[k] != 0)
          stat   += (Y_pks[k]   * log(mu_ks[k]))   - (Y_ks[k]   * mu_ks[k]);

        if (mu_ks_L[k] != 0)
          stat_L += (Y_pks_L[k] * log(mu_ks_L[k])) - (Y_ks_L[k] * mu_ks_L[k]);

        if (mu_ks_R[k] != 0)
          stat_R += (Y_pks_R[k] * log(mu_ks_R[k])) - (Y_ks_R[k] * mu_ks_R[k]);
    }

    /* free memory */
    free(mu_ks);
    free(Y_ks);
    free(Y_pks);
    free(t_ks);

    free(mu_ks_L);
    free(Y_ks_L);
    free(Y_pks_L);
    free(t_ks_L);

    free(mu_ks_R);
    free(Y_ks_R);
    free(Y_pks_R);
    free(t_ks_R);

    //printf("%f\n", stat_L + stat_R - stat);

    /* determine delta, return */
    return (stat_L + stat_R - stat);


}

double getCustomSplitStatisticMultivariateRegressionSix (unsigned int n,
                                                      double k_for_alpha,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount)
{
  fprintf(stderr, "outcome must be a factor; custom regression rule not yet implemented.\n");
  exit(1);

}

/******************************************************************************/

/**
* Poisson split 6: with stratification by interval time but not risk time,
*                  no Bayes estimate.
* ------------------------------------------------------------------------
* for speed, we unroll computations for each of the daughter nodes.
*/
double poissonSplit6 (unsigned int n,
                      double k_for_alpha,
                      char        *membership,
                      double      *time,
                      double      *event,
                      
                      unsigned int eventTypeSize,
                      unsigned int eventTimeSize,
                      double      *eventTime,

                      double      *response,
                      double       mean,
                      double       variance,
                      unsigned int maxLevel,

                      double     **feature,
                      unsigned int featureCount)
{

    /* necessary vars */
    int i, k, K = 0;
    double stat = 0.0, stat_L = 0.0, stat_R = 0.0;

    /**
    * determine K; also, check that response conforms
    * with expectations (2 classes, 1.0 and 2.0)
    */
    for (i = 1; i <= n; i++) {
        if (feature[1][i] > K) 
            K = feature[1][i];
        if (response[i] > 2.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  does not yet support k-class classification for k > 2; your response\n \
                  took value %f.\n", response[i]);
          exit(1);
        }
        if (response[i] < 1.0) {
          fprintf(stderr, "response variable error: this custom split function for classification\n \
                  expects responses with values in {1.0, 2.0}. Yours assumed value %f.\n", 
                  response[i]);
          exit(1);
        }
    }
    

    /* arrays for results; pre-allocate everything, 
       for L and R, too, for speed */
    double *mu_ks = calloc((K + 1), sizeof(double));
    double *Y_ks  = calloc((K + 1), sizeof(double));
    double *Y_pks = calloc((K + 1), sizeof(double));
    double *t_ks  = calloc((K + 1), sizeof(double));

    double *mu_ks_L = calloc((K + 1), sizeof(double));
    double *Y_ks_L  = calloc((K + 1), sizeof(double));
    double *Y_pks_L = calloc((K + 1), sizeof(double));
    double *t_ks_L  = calloc((K + 1), sizeof(double));

    double *mu_ks_R = calloc((K + 1), sizeof(double));
    double *Y_ks_R  = calloc((K + 1), sizeof(double));
    double *Y_pks_R = calloc((K + 1), sizeof(double));
    double *t_ks_R  = calloc((K + 1), sizeof(double));

    /* compute mu_ks, Y_ks! */
    /* make the pass through, count necessary quantities */
    int cur_k;
    double cur_y, cur_pt;

    for (i = 1; i <= n; i++) {

        cur_y  = response[i];
        cur_k  = (int) feature[1][i];
        cur_pt = 1;//feature[2][i];

        /****************/
        /**   parent   **/
        /****************/
        /* increment Y_pks, Y_ks appropriately */
        if (cur_y == 2.0) Y_pks[cur_k]++;
        Y_ks[cur_k]++;

        /* increment t_k appropriately */
        t_ks[cur_k] += cur_pt;

        /* increment numerator of mu_k for this fraction */
        mu_ks[cur_k] += (cur_y - 1) * cur_pt;

        /****************/
        /** L DAUGHTER **/
        /****************/

        if (membership[i] == LEFT) {
            if (cur_y == 2.0) Y_pks_L[cur_k]++;
            Y_ks_L[cur_k]++;
            t_ks_L[cur_k]  += cur_pt;
            mu_ks_L[cur_k] += (cur_y - 1) * cur_pt;
        }
        
        /****************/
        /** R DAUGHTER **/
        /****************/
        else {
            if (cur_y == 2.0) Y_pks_R[cur_k]++;
            Y_ks_R[cur_k]++;
            t_ks_R[cur_k]  += cur_pt;
            mu_ks_R[cur_k] += (cur_y - 1) * cur_pt;
        }
        
    }

    /* divide to find mu_ks, calc. split statistic for 
       each case */
    for (i = 1; i <= K; i++) {
 
        if (t_ks[i] == 0) mu_ks[i] = 0.0;
        else
          mu_ks[i]   = mu_ks[i]   / t_ks[i];
        
        if (t_ks_L[i] == 0) mu_ks_L[i] = 0.0;
        else
          mu_ks_L[i] = mu_ks_L[i] / t_ks_L[i];

        if (t_ks_R[i] == 0) mu_ks_R[i] = 0.0;
        else
          mu_ks_R[i] = mu_ks_R[i] / t_ks_R[i];
    }

    /* calculate the split statistic for each case; ignore
       cases when the value of mu is 0 (else will result in 
       NaNs) */
    for (k = 1; k <= K; k++) {

        if (mu_ks[k] != 0)
          stat   += (Y_pks[k]   * log(mu_ks[k]))   - (Y_ks[k]   * mu_ks[k]);

        if (mu_ks_L[k] != 0)
          stat_L += (Y_pks_L[k] * log(mu_ks_L[k])) - (Y_ks_L[k] * mu_ks_L[k]);

        if (mu_ks_R[k] != 0)
          stat_R += (Y_pks_R[k] * log(mu_ks_R[k])) - (Y_ks_R[k] * mu_ks_R[k]);
    }

    /* free memory */
    free(mu_ks);
    free(Y_ks);
    free(Y_pks);
    free(t_ks);

    free(mu_ks_L);
    free(Y_ks_L);
    free(Y_pks_L);
    free(t_ks_L);

    free(mu_ks_R);
    free(Y_ks_R);
    free(Y_pks_R);
    free(t_ks_R);

    //printf("%f\n", stat_L + stat_R - stat);

    /* determine delta, return */
    return (stat_L + stat_R - stat);


}

double getCustomSplitStatisticMultivariateRegressionSeven (unsigned int n,
                                                      double k_for_alpha,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount)
{
  fprintf(stderr, "outcome must be a factor; custom regression rule not yet implemented.\n");
  exit(1);

}

/******************************************************************************/

/**
 * Multinomial split: random survival forest with maxLevel - 1 competing risks and equal
 * length discrete time periods
 * ------------------------------------------------------------------------
 * for speed, we unroll computations for each of the daughter nodes.
 */
double multinomialSplit (unsigned int n,
                      double k_for_alpha,
                      char        *membership,
                      double      *time,
                      double      *event,
                      
                      unsigned int eventTypeSize,
                      unsigned int eventTimeSize,
                      double      *eventTime,
                      
                      double      *response,
                      double       mean,
                      double       variance,
                      unsigned int maxLevel,
                      
                      double     **feature,
                      unsigned int featureCount)
{
  /* necessary vars */
  // i is observation, p is event, k is strata (period), K is last period in parent leaf
  int i, p, k, K = 0; 
  double stat = 0.0, stat_L = 0.0, stat_R = 0.0;
  
  /**
   * determine K;
   */
  for (i = 1; i <= n; i++) {
    if (feature[1][i] > K){
      // feature [1][i] contains period of observation i
      K = feature[1][i];
    } 
  }
  
  /* arrays for results; pre-allocate everything, 
   for L and R, too, for speed */
  
  // event set cardinalities at time k for event p in parent s
  unsigned int   **e_kps,  **e_kps_L, **e_kps_R;
  // risk set cardinalities at time  k in parent s
  unsigned int   *r_ks, *r_ks_L, *r_ks_R;
  
  /****************/
  /**   parent   **/
  /****************/
  e_kps = calloc_uimatrix(K, maxLevel);
  r_ks = calloc_uivector(K);
  
  /****************/
  /** L DAUGHTER **/
  /****************/
  e_kps_L = calloc_uimatrix(K, maxLevel);
  r_ks_L = calloc_uivector(K);
  
  /****************/
  /** R DAUGHTER **/
  /****************/
  e_kps_R = calloc_uimatrix(K, maxLevel);
  r_ks_R = calloc_uivector(K);
  
  /* compute e_kps and  r_ks! */
  /* make the pass through, count necessary quantities */
  
  // current resopnse, current strata
  int cur_y, cur_k;
  
  for (i = 1; i <= n; i++) {
    
    cur_y  = (int) response[i];
    cur_k  = (int) feature[1][i];
    // risk time 1 due to periods of length 1
    
    /****************/
    /**   parent   **/
    /****************/
    /* increment e_kps, r_ks appropriately  */
    /*
     * number of persons experiecing event cur_y at k
     */
    e_kps[cur_k][cur_y]++;
    /*
     * risk set for period k is simply the number of 
     * person period observations at k
     */
    r_ks[cur_k]++;
    
    /****************/
    /** L DAUGHTER **/
    /****************/
    if (membership[i] == LEFT) {
      e_kps_L[cur_k][cur_y]++;
      r_ks_L[cur_k]++;
    }
    
    /****************/
    /** R DAUGHTER **/
    /****************/
    else {
      e_kps_R[cur_k][cur_y]++;
      r_ks_R[cur_k]++;
    }
    
  }
  
  /* calculate the split statistic for each case; ignore
   cases when the value of r_ks is 0 (else will result in 
   NaNs) */
  for (k = 1; k <= K; k++) {
    /*
     * parent negative log likelihood per period:
     * number of event occurences * log (empirical MLE of likelhood of event occurence)
     */
    /****************/
    /**   parent   **/
    /****************/
    // if no person period observations at k, then no hazard for all events
    if (r_ks[k] != 0){
      for(p = 1; p <= maxLevel; p ++){
        // if no event observations for event p, then no likelihood contribution for that event
        if (e_kps[k][p] != 0){
          stat += (e_kps[k][p] * log((double) e_kps[k][p] / (double) r_ks[k]));
        }
      }
    }
    
    /****************/
    /** L DAUGHTER **/
    /****************/
    if (r_ks_L[k] != 0){
      for(p = 1; p <= maxLevel; p ++){
        if (e_kps_L[k][p] != 0){
          stat_L += (e_kps_L[k][p] * log((double) e_kps_L[k][p] / (double) r_ks_L[k]));
        }
      }
    }
    
    /****************/
    /** R DAUGHTER **/
    /****************/
    if (r_ks_R[k] != 0){
      for(p = 1; p <= maxLevel; p ++){
        if (e_kps_R[k][p] != 0){
          stat_R += (e_kps_R[k][p] * log((double) e_kps_R[k][p] / (double) r_ks_R[k]));
        }
      }
    }
    
  }
  
  /* free memory
   * h_kps e_kps r_ks
   */
  dealloc_uimatrix(e_kps, K, maxLevel);
  dealloc_uivector(r_ks, K);
  
  dealloc_uimatrix(e_kps_L, K, maxLevel);
  dealloc_uivector(r_ks_L, K);
  
  dealloc_uimatrix(e_kps_R, K, maxLevel);
  dealloc_uivector(r_ks_R, K);
  
  /* determine delta, return */
  return (stat_L + stat_R - stat);
}

double getCustomSplitStatisticMultivariateRegressionEight (unsigned int n,
                                                           double k_for_alpha,
                                                           char        *membership,
                                                           double      *time,
                                                           double      *event,
                                                           
                                                           unsigned int eventTypeSize,
                                                           unsigned int eventTimeSize,
                                                           double      *eventTime,
                                                           
                                                           double      *response,
                                                           double       mean,
                                                           double       variance,
                                                           unsigned int maxLevel,
                                                           
                                                           double     **feature,
                                                           unsigned int featureCount)
{
  fprintf(stderr, "outcome must be a factor; custom regression rule not yet implemented.\n");
  exit(1);
  
}

/*
  Memory allocation and deallocation.
    [nh] = the length of the array
    
  Note that indexing is one-based:  
    array[1] ... array[nh]

  Multi-dimensional array allocationis 
  accomplished via multiple one-dimensional
  array allocations.
*/

unsigned int *alloc_uivector(unsigned int nh)
{
  return (unsigned int *) malloc((size_t) ((nh+1) * (sizeof(unsigned int))));
}

unsigned int *calloc_uivector(unsigned int nh)
{
  return (unsigned int* ) calloc(nh + 1, sizeof(unsigned int));
}

void dealloc_uivector(unsigned int *v, unsigned int nh)
{
  free((char *) v);
}


double *alloc_dvector(double *v, unsigned int nh)
{
  return (double *) malloc((size_t) ((nh+1) * (sizeof(double))));
}

void dealloc_dvector(double *v, unsigned int nh)
{
  free((char *) v);
}

unsigned int **alloc_uimatrix(unsigned int n2h, unsigned int nh)
{
  unsigned int **v = (unsigned int **) malloc((size_t) ((n2h+1) * (sizeof(unsigned int *))));

  for (unsigned int i = 1; i <= n2h; i++) {
    v[i] = alloc_uivector(nh);
  }
  return v;
}

unsigned int **calloc_uimatrix(unsigned int n2h, unsigned int nh)
{
  unsigned int **v = (unsigned int **) calloc(n2h + 1, sizeof(unsigned int *));
  
  for (unsigned int i = 1; i <= n2h; i++) {
    v[i] = calloc_uivector(nh);
  }
  return v;
}

void dealloc_uimatrix(unsigned int **v, unsigned int n2h, unsigned int nh)
{
  for (unsigned int i = 1; i <= n2h; i++) {
    dealloc_uivector(v[i], nh);
  }
  free((char *) v);
}

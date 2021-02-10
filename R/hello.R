# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' calculateRFM
#'
# Description
#' Calculate the a weighted RFM score: recency, frequency, and monetary for every customers
#
# Arguments
#'@param data - A data.table containing the transaction rececord details for every customer.
#'@param weight_recency - Weight of recency.
#'@param weight_frequency - Weight of frequency.
#'@param weight_monetary - Weight of monetary.
#'
#' @details
#' \code{data} contains the transactional data. The dataset must contain a
#' column labeled "Customer" that allows unique customer identification
#' and a column labeled "TransDate", indicating the purchase date.
#' The column "PurchAmount" specifies the total spending per purchase.
#
# Return Value
#'@return Returns a data.data containing the recency, frequency and monetary
#'score as well as the weighted final score and the group membership.
# Examples
#'@examples
#' temp <- RFMfunction(transactions,60,20,20)
#'
#'@export
calculateRFM <- function(data, default_r, default_f, default_m){
                          weight_r <- default_r/sum(default_r, default_f, default_m)
                          weight_f <- default_f/sum(default_r, default_f, default_m)
                          weight_m <- default_m/sum(default_r, default_f, default_m)

  print("caculated_weight")

  # RFM measures
  maxDate <- max(data$TransDate)

  temp <- data[,list(
    recency = as.numeric(maxDate - max(TransDate)),
    frequency = .N,
    monetary = mean(PurchAmount)),
    by="Customer"
  ]


  print("RFM Measure done")


  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]



  # Overall RFM score
  temp[,finalscore:=weight_r*recency+weight_f*frequency+weight_m*monetary]



  print("Overall RFM Measure done")



  # RFM group
  temp[,group:=round(finalscore)]



  # Return final table
  return(temp)
}



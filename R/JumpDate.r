#' JumpDate
#'
#' Personal function to jump forward/backwards X number of days/weeks/months in base R.
#' Sometimes I don't want to bother with packages
#'
#' @param DateIn    An R-Date, or something that can be converted automatically by as.Date
#' @param TimeJump  Number of units back/forward e.g. 3 days, TimeJump=3
#' @param TimeUnit  A character string, containing one of "day", "week", "month", "quarter" or "year". This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".
#' @param Direction "back" for back, everything else goes forward
#' @param verbose   Do you want it to print out what you are doing? Useful for debugging
#'
#' @return Your new date
#'
#' @details
#' This is a wrapper for \code{\link[seq.Date]{seq.Date()}}
#' I want to be able to easily move forward and back in time without having to use a package.
#'
#' See here for other ways to achieve this for example using lubridate and the background to this function
#'    \href{https://stackoverflow.com/questions/5225823/how-to-subtract-months-from-a-date-in-r}{https://stackoverflow.com/questions/5225823/how-to-subtract-months-from-a-date-in-r}
#'
#' @seealso \code{\link[seq.Date]{seq.Date()}}
#'
#' @examples
#' # Move 1 month back from 2000-02-29
#' JumpDate("2000-02-29",2,"months","back")
#'
#' # Move 14 weeks forward from Jan-1-2002 and print out all output
#' Ans <- JumpDate(DateIn="2002-01-01",TimeJump=14,TimeUnit="week",Direction="forward",verbose=TRUE)
#' print(Ans)
#' @export
JumpDate <- function(DateIn=NA,TimeJump=NA,TimeUnit=NA,Direction="back",verbose=FALSE){

   #---------------------------------------------------------
   # Check input is there
   #---------------------------------------------------------
   if((is.na(DateIn)==TRUE)|(is.na(TimeJump)==TRUE)|(is.na(TimeUnit)==TRUE)){stop("Function inputs missing")}

   #---------------------------------------------------------
   # Set up the by string, using tolower to make it not case sensitive
   #---------------------------------------------------------
   if(tolower(Direction)=="back"){
      bystring <- paste("-",TimeJump," ",tolower(TimeUnit),sep="")
   }else{
      bystring <- paste(TimeJump," ",tolower(TimeUnit),sep="")
   }

   #---------------------------------------------------------
   # Output what you entered if required
   #---------------------------------------------------------
   if(verbose==TRUE){
      message("You are trying to move")
      print(paste(bystring, Direction))
      message("From the date")
      print(DateIn)
   }

   #---------------------------------------------------------
   # Return the new date using seq in the base package
   #---------------------------------------------------------
   output <- seq(as.Date(DateIn),length.out=2,by=bystring)[2]
   return(output)
}

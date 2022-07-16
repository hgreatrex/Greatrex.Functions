#' JumpDate
#'
#' Jump forward/backwards X number of days/weeks/months in base R.
#' Sometimes you don't want to bother with packages
#'
#' @param DateIn   Date: An R-Date, or something that can be converted automatically by as.Date
#' @param TimeJump  numeric: Number of units back/forward e.g. 3 days, TimeJump=3
#' @param TimeUnit  string: String. Anything that works in seq.Date e.g. "years" "months" "days"
#' @param Direction  string: "back" for back, everything else goes forward
#' @param verbose    TRUE/FALSE. Do you want it to print out what you are doing? Useful for debugging
#' @return Your new date
#' @details
#' I want to be able to easily move forward and back in time without having to use a package
#' See ?seq.Date for more examples of what you can add in TimeUnit
#' @examples
#' # Move 1 month back from 2000-02-29
#' JumpDate("2000-02-29",2,"months","back")
#'
#' # Move 14 weeks forward from Jan-1-2002 and print out all output
#' JumpDate(DateIn="2002-01-01",TimeBack=14,TimeUnit="weeks",Direction="forward",verbose=TRUE)
#' @export
JumpDate <- function(DateIn=NA,TimeBack=NA,TimeUnit=NA,Direction="back",verbose=FALSE){

   #---------------------------------------------------------
   # Check input is there
   #---------------------------------------------------------
   if((is.na(DateIn)==TRUE)|(is.na(TimeBack)==TRUE)|(is.na(TimeUnit)==TRUE)){stop("Function inputs missing")}

   #---------------------------------------------------------
   # Set up the by string, using tolower to make it not case sensitive
   #---------------------------------------------------------
   if(tolower(Direction)=="back"){
      bystring <- paste("-",TimeBack," ",tolower(TimeUnit),sep="")
   }else{
      bystring <- paste(TimeBack," ",tolower(TimeUnit),sep="")
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

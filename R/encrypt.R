#' @return results
#' @name encrypt
#' @import Rmpfr
#' @export
encrypt <-function(code, systems, current){
  if (!requireNamespace("Rmpfr", quietly = TRUE)) {
    stop("Rmpfr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  S<<-mpfrArray(NA,prec=159,dim=systems)
  m<<-mpfrArray(NA,prec=159,dim=systems)
  R<<-mpfrArray(NA,prec=159,dim=systems)

  C<-code
  C<-paste(1,C,sep="")
  results<<-data.frame(NULL)

  from<-c("a","b","c","d","e","f","g","h","i","j","k",
           "l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"," ")
  to<-c(01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27) #pre defined base 10 alphabet

  #from text to decimal notation:
  for(a in 1:27)
  { C<-gsub(from[a],to[a],C)
  print(C)}

  R1<-mpfr(floor(runif(1, min=10^(nchar(C)-1), 10^(nchar(C)+1))),159) #Random number
  R[current]<<-R1
  m[current]<<-C-R1 #message

  print("text to base 10")

  #process of tracing the sequence of C, variable called i, sequence stored in matrix called Csequence:
  i<-mpfr(as.numeric(C), 159)

  Csequence<-mpfrArray(NA, prec = 80, dim = 1000000)
  Csequence[1]<-i
  a=2
  while(!i==1)
  {
    print(Csequence[a-1])
    if(is.whole(i/2))
    {
      i<-i/2
      Csequence[a]<-i
      a<-a+1
      }
    else if(!is.whole(i/2))
    {
      i<-3*i+1
      Csequence[a]<-i
      a<-a+1
    }

  }
  print("C sequence traced")
    #process of tracing the sequence of R1, variable called j, sequence stored in matrix called R1sequence:
    j<-mpfr(R1, 159)
    R1sequence<-mpfrArray(NA, prec = 80, dim = 1000000)
    R1sequence[1]<-j
    b<-2
    while(!j==1)
    {
      print(R1sequence[b-1])
      if(is.whole(j/2))
      {
        j<-j/2
        R1sequence[b]<-j
        b<-b+1}
      else if(!is.whole(j/2))
      {
        j<-3*j+1
        R1sequence[b]<-j
        b<-b+1}

    }
    print("R1 sequence traced")
    #seppos y sepnum; position of separation and last common element:
    e<-a
    if(b>a){e=b}
    for(d in 0:e)
    {
      if(!Csequence[a-d-1]==R1sequence[b-d-1])
      {
        R1seppos<-b-d-1
        R1sepnum<-R1sequence[b-d-1]
        break
      }
    }
    print("seppos and sepnum calculated")
    #checkR1:
    checkR1<-mpfr(R1sepnum, 159)
    for(d in 2:R1seppos-1)
    {
      checkR1<-checkR1+R1sequence[R1seppos-d]
    }
    print("checkR1 calculated")
    #Solution 1, S1:
    S[current]<<-checkR1-R1
    print("S[current] calculated")

}


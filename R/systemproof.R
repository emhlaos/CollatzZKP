#' @return results
#' @name systemproof
#' @import Rmpfr
#' @export
systemproof <-function(C,systems, current, check){
  if (!requireNamespace("Rmpfr", quietly = TRUE)) {
    stop("Rmpfr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  S2<<-mpfrArray(NA,prec=159,dim=systems)
  C<-paste(1,C,sep="")
  result<<-data.frame(NULL)

  from<-c("a","b","c","d","e","f","g","h","i","j","k",
          "l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"," ")
  to<-c(01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27) #pre defined decimal alphabet

  #from text to decimal notation:
  for(a in 1:27)
  { C<-gsub(from[a],to[a],C)}
  CODE<<-C
  ################   KNOWLEDGE CHECK FROM SYSTEM[current]   ###############


  R2<<-as.numeric(C)-m[check]

  #tracing R2 sequence, varaible called l. Stored in matrix called R2sequence:

  l<-mpfr(as.numeric(R2), 159)
  R2sequence<-mpfrArray(NA, prec = 80, dim = 1000000)
  R2sequence[1]<-l
  b=2
  while(!l==1)
  {
    print(R2sequence[b-1])
    if(is.whole(l/2))
    {
      l<-l/2
      R2sequence[b]<-l
      b<-b+1}
    else if(!is.whole(l/2))
    {
      l<-3*l+1
      R2sequence[b]<-l
      b<-b+1}
  }


  print("R2 sequence traced")
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

  #seppos y sepnum; position of separation and last common element:
  e<-a
  if(b>a){e=b}
  for(d in 0:e)
  {
    print(d)
    if(!Csequence[a-d-1]==R2sequence[b-d-1])
    {
      R2seppos<<-b-d-1
      R2sepnum<<-R2sequence[b-d-1]
      break
    }
  }

  #checkR2
  checkR2<<-mpfr(R2sepnum, 159)
  for(d in 2:R2seppos-1)
  {
    checkR2<<-checkR2+R2sequence[R2seppos-d]
  }

  S2[current]<<-checkR2-R2 #the S2, solution of other systems, for system [check]
  if(S2[current]==S[check]) #if the solution computed of system [check] is the same as the computed by them then they are trustworthy
  {
    print(TRUE)
  }
  else{print(FALSE)}

}

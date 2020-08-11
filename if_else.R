#--------- -2 ------- -1 ------- 0 ------ 1 --------- 2 -------

#to generate a set of normal random number

for (i in 1:20) {
#rm(answer)
x <- rnorm(1)
if (x>1 & x<3) {
  answer <- "Greater than 1"
}else{
  answer <- "default"
}
}

answer

#else if
x <- rnorm(1)
if (x>1) {
  answer <- "Greater than 1"
}else{
  
  if(x>= -1){
  answer <- "Between -1 and 1"
  }else{
    answer <- "Less than -1"
  }
}
answer


#Chaning statements

x <- rnorm(1)
if(x>1){
  answer <- "Greater than 1"
} else if(x>=-1){
  answer <- "Between -1 and 1"
} else{
  answer <- "Less than -1"
}
answer














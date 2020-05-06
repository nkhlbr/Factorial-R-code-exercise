

##If the following libraries are not present in the R, please install and load the following:

install.packages('purrr')
library('purrr')
install.packages('microbenchmark')
library('microbenchmark')

#1. Factorial_loop
  
  Factorial_loop = function(a) {
    if (a == 0) {
      x = 1
    } else if (a == 1) {
      x = 1
    }
    
    else {
      x = 1
      for (i in 1:a) {
        x = x * ((1:a)[i])
      }
    }
    message(message(paste('The factorial of', (a))), message(paste('=', (x))))
  }
  Factorial_loop(8)

#2. Factorial_reduce
  Factorial_reduce = function(b) {
    if (b == 0 || b == 1){
    message(message(paste('The factorial of', (b))), message(paste('is:', 1)))
    }
    else {
    y = reduce(1:b, `*`)
   # return(y)
    message(message(paste('The factorial of', (b))), message(paste('is:', (y))))
    }
  }
  Factorial_reduce(8)

#3. Factorial_func  
  
  Factorial_func <- function(c) {
    if (c == 0 || c == 1){
      return(1)
    }
    else {
    c * Factorial_func(c-1) 
    }
  }
  
 c = 8 
 
 Answer <- Factorial_func(c)
 
 message(Answer, message('The factorial of: ',(c)))
  
 
#4. Factorial_mem 
  
  fact_tbl <- c(rep(NA, 9000))
  
  Factorial_mem <- function(d) {
    if (d == 0 || d == 1)
      return(1)
    if (!is.na(fact_tbl)[d])
      return(fact_tbl[d])
    fact_tbl[d] <<- d * Factorial_mem(d - 1)
    fact_tbl[d]
  }

  Answer <- Factorial_mem(d)
  
  Factorial_mem(8)
  
  #Diagnostic comparisons between functions of ascending inputs
  
  ##Comparison between functions for value  2
  microbenchmark(a = Factorial_loop(2),
                 b = Factorial_reduce(2),
                 c = Factorial_func(2),
                 d = Factorial_mem(2))
  
  ##Comparison between functions for value 8
  microbenchmark(a = Factorial_loop(8),
                 b = Factorial_reduce(8),
                 c = Factorial_func(8),
                 d = Factorial_mem(8))
  
  ##Comparison between functions for value 32
  microbenchmark(a = Factorial_loop(32),
                 b = Factorial_reduce(32),
                 c = Factorial_func(32),
                 d = Factorial_mem(32))
  
  ##Comparison between functions for value 128
  microbenchmark(a = Factorial_loop(128),
                 b = Factorial_reduce(128),
                 c = Factorial_func(128),
                 d = Factorial_mem(128))
  
  
  ##Comparison between functions for value 1000
  microbenchmark(a = Factorial_loop(1000),
                 b = Factorial_reduce(1000),
                 c = Factorial_func(1000),
                 d = Factorial_mem(1000))
  

  
  

     
  
  
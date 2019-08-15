# Largest prime factor
x <-  # <- input a number you want
primes <- numeric(0)
# Split the whole algorithm into two parts for a search of primes for small numbers
if (x > 19^2){
  for (i in 2:sqrt(x)){
    # Check whether a number is a factor of x up to sqrt(x)
    if (x%%i==0) {
      factor <- i
      # Check the factor on these two conditions that narrow down the range of possible primes
      # If any of them is met, than we can be sure that a number "may be" prime, 
      # conversaly, the number is not prime for sure
      cond1 <- ((factor-1)/6)%%1==0 
      cond2 <- ((factor+1)/6)%%1==0
      if (any(factor == c(2,3,5,7,11,13,17,19))){
        prime <- factor
        primes <- c(primes, prime)
      } else if(any(cond1,cond2)){
        maybe_prime <- factor
        check_vec <- logical(0)
        # Now we can check each of the potential primes on whether it's truly prime or not 
        # by dividing the candidate by a range of numbers up to sqrt(maybe_prime)
        for (i in 2:sqrt(maybe_prime)){
          if (maybe_prime%%i == 0) a <- F else a <- T
          check_vec <- c(check_vec, a)
        }
      }
      if (all(check_vec)){
        prime <- maybe_prime
        primes <- c(primes,prime)
      }
    }
  }
} else {
  for (i in 2:x){
    if(x%%i==0){
      factor <- i
      if (any(factor == c(2,3,5,7,11,13,17,19))){
        prime <- factor
        primes <- c(primes, prime)
      }
    } 
  }
}
primes

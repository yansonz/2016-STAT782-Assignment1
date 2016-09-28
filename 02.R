n = 5
x = seq(-1, 1, by = 0.2)

total = 0
for(k in 0:floor(n/2)) {
  total = total + (-1)^k * factorial(2*n - 2*k) / ( factorial(k) * factorial(n-k) * factorial(n-2*k) ) * x^(n-2*k)
}
1/2^n * total

pn1 = function(n) {
  x = seq(-1, 1, by = 0.2)
  total = 0
  result = numeric(length(x))
  for(i in 1:length(x)) {
    for(k in 0:floor(n/2)) {
      total = total + (-1)^k * factorial(2*n - 2*k) / ( factorial(k) * factorial(n-k) * factorial(n-2*k) ) * x[i]^(n-2*k)
    }
    result[i] = 1/2^n * total
    total = 0
  }
  
  result
}

pn1(5)


pn2 = function(n) {
  x = seq(-1, 1, by = 0.2)
  k = 0:floor(n/2)
  total = 0
  result = numeric(length(x))
  
  for(i in 1:length(x)) {
    total= sum((-1)^k * factorial(2*n - 2*k) / ( factorial(k) * factorial(n-k) * factorial(n-2*k) ) * x[i]^(n-2*k))
    result[i] = 1/2^n * total
    total = 0
  }
  
  result  
}
pn2(5)
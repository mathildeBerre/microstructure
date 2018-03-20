
E = matrix (data = c(2, 4, 3, 1), nrow= 2, ncol = 2, byrow=TRUE)
T = matrix (data = c(1, 4, 7, 1), nrow= 2, ncol = 2, byrow=TRUE)
Z = matrix (data = c(1, 3, 6, 1), nrow= 2, ncol = 2, byrow=TRUE)

lambda = 2
a = 2

N <- function(T,E,Z)
{
  VAR = T + (a^2/lambda^2) * E %*% Z %*% E
  T - T%*%solve(VAR)%*%T
}

f <- function(T,E,Z)
{
  N = N(T,E,Z);
  invN = solve(N)
  result = solve(invN%*%E%*%invN) + (invN + lambda*solve(E));
  result;
}

Cov <- function(T,E,Z)
{
  FN = f(T,E,Z)
  N1 = N(T,E,Z)
  - lambda *  FN - lambda^2 * FN %*% solve(T-N1) %*% FN
}

listN = vector("list", length = 48)
listCOV = vector("list", length = 48)

for (i in 0:47)
{ 
  T[1,1] = i/4
  #E[1,1] = i/16
  Z[1,1] = i/4
  N1 = N(T,E,Z)
  listN[i+1] = N1[1,1]
  Cov1 = Cov(T,E,Z)
  listCOV[i+1] = Cov1[1,1]
}
print(listN)
print(listCOV)

plot(listN ,listCOV)

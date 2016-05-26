# In mathematics, the generalized taxicab number Taxicab(k, j, n) is the smallest number
# which can be expressed as the sum of j ..  k-th positive powers ..  in n different ways.
# For k = 3 and j = n = 2, they coincide with taxicab numbers.
# 
k <- 3    # k-th positive powers
j <- 2    # smallest number which can be expressed as the sum of j .. k-th positive powers
n <- 2    # in n different ways
# 
ceiling <- 12   # limit search to this ceiling
combosWithReplace <- TRUE    # when generating combinations use replacement
# 
ptm <- proc.time()
#
# The possible numbers
numbers <- 1:ceiling
# print(numbers)

# R Package to generate COMBINATIONS with REPALCEMENT
if (combosWithReplace) {
  if (!require("iterpc")) {
    cat("please load R library module 'iterpc'")
    #  I <- iterpc(ceiling, j)
    #  print(getall(I))
  }
}

# The possible combinations of numbers - with OR without REPALCEMENT
if (combosWithReplace) {
      point <-  iterpc(ceiling, j, replace=TRUE)
      combosWR <- getall(point)
      # print(str(combosWR))
      # print(combosWR) 
      combos <- data.frame(combosWR)
      #  MYSTERY  !!!  
      #  combos <- data.frame(matrix(iterpc(numbers, j, replace=TRUE), ncol=j, byrow=TRUE))
      #  !*!*!*!  FAILS  .. applying matrix does wierd things
    } else {
      combos <- data.frame(matrix(combn(numbers, j), ncol=j, byrow=TRUE))
    }
# print(combos)

# exit()

# rowSum k Powers generated
rowSumPowers = NULL
for(x in 1:nrow(combos)){
  rowSumPowers <- c(rowSumPowers, sum(combos[x,]^k))
}
combos <- cbind(combos,rowSumPowers)
# print(combos)

labels <- paste(letters[1:j],"_Int",sep="")    # max is 26
# print(labels)

colnames(combos) = c(labels,"sumPowers_Num")
# print(combos); cat("\n")

combos[,3] <- as.character(combos[,3])
colnames(combos) = c(labels,"sumPowers_Char")
# print(table(combos[,3]))

taxiCabCharName <- which(table(combos[,3]) == n)
print(str(taxiCabCharName))
print(taxiCabCharName)

ofNote <- c(names(taxiCabCharName))
# print(ofNote)
result <- subset(combos, sumPowers_Char %in% ofNote)

result[,3] <- as.numeric(result[,3])
colnames(result) = c(labels,"sumPowers_Num")

result <- result[order(result$sumPowers_Num), ]
result <- result[!is.na(result[,3]),]

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Ta(x) or Taxicab(3, 2, 2) = 1729 = 1^3 + 12^3 = 9^3 + 10^3 - famously stated by Ramanujan.\n\n")
cat(k,"= k .. k-th positive powers.\n")
cat(j,"= j .. expressed as the sum of j .. k-th positive powers.\n")
cat(n,"= n .. expressed in n different ways.\n\n")
cat(ceiling,"= ceiling .. search limited to this ceiling.\n")
cat(combosWithReplace,"= combosWithReplace .. with REPLACEMENT when generating combinations.\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
print(result)

cat("\n")
print(proc.time() - ptm)
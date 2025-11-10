rm(list=ls())

## The Greedy Shortest Common Superstring

"""
> Create also functions:
  > * `Overlap()` to calculate overlap between two sequences.
> * `OverlapMatrix()` to create a matrix of overlaps among all sequences in `S`.


GreedySuperstring(S)
1   while length of S > 1
2     overlapMat <- OverlapMatrix(S)
3     if max(overlapMat) = 0
4       return S
5     else
  6       seq1, seq2 ← Two sequences from S with the longest overlap
7       Merge seq1 and seq2 and add the new sequence to S
8       Remove seq1 and seq2 from S
9   return S
"""

Overlap <- function(x, y){
  lenx <- nchar(x)
  leny <- nchar(y)
  max_ol <- min(lenx, leny)
  
  longest <- ""
  for (o in 1:max_ol){ # o = delka
    endx <- substr(x, lenx - o +1, lenx)
    starty <- substr(y, 1, 1+o-1)
    #print(endx)
    #print(starty)
    if (endx == starty){
      longest <- endx
    }
  }
  return(longest)
}

x <- "abcdeb"
y <- "deab"
Overlap(x,y)

st <- "abcde"
substr(st, nchar(st) - 4,nchar(st))

OverlapMatrix <- function(S){
  r <- length(S)
  c <- length(S)
  om <- matrix(0, r, c)
  for (i in 1:r){
    for (j in 1:c){
      if (i!=j){
        overlap <- nchar(Overlap(S[i],S[j]))
        #print(overlap)
        om[i,j] <- overlap
      }
    }
  }
  return(om)
}

S = c("CATGC", "CTAAGT", "GCTA", "TTCA", "ATGCATC")
om <- OverlapMatrix(S)
max_value <- max(om)
pos <- which(om == max_value, arr.ind = TRUE)
seqx <- S[pos[1]]
seqy <- S[pos[2]]
merged <- paste0(seqy, substr(seqx, max_value, nchar(seqx)))
S <- c(S, merged)
S <- S[S != seqx]
S <- S[S != seqy]

print(S)


GreedySuperstring <- function(S){
  while (length(S)>1) {
    overlapMat <- OverlapMatrix(S)
    if (max(overlapMat) == 0){
      return(S)
    }
    else{
      max_value <- max(overlapMat)
      pos <- which(overlapMat == max_value, arr.ind = TRUE)
      seqx <- S[pos[1]]
      seqy <- S[pos[2]]
      merged <- paste0(seqy, substr(seqx, max_value, nchar(seqx)))
      S <- c(S, merged)
      S <- S[S != seqx]
      S <- S[S != seqy]
    }
  }
  return(S)
}
  
S = c("CATGC", "CTAAGT", "GCTA", "TTCA", "ATGCATC")
GreedySuperstring(S)
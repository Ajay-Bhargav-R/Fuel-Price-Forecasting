#HMM
#Baum-Welch
library(HMM)
Final_data <- read.csv("C:/R1/Final_data.csv", header=TRUE) 
View(Final_data)

# Initial HMM
hmm = initHMM(c("Upward","Downward"),c("Up","Same","Down"),
              transProbs=matrix(c(.8,.2,
                                  .2,.8)
                                ,2),
              emissionProbs=matrix(c(.35,.25,.32,
                                     .33,.5,.33,
                                     .32,.25,.35)
                                   ,3))

#c(.5,.25,.2,
#    .3,.5,.3,
#    .2,.25,.5)
#     ,3)
#(c(.35,.25,.32,
#.33,.5,.33,
#.32,.25,.35)
#,3))

hmm = initHMM(c("Upward","Stable","Downward"),c("Up","Same","Down"),
              transProbs=matrix(c(.5,.25,.2,.3,.5,.3,.2,.25,.5),3),
              emissionProbs=matrix(c(.9,.25,.03,
                                     .07,.5,.07,
                                     .03,.25,.9),3))
print(hmm)
observation = Final_data[,'Del_Dsl']
# Sequence of observation
#a = sample(c(rep("L",100),rep("R",300)))
#b = sample(c(rep("L",300),rep("R",100)))


observation = Final_data[,'Blr_Dsl']
# Baum-Welch
bw = baumWelch(hmm,observation,1000, delta=1e-02)
print(bw$hmm)
guess1 <- bw$hmm
guess2 <- bw$hmm
guess3 <- bw$hmm
guess4 <- bw$hmm
guess5 <- bw$hmm
guess6 <- bw$hmm




#Converting given values into states

values <- c(67,67,67.4,67.1,66.8)


state_convert <- function(values){
  n <- length(values)
  states <- c("Up","Down","Same")
  obs <- c()
  for(i in c(2:n)){
    if(values[i]>values[i-1]){
      obs[i-1]=states[1]
    }
    if(values[i]<values[i-1]){
      obs[i-1]=states[2]
    }
    if(values[i]==values[i-1]){
      obs[i-1]=states[3]
    }
  }
  return(obs)
}

state_convert(values)

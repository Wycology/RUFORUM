install.packages("agricolae") #installing the package
library(agricolae) # calling the package agricolae

#Illustration 1 (slide 14, 22, 37)
reaction<-c(10,9.5, 11, 9) # a vector show reaction times of rats to treatment
mean(reaction) #calculate mean of reaction times of rats
var(reaction) #calculate variance of reaction times of rats
reaction2<-c(50, 90, 150, 500) # reaction times of dogs to one treatment
mean(reaction2) #calculate mean of reaction times of dogs
var(reaction2) #calculate variance of reaction times of dogs
#Illustration 2 (slide 15) - calculation variance of the mean
reaction<-c(10,9.5,11, 9) #observations from 4 rats
var(reaction) #variance of observations from 4 rats
var(reaction)/8 # variance of the mean obtained from the 4 rats

reaction3<-c(10, 9.5, 11, 9, 10, 9.5, 11, 8.5) #observations from 8 rats
var(reaction3) #variance of observations from 8 rats
var(reaction)/8 # variance of the mean obtained from the 8 rats

#Illustration 3 (slide 34)
#randomization for CRD using R
 set.seed(500) #allow for get same randomization 
 f <- factor( rep( c("A", "B", "C" ), each = 4)) #creating a vector of factor levels (treatments)
 fac <- sample( f, 12 ) #Randomizes the order of the levels (sample 12 without replacement)
 expt_unit <- 1:12 #
 plan <- data.frame( Rat=expt_unit, Treatment=fac )
 plan #print field plan
#randomization of CRD using agricolae package
 crddesign<-design.crd(f, r, serie = 2, seed = 50, kinds = "Super-Duper",randomization=TRUE)
 crddesign$book #print the design

 #Illustration 4 (slide 41, 43)
 #Randomise complete Block Design - plan
  treat <- c("A","B","C","D", "E", "F") 
  b1t <- sample(treat,6) #randomization for block 1
  b2t <- sample(treat,6) #randomization for block 2
  b3t <- sample(treat,6) #randomization for block 3
  b4t <- sample(treat,6) #randomization for block 3
  b5t <- sample(treat,6) #randomization for block 3
  b6t <- sample(treat,6) #randomization for block 3
  treatment<-c(b1t, b2t, b3t) # create a combined vector for treatments in the different blocks
  block <- factor( rep(c("Block 1", "Block2", "Block3","Block4", "Block5", "Block6"),6))
  plot <- rep(1:4,3) #generating labels from plot in each block/category
  plan<-data.frame(Block = block, Plot.Number = plot,treatment=treat)
  plan
    #Generating Randomize complete Block Design using agricolae
  rcbddesign<-design.rcbd(treat, r=6, serie = 2, seed = 11, kinds = "Super-Duper", first=TRUE,
                continue=FALSE,randomization=TRUE )
  rcb<-rcbddesign$book #picking one element from a list rcbddesign
  levels(rcb$block) <- c("Block 1", "Block 2", "Block 3","Block 4","Block 5","Block 6") #labeling the blocks
  rcb #print rcb
 
  #Illustration 5 (slide 51)
  #Generating Latin Square Design   
  Nutrition <- c("Nutrition 1", "Nutrition 2", "Nutrition 3", "Nutrition 4", "Nutrition 5") #treatments
  lsddesign <- design.lsd( Nutrition, seed = 23) #generating the design
  lsd <- lsddesign$book #picking a component
  levels(lsd$row) <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5")
  levels(lsd$col) <- c("Animal 1", "Animal 2", "Animal 3","Animal 4", "Animal 5")
  head(lsd) #showing the first 6 rows
    
#Illustration 6 (slide 56)   
#Generating Split-plot design  
treatment1<-c("Spray", "No Spray") # main plot treatment
treatment2<-c("Variety 1","Variety 2", "Variety 3","Variety 4","Variety 5","Variety 6") #subplot treatment

spdesign<-design.split(treatment1, treatment2,r=3, design="rcbd",serie = 2,
             seed = 10, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
spdesign$book # print the plan

#Illustration 7 (slide 69, 70)
#Generation of Balanced Incomplete Design (BIB)
trt<-c("25C", "30C", "35C", "40C") #defined treatments
bibdesign<-design.bib(trt, k=3, r=NULL, serie = 2, seed = 30, kinds = "Super-Duper",
           maxRep=20,randomization=TRUE)
bibdesign$book

#Illustration 8 (slide 84)
#Generating alpha lattice design
trt<-1:100
t<-length(trt)
k<-5 #number of blocks
s<-t/k
r<-2 #number of replications

alphadesign<-design.alpha(trt, k=5, r=2, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)
class(alphadesign) #finding out the type of object
str(alphadesign) #finding out the structure of the object outdesign
book<-alphadesign$book # picking one element from our list called book
plots<-book[,1] #creating an object called plot showing experimental units/plots
dim(plots)<-c(k,s,r) #showing plot labels with respect to k, s, r
for (i in 1:r) print(t(plots[,,i]))
alphadesign$sketch #field sketch

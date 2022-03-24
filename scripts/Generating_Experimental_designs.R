# install.packages("agricolae") 

library(agricolae)

reaction <- c(10, 9.5, 11, 9) 
mean(reaction)
var(reaction)
reaction2 <- c(50, 90, 150, 500)
mean(reaction2)
var(reaction2)

var(reaction)/8

reaction3 <- c(10, 9.5, 11, 9, 10, 9.5, 11, 8.5)
var(reaction3)
var(reaction)/8

set.seed(500)
f <- factor(rep(c("A", "B", "C" ), each = 4))
fac <- sample(f, 12 )
expt_unit <- 1:12
plan <- data.frame(Rat = expt_unit, Treatment = fac)
plan

# CRD Randomization

crddesign <- design.crd(f, r = 4, serie = 2, seed = 50, 
                        kinds = "Super-Duper", randomization = TRUE)

crddesign$book

# Randomise complete Block Design - plan

treat <- c("A","B","C","D", "E", "F")
b1t <- sample(treat,6) 
b2t <- sample(treat,6) 
b3t <- sample(treat,6) 
b4t <- sample(treat,6) 
b5t <- sample(treat,6) 
b6t <- sample(treat,6) 

treatment <- c(b1t, b2t, b3t)
block <- factor(rep(c("Block1", "Block2", "Block3","Block4", "Block5", "Block6"), 6))

plot <- rep(1:4,3)
plan <- data.frame(Block = block, Plot.Number = plot,treatment = treat)
plan

# Generating RCBD

rcbddesign <- design.rcbd(treat, r = 6, serie = 2, seed = 11,
                          kinds = "Super-Duper", first = TRUE,
                          continue = FALSE,randomization = TRUE)

rcb <- rcbddesign$book
levels(rcb$block) <- c("Block 1", "Block 2", "Block 3","Block 4","Block 5","Block 6")
rcb

# Generating Latin Square Design   

Nutrition <- c("Nutrition 1", "Nutrition 2", "Nutrition 3", "Nutrition 4", "Nutrition 5")

lsd_design <- design.lsd(Nutrition, seed = 23)

lsd <- lsd_design$book
levels(lsd$row) <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5")
levels(lsd$col) <- c("Animal 1", "Animal 2", "Animal 3","Animal 4", "Animal 5")
head(lsd)
    
# Generating Split-plot design  

treatment1 <- c("Spray", "No Spray") 
treatment2 <- c("Variety 1","Variety 2", "Variety 3","Variety 4","Variety 5","Variety 6") 

sp_design <- design.split(treatment1, treatment2,r = 3, design = "rcbd", serie = 2,
             seed = 10, kinds = "Super-Duper", first = TRUE, randomization = TRUE)
sp_design$book

# Generation of Balanced Incomplete Design (BIB)

trt <- c("25C", "30C", "35C", "40C")
bibdesign <- design.bib(trt, k = 3, r = NULL, serie = 2, seed = 30, kinds = "Super-Duper",
           maxRep = 20, randomization = TRUE)
bibdesign$book

# Generating alpha lattice design

trt <- 1:100
t <- length(trt)
k <- 5 # number of blocks
s <- t/k
r <- 2 # number of replications

alpha_design <- design.alpha(trt, k = 5, r = 2, serie = 2, 
                          seed = 0, kinds = "Super-Duper",randomization = TRUE)
class(alpha_design)
str(alpha_design)
book <- alpha_design$book
plots <- book[,1]
dim(plots) <- c(k, s, r)
for (i in 1:r) print(t(plots[,,i]))

alpha_design$sketch

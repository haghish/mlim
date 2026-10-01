setwd("~/PROJECTS/mlim_paper/code")
library(mlim) # MLIM v. 0.0.6.2
library(missMDA)
library(mice)
library(VIM)
library(CALIBERrfimpute)
data("iris")

# add stratified missing at 15% rate
# ==================================================
df <- iris
dfNA <- mlim.na(df, p = .15, stratify = TRUE)

# impute with default mlim
# ==================================================
mm <- mlim(data=dfNA, m=5, algos = c("GBM", "RF","ELNET","Ensemble"),
           flush = FALSE,
           preimpute = "random",
           tuning_time = 60, ram = 130, cpu = 20, seed=2022
           #save="irisstatus.mlim", verbosity = "debug",
           #report = "iris_15p.md",
           #tolerance = 1e-03, doublecheck = TRUE
)

#mm <- mlim(load=readRDS("mlimstatus"))
(mlimError <- mlim.error(mm, dfNA, df, varwise = F, transform = NULL))
mean(mlim.error(mm, dfNA, df, varwise = F, transform = NULL)[, 1])

# missMDA
# ==================================================
nb <- estim_ncpFAMD(dfNA) #number of components = 4
missmda <- MIFAMD(dfNA, ncp = 4, nboot=50)
#nb <- estim_ncpMCA(dfNA)
#missmda <- MIMCA(dfNA, ncp=nb$ncp, nboot=5, verbose=TRUE)
(mdaError <- mlim.error(missmda$res.MI, dfNA, df, varwise = F))

midsmda <- prelim(missmda, dfNA)
mda.slsw <- with(data=midsmda, exp=cor(Sepal.Length, Sepal.Width))
unlist(mda.slsw$analyses)
mean(unlist(mda.slsw$analyses))

# missForest
# ==================================================
mf <- missForest::missForest(dfNA)
(mfError <- mlim.error(mf$ximp, dfNA, df, varwise = F))

# missRanger
# ==================================================
msr <- missRanger::missRanger(dfNA)
(mfError <- mlim.error(msr, dfNA, df, varwise = F))


# MICE PMM
# ==================================================
mc <- mice(dfNA, m = 5, method = "pmm")
mcList <- complete(mc, action = "all", include = FALSE)
(mcError <- mlim.error(mcList, dfNA, df, varwise = F))


mice.slsw <- with(data=mc, exp=cor(Sepal.Length, Sepal.Width))
unlist(mice.slsw$analyses)
mean(unlist(mice.slsw$analyses))

# # MICE RF
# # ==================================================
# mcrf <- mice(dfNA, m = 5, meth = "rf", ntree = 500)
# mcList <- complete(mcrf, action = "all", include = FALSE)
# mlim.error(mcList, dfNA, df, varwise = F)

# MICE CALIBERrfimpute
# ==================================================
cal <- mice(dfNA, method = c('rfcont', 'rfcont','rfcont','rfcont','rfcat'), m = 5, ntree = 500, maxit = 10)
calList <- complete(cal, action = "all", include = FALSE)
(calibError <- mlim.error(calList, dfNA, df, varwise = F))
cal.slsw <- with(data=cal, exp=cor(Sepal.Length, Sepal.Width))
mean(unlist(cal.slsw$analyses))

# kNN Imputation with VIM
# ==================================================
kNN <- kNN(dfNA, imp_var=FALSE)
print(kNNerror <- mlim.error(kNN, dfNA, df))

save.image(file = "iris_15p.RData")

# find 2 more packages


# Create the plot
# ===========================================================
plotdata <- data.frame(Error = c(mean(calibError[,1]), mean(mlimError[,1]), kNNerror[1], mean(mcError[,1]), mfError[1], mean(mdaError[,1])),
                       SD = c(sd(calibError[,1]), sd(mlimError[,1]),
                              NA, sd(mcError[,1]), NA, sd(mdaError[,1])),
                       Algorithms = c("RF", "MLIM", "kNN", "PMM", "missForest", "missMDA"))

plotdata$Error <- as.numeric(plotdata$Error)
plotdata$SD    <- as.numeric(plotdata$SD)

library(ggplot2)
# Default bar plot
print(p <- ggplot(plotdata, aes(x=Algorithms, y=Error, fill=Algorithms)) +
        geom_bar(stat="identity", color="3e1063", alpha=0.75,
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=Error-SD, ymax=Error+SD),
                      width=0.25, colour="#230b47", alpha=0.9, size=1.25, position=position_dodge(.9)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
        #width=0.4, colour="orange", alpha=0.9, size=1.3
        ggtitle("Imputation error of continuous variables (iris dataset)") +
        ylab("Normalized Root Mean Square Error\n") +
        xlab("") +


        #theme_classic() +
        #theme_bw() +
        theme(legend.position = "none",
              axis.text = element_text(face="bold"),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "#d5eef0",
                                              colour = "#d5eef0",
                                              size = 0.5, linetype = "solid")
              #panel.border = element_blank()
        ) +
        scale_fill_manual(values = c("#5d1869", "#3c0c4f", "#650875", "#3cc7c0",
                                     "#3e1063", "#430c4d", "#D55E00", "#CC79A7"))
)


# Create the plot
# ===========================================================
plotdata <- data.frame(Error = c(mean(calibError[,3]), mean(mlimError[,3]), kNNerror[3], mean(mcError[,3]), mfError[3], mean(mdaError[,3])),
                       SD = c(sd(calibError[,3]), sd(mlimError[,3]),
                              NA, sd(mcError[,3]), NA, sd(mdaError[,3])),
                       Algorithms = c("RF", "MLIM", "kNN", "PMM", "missForest", "missMDA"))

plotdata$Error <- as.numeric(plotdata$Error)
plotdata$SD    <- as.numeric(plotdata$SD)
plotdata$lowend <- plotdata$Error-plotdata$SD
plotdata$lowend[plotdata$lowend < 0] <- 0

library(ggplot2)
# Default bar plot
print(p <- ggplot(plotdata, aes(x=Algorithms, y=Error, fill=Algorithms)) +
        geom_bar(stat="identity", color="3e1063", alpha=0.75,
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lowend, ymax=Error+SD),
                      width=0.25, colour="#230b47", alpha=0.9, size=1.25, position=position_dodge(.9)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, .5)) +
        #width=0.4, colour="orange", alpha=0.9, size=1.3
        ggtitle("Imputation error of balanced multinomial variables (iris dataset)") +
        ylab("Mean Per Class Error\n") +
        xlab("") +


        #theme_classic() +
        #theme_bw() +
        theme(legend.position = "none",
              axis.text = element_text(face="bold"),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "#d5eef0",
                                              colour = "#d5eef0",
                                              size = 0.5, linetype = "solid")
              #panel.border = element_blank()
        ) +
        scale_fill_manual(values = c("#5d1869", "#3c0c4f", "#650875", "#3cc7c0",
                                     "#3e1063", "#430c4d", "#D55E00", "#CC79A7"))
)


save.image(file = "iris_15p.RData")

load("iris_15p.RData")

# impute with default mlim
# ==================================================


post <- mlim(data=dfNA,
             algos = c("ELNET","RF","Ensemble"),
             tuning_time = 60*15, m = 5,
             #maxiter = 3, tolerance = 0.01, doublecheck = FALSE,
             #ram = 100, cpu = 21,
             seed=2022,
             save = "iris_post_p15.rds",
             verbosity = "debug", report = "charity_elnet_15p.md")
mlim.error(post, dfNA, df, varwise = F, transform = NULL)
mean(mlim.error(post, dfNA, df, varwise = F, transform = NULL)[, 1])

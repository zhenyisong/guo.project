# @author Yisong Zhen
# @since  2017-10-08
# @update 2017-10-11
# see the original image file
# in the Guo's coorespondance
# raw data was sent by Guo in two seperated mails.
#---

# install.packages('metafor', repo = 'http://mirrors.ustc.edu.cn/CRAN/')
pkgs      <- c('metafor','tidyverse','magrittr')
load.libs <- lapply(pkgs, require, character.only = T)

# rawdata load from the specified directory
rawdata.file.path  <- file.path('D:\\sourcecode\\guo.project\\update.data')
rawdata.file.names <- list( 'B07_2.csv',  'C0302_2.csv','DRB07_2.csv',
                            'DRB12_2.csv','B07_F.csv', 'B07_M.csv',
                            'DRB07_F.csv', 'DRB07_M.csv', 'B07_FM.csv',
                            'DRB07_FM.csv')
lab.notation <- c( 'Southwest','Northeast','North',
                   'East','Northwest','Central', 'South')
getForest.data <- function(file.name) {
    f.data   <- read.csv(file.name) %>%
                arrange( match(Region,lab.notation) )
    
    f.result <- f.data %$% 
                {rma(yi = beta, sei = se, method = 'FE')}
    (summary(f.result))
    return(f.result)
}


setwd(rawdata.file.path)
f.all.results <- map(rawdata.file.names, getForest.data)
names(f.all.results) <- rawdata.file.names %>% unlist %>%
                        sub(pattern = '.csv', replacement = '')



#---
#  Working on side by side forest plot
#  mfrow meaning number of row, 2 is how many panel will be side by side;
#  mar meaning margin
#---
par(mfrow = c(2,2), mar   = c(5,4,2,2))


#---
# @params for metaR forest function
# transf=exp meaning beta will transformed to OR;
# refline=1 meaning refrence line (0 for beta and 1 for OR);
# annotate=FALSE remove the annotation,for example, .007[-0.01,0.16];
# slab=rep("",length(res$yi) meaning remove study group;
# mlab="Summary Estimate" meaning the lable of summary esitiate;
# xlim=c(0.9,1.3) tell the postion of figure;
# alim the actual x-axis limits;
# steps the number of tick marks for the x-axis (the default is 5);
# cat(paste('"',"Beta (95%CI)","Heterogeneity p-value=0.7321",'"',sep='\n')), but failed;
# cex=0.8 control the size of text;
# size control the position of the figure;
# line : on which MARgin line;
#---

plot.img <- function(data.df, mlab, mtext.1, mtext.2) {
    forest( data.df, annotate = FALSE,refline = 0, 
            xlab = 'Beta (95%CI)', slab = lab.notation, 
            mlab = mlab, psize = 3, alim = c(-0.3,0.3), steps = 10)
    mtext(mtext.1, side = 3, line = -2.5)
    mtext(mtext.2, side = 1, line = 4, cex = 0.8)
}

plot.1.list.names <- c('B07_2', 'C0302_2', 'DRB07_2', 'DRB12_2')
plot.1.results    <- f.all.results[plot.1.list.names]

mlab.param        <- list( B07_2   = 'Meta p-value=6.90E-10', C0302_2 = 'Meta p-value=4.450E-8',
                           DRB07_2 = 'Meta p-value=1.318E-8', DRB12_2 = 'Meta p-value=1.518E-9' )
mtext.param.1     <- list( B07_2   = 'HLA-B*07',    C0302_2 = 'HLA-C*03:02',
                           DRB07_2 = 'HLA-DRB1*07', DRB12_2 = 'HLA-DRB1*12' )
mtext.param.2     <- list( B07_2   = '                   p=0.7550',    C0302_2 = '                   p=0.1729',
                           DRB07_2 = '                   p=0.1729', DRB12_2 = '                   p=0.1139' )

args.plot.1 <- list(plot.1.results, mlab.param, mtext.param.1, mtext.param.2)
invisible( pmap(args.plot.1, plot.img) )

# Figure 2.
#---

plot.2.list.names <- c('B07_F', 'B07_M', 'DRB07_F', 'DRB07_M')
plot.2.results    <- f.all.results[plot.2.list.names]

mlab.param        <- list( B07_F   = 'HLA-B*07 Female Meta p-value=0.009', 
                           B07_M   = 'HLA-B*07 Male Meta p-value=1.50E-9',
                           DRB07_F = 'HLA-DRB1*07 Male Meta p-value=1.54E-10', 
                           DRB07_M = 'HLA-DRB1*07 Female Meta p-value=0.0559' )
mtext.param.1       <- list( B07_F   = 'HLA-B*07 Female ', 
                           B07_M   = 'HLA-B*07 Male ',
                           DRB07_F = 'HLA-DRB1*07 Male ', 
                           DRB07_M = 'HLA-DRB1*07 Female ' )
mtext.param.2       <- list( B07_F   = '                   p=0.4531', 
                           B07_M   = '                   p=0.2848',
                           DRB07_F = '                   p=0.3634', 
                           DRB07_M = '                   p=0.0407' )
args.plot.1       <- list(plot.2.results, mlab.param, mtext.param.1, mtext.param.2)
invisible( pmap(args.plot.1, plot.img) )

# Figure 3.
#---
plot.3.list.names <- c('B07_FM', 'DRB07_FM')
plot.3.results    <- f.all.results[plot.3.list.names]
lab.notation <- c( 'Southwest Female', 'Northeast Female', 'North Female',
                   'East Female', 'Northwest Female', 'Central Female',
                   'South Female', 'Southwest Male', 'Northeast Male',
                   'North Male', 'East Male','Northwest Male',
                   'Central Male','South Male')
mlab.param        <- list( B07_FM   = 'Meta p-value=0.009', 
                           DRB07_FM = 'Meta p-value=0.0559')
mtext.param.1       <- list( B07_FM   = 'HLA-B*07 Female ', 
                             DRB07_FM = 'HLA-DRB1*07 Female ' )
mtext.param.2       <- list( B07_FM   = '                   p=0.4531', 
                             DRB07_FM = '                   p=0.0407' )
args.plot.1       <- list(plot.3.results, mlab.param, mtext.param.1, mtext.param.2)
par(mfrow = c(2,1), mar = c(5,4,1,1))
invisible( pmap(args.plot.1, plot.img) )


























install.packages("rmeta")

# Simple Meta-analysis
library(rmeta)

# examples of case control analysis;
data(cochrane)
cochrane

colnames(cochrane)

# fit both fixed effects and random effects models to look at the odds ratios.
model.FE <- meta.MH(n.trt,n.ctrl,ev.trt,ev.ctrl, names=name,data=cochrane)
model.RE <- meta.DSL(n.trt,n.ctrl,ev.trt,ev.ctrl, names=name,data=cochrane)

# Fixed effects ( Mantel-Haenszel ) meta-analysis
meta.MH(ntrt = n.trt, nctrl = n.ctrl, ptrt = ev.trt, pctrl = ev.ctrl, 
              names = name, data = cochrane)


# forest plot

CPplot <- function(model)
{
  c1 <- c("","Study",model$names,NA,"Summary")
  c2 <- c("Deaths","(Steroid)",cochrane$ev.trt,NA,NA)
  c3 <- c("Deaths","(Placebo)",cochrane$ev.ctrl,NA,NA)
  c4 <- c("","OR",format(exp(model[[1]]),digits=2),NA,format(exp(model[[3]]),digits=2))
  
  tableText <-cbind(c1,c2,c3,c4)
  mean   <- c(NA,NA,model[[1]],NA,model[[3]])
  stderr <- c(NA,NA,model[[2]],NA,model[[4]])
  low <- mean - 1.96*stderr
  up <- mean + 1.96*stderr
  forestplot(tableText,mean,low,up,zero=0, 
             is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),clip=c(log(0.1),log(2.5)),xlog=TRUE)
}

CPplot(model.FE)











# http://www.metafor-project.org/doku.php/plots

install.packages("metafor")

library(metafor)


data(dat.bcg)

labs <- paste(dat.bcg$author, dat.bcg$year)

## Replace the first label with a greek letter, a superscript and a subscript
# labs[1] <- expression(beta[123]^123)
labs[1] <- c("Region")

res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR", method="REML")

forest(res, slab = labs)





# Forest Plots in R
getwd()
setwd("/Users/BEmorrow/Desktop/HLA_NEJM/meta_plot")
# Load metafor library
library(metafor)
# Load input data for all the alleles;
data <- read.csv("heart.txt")
# # If OR's and 95% CI's, run these commented out lines
# data$beta <- log(data$OR)
# data$se   <- (log(data$UCL)-log(data$LCL))/(2*1.96)
# Assign values for plotting
labs <- data$Region
yi   <- data$beta
sei  <- data$se
# Combine data into summary estimate
res_B0702  <- rma(yi=yi, sei=sei, method="FE")
summary(res_B0702)

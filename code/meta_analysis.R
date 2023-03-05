rm(list=ls())
# install.packages("remotes")
# install.packages("devtools")
# library('devtools')
# remotes::install_github("wviechtb/metafor")
# devtools::install_github("NightingaleHealth/ggforestplot")
library('dplyr')
library('metafor')
library('meta')
library('ggforestplot')
library('forestplot')
require('gridExtra')
library('effectsize')
library('ggplot2')
library('forcats')
library('stringr')

d<-read.csv('/Users/fabianyii/Desktop/PmProgressionSr/data/data.csv')
# d$study <- factor(d$study)
# 95% CI for age OR in Wong et al. are reported to 1 decimal place and  
# equal to 1; including them will lead to zero variance and in turn division by
# zero when computing inverse-variance weight. Hence, add a small fixed value of ±0.01 
# to either side of the 95% CI
d[which(d$adjusted==1 & d$onset==0 & d$study=='wong' & d$factor=='age'),]$OR_lwr <- 0.99
d[which(d$adjusted==1 & d$onset==0 & d$study=='wong' & d$factor=='age'),]$OR_upr <- 1.01
# convert rows which reported risk ratio to odds ratio 
# Wong et al. (onset)
rows <- which(d$note=='rr' & d$adjusted==1 & d$onset==1 & d$study=='wong') 
d[rows,]$OR <- riskratio_to_oddsratio(d[rows,]$OR, 0.012)
d[rows,]$OR_lwr <- riskratio_to_oddsratio(d[rows,]$OR_lwr, 0.012)
d[rows,]$OR_upr <- riskratio_to_oddsratio(d[rows,]$OR_upr, 0.012)
# Wong et al. (progression)
rows <- which(d$note=='rr' & d$adjusted==1 & d$onset==0 & d$study=='wong') 
d[rows,]$OR <- riskratio_to_oddsratio(d[rows,]$OR, 0.17)
d[rows,]$OR_lwr <- riskratio_to_oddsratio(d[rows,]$OR_lwr, 0.17)
d[rows,]$OR_upr <- riskratio_to_oddsratio(d[rows,]$OR_upr, 0.17)
# Foo et al. (onset)
rows <- which(d$note=='rr' & d$adjusted==1 & d$onset==1 & d$study=='foo') 
d[rows,]$OR <- riskratio_to_oddsratio(d[rows,]$OR, 0.103)
d[rows,]$OR_lwr <- riskratio_to_oddsratio(d[rows,]$OR_lwr, 0.103)
d[rows,]$OR_upr <- riskratio_to_oddsratio(d[rows,]$OR_upr, 0.103)
# Foo et al. (progression)
rows <- which(d$note=='rr' & d$adjusted==1 & d$onset==0 & d$study=='foo') 
d[rows,]$OR <- riskratio_to_oddsratio(d[rows,]$OR, 0.123)
d[rows,]$OR_lwr <- riskratio_to_oddsratio(d[rows,]$OR_lwr, 0.123)
d[rows,]$OR_upr <- riskratio_to_oddsratio(d[rows,]$OR_upr, 0.123)
# # round
# d$OR<-round(d$OR,2); d$OR_lwr<-round(d$OR_lwr,2); d$OR_upr<-round(d$OR_upr,2)

# # odds ratio vs risk ratio by different baseline risks
# or <- c(riskratio_to_oddsratio(rr,0.1), riskratio_to_oddsratio(rr,0.2), 
#   riskratio_to_oddsratio(rr,0.3), riskratio_to_oddsratio(rr,0.4))
# br <- c(rep(0.1, length(rr)), rep(0.2, length(rr)), rep(0.3, length(rr)), rep(0.4, length(rr)))
# plot_d <- data.frame('rr'=rep(rr, 4), 'or'=or, 'br'=br)
# ggplot(plot_d, aes(x=rr, y=or, group=br, colour=factor(br))) +
#   geom_point() +
#   geom_smooth(se=FALSE) +
#   labs(x='Risk ratio', y='Odds ratio', title='Odds ratio vs risk ratio by baseline risk') +
#   xlim(c(0.5,2.5)) + ylim(c(0,7)) +
#   scale_color_discrete(name='Baseline risk')
  
# compute sampling variance, i.e. squared standard error SE (assumed normal distribution)
# SE = (upper CI - lower CI) / 3.92
# sampling variance = (SE)^2
# finally, log transformed sampling variance (saved as 'vi)
# log transfromed OR also computed and saved as 'yi'
d <- conv.wald(out=OR, ci.lb=OR_lwr, ci.ub=OR_upr, data=d, n=n, transf=log )
d$study <- str_to_title(d$study)
d$factor <- toupper(d$factor)

#### Visualise all studies (w/o meta-analysis) with forest plots ####
# PN ONSET
pdf(file='/Users/fabianyii/Desktop/PM_onset_all.pdf')
d_onset <- subset(d, adjusted==1 & onset==1)
d_onset <- d_onset[!is.na(d_onset$OR),]
m_onset <- rma(yi, vi, data=d_onset)
forest(m_onset, order=factor,  cex=0.5, psize=1, atransf=exp,
       at=log(c(0.25, 0.5, 1, 5)),
       xlim=c(-5,3.5), ylim=c(-1,37),
       slab=study, ilab=cbind(n, fu_year, covariates, p), 
       ilab.xpos=c(-4.4,-3.85, -3.5, 1.7),
       ilab.pos=4,
       rows=c(1:4, 6:9, 11, 13, 15, 17:19, 21, 23:24, 26, 28, 30:31, 33),
       header=c('Study', 'OR [95% CI]'),
       xlab='Adjusted Odds Ratio (OR)',
       addfit=FALSE)
### switch to bold font
par(cex=0.5, font=2)
### add additional column headings to the plot
text(c(-4.45,-3.85, -3.1, 1.7), 36, 
     c('N eyes', 'FU', 'Adjusted for', 'P-value'), pos=4)
### add text for the subgroups
text(-5.12, c(4.8, 9.8, 11.8, 13.8, 15.8, 19.8, 21.8, 24.8, 26.8, 28.8, 31.8, 33.8), pos=4, 
     c('Age', 'AL', 'AL change', 'Cataract', 'Chinese', 'Female', 'G/D zones', 'Higher edu', 'Indian', 'Malay', 'SER', 'Tessellation'),
     col='maroon')
### legend for abbreviations
text(-5, -1.5, pos=4, cex=0.95, font=1, col='darkblue',
'Tessellation: tessellated fundus; Malay: Malay vs Indian; Chinese: Chinese vs Indian; Indian: Indian vs Malay; Edu: educational level; FU: follow-up duration (year);
G/D: development/ enlargement of peripapillary Gamma & Delta zones; SER/ AL change: change in SER/ AL b/w baseline and follow-up; MMD: baseline MMD category')
dev.off()

# PN PROGRESSION
pdf(file='/Users/fabianyii/Desktop/PM_prog_all.pdf')
### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.5, font=2)
### set par back to the original settings
par(op)
d_prog <- subset(d, adjusted==1 & onset==0)
d_prog <- d_prog[!is.na(d_prog$OR),]
m_prog <- rma(yi, vi, data=d_prog)
forest(m_prog, order=factor,  cex=0.5, psize=1, atransf=exp,
       at=log(c(0.25, 0.5, 1, 5)),
       xlim=c(-5,3.5), ylim=c(-1,44),
       slab=study, ilab=cbind(n, fu_year, covariates, p), 
       ilab.xpos=c(-4.4,-3.85, -3.5, 1.9),
       ilab.pos=4,
       rows=c(1:5, 7:9, 11, 13, 15:19, 21, 23:25, 27, 29, 31, 33, 35:38, 40),
       header=c('Study', 'OR [95% CI]'),
       xlab='Adjusted Odds Ratio (OR)',
       addfit=FALSE)
### switch to bold font
par(cex=0.5, font=2)
### add additional column headings to the plot
text(c(-4.45,-3.85, -3.1, 1.85), 43, 
     c('N eyes', 'FU', 'Adjusted for', 'P-value'), pos=4)
### add text for the subgroups
text(-5.1, c(5.8, 9.8, 11.8, 13.8, 19.8, 21.8, 25.8, 27.8, 29.8, 31.8, 33.8, 38.8, 40.8), pos=4, col='maroon',
     c('Age', 'AL', 'AL change', 'Chinese', 'Female', 'G/D', 'Higher edu', 'HTN', 'Indian', 'IOP', 'MMD 3/4', 'SER', 'SER change'))
### legend for abbreviations
text(-5, -1.5, pos=4, cex=0.95, font=1, col='darkblue',
     'SER/ AL change: change in SER/ AL b/w baseline and follow-up; MMD 3/4: baseline MMD category 3 or 4 vs category 2; Chinese: Chinese vs Malay + Indian; 
HTN: hypertension; G/D: development/ enlargement of peripapillary Gamma & Delta zones; FU: follow-up duration (year); Edu: educational level')
dev.off()

# owing to a limited number of studies per factor, fixed-effects models
# rather than random-effects ones are used as per the guidance below
# https://journals.sagepub.com/doi/full/10.1177/21925682221110527

##########################################################################################
######################################## PM Onset ########################################
##########################################################################################
###### risk factor 1: AGE ######
age_onset <- subset(d, factor=='AGE' & adjusted==1 & onset==1)
# Short-term: Ueda & Wong
m_short <- rma(yi=yi, vi=vi, data=age_onset[3:4,], measure='OR', method='EE')
summary(m_short)
predict(m_short, transf=exp)
###### risk factor 2: AL ######
AL_onset <- subset(d, factor=='AL' & adjusted==1 & onset==1)
# Short-term: Ueda & Wong
m_short <- rma(yi=yi, vi=vi, data=AL_onset[3:4,], measure='OR', method='EE') # short
summary(m_short)
predict(m_short, transf=exp)
###### risk factor 3: FEMALE ######
female_onset <- subset(d, factor=='FEMALE' & adjusted==1 & onset==1)
m_short <- rma(yi=yi, vi=vi, data=female_onset[3:4,], measure='OR', method='EE') # short
summary(m_short)
predict(m_short, transf=exp)
##############################################################################        
########################### risk factor plots ################################
##############################################################################
data <- subset(d, adjusted==1 & onset==1 & factor!='TESSELATED FUNDUS' & factor!='CHINESE' & factor!='MALAY' & factor!='CATARACT' & factor!='HIGHER EDU' & factor!='SER' & fu_year!=12 & fu_year!=18)
data <- data %>% select(study, OR, OR_upr, OR_lwr, n, p, factor)
# Pooled Age
data[nrow(data)+1,] <- c('Pooled', 1.0819, 1.1173, 1.0476, 5537,'<0.001', 'AGE')
# Pooled AL
data[nrow(data)+1,] <- c('Pooled', 2.2380, 2.7534, 1.8190, 5537,'<0.001', 'AL')
# Pooled Female
data[nrow(data)+1,] <- c('Pooled', 1.1176, 1.9601, 0.6373, 5537,'0.70', 'FEMALE')
# make sure numeric data are coded as such
data[,2:5] <- sapply(data[,2:5], as.numeric)
# 95% CI label
data$CI <- paste0('[', format(round(data$OR_lwr,2), nsmall=2), ' to ', format(round(data$OR_upr,2), nsmall=2), ']')
# Reorder label levels such that pooled labels appear last
label_levels <- c('AGE : Pooled', 'AGE : Wong', 'AGE : Ueda',
                  'AL : Pooled', 'AL : Wong', 'AL : Ueda',
                  'FEMALE : Pooled', 'FEMALE : Wong', 'FEMALE : Ueda')
data$label <- factor(paste(data$factor, ':', data$study), levels=label_levels)
study_levels <- c('Wong','Ueda','Pooled')
data$study <- factor(data$study, levels=study_levels )
data <- data %>% arrange(label)

factor.labs <- c('—————————————————————————', '—————————————————————————', '—————————————————————————')
names(factor.labs) <- c('AGE', 'AL', 'FEMALE')

shapes <- c(10,22,22,10,22,22,10,22,22)
p <- ggplot(data, aes(x=OR, y=label, xmin = OR_lwr, xmax = OR_upr, colour=study, fill=study)) +
  geom_pointrange(shape=shapes, size=0.8) +
  geom_vline(xintercept = 1, linetype = 3) +
  xlab("Adjusted OR (95% CI)") +
  theme_classic() +
  scale_x_log10(limits = c(0.25, 4), 
                breaks = c(0.25, 0.5, 1, 2, 4), 
                labels = c("0.25", "0.5", "1", "2", "4"), expand = c(0,0)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y =element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = margin(30, 0, 20, 0),
        legend.position = 'left',
        legend.title=element_blank(),
        title=element_text(size=10),
        strip.text.x = element_text(size = 12),
        strip.background = element_blank()) +
  guides(color = guide_legend(override.aes=list(shape = c(22,22,10)))) +
  facet_wrap(~factor, ncol=1, scales='free_y', strip.position = 'top') 

data_tab <- ggplot(data, aes(y = label)) +
  xlim(c(0,3)) +
  geom_text(aes(x = 0.05, label = n), hjust = 0) +
  geom_text(aes(x = 1, label = round(OR,2) )) +
  geom_text(aes(x = 2.3, label = CI), hjust = 1) +
  geom_text(aes(x = 2.7, label = p)) +
  scale_colour_identity() +
  theme_void() + 
  labs(subtitle='  N eyes           OR [95% CI]     P-value') +
  theme(plot.margin = margin(20, 0, 40, 0)) +
  facet_wrap(~factor, ncol=1, scales='free_y', labeller=labeller(factor=factor.labs)) 
grid.arrange(p,data_tab, widths = c(2,1.3))

# Save plot
meta_onset <- arrangeGrob(p,data_tab, widths = c(2,1.3))
ggsave(file="/Users/fabianyii/Desktop/meta_onset_meta.pdf", meta_onset)

##########################################################################################
######################################## PM progression ##################################
##########################################################################################
###### prognostic factor 1: AGE ######
age_prog <- subset(d, factor=='AGE' & adjusted==1 & onset==0)
# Short-term: Hopf & Lin
m_short <- rma(yi=yi, vi=vi, data=age_prog[3:5,], measure='OR', method='EE')
summary(m_short)
predict(m_short, transf=exp)
###### prognostic factor 2: FEMALE ######
female_prog <- subset(d, factor=='FEMALE' & adjusted==1 & onset==0)
# Short-term: Hopf, Lin & Wong
m_short <- rma(yi=yi, vi=vi, data=female_prog[3:5,], measure='OR', method='EE') 
summary(m_short)
predict(m_short, transf=exp)
###### prognostic factor 3: SER ######
SER_prog <- subset(d, factor=='SER' & adjusted==1 & onset==0)
# Short-term: Hopf, Lin & Wong
m_short <- rma(yi=yi, vi=vi, data=SER_prog[2:4,], measure='OR', method='EE') 
summary(m_short)
predict(m_short, transf=exp)
###### prognostic factor 4: higher education level ######
edu_prog <- subset(d, factor=='HIGHER EDU' & adjusted==1 & onset==0)
# Short-term: Lin & Wong
m_short <- rma(yi=yi, vi=vi, data=edu_prog[2:3,], measure='OR', method='EE') 
summary(m_short)
predict(m_short, transf=exp)

##############################################################################        
######################## prognostic factor plots #############################
##############################################################################
data <- subset(d, adjusted==1 & onset==0 & factor!='MMD 3 OR 4' & factor!='CHINESE' & factor!='SER CHANGE' & factor!='HTN' & factor!='TESSELATED FUNDUS' & factor!='IOP' & factor!='HYPERTENSION' & factor!='AL' & fu_year!=12 & fu_year!=18)
data <- data %>% select(study, OR, OR_upr, OR_lwr, n, p, factor)
data$p[2] <- '0.13'
# Pooled Age
data[nrow(data)+1,] <- c('Pooled', 0.9967, 1.0084, 0.9851, 373,'0.58', 'AGE')
# Pooled Female
data[nrow(data)+1,] <- c('Pooled', 2.2936, 4.5000, 1.1690, 373,'0.02', 'FEMALE')
# Pooled SER
data[nrow(data)+1,] <- c('Pooled', 0.8726, 0.9172, 0.8302, 373,'<0.001', 'SER')
# Pooled HIGHER EDU
data[nrow(data)+1,] <- c('Pooled', 3.1653, 7.3468, 1.3638, 339,'0.01', 'HIGHER EDU')
# make sure numeric data are coded as such
data[,2:5] <- sapply(data[,2:5], as.numeric)
# 95% CI label
data$CI <- paste0('[', format(round(data$OR_lwr,2), nsmall=2), ' to ', format(round(data$OR_upr,2), nsmall=2), ']')
# Reorder label levels such that pooled labels appear last
label_levels <- c('AGE : Pooled', 'AGE : Hopf','AGE : Lin', 'AGE : Wong',
                  'FEMALE : Pooled', 'FEMALE : Hopf','FEMALE : Lin', 'FEMALE : Wong',
                  'SER : Pooled', 'SER : Hopf','SER : Lin', 'SER : Wong', 
                  'HIGHER EDU : Pooled', 'HIGHER EDU : Lin', 'HIGHER EDU : Wong')
data$label <- factor(paste(data$factor, ':', data$study), levels=label_levels)
study_levels <- c('Hopf', 'Lin', 'Wong', 'Pooled')
data$study <- factor(data$study, levels=study_levels )
data <- data %>% arrange(label)
# Clip upper 95%CI at 10
data[data$OR_upr>10,]$OR_upr <- 10

factor.labs <- c('———————————————————————', '———————————————————————', '———————————————————————', '———————————————————————')
names(factor.labs) <- c('AGE', 'FEMALE', 'SER', 'HIGHER EDU')

shapes <- c(10,22,22,22,10,22,22,22,10,22,22,22,10,22,22)
p <- ggplot(data, aes(x=OR, y=label, xmin = OR_lwr, xmax = OR_upr, colour=study, fill=study)) +
  geom_pointrange(shape=shapes, size=0.6) +
  geom_vline(xintercept = 1, linetype = 3) +
  xlab("Adjusted OR (95% CI)") +
  theme_classic() +
  scale_x_log10(limits = c(0.25, 10), 
                breaks = c(0.25, 0.5, 1, 2, 4, 8), 
                labels = c("0.25", "0.5", "1", "2", "4", "8"), expand = c(0,0)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y =element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = margin(10, 0, 2, 0),
        legend.position = 'left',
        legend.title=element_blank(),
        title=element_text(size=10),
        strip.text.x = element_text(size = 11),
        strip.background = element_blank()) +
  guides(color = guide_legend(override.aes=list(shape = c(22,22,22,10)))) +
  facet_wrap(~factor, ncol=1, scales='free_y', strip.position = 'top') 

data_tab <- ggplot(data, aes(y = label)) +
  xlim(c(0,3)) +
  geom_text(aes(x = 0.1, label = n), hjust = 0, size=4) +
  geom_text(aes(x = 0.8, label = round(OR,2)), size=4) +
  geom_text(aes(x = 2.2, label = CI), hjust = 1, size=4) +
  geom_text(aes(x = 2.7, label = p), size=4) +
  scale_colour_identity() +
  theme_void() + 
  labs(subtitle='     N eyes        OR (95% CI)         P-value') +
  theme(plot.margin = margin(0, 0, 29, 0),
        plot.subtitle=element_text(size=10)) +
  facet_wrap(~factor, ncol=1, scales='free_y', labeller=labeller(factor=factor.labs)) 
grid.arrange(p,data_tab, widths = c(2,1.3))
# Save plot
meta_progression <- arrangeGrob(p,data_tab, widths = c(2,1.3))
ggsave(file="/Users/fabianyii/Desktop/pm_prog_meta.pdf", meta_progression)








### copy BCG vaccine meta-analysis data into 'dat'
dat <- dat.bcg

### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat,
              slab=paste(author, year, sep=", "))

### fit random-effects model
res <- rma(yi, vi, data=dat)

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
forest(res, xlim=c(-16, 4.6), at=log(c(0.05, 0.25, 1, 4)), atransf=exp,
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.75, ylim=c(-1, 27), order=alloc, rows=c(3:4,9:15,20:23),
       mlab=mlabfun("RE Model for All Studies", res),
       psize=1, header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)

### add additional column headings to the plot
text(c(-9.5,-8,-6,-4.5), 26, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))

### switch to bold italic font
par(font=4)

### add text for the subgroups
text(-16, c(24,16,5), pos=4, c("Systematic Allocation",
                               "Random Allocation",
                               "Alternate Allocation"))

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.s <- rma(yi, vi, subset=(alloc=="systematic"), data=dat)
res.r <- rma(yi, vi, subset=(alloc=="random"),     data=dat)
res.a <- rma(yi, vi, subset=(alloc=="alternate"),  data=dat)

### add summary polygons for the three subgroups
addpoly(res.s, row=18.5, mlab=mlabfun("RE Model for Subgroup", res.s))
addpoly(res.r, row= 7.5, mlab=mlabfun("RE Model for Subgroup", res.r))
addpoly(res.a, row= 1.5, mlab=mlabfun("RE Model for Subgroup", res.a))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ alloc, data=dat)

### add text for the test of subgroup differences
text(-16, -1.8, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                              Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                              ", p = ", .(formatC(res$QMp, digits=2, format="f")))))


  

    





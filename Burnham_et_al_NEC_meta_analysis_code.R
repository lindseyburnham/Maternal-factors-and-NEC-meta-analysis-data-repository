install.packages("meta")
library(meta)
library(tidyverse)
##loading in data
#filter function is to filter out articles that did not meet the final 
#inclusion criteria
cs_up<-read.csv("Final OR sheet  - C-section.csv")
cs_up$Weight <- 1/cs_up$se^2
cs_up$Weight <- cs_up$Weight / sum(cs_up$Weight)
cs_up$Weight <- round(100 * (cs_up$Weight), 2)
cs_up<-cs_up |>
  dplyr::filter( Authors!="Son et al., 2016",
         Authors != "Ahle et al., 2018")

sm_up<-read.csv("Final OR sheet  - Smoking.csv")
sm_up$Weight <- 1/sm_up$SE^2
sm_up$Weight <- sm_up$Weight / sum(sm_up$Weight)
sm_up$Weight <- round(100 * (sm_up$Weight), 2)

sm_up<-sm_up|>
  filter(Authors!="Hua et al., 2014",
         Authors!="Isiyama et al., 2015",
         Authors != "Ahle et al., 2018",
         Authors != "Henderson et al., 2007",
         Authors != "Bexelius et al., 2018")

pe_up<-read.csv("Final OR sheet  - Preeclampsia.csv")
pe_up$Weight <- 1/pe_up$se^2
pe_up$Weight <- pe_up$Weight / sum(pe_up$Weight)
pe_up$Weight <- round(100 * (pe_up$Weight), 2)

pe_up<-pe_up|>
  filter(authors!="Duci et al., 2019",
         authors!="Irles et al., 2018",
         authors != "Ahle et al., 2018",
         authors != "Henderson et al., 2007",
         authors != "Kim et al., 2018")

chor_up<-read.csv("Final OR sheet  - Clinical Chorioamnionitis.csv")
chor_up$Weight <- 1/chor_up$se^2
chor_up$Weight <- chor_up$Weight / sum(chor_up$Weight)
chor_up$Weight <- round(100 * (chor_up$Weight), 2)
chor_up<-chor_up|>
  filter(Authors!="Duci et al., 2019",
         Authors!="Kovo et al., 2020",
         Authors!="Venkatesh et al. 2019",
         Authors!="Hua et al., 2014",
         Authors!="Irles et al., 2018",
         Authors!="Hallstrom et al., 2003",
         Authors!="Osmanağaoğlu et al., 2004",
         Authors != "Ahle et al., 2018")

hchor_up<-read.csv("Final OR sheet  - Histological Chorioamnionitis (1).csv")
hchor_up$Weight <- 1/hchor_up$se^2
hchor_up$Weight <- hchor_up$Weight / sum(hchor_up$Weight)
hchor_up$Weight <- round(100 * (hchor_up$Weight), 2)
hchor_up<-hchor_up|>
  filter(Authors!="Duci et al., 2019")

ne_up<-read.csv("Final OR sheet  - No education.csv")
ne_up$Weight <- 1/ne_up$se^2
ne_up$Weight <- ne_up$Weight / sum(ne_up$Weight)
ne_up$Weight <- round(100 * (ne_up$Weight), 2)
ne_up<-ne_up|>
  filter(Authors != "Ahle et al., 2018")

hs_up<-read.csv("Final OR sheet  - Over 12 years education.csv")
hs_up$Weight <- 1/hs_up$se^2
hs_up$Weight <- hs_up$Weight / sum(hs_up$Weight)
hs_up$Weight <- round(100 * (hs_up$Weight), 2)
hs_up<-hs_up|>
  filter(Authors != "Ahle et al., 2018")

dm_up<-read.csv("Final OR sheet  - Diabetes Mellitus.csv")
dm_up$Weight <- 1/dm_up$se^2
dm_up$Weight <- dm_up$Weight / sum(dm_up$Weight)
dm_up$Weight <- round(100 * (dm_up$Weight), 2)

dm_up<-dm_up|>
  filter(Authors != "Henderson et al. 2007",
         Authors != "Ahle et al., 2018")

#making the forest plots
m.gen_cs <- metagen(TE = OR,
                    lower = CI.lower,
                    upper = CI.upper,
                    seTE = se,
                    studlab = Authors,
                    data = cs_up,
                    common = FALSE)
m.gen_cs_fun <- metagen(TE = log(cs_up$OR),
                        lower = log(cs_up$CI.lower),
                        upper = log(cs_up$CI.upper),
                        seTE = se,
                        studlab = Authors,
                        data = cs_up,
                        common = FALSE)

m.gen_pe <- metagen(TE = OR,
                    lower = CI.lower,
                    upper = CI.upper,
                    seTE = se,
                    studlab = authors,
                    data = pe_up,
                    common = FALSE,
                    title = "Preeclampsia and NEC")

m.gen_sm <- metagen(TE = OR,
                    lower = CI.lower,
                    upper = CI.upper,
                    seTE = SE,
                    studlab = Authors,
                    data = sm_up,
                    common = FALSE,
                    title = "Smoking and NEC")
m.gen_sm_fun <- metagen(TE = log(sm_up$OR),
                          lower =  log(sm_up$CI.lower),
                          upper =  log(sm_up$CI.upper),
                          seTE = SE,
                          studlab = Authors,
                          data = sm_up,
                          common = FALSE)

m.gen_chor <- metagen(TE = OR,
                      lower = CI.lower,
                      upper = CI.upper,
                      seTE = se,
                      studlab = Authors,
                      data = chor_up,
                      common = FALSE)

m.gen_chor_fun <- metagen(TE = log(chor_up$OR),
                          lower =  log(chor_up$CI.lower),
                          upper =  log(chor_up$CI.upper),
                          seTE = se,
                          studlab = Authors,
                          data = chor_up,
                          common = FALSE)

m.gen_dm <- metagen(TE = our.OR,
                    lower = CI.lower,
                    upper = CI.upper,
                    seTE = se,
                    studlab = Authors,
                    data = dm_up,
                    common = FALSE)

m.gen_ne <- metagen(TE = our.OR,
                    lower = CI.lower,
                    upper = CI.upper,
                    seTE = se,
                    studlab = Authors,
                    data = ne_up,
                    common = FALSE)

m.gen_hs <- metagen(TE = our.OR,
                    lower = CI.lower,
                    upper = CI.upper,
                    seTE = se,
                    studlab = Authors,
                    data = hs_up,
                    common = FALSE)

m.gen_hchor <- metagen(TE = our.OR,
                       lower = CI.lower,
                       upper = CI.upper,
                       seTE = se,
                       studlab = Authors,
                       data = hchor_up,
                       common = FALSE)
#forest plot output

meta::forest(m.gen_cs,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             prediction = FALSE,
             ref = 1,
             width= 10,
             plotwidth = "8cm",
             col.square =  "red",
             col.inside = "black",
             xlim = c(0, 8))

meta::forest(m.gen_sm ,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             col.square =  "red",
             prediction = FALSE,
             ref = 1,
             width = 10,
             plotwidth = "8cm",
             col.inside = "black",
             xlim = c(0, 8))

meta::forest(m.gen_chor ,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             prediction = FALSE,
             ref = 1,
             width= 10,
             plotwidth = "8cm",
             col.square = "red",
             col.inside = "black",
             xlim = c(0, 8))

meta::forest(m.gen_dm ,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             prediction = FALSE,
             ref = 1,
             width= 10,
             plotwidth = "8cm",
             col.square = "red",
             col.inside = "black",
             xlim = c(0, 8))

meta::forest(m.gen_ne ,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             prediction = FALSE,
             ref = 1,
             width= 10,
             plotwidth = "8cm",
             col.square = "red",
             col.inside = "black",
             xlim = c(0, 8))

meta::forest(m.gen_hs ,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             prediction = FALSE,
             ref = 1,
             width= 10,
             plotwidth = "8cm",
             col.square = "red",
             col.inside = "black",
             xlim = c(0, 8))

meta::forest(m.gen_pe ,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             prediction = FALSE,
             ref = 1,
             leftwidth = "10cm",
             plotwidth = "8cm",
             col.square = "red",
             col.inside = "black",
             xlim = c(0, 8))

meta::forest(m.gen_hchor ,
             layout = "JAMA",
             leftcols = c("studlab",  "effect", "ci", "w.random"),
             prediction = FALSE,
             ref = 1,
             width= 10,
             plotwidth = "8cm",
             col.square = "red",
             col.inside = "black",
             xlim = c(0, 8))

#small study effects and funnel plots
meta::metabias(m.gen_cs_fun)
meta::metabias(m.gen_chor_fun)
col.contour = c("gray75", "gray85", "gray95")
meta::funnel(m.gen_cs_fun, 
             xlim = c(-4,5),
             xlab = "Log(Odds Ratio)",
             contour = c(0.9, 0.95, 0.99),
             col.contour = col.contour)

legend(x = 2.8, y = 0.15, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour,
       cex = 3/4)

meta::funnel(m.gen_sm_fun, 
             xlim = c(-4,5),
             xlab = "Log(Odds Ratio)",
             contour = c(0.9, 0.95, 0.99),
             col.contour = col.contour)

legend(x = 2.8, y = 0.15, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour,
       cex = 3/4)
meta::funnel(m.gen_chor_fun,
             xlim=c(-4,5),
             xlab = "Log(Odds Ratio)",
             contour = c(0.9, 0.95, 0.99),
             col.contour = col.contour)
legend(x = 3, y = 0.15, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour,
       cex = 3/4)

#ROB plot
#can find the function in the data repository
library(robvis)
rob_final<-read.csv("Risk of Bias assessment - Sheet1.csv")
test3<-Rob_summary3(rob_final,
                    tool = "ROB2", 
                    colour = "colourblind",
                    overall = FALSE,
                    weighted = FALSE)

#####Sensitivity Analysis 
#sensitivity according to bookdown: 
library(dmetar)

meta_cs <- metagen(TE=OR,
                seTE=se, 
                data = cs_up, 
                studlab = cs_up$Authors,
                method.tau = "SJ",
                common = FALSE)
meta_sm <- metagen(TE=OR,
                   seTE=SE, 
                   data = sm_up, 
                   studlab = sm_up$Authors,
                   method.tau = "SJ",
                   common = FALSE)
meta_dm <- metagen(TE=our.OR,
                   seTE=se, 
                   data = dm_up, 
                   studlab = dm_up$Authors,
                   method.tau = "SJ",
                   common = FALSE)
meta_chor <- metagen(TE=OR,
                   seTE=se, 
                   data = chor_up, 
                   studlab = chor_up$Authors,
                   method.tau = "SJ",
                   common = FALSE)
meta_pe <- metagen(TE=OR,
                   seTE=se, 
                   data = pe_up, 
                   studlab = pe_up$authors,
                   method.tau = "SJ",
                   common = FALSE)
meta_hs <- metagen(TE=our.OR,
                   seTE=se, 
                   data = hs_up, 
                   studlab = hs_up$Author,
                   method.tau = "SJ",
                   common = FALSE)
meta_ne <- metagen(TE=our.OR,
                   seTE=se, 
                   data = ne_up, 
                   studlab = ne_up$Author,
                   method.tau = "SJ",
                   common = FALSE)
meta_hchor <- metagen(TE=our.OR,
                      seTE=se, 
                      data = hchor_up, 
                      studlab = hchor_up$Author,
                      method.tau = "SJ",
                      common = FALSE)
#HK CIs
meta_sm_hk <- metagen(TE=OR,
                   seTE=SE, 
                   data = sm_up, 
                   studlab = sm_up$Authors,
                   method.tau = "SJ",
                   method.random.ci = "HK",
                   common = FALSE)
meta_pe_hk <- metagen(TE=OR,
                   seTE=se, 
                   data = pe_up, 
                   studlab = pe_up$authors,
                   method.tau = "SJ",
                   method.random.ci = "HK",
                   common = FALSE)


find.outliers(meta_cs)
find.outliers(meta_sm)
find.outliers(meta_dm)
find.outliers(meta_chor)
find.outliers(meta_pe)
find.outliers(meta_hs)
find.outliers(meta_ne)

sens_cs<-InfluenceAnalysis(meta_cs, random = TRUE)
sens_sm<-InfluenceAnalysis(meta_sm, random = TRUE)
sens_sm_hk<-InfluenceAnalysis(meta_sm_hk, random = TRUE)
sens_chor<-InfluenceAnalysis(meta_chor, random = TRUE)
sens_dm<-InfluenceAnalysis(meta_dm, random = TRUE)
sens_pe<-InfluenceAnalysis(meta_pe, random = TRUE)
sens_pe_hk<-InfluenceAnalysis(meta_pe_hk, random = TRUE)
sens_hs<-InfluenceAnalysis(meta_hs, random = TRUE)
sens_ne<-InfluenceAnalysis(meta_ne, random = TRUE)
sens_hchor<-InfluenceAnalysis(meta_hchor, random = TRUE)

plot(sens_cs)
plot(sens_sm)
plot(sens_sm_hk)
plot(sens_chor)
plot(sens_dm)
plot(sens_pe)
plot(sens_pe_hk)
plot(sens_hs)
plot(sens_ne)
plot(sens_hchor)



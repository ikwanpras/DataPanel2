library(readxl)
library(plm)
library(kableExtra)


datapanel <- data_uji_regresi_panel
View(data_uji_regresi_panel)

common=plm(Miskin~Inflasi+PDRB, data = datapanel, model = "pooling")
fixed=plm(Miskin~Inflasi+PDRB, data = datapanel,model = "within" )
pooltest(common,fixed)

fixed=plm(Miskin~Inflasi+PDRB,data = datapanel, model = "within",index = c("Kabupaten", "Tahun"))
random=plm(Miskin~Inflasi+PDRB,data = datapanel, model = "random",index = c("Kabupaten", "Tahun"))
phtest(fixed,random)

gr=plm(Miskin~INflasi+PDRB,data = datapanel,model = "random")
plmtest(gr,effects="twoways",type="bp")

m1=plm(Miskin~Inflasi+PDRB,data = datapanel, model = "random",effect = "individual",index = c("Kabupaten", "Tahun"))
summary(m1)

ranef(m1)

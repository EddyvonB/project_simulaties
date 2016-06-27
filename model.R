##Eindodpracht
##laden van packages
library(ggplot2)
library(dplyr)

##defineren van aannames ##########################
#minimumloon met vakantiegeld maakt het basisinkomen
min_loon <- 1524
vak_geld <- 8
bas_ink <- min_loon *(1+vak_geld/100)

##de totale beroepsbevolking
wer_bev <- 8900000

##het aantal 18+ers
werk_bev <- 16900000 * 0.7971

##aantal arbeidsongeschikte
arb_ong <- 815000

##huidige kosten sociaal stelsel
leen_uit <- 140000000

##aantal basis banen
aan_bas_baan <- werk_bev -arb_ong - wer_bev

###functie definitie ########################
##directe kosten voor iemand van de basisinkomen
dir_kost <- function(aantal_volwassen, basisinkomen){
  aantal_volwassen * basisinkomen
}

##Adminiestratiekosten voor zowel basisinkomen als basisbaan
adm_kost <- function(mu, sigma){
  rnorm(1, mu, sigma)
}

##De non participatiefactor dit is voor basisinkomen
non_part <- function(a, b){
  runif(1, a, b)
}

##Laag inkomen arbeidsproductiviteit bij basisinkomen
arb_prod <- function(mu, sigma){
  rnorm(1, mu, sigma)
}

##rowling waarde voor basisinkomen
rowling_waarde <- function(arbeids_ongeschikte){
  rbinom(1, arbeids_ongeschikte, 1/10^7) *10^9
}

##toe of afname van arbeid door introductie basisinkomen
basis_inkomen_door_bas <- function(){
  -1 * (werk_bev - wer_bev - arb_ong) * non_part(-0.05, 0.10) * 40 * 50 * arb_prod(10,1)
}

##De toegevoegde waarde van de arbeid per uur bij een basisbaan
toe_waarde <- function(a, b){
  runif(1, a, b)
}

##Het kosten-batenmodel per vervulde basisbaan
bas_baan_kost_baten <- function(){
  bas_ink + adm_kost(5000,1500) + 40*50*toe_waarde(0,9)
}

##kosten-batenmodel per arbeidsongeschikte bij basisbaan
arb_ong_kost_baten <-   function(){
  bas_ink + adm_kost(500,150)
}

##De modellen

##het totaalplaatje van basisinkomen
totaal_plaatje_ink <- function(){
  dir_kost(werk_bev, bas_ink) + adm_kost(250,75) + basis_inkomen_door_bas() + rowling_waarde(arb_ong)
}
##het totaalplaatje vanbasisbaan

totaal_plaatje_baan <- function(){
  aan_bas_baan * bas_baan_kost_baten() + arb_ong * arb_ong_kost_baten()
}

##monte carlo simulatie

df <- data.frame(meetnummer = integer(),
                 kosten     = double())

for(i in 1:100000){
  df[i,1] <- i
  df[i,2] <- totaal_plaatje_ink()
}

rm(i)
for(i in 100001:200000){
  df[i,1] <- i
  df[i,2] <- totaal_plaatje_baan()
}

df <- df %>%
  mutate(type = ifelse(meetnummer < 100001, "basisinkomen", "basisbaan"))

write.csv(df, file = "data.csv")



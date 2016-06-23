##opdracht 11a

a <- c(3,4)
b <- c(8,0)

#Om een driehoek te maken zodat we makkelijk hoek A kunne berekenen voeg ik punt c toe. Punt c maakt een rechthoekige driehoek en ligt dus op punt (0,3)

c <- c(3,0)

## grootte van hoek a kan je op verschillende manieren berekenen ik kies voor tan(lengte(a-c)/lengte(b-c))

ans <- atan(sqrt((a[1] - c[1])^2 + (a[2] - c[2])^2) / sqrt((b[1] - c[1])^2 + (b[2] - c[2])^2))

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

##opdracht 11b
snelheid <- 2
tijd <- 0.5
afstand <- tijd*snelheid

len_a_b <- sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)

#We maken punt d het te bepalen punt en punt e het punt op de x-as die de driehoek compleet maakt.
#calc punt e want dan weet je de x coordinaat
e <- c(8-cos(ans) * (len_a_b-afstand),0)

#calc lengte d_e want dan heb je bij de y coordinaat
len_d_e <- sin(ans)*(len_a_b-afstand)

d <- c(e[1], len_d_e)

##opdracht 11c
## zie bijgesloten xlsm

## Na 5 seconde kan de vos de haas pakken. 
## Wanneer je vanuit gaat dat vos niet harder rent moet de haas 11.1m/s rennen om eerder bij het hol te zijn dan de vos.
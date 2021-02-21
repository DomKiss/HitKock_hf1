library(dplyr)

# Egy M nevu 10*10-es mátrixot készít, amelynek minden eleme 0 és 1 közötti véletlen szám
M <- matrix(runif(100, min = 0, max = 1), nrow = 10, ncol = 10)
View(M)

#Mekkora az M mátrix sorainak összege?
matrix_sum <- apply(M, 2, sum)
matrix_sum

# Egy K nevu 10*10-es mátrixot készít, amelynek minden eleme egész szám, a bal fels® eleme 1, majd jobbra
#haladva 1-el növekszik, a sor végén pedig új sorban folytatódik.
K <- matrix(seq(100),
            byrow = TRUE,
            nrow = 10,
            ncol = 10)
View(K)

#K matrix negyzete
K2 <- K %*% K

#Készítsen egy N mátrixot, amely 1 értéket vesz fel ott, ahol K2
#eleme páros, és nullát egyébként.
paros <- function(szam) {
  ifelse(szam %% 2 == 0, 1, 0)
}
N <- apply(K2, 2, paros)

#Készítsen egy olyan ciklust, amely a Fibonacci-számokat írja ki
#tetszőleges egész n-től k-ig!
fibonacci_tetszoleges <- function(Fib_start=0, Fib_end=89) {
  Fib <- numeric(10)
  Fib[1] <- 0
  Fib[2] <- 1
  Fib[3] <- 1
  
  for (i in 4:Fib_end) {
    Fib[i] <- Fib[i - 2] + Fib[i - 1]
  }
  
  start_index <- match(c(Fib_start, Fib_end), Fib)[1]
  end_index <- match(c(Fib_start, Fib_end), Fib)[2]
  ifelse((is.na(match(
         c(Fib_start, Fib_end), Fib
     )) %>% sum) > 0, return("Hiba:Legalább az egyik megadott szám nem része a Fibonacci sorozatnak. 'Fib_start'-nak és 'Fib_end'-nek is fibonacci számnak kell lennie."),
     return(Fib[start_index:end_index])) }


fibonacci_tetszoleges(2, 55)


#
# Defniáljon egy olyan data.frame-t, amely a következ® alakot ölti a csapat adataival:
#   A keresztnév legyen karakter változó, a születés év egy évszám, a magasságot cm-ben kell megadni. A nem
# (férfi, nő) faktor, az elkötelezettség 0 és 100 közötti egész legyen (a 100-as érték a csapat és a csapattagok
#                                                                      felé való maximális elkötelez®dést jelöli).

csapat_df <- data.frame(
  id = c(1, 2, 3, 4),
  keresztnev = as.character(c("Dávid", "Dávid", "Bence", "Domonkos")),
  szuletesiev=c(1998,1997,1996,1996),
  magassag=c(177,173,180,173),
  nem=as.factor(c("férfi","férfi","férfi","férfi")),
  elkotelezettseg=c(100,100,100,100), stringsAsFactors=FALSE
) 

#listázza a csapattagoknak nem ismétl®d® keresztneveit
csapat_df$keresztnev %>% unique

#kiszámolja, mekkora a férj csapattagok átlagos magassága
csapat_df %>% filter(nem=="férfi") %>% summarise(mean(magassag))

#meghatározza, mekkora a n®i csapattagok átlagos életkora (idén 2019 van)
csapat_df %>% filter(nem=="nő") %>% mutate(eletkor=2019-szuletesiev) %>% summarise(mean(eletkor))

#felsorolja, milyen a 180 cm alatti csapattagok eloszlása nemek szerint (fér, n®).
csapat_df %>% filter(magassag<180) %>% select(nem, magassag) %>% group_by(nem, magassag) %>%  count(magassag)




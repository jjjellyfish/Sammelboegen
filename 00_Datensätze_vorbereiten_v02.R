### Wrangling & Plausibilisierung ###

library(tidyverse)
library(lubridate)
library(hms)

library(psych)


setwd('O:/U4951/Consulimus_Abschlussdatenlieferung/03_Sammelbögen')


### Datensatz
dat <- read_delim('Sammelboegen_20220107.csv', delim=';', locale=locale(encoding="latin1"))
# Spezifikation der coltypes hängt einfach hinten zwei Nullen an, cols müssen 
# also erst bearbeitet und dann zum Zeitobjekt umgewandelt werden

str(dat)


#### 1. Minutenformate ####
# Ziel: Zeitangaben müssen in Minuten umgerechnet werden
## Schritt 1: all Zeitangaben sind im Format hms 00:00:00

## Schritt 1a: alle als hms eingelesenen müssen geändert werden
tmp <- dat %>% select(where(is.hms)) 


## Schritt 1b: all nicht als hms eingelsenen müssen geändert werden
# alle Minutenformate identifizieren, die nicht korrekt geparst werden

# ... in der neuen Datenlieferung scheinbar korrekt

## Schritt 2: Minutenformate ändern
# Funktion zum Ändern:
get.minute <- function(x, na.rm=TRUE) {
  round(lubridate::minute(x)+(lubridate::second(x)/60),2)
}

dat <- dat %>% mutate_if(is_hms, get.minute, na.rm=TRUE)


# Variablennamen mit Regex ändern
names <- names(dat)
names <- names %>% str_replace('(\\..+)(\\..+)(\\..+)', '\\1t\\3')
names(dat) <- names
names(dat)


#### 2) Datensätze bauen ####
#### U2 ####
u2 <- select(dat, id, contains('U2') & contains('K'))
str(u2)
# u2$U2.9.K2 <- as.numeric(u2$U2.9.K2)
# u2$U2.16.K5 <- as.numeric(u2$U2.16.K5)

u2 <- tidyr::pivot_longer(u2,  cols=starts_with('U2'), names_to=c('U','item','kind'),
                          names_sep='\\.')

u2 <- u2 %>% group_by(id, kind) %>% mutate(item=paste('F', seq_along(value), sep=''))
u2 <- u2 %>% pivot_wider(names_from=item, values_from=value)
u2 <- u2[rowSums(u2[,4:length(u2)])>0, ]
#u2 <- u2[rowSums(is.na(u2)) != ncol(u2), ]
# Notiz: in U2 bis U4 enthalten keine Textfelder , da es sich nur um 
# Bemerkungen handelt und diese  keinem Kind zuzuordnen sind, sondern nur den Ärzt*innen

# remove all-NA rows
u2 <- u2[rowSums(is.na(u2)) != ncol(u2), ]


#### U3 ####
u3 <- select(dat, id, contains('U3') & contains('K'))
str(u3)

u3 <- tidyr::pivot_longer(u3,  cols=starts_with('U3'), names_to=c('U','item','kind'),
                          names_sep='\\.')

u3 <- u3 %>% group_by(id, kind) %>% mutate(item=paste('F', seq_along(value), sep=''))
u3 <- u3 %>% pivot_wider(names_from=item, values_from=value)
u3 <- u3[rowSums(u3[,4:length(u3)])>0, ]
#u3 <- u3[rowSums(is.na(u3)) != ncol(u3), ]

# remove all-NA rows
u3 <- u3[rowSums(is.na(u3)) != ncol(u3), ]



#### U4 ####
u4 <- dat %>% select(id, contains('U4') & contains('K')) %>% 
  mutate(across(everything(), as.character))

u4 <- tidyr::pivot_longer(u4,  cols=starts_with('U'), names_to=c('U','item','kind'),
                          names_sep='\\.')

u4 <- u4 %>% group_by(id, kind) %>% mutate(item=paste('F', item, sep='')) 
u4$item <- rep_len(u4$item[1:24], length(u4$item)) # manuell Zahlen für Länge des Bogens eintragen (letztes item +1)

u4 <- u4 %>% pivot_wider(names_from=item, values_from=value)
u4 <- u4 %>% mutate_at(vars(-id, -U, -kind, -contains('t')), as.numeric)

u4 <- u4[rowSums(u4[sapply(u4, is.numeric)])>0, ]
u4 <- u4[rowSums(is.na(u4)) != ncol(u4), ]


#### U5 ####
u5 <- dat %>% select(id, contains('U5') & contains('K')) %>% 
  mutate(across(everything(), as.character))

u5 <- tidyr::pivot_longer(u5,  cols=starts_with('U'), names_to=c('U','item','kind'),
                          names_sep='\\.')


u5 <- u5 %>% group_by(id, kind) %>% mutate(item=paste('F', item, sep='')) 
u5$item <- rep_len(u5$item[1:22], length(u5$item)) # manuell Zahlen für Länge des Bogens eintragen (letztes item +1)

u5 <- u5 %>% pivot_wider(names_from=item, values_from=value)
u5 <- u5 %>% mutate_at(vars(-id, -U, -kind, -contains('t')), as.numeric)

u5 <- u5[rowSums(u5[sapply(u5, is.numeric)])>0, ]
u5 <- u5[rowSums(is.na(u5)) != ncol(u5), ]



#### U6 ####
u6 <- dat %>% select(id, contains('U6') & contains('K')) %>% 
  mutate(across(everything(), as.character))

u6 <- tidyr::pivot_longer(u6,  cols=starts_with('U'), names_to=c('U','item','kind'),
                          names_sep='\\.')

u6 <- u6 %>% group_by(id, kind) %>% mutate(item=paste('F', item, sep='')) 
u6$item <- rep_len(u6$item[1:24], length(u6$item)) # manuell Zahlen für Länge des Bogens eintragen (letztes item +1)

u6 <- u6 %>% pivot_wider(names_from=item, values_from=value)
u6 <- u6 %>% mutate_at(vars(-id, -U, -kind, -contains('t')), as.numeric)

u6 <- u6[rowSums(u6[sapply(u6, is.numeric)])>0, ]
u6 <- u6[rowSums(is.na(u6)) != ncol(u6), ]


#### U7 ####
u7 <- dat %>% select(id, contains('U7') & contains('K') & -contains('U7a')) %>% 
  mutate(across(everything(), as.character))

u7 <- tidyr::pivot_longer(u7,  cols=starts_with('U'), names_to=c('U','item','kind'),
                          names_sep='\\.')

u7 <- u7 %>% group_by(id, kind) %>% mutate(item=paste('F', item, sep='')) 
u7$item <- rep_len(u7$item[1:22], length(u7$item)) # manuell Zahlen für Länge des Bogens eintragen (letztes item +1)

u7 <- u7 %>% pivot_wider(names_from=item, values_from=value)
u7 <- u7 %>% mutate_at(vars(-id, -U, -kind, -contains('t')), as.numeric)

u7 <- u7[rowSums(u7[sapply(u7, is.numeric)])>0, ]
u7 <- u7[rowSums(is.na(u7)) != ncol(u7), ]


#### U7a ####
u7a <- dat %>% select(id, contains('U7a') & contains('K')) %>% 
  mutate(across(everything(), as.character))

u7a <- tidyr::pivot_longer(u7a,  cols=starts_with('U'), names_to=c('U','item','kind'),
                          names_sep='\\.')

u7a <- u7a %>% group_by(id, kind) %>% mutate(item=paste('F', item, sep='')) 
u7a$item <- rep_len(u7a$item[1:31], length(u7a$item)) # manuell Zahlen für Länge des Bogens eintragen (letztes item +1)

u7a <- u7a %>% pivot_wider(names_from=item, values_from=value)
u7a <- u7a %>% mutate_at(vars(-id, -U, -kind, -contains('t')), as.numeric)

u7a <- u7a[rowSums(u7a[sapply(u7a, is.numeric)])>0, ]
u7a <- u7a[rowSums(is.na(u7a)) != ncol(u7a), ]


#### U8 ####
u8 <- dat %>% select(id, contains('U8') & contains('K')) %>% 
  mutate(across(everything(), as.character))
u8 <- tidyr::pivot_longer(u8,  cols=starts_with('U'), names_to=c('U','item','kind'),
                          names_sep='\\.')
u8 <- u8 %>% group_by(id, kind) %>% mutate(item=paste('F', item, sep='')) 
u8$item <- rep_len(u8$item[1:43], length(u8$item)) # manuell Zahlen für Länge des Bogens eintragen
u8 <- u8 %>% pivot_wider(names_from=item, values_from=value)

# as.numeric () introduces NAs in F3 & F24, es sind also irgendwo Fehler --> 
table(u8$F3)
u8$F3[u8$F3=='2á17'] <- '2017'

table(u8$F24)
u8$F24[u8$F24==','] <- '0'

u8 <- u8 %>% mutate_at(vars(-id, -U, -kind, -contains('t')), as.numeric)
#u8 <- u8 %>% ungroup() %>%  mutate_if(is.numeric, rowsum)
u8 <- u8[rowSums(u8[sapply(u8, is.numeric)])>0, ]
u8 <- u8[rowSums(is.na(u8)) != ncol(u8), ]


#### U9 ####
u9 <- dat %>% select(id, contains('U9') & contains('K')) %>% 
  mutate(across(everything(), as.character))
u9 <- tidyr::pivot_longer(u9,  cols=starts_with('U'), names_to=c('U','item','kind'),
                          names_sep='\\.')

u9 <- u9 %>% group_by(id, kind) %>% mutate(item=paste('F', item, sep='')) 
u9$item <- rep_len(u9$item[1:31], length(u9$item)) # manuell Zahlen für Länge des Bogens eintragen

u9 <- u9 %>% pivot_wider(names_from=item, values_from=value)

u9 <- u9 %>% mutate_at(vars(-id, -U, -kind, -contains('t')), as.numeric)
#u9 <- u9 %>% ungroup() %>%  mutate_if(is.numeric, rowsum)
u9 <- u9[rowSums(u9[sapply(u9, is.numeric)])>0, ]
u9 <- u9[rowSums(is.na(u9)) != ncol(u9), ]



#*******************************************************************************
#### speichern ####
setwd('O:/U4951/Consulimus_Abschlussdatenlieferung/03_Sammelbögen/01_Data')

write.csv(u2, 'u2.csv')
write.csv(u3, 'u3.csv')
write.csv(u4, 'u4.csv')
write.csv(u5, 'u5.csv')
write.csv(u6, 'u6.csv')
write.csv(u7, 'u7.csv')
write.csv(u7a, 'u7a.csv')
write.csv(u8, 'u8.csv')
write.csv(u9, 'u9.csv')


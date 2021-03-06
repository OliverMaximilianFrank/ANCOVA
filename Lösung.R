# Geschrieben von Oliver Frank


# Ben�tigte libraries

# install.packages("ggplot2")
library(ggplot2)
# install.packages("car")
library(car)
# install.packages("tidyverse")
library(tidyverse) 
# install.packages("rstatix")
library(rstatix)
# install.packages("emmeans")
library(emmeans)
# install.packages("pwr")
library(pwr)


# Get Working Directory
getwd()
setwd('C:/YourDirectory')
# Ersten Zeilen anschauen 
readLines('C:/YourDirectory/reaction.dat.txt', n=10)
# File importieren und als data frame abspeichern 
df <- read.table('reaction.dat.txt')

# a)
# zentraler Pr�diktor: 'routine' -> kategorial 

# Kontrollvariable: 'age' -> metrisch -> Kovariate 

# Kriterium: 'time' -> metrisch


# Ich m�chte aus theoretischen �berlegungen eine ANCOVA durchf�hren



# Voraussetzungen f�r ANCOVA:

# metrisches Kriterium

# Varianzhomogenit�t der AV in den Gruppen (unterschieden nach routine) -> Levene-Test

# Normalverteilung der AV �ber die Gruppen -> QQ Diagramm



# Voruntersuchungen zur Entscheidungsfindung:

# Zusammenhang Alter und Reaktionszeit 
plot(x=df$age,                            
     y=df$time) 
# leichter lin. Zusammenhang zu erkennen --> hohes Alter f�hrt zu h�herer Reaktionszeit 

# Zusammenhang Fahrpraxis und Reaktionszeit 
plot(x=df$routine,
     y= df$time)




# ANCOVA: Kriterium -> Reaktionszeit  zentr. Pr�diktor -> Fahrpraxis  Kontrollvariable -> Alter

# HYPOTHESEN:

# H0: Fahrer:innen mit niedriger Fahrpraxis (routine -> Gruppe 1) ben�tigen keine 
# signifikant l�ngere Zeit zum Einleiten des Bremsvorganges (time) als Fahrer:innen 
# mit mittlerer oder hoher Fahrpraxis (routine -> Gruppe 2, Gruppe 3) bei Kontrolle 
# des Einflusses des Fahrer:innenalters (age).

# H1: Fahrer:innen mit niedriger Fahrpraxis (routine -> Gruppe 1) ben�tigen eine 
# signifikant l�ngere Zeit zum Einleiten des Bremsvorganges (time) als Fahrer:innen 
# mit mittlerer oder hoher Fahrpraxis (routine -> Gruppe 2, Gruppe 3) bei Kontrolle 
# des Einflusses des Fahrer:innenalters (age).


# Ich gehe davon aus, dass das Alter einen erheblichen Einfluss auf die Reaktionszeit hat,
# deshalb m�chten wir sie kontrollieren, um eine kleinere Fehlervarianz zu erhalten.
# Reaktionszeit (time) wird von Alter(age bereinigt)


# b) 

# Zusammenhang der einzelnen Pr�diktoren auf Reaktionszeit 
# Metrischer Pr�diktor: Alter 

y <- df$time
x1 <- df$age

# Regressionsanalyse

reg1 <- lm(y~x1)
summary(reg1)

# Plotten mit Regressionsgerade 

plot(x=df$age,                            
     y=df$time,
     main="Zsh. Reaktionszeit und Alter", 
     xlab = "Alter", 
     ylab = "Reaktionszeit") 

a_1 <- reg1$coefficients[1] 
b_1 <- reg1$coefficients[2]  

abline(a=a_1, b=b_1, col ="blue")

# Beschreibung: Es ist ein positiv linearer Zusammenhang zwischen den beiden 
# Variablen zu erkennen, jedoch liegt eine hohe Varianz vor. Wir k�nnen also nur 
# mit gro�er Unsicherheit davon ausgehen, dass die Reaktionszeit mit steiegndem 
# Alter zunimmt.


# Kategorialer Pr�diktor: Fahrpraxis

y <- df$time
x2 <- df$routine 

# Regressionsanalyse

reg2 <- lm(y~x2)
summary(reg)

# Plotten mit Regressionsgerade 

plot(x=df$routine,                            
     y=df$time,
     main="Zsh. Reaktionszeit und Fahrpraxis", 
     xlab = "Fahrpraxis", 
     ylab = "Reaktionszeit") 

a_2 <- reg2$coefficients[1] 
b_2 <- reg2$coefficients[2]  

abline(a=a_2, b=b_2, col ="red")


# Boxplot f�r kategoriale Variablen 

df$routine <- as.factor(df$routine)
ggplot(df, aes(x=routine, y=time)) + geom_boxplot()

# Beschreibung: Die Betrachtung der beiden Plots zeigt, dass die Reaktionszeit in
# der zweiten Gruppe in unserer Stichprobe im Mittel am h�chsten ist, sie besitzt
# jedoch auch drei Ausrei�er. Die Reaktionszeit der beiden anderen Gruppen ist niedriger, 
# die der Dritten am niedrigsten. 


# Beide Pr�diktoren in einem Plot: 

plot(x=df$age,                            
     y=df$time, 
     main="Zsh. Alter und Reaktionszeit aufgeteilt nach Gruppenzugeh�rigkeit", 
     xlab = "Alter", 
     ylab = "Reaktionszeit") 


# routine = 1

points(x=df$age[df$routine==1],                            
       y=df$time[df$routine==1],
       col = "blue",
       pch = 3)


# routine = 2

points(x=df$age[df$routine==2],                            
       y=df$time[df$routine==2],
       col = "green",
       pch = 3)


# routine = 3

points(x=df$age[df$routine==3],                            
       y=df$time[df$routine==3],
       col = "red",
       pch = 3)


# Regressionsanalyse f�r Fahrpraxis-Untergruppen:

reg3 <- lm(y[df$routine==1]~df$age[df$routine==1])
summary(reg3)
a_3 <- reg3$coefficients[1] 
b_3 <- reg3$coefficients[2] 
abline(a=a_3, b=b_3, col ="blue")

reg4 <- lm(y[df$routine==2]~df$age[df$routine==2])
summary(reg4)
a_4 <- reg4$coefficients[1] 
b_4 <- reg4$coefficients[2] 
abline(a=a_4, b=b_4, col ="green")

reg5 <- lm(y[df$routine==3]~df$age[df$routine==3])
summary(reg5)
a_5 <- reg5$coefficients[1] 
b_5 <- reg5$coefficients[2] 
abline(a=a_5, b=b_5, col ="red")

# Legende
legend("topleft", 
       legend = c("< 5 Jahre", "6-10 Jahre", "> 10 Jahre"), 
       pch = 3,
       col = c("blue", "green", "red"))


# Beschreibung: Analysiert man den Zusammenhang der beiden metrischen Variablen 
# aufgeteilt auf ihre Zugeh�rigkeit der kategorialen Variable Fahrpraxis, sieht man 
# das der vorherige globale positive Zusammenhang in allen Gruppen �hnlich ist, 
# mit der Besonderheit, dass Gruppe 3 einen deutlich niedrigeren Intercept besitzt und
# somit eine insgesamt niedrigere Reaktionszeit zeigt. Au�erdem sind die Regressions-
# geraden nahezu parallel zueinander, man kann also davon ausgehen, dass keine Interaktion
# zwischen den beiden Pr�diktoren besteht. 

# Statistisch ist das folgenderma�en zu beweisen: 
df %>% anova_test(time ~ routine*age)
# routine:age -> F = 0.02, p = .98 -> nicht sig.


# Weitere Voraussetzungen pr�fen 

# VARIANZHOMOGENIT�T der AV in den Gruppen (unterschieden nach routine) -> Levene-Test

df$routine <- as.factor(df$routine)
leveneTest(df$time,df$routine)
# -> p = 0.94 -> H0 kann beibehalten werden -> Varianzhomogenit�t liegt vor


# NORMALVERTEILUNG der AV �ber die Gruppen -> QQ Diagramm

# Erstes Gruppe: QQ Diagramm 
qqnorm(df$time[df$routine==1])
qqline(df$time[df$routine==1])
# -> Best�tigung der NV

# Zweite Gruppe: QQ Diagramm 
qqnorm(df$time[df$routine==2])
qqline(df$time[df$routine==2])
# -> Leichte Abweichungen der NV

# Dritte Gruppe: QQ Diagramm 
qqnorm(df$time[df$routine==3])
qqline(df$time[df$routine==3])
# -> Best�tigung der NV


# An dieser Stelle w�re ein Resampling-Verfahren sinnvoll, um noch gr��ere und noch 
# homogenere Stichproben in den Gruppen zu erlangen. Da jedoch die Voraussetzungen 
# weitesgehend erf�llt sind und dies auch in der Aufgabenstellung nicht diskutiert 
# wird, habe ich mich dagegen entschieden und verwende die Daten so wie sie uns
# vorliegen. 
# Es ist au�erdem anzumerken, dass in den QQ Plots zu erkennen ist, dass es in allen
# drei Gruppen keine gr��eren Ausrei�er gibt, was in der Literatur unter den anderen
# genannten Aspekten ebenfalls als Voraussetzung f�r eine ANCOVA diskutiert wird. 

# Abschlie�end ist zu sagen, dass alle Annahmen f�r eine ANCOVA mit gutem Gewissen
# weitesgehend best�tigt werden k�nnen.


# c)

# ALM 

# Designmatrizen erstellen 

# Modell entwickeln
# AV 
y <- df$time

eins <- rep(1,nrow(df)) 
# Zentraler Pr�diktor, entweder x1, x2 oder x3=-x1-x2 -> deshalb zwei Variablen
# Dummykodierung, weil wir den Unterschied der Gruppe 2 und 3 zu 1 untersuchen 
x1 <- ifelse(df$routine == 2,1,0)
x2 <- ifelse(df$routine == 3,1,0)
# Kontrollvariable
x3 <- df$age 

# Designmatrix
X <- cbind(x1,x2,x3)


# Modellgleichung Yi
# Auf Papier 

# Matrizengleichung Y
# Auf Papier 


# d) 

# H0: Fahrer:innen mit niedriger Fahrpraxis (routine -> Gruppe 1) ben�tigen keine 
# signifikant l�ngere Zeit zum Einleiten des Bremsvorganges (time) als Fahrer:innen 
# mit mittlerer oder hoher Fahrpraxis (routine -> Gruppe 2, Gruppe 3) bei Kontrolle 
# des Einflusses des Fahrer:innenalters (age).

# H1: Fahrer:innen mit niedriger Fahrpraxis (routine -> Gruppe 1) ben�tigen eine 
# signifikant l�ngere Zeit zum Einleiten des Bremsvorganges (time) als Fahrer:innen 
# mit mittlerer oder hoher Fahrpraxis (routine -> Gruppe 2, Gruppe 3) bei Kontrolle 
# des Einflusses des Fahrer:innenalters (age).


# Statistische Form der Hypothesen:

# H0: mu1 <= (mu2+mu3)/2
# -> Psi = c' * beta <= 0 mit c = (1,-1/2,-1/2) und beta = (mu1,mu2,mu3) 

# H1: mu1 - 1/2*mu2 - 1/2*mu3 > 0
# -> Psi > Psi.0                

# Koeffizientenvektor der Hypothese
c = c(-1/2,-1/2,1)


# Statistisches Testen -> Mittelwert von mu1 mit dem Mittelwert von (mu2+mu3)/2 vergleichen 

# MIT ALM

summary(reg<-lm(df$time~X))

# Sch�tzung des Parametervektors durch lm():

#            Estimate   Std. Error t value Pr(>|t|) 
# Intercept  1.3201365  0.0207465  63.632  < 2e-16 *** -> mu1 =         1.3201365 -> Referenzwert
# Xx1        0.0095562  0.0200528   0.477  0.63554     -> mu2 =         1.329693
# Xx2       -0.0854615  0.0301931  -2.830  0.00644 **  -> mu3 =         1.234675
# Xx3        0.0030615  0.0005269   5.811 3.08e-07 *** -> coeff. =      0.0030615
        

# dies Werte bilden unseren Parametervektor und bilden multipliziert mit der
# Designmatrix unser Modell zur Vorhersage der AV:Reaktionszeit 
# Adjusted R-squared:  0.4036 


#Datenvektor
y <- df$time
# Sch�tzung des Parametervektors (X�X)^-1X�y
beta.dach <- solve(t(X) %*% X) %*% t(X) %*% y
# durch das Modell vorhergesagte y-Werte
y.dach <- X%*%beta.dach 
# Sch�tzer der Linearkombination Psi.dach berechen
Psi.dach <- c%*%beta.dach


# t-Bruch Funktion
t.bruch <- function(y,X,c,Psi.0=0){ # Funktion verlangt Datenvektor y, Designmatrix X, 
        # Koeffizientenvektor c und Psi_0
        # Sch�tzung des Parametervektors beta           
        beta.dach      <-  solve(t(X) %*% X) %*% t(X) %*% y
        
        # Sch�tzung der parametrischen Funktion Psi, das hei�t der Linearkombination 
        # der Parameter in beta
        Psi.dach       <-  c%*%beta.dach
        
        # Anzahl der Versuchspersonen, L�nge des Datenvektors y
        n              <-  length(y) 
        
        # p = Anzahl der gesch�tzten Parameter oder Anzahl der Elemente von beta.dach bzw. beta
        p              <-  length(beta.dach)
        
        # durch das Modell vorhergesagte y-Werte
        y.dach         <-  X %*% beta.dach 
        
        # mit (p=k+1) : s2 <- (1/(n-(k+1))) * sum((y-y.dach)^2)
        s2             <- (1/(n-p)) * sum((y-y.dach)^2)
        
        # Sch�tzung der Varianz des Sch�tzers
        est.V.Psi.dach <-  s2 * t(c)%*% solve(t(X) %*% X)%*%c
        
        # Ausgabe der Funktion: t-Bruch
        (Psi.dach - Psi.0) / (sqrt(est.V.Psi.dach))           
}


# Funktion anwenden 
(t.emp <- t.bruch(y,X,c))
# t.emp = -0.3374668 -> mit Kontrollvariable 
# t.emp = -5.334012 -> ohne Kontrollvariable 

# Signifikangrenze setzen
alpha <- .05

# linksseitiger Test
# Anzahl der Versuchspersonen, L�nge des Datenvektors y
(n      <- length(y)) 

# p = Anzahl der gesch�tzten Parameter oder Anzahl der Elemente von beta.dach bzw. beta
(p      <- length(beta.dach))

# der Wert, der die Fl�che alpha unter der t-Verteilung mit n-p Freiheitsgraden 
# links abschneidet
(t.krit <- -qt(1-alpha,n-p))
# t.krit = -1.671553


# Entscheidung:
ifelse(t.emp < t.krit,"H0 wird verworfen.","H0 wird vorlaeufig beibehalten.")
# t.emp = -0.3374668 > t.krit = -1.671553
# Die H0 wird vorl�ufig beibehalten 

# Interpretation: 
# Das Ergebnis zeigt, dass Fahrer:innen mit geringer Fahrpraxis (routine=1) in unserer
# ber�cksichtigten Stichprobe keine signifikant schlechtere Reaktionszeit (time) beim 
# Einleiten des Bremsvorganges haben als Fahrer:innen mit mittlerer und 
# hoher Fahrpraxis (routine=2,routine=3) unter Kontrolle der Altersvariable (age).



# e) Poweranalyse Post Hoc

# Effektst�rken (d-Werte) vorgeben
d <- seq(0,2,by=0.01)

# F�r jede Effektst�rke die Power berechnen
ncp <- as.vector((t(c)%*% solve(t(X) %*% X)%*%c)^(-0.5) %*% d)
power <- 1-pt(abs(t.krit), n-p, ncp=ncp)   

# Tabelle:
tab <- cbind(d, power) 

# Antwort: 
# Cohens d zwischen 0.88 und 0.89

# Qualifikation von d): 
# Bei einer Power von 0.9 w�rden wir ab einem 0.88/0.89-*-Standardabweichungs-
# Unterschied der beiden Erwartungswerte mit gro�er W.keit einen signifikanten 
# Test erhalten. 
# Mein in d) berechneter t-Wert von |t.emp| = 0.33 liegt deutlich unter dieser 
# Abweichungsgrenze, entsprechend erscheint es logisch, dass der Test nicht 
# signifikant ausgefallen ist.
# Dies kann nat�rlich nur unter der Annahme behauptet werden, dass mein
# Untersuchungsdesign auch die ben�tigte Power besessen hat und der zugrunde 
# liegenden Population ausreichend entsprochen hat. Dies gilt es in der Folge 
# zu �berpr�fen.


# f) Poweranalyse A Priori 

p.out <- pwr.t.test(d=0.5,power=0.9,sig.level=0.05,type="two.sample",alternative="greater")

plot(p.out)
# Perfekte Stichprobengr��e pro Gruppe n = 70

# Anzahl unserer Gruppen 
count(df, vars = routine)
# n1 = 36 -> routine = 1
# n2 = 24 -> routine = 2, routine = 3

# Wir m�ssen also f�r ein ideales Design 34 weitere Personen mit niedriger Fahrpraxis 
# und 46 Personen mit mittlerer oder hoher Fahrpraxis erheben und in unsere Untersuchung 
# einbetten. 

# Wie oben schon erl�utert bedeutet d = 0.5, also eine Effektst�rke von 0.5, dass 
# bei einer Differenz zwischen den untersuchten Gruppen von einer halben Standardabweichung 
# ein signifikantes Ergebnis mit der Wahrscheinlichkeit in H�he der Power gefunden wird. 
# Ab dieser Grenze geht man also von einem signifikanten Unterschied in der zugrunde 
# liegenden Population aus.


# g)

# Hat Werte berechnen 
summary(reg<-lm(df$time~X))
hat <- hatvalues(reg)


# Plotten
plot(hatvalues(reg), type = "h", xaxt="n", xlab = " ")
axis(1, at = seq(1,nrow(df),1), labels = row.names(df),  las= 2)

# drei gr��ten hat-Werte 
orden <- hat[order(-hat)]
orden[1:3]

# Drei gr��ten Hebelwirkungen 
# 50 -> 0.2027336
# 37 -> 0.1970484
# 49 -> 0.1816173

plot(x=df$age,                            
     y=df$time, 
     main="Zsh. Alter und Reaktionszeit", 
     xlab = "Alter", 
     ylab = "Reaktionszeit") 


# Zentroid 
cen
points(mean(df$age), mean(df$time), pch = 3)

# Drei Punkte mit der h�chsten ermittelten Hebelwirkung 
points(df$age[37], df$time[37], pch = 3)
points(df$age[49], df$time[49], pch = 3)
points(df$age[50], df$time[50], pch = 3)

# Antwort: 
# Die Studienteilnehmer 50, 37 und 49 haben nach unseren Untersuchungen die h�chste Hebelwirkung.
# Diese Punkte haben also im Sinne der Mahalanobis-Entfernung eine gr��ere Entfernung zum Zentroid
# als die anderen Datenpunkte, sind entsprechend untypisch f�r die Pr�diktorwerte und haben einen
# potentiell gro�en EInfluss auf die Lage der Regressionsgerade.


# Ausrei�ertest Bonferroni

# studentisierten Residuen
stud <- rstudent(reg)

# Outlier Test - F�r jede Beobachtung anzeigen 
outlierTest(reg,cutoff=Inf,n.max=Inf) 
# Outlier Test - bis Sig.Grenze 0.2 -> alle Werte die diese nicht �berschreiten,
# werden ausgespart 
outlierTest(reg,cutoff=0.2,n.max=Inf) 

# Signifikante Ausrei�er:
# rstudent unadjusted p-value Bonferroni p Teilnehmer 
# 2.815663        0.0067441    0.40465     24

# Antwort: 
# Der einzige Ausrei�er, der die Signifikanzgrenze von .2 �berschreitet, ist der
# Datenpunkt des Studienteilnehmers Nr. 24 mit studentisierten Residuuenwert von 
# 2.815663296. Die Abweichung dieses Datenpunktes von der ohne ihn gefitteten Regressions-
# gerade ist somit statistisch signifikant. 


# Cook's Distance 

cooks.distance(reg)
# Plot der Cook's Distance Werte als Histogramm 
plot(cooks.distance(reg), type = "h", xaxt="n", xlab = " ")
axis(1, at = seq(1,nrow(df),1), labels = row.names(df),  las= 2)

# Bubble-Plot -> n=2 zeigt die 2 h�chsten Werte von jeweils den studentisierten Residuuen,
# den Hat-Values und der Cook's Distance, also max. 6 wenn es keine �berschneidungen gibt 
influencePlot(reg, id = list(n=2)) 

# StudRes        Hat        CookD
# 17  2.165687081 0.03820437 4.369650e-02
# 24  2.815663296 0.02784927 5.052728e-02
# 34  1.798589101 0.17124186 1.606906e-01
# 37 -0.506030467 0.19704840 1.592154e-02
# 38 -1.281099562 0.17804374 8.786972e-02
# 50  0.009594387 0.20273356 5.958285e-06

# Antwort: 
# Aus der einfachen �berlegung heraus, dass Cook's Distance ein Kennwert ist, dessen 
# Gr��e direkt von den Hat-Werten und den Studentisierten Residuen abgeleitet wird und 
# somit den tats�chlichen Einfluss auf unsere Regressionsgerade wiederspiegelt, w�rde 
# ich mich dazu entscheiden auch eben haupts�chlich diese beiden Datenpunkte 34 und 38 
# mit der h�chsten Cookschen Distanz als besonders einflussreich zu bezeichnen. 
# Interessant ist dabei, dass die beiden Punkte einander gegen�berstehen und einen 
# �hnlichen Effekt in entgegen gesetzte Richtung haben d�rften und sich somit 
# m�glicherweise gegenseitig aushebeln. 
# 4 weitere Punkte, die mein Interesse in geringem Ma� geweckt haben sind einmal 
# 24 und 17 hinsichtlich der Studentisierten Residuuen und 37 und 50 hinsichtlich 
# der Hat-Values. Ihre Werte sind signifikant gro� und sie k�nnten bei einer gr��eren 
# Untersuchung mit mehr Studienteilnehmern einen Hinweis zu m�glicherweise interessanten 
# Schlussfolgerungen und Verbesserungen des Studiendesign und die Modellierung f�hren. 
# Bei weiteren Untersuchungen k�nnte man nach m�glichen fehlenden Pr�diktoren suchen, 
# die dann solche systematischen Abweichungen ber�cksichtigen und aufkl�ren k�nnen oder 
# es handelt sich gar um ein nicht-linearen Zusammenhang.
# In jedem Falle m�chte ich die Daten so wie sie sind beibehalten und die Auff�lligkeiten 
# nicht etwa aus dem Datensatz entfernen, was ich f�r schlechte Forscherpraxis halten w�rde.





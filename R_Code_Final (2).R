library(dplyr)
library(ggplot2)
library(MatchIt)
library(fixest)
library(tidyverse)


#Jummelage / Nettoyage de nos bases de données

characteristiques <- read.csv("C:\\Users\\imamh\\OneDrive\\Desktop\\Devoir2\\caracteristiques.csv")
resultats <- read.csv("C:\\Users\\imamh\\OneDrive\\Desktop\\Devoir2\\resultats.csv")
evaluateurs <- read.csv("C:\\Users\\imamh\\OneDrive\\Desktop\\Devoir2\\evaluateurs.csv")
presences <- read.csv("C:\\Users\\imamh\\OneDrive\\Desktop\\Devoir2\\presences.csv")


#Jummeler nos dataset en un seul.
dataset <- characteristiques %>%
  left_join(resultats, by = "eleve_id") %>%
  left_join(evaluateurs, by = "eleve_id") %>%
  left_join(presences, by = "eleve_id")

#Transforme les N/a en 0
dataset[is.na(dataset)]<-0



####################Question 1  // Profil-Type //##################

#Utiliser as.factor pour les variables categorielles.

#Utiliser glm pour avoir une probabilité de participer

regression <- glm(programme ~ as.factor(quartier) + as.factor(parents_education) + parents_revenu + score0, data= dataset )

summary(regression)



####################Question 2  // Effet sur performance //##################

#Séparé data traité et non traité dans deux dataset unique.

data_t <- subset(dataset,dataset$programme=="1")

data_nt <- subset(dataset,dataset$programme=="0")

#Proceder a la-DOUBLE-DIFFÉRENCE 

#moyenne geometriques

#groupe non traité
mean_score0_nt <- mean(data_nt$score0)

mean_score_ang_nt <- mean(data_nt$score1_anglais)

mean_score_math_nt <- mean(data_nt$score1_maths)


#groupe traité
mean_score0_t <- mean(data_t$score0)

mean_score_ang_t <- mean(data_t$score1_anglais)

mean_score_math_t <- mean(data_t$score1_maths)


#Création de nouvelles vecteurs pour la double difference

traite <- c(1,1,0,0)

post <- c(0,1,0,1)

traite_x_post <- traite*post


#Placer les moyennes dans un vecteur

score0_score_ang <- c(mean_score0_t, mean_score_ang_t, mean_score0_nt, mean_score_ang_nt)

#Placer nos vecteurs dans un dataframe
  
data_dd_ang <- data.frame(score0_score_ang, traite, post, traite_x_post)

#Procéder a la double difference par régression

reg_ang_dd <- lm(score0_score_ang ~ traite + post + traite_x_post, data = data_dd_ang)

#Afficher notre coefficient d'interet.

coeff_double_diff_ang <- coefficients(reg_ang_dd)

b3_ang<-coeff_double_diff_ang[4];


print(b3_ang)



#Effet traitement sur note mathematique (Meme proc/dure)

score0_score_math <- c(mean_score0_t, mean_score_math_t, mean_score0_nt, mean_score_math_nt);


data_dd_math <- data.frame(score0_score_math, traite, post, traite_x_post)


reg_math_dd<-lm(score0_score_math ~  traite + post + traite_x_post, data = data_dd_math)



#Placer les coefficients dans un vecteur 

coeff_double_diff_math <- coefficients(reg_math_dd)

#Afficher notre coefficient d'interet.


b3_math<-coeff_double_diff_math[4];


print(b3_math)





####################Question 3  // Effet sur présence //##################


#Q3 VARIABLE INSTRUMENTALE
#trouver le nombre de cas par évaluateur
data_eval_ordre<-arrange(dataset,evaluateur_id);  #a noter que l'on va de a à y


#Compter le nombre d'occurence totale par évaluateur
A<-sum(data_eval_ordre$evaluateur_id=="Eval_A");
B<-sum(data_eval_ordre$evaluateur_id=="Eval_B");
C<-sum(data_eval_ordre$evaluateur_id=="Eval_C");
D<-sum(data_eval_ordre$evaluateur_id=="Eval_D");
E<-sum(data_eval_ordre$evaluateur_id=="Eval_E");
F<-sum(data_eval_ordre$evaluateur_id=="Eval_F");
G<-sum(data_eval_ordre$evaluateur_id=="Eval_G");
H<-sum(data_eval_ordre$evaluateur_id=="Eval_H");
I<-sum(data_eval_ordre$evaluateur_id=="Eval_I");
J<-sum(data_eval_ordre$evaluateur_id=="Eval_J");
K<-sum(data_eval_ordre$evaluateur_id=="Eval_K");
L<-sum(data_eval_ordre$evaluateur_id=="Eval_L");
M<-sum(data_eval_ordre$evaluateur_id=="Eval_M");
N<-sum(data_eval_ordre$evaluateur_id=="Eval_N");
O<-sum(data_eval_ordre$evaluateur_id=="Eval_O");
P<-sum(data_eval_ordre$evaluateur_id=="Eval_P");
Q<-sum(data_eval_ordre$evaluateur_id=="Eval_Q");
R<-sum(data_eval_ordre$evaluateur_id=="Eval_R");
S<-sum(data_eval_ordre$evaluateur_id=="Eval_S");
T<-sum(data_eval_ordre$evaluateur_id=="Eval_T");
U<-sum(data_eval_ordre$evaluateur_id=="Eval_U");
V<-sum(data_eval_ordre$evaluateur_id=="Eval_V");
W<-sum(data_eval_ordre$evaluateur_id=="Eval_W");
X<-sum(data_eval_ordre$evaluateur_id=="Eval_X");
Y<-sum(data_eval_ordre$evaluateur_id=="Eval_Y");



#Creeation des vecteurs
a<-rep(A,A);
b<-rep(B,B);
c<-rep(C,C);
d<-rep(D,D);
e<-rep(E,E);
f<-rep(F,F);
g<-rep(G,G);
h<-rep(H,H);
i<-rep(I,I);
j<-rep(J,J);
k<-rep(K,K);
l<-rep(L,L);
m<-rep(M,M);
n<-rep(N,N);
o<-rep(O,O);
p<-rep(P,P);
q<-rep(Q,Q);
r<-rep(R,R);
s<-rep(S,S);
t<-rep(T,T);
u<-rep(U,U);
v<-rep(V,V);
w<-rep(W,W);
x<-rep(X,X);
y<-rep(Y,Y);

N_eval<-c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y);

data_eval<-data.frame(data_eval_ordre)

#calcul de l'instrument 

#subdiviser les dataframe par evaluateur et calculer le nombre de programme == 1 par evaluateurs

data_A <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_A");

sum_tA<-sum(data_A$programme == 1)


data_B <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_B");

sum_tB<-sum(data_B$programme)



data_C <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_C");

sum_tC<-sum(data_C$programme)



data_D <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_D");

sum_tD<-sum(data_D$programme)



data_E <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_E");

sum_tE<-sum(data_E$programme)



data_F <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_F");

sum_tF<-sum(data_F$programme)



data_G <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_G");

sum_tG<-sum(data_G$programme)



data_H <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_H");

sum_tH<-sum(data_H$programme)



data_I <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_I");

sum_tI<-sum(data_I$programme)



data_J <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_J");

sum_tJ<-sum(data_J$programme)



data_K <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_K");

sum_tK<-sum(data_K$programme)



data_L <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_L");

sum_tL<-sum(data_L$programme)



data_M <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_M");

sum_tM<-sum(data_M$programme)



data_N <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_N");

sum_tN<-sum(data_N$programme)



data_O <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_O");
sum_tO<-sum(data_O$programme)

data_P <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_P");
sum_tP<-sum(data_P$programme)

data_Q <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_Q");
sum_tQ<-sum(data_Q$programme)

data_R <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_R");
sum_tR<-sum(data_R$programme)

data_S <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_S");
sum_tS<-sum(data_S$programme)

data_T <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_T");
sum_tT<-sum(data_T$programme)

data_U <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_U");
sum_tU<-sum(data_U$programme)

data_V <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_V");
sum_tV<-sum(data_V$programme)

data_W <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_W");
sum_tW<-sum(data_W$programme)

data_X <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_X");
sum_tX<-sum(data_X$programme)

data_Y <- data_eval %>% filter(data_eval$evaluateur_id == "Eval_Y");
sum_tY<-sum(data_Y$programme)




#calculer z our evaluateurs [a:y] 
z_a<-rep(NA, times = A)
for (i in 1:39) {
  z_a<-(sum_tA-data_A$programme[])/(38)
  z_a[i]<-z_a
}

z_b<-rep(NA, times = B)
for (i in 1:42) {
  z_b<-(sum_tB-data_A$programme[])/(41)
  z_b[i]<-z_b
}

z_c<-rep(NA, times = C)
for (i in 1:49) {
  z_c<-(sum_tC-data_C$programme[])/(48)
  z_c[i]<-z_c
}


z_d<-rep(NA, times = D)
for (i in 1:40) {
  z_d<-(sum_tD-data_D$programme[])/(39)
  z_d[i]<-z_d
}

z_e<-rep(NA, times = E)
for (i in 1:38) {
  z_e<-(sum_tE-data_E$programme[])/(37)
  z_e[i]<-z_e
}

z_f<-rep(NA, times = F)
for (i in 1:46) {
  z_f<-(sum_tF-data_F$programme[])/(45)
  z_f[i]<-z_f
}

z_g<-rep(NA, times = G)
for (i in 1:28) {
  z_g<-(sum_tG-data_G$programme[])/(27)
  z_g[i]<-z_g
}


z_h<-rep(NA, times = H)
for (i in 1:54) {
  z_h<-(sum_tH-data_H$programme[])/(53)
  z_h[i]<-z_h
}

z_i<-rep(NA, times = I)
for (i in 1:43) {
  z_i<-(sum_tI-data_I$programme[])/(42)
  z_i[i]<-z_i
}

z_j<-rep(NA, times = J)
for (i in 1:44) {
  z_j<-(sum_tJ-data_J$programme[])/(43)
  z_j[i]<-z_j
}

z_k<-rep(NA, times = K)
for (i in 1:41) {
  z_k<-(sum_tK-data_K$programme[])/(40)
  z_k[i]<-z_k
}


z_l<-rep(NA, times = L)
for (i in 1:44) {
  z_l<-(sum_tL-data_L$programme[])/(43)
  z_l[i]<-z_l
}

z_m<-rep(NA, times = M)
for (i in 1:39) {
  z_m<-(sum_tM-data_M$programme[])/(38)
  z_m[i]<-z_m
}

z_n<-rep(NA, times = N)
for (i in 1:37) {
  z_n<-(sum_tN-data_N$programme[])/(36)
  z_n[i]<-z_n
}

z_o<-rep(NA, times = O)
for (i in 1:49) {
  z_o<-(sum_tO-data_O$programme[])/(48)
  z_o[i]<-z_o
}


z_p<-rep(NA, times = P)
for (i in 1:47) {
  z_p<-(sum_tP-data_P$programme[])/(46)
  z_p[i]<-z_p
}

z_q<-rep(NA, times = Q)
for (i in 1:35) {
  z_q<-(sum_tQ-data_Q$programme[])/(34)
  z_q[i]<-z_q
}

z_r<-rep(NA, times = R)
for (i in 1:34) {
  z_r<-(sum_tR-data_R$programme[])/(33)
  z_r[i]<-z_r
}

z_s<-rep(NA, times = S)
for (i in 1:38) {
  z_s<-(sum_tS-data_S$programme[])/(37)
  z_s[i]<-z_s
}


z_t<-rep(NA, times = T)
for (i in 1:42) {
  z_t<-(sum_tT-data_T$programme[])/(41)
  z_t[i]<-z_t
}

z_u<-rep(NA, times = U)
for (i in 1:32) {
  z_u<-(sum_tU-data_U$programme[])/(31)
  z_u[i]<-z_u
}

z_v<-rep(NA, times = V)
for (i in 1:36) {
  z_v<-(sum_tV-data_V$programme[])/(35)
  z_v[i]<-z_v
}

z_w<-rep(NA, times = W)
for (i in 1:40) {
  z_w<-(sum_tW-data_W$programme[])/(39)
  z_w[i]<-z_w
}


z_x<-rep(NA, times = X)
for (i in 1:29) {
  z_x<-(sum_tX-data_X$programme[])/(28)
  z_x[i]<-z_x
}

z_y <- rep(NA, times = Y)
for (i in 1:34) {
  z_y<-(sum_tY-data_Y$programme[])/(33)
  z_y[i]<-z_y
}


data_z<-c(z_a,z_b,z_c,z_d,z_e,z_f,z_g,z_h,z_i,z_j,z_k,z_l,z_m,z_n,z_o,z_p,z_q,z_r,z_s,z_t,z_u,z_v,z_w,z_x,z_y)
data_eval_2<-data.frame(data_z,data_eval)



#méthode forme réduite


# regression premiere etape
first_stage <- lm(programme ~ data_z,data = data_eval)

coef_first_stage <- coefficients(first_stage)

alpha <- coef_first_stage[2]


#Étape 2
summary(first_stage)  #La t-valeur pour z est de 11.414> racine(10) alors notre instru est pertinent





#Étape 3
#modification de donné, taux de présence
presences_2 <- presences[,2:294]
t_presence <- c(rowSums(presences_2)/293);
presences<-data.frame(t_presence,presences);
data_try_2<-data.frame(presences$eleve_id,presences$t_presence);


#Renommer les variables
names(data_try_2)[names(data_try_2) == "presences.eleve_id"] <- "eleve_id"
names(data_try_2)[names(data_try_2) == "presences.t_presence"] <- "t_presence"

#Jummeler les deux bases par eleve id
data_final <- merge(data_eval_2,data_try_2, by = "eleve_id",all.x = TRUE)

#Remplacer les n/a par 0
data_final[is.na(data_final)] <- 0



#reg forme reduite
reg_reduit<-lm(t_presence ~ data_z, data =  data_final)

#Afficher le coeficient 
coef_reduit<-coefficients(reg_reduit)

alpha_beta<-coef_reduit[2]

print(alpha_beta)


#calcul effet traitement
effet_traitement_presence <- alpha_beta/alpha

print(effet_traitement_presence)








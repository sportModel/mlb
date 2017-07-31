source("setup.R")

td <- import.team.data() ## td = Team Data
yt <- get.year.totals(td) ## yt = Year Totals
year <- yt[,"year"]
n.year <- length(year)
match.id <- match(td$yearID,yt[,"year"])
decision.year <- 1951

## Runs-wins model
eR <- td$G * (yt[,"R"]/yt[,"G"])[match.id]
dRS <- td$R - eR
dRA <- td$RA - eR
dW <- td$W - td$G/2
dR <- dRS-dRA
ind <- which(td$yearID >= decision.year)
mod <- lm(dW[ind]~0+dR[ind])
print(summary(mod))
beta1 <- mod$coef
names(beta1) <- gsub("[ind]","",names(beta1),fixed=T)
names(beta1) <- gsub("d","",names(beta1),fixed=T)

## Offensive model
eRS <- td$G * (yt[,"R"]/yt[,"G"])[match.id]
dRS <- td$R - eRS
td.1B <- td$H - td[,"2B"] - td[,"3B"] - td$HR
yt.1B <- yt[,"H"] - yt[,"2B"] - yt[,"3B"] - yt[,"HR"]
PA <- td$AB + td$BB
yt.PA <- yt[,"AB"] + yt[,"BB"]
e1B <- PA * (yt.1B/yt.PA)[match.id]
e2B <- PA * (yt[,"2B"]/yt.PA)[match.id]
e3B <- PA * (yt[,"3B"]/yt.PA)[match.id]
eHR <- PA * (yt[,"HR"]/yt.PA)[match.id]
##eOuts <- PA * (yt[,"Outs"]/yt.PA)[match.id]
eBB <- PA * (yt[,"BB"]/yt.PA)[match.id]
eSB <- PA * (yt[,"SB"]/yt.PA)[match.id]
eCS <- PA * (yt[,"CS"]/yt.PA)[match.id]
d1B <- td.1B - e1B
d2B <- td[,"2B"] - e2B
d3B <- td[,"3B"] - e3B
dHR <- td$HR - eHR
##dOuts <- td$Outs - eOuts
dBB <- td$BB - eBB
dSB <- td$SB - eSB
dCS <- td$CS - eCS
ind <- which(td$yearID >= decision.year)
mod <- lm(dRS[ind]~0+d1B[ind]+d2B[ind]+d3B[ind]+dHR[ind]+dBB[ind]+dSB[ind]+dCS[ind])
beta2 <- mod$coef
names(beta2) <- gsub("[ind]","",names(beta2),fixed=T)
names(beta2) <- gsub("d","",names(beta2),fixed=T)
print(summary(mod))

## Defensive model
eRA <- td$G * (yt[,"R"]/yt[,"G"])[match.id]
PAA <- td$Outs + td$HA + td$BBA
yt.PAA <- yt[,"Outs"] + yt[,"HA"] + yt[,"BBA"]
BIPH <- td$HA - td$HRA
BIPO <- td$Outs - td$SOA
BIP <- BIPH + BIPO
yt.BIPH <- yt[,"HA"] - yt[,"HRA"]
yt.BIPO <- yt[,"Outs"] - yt[,"SOA"]
yt.BIP <- yt.BIPH + yt.BIPO
eSOA <- PAA * (yt[,"SOA"]/yt.PAA)[match.id]
eBBA <- PAA * (yt[,"BBA"]/yt.PAA)[match.id]
eHRA <- PAA * (yt[,"HRA"]/yt.PAA)[match.id]
eBIPH <- BIP * (yt.BIPH/yt.BIP)[match.id]
##eE1 <- BIP * (yt[,"E1"]/yt.BIP)[match.id]
dRA <- td$RA-eRA
dSOA <- td$SOA-eSOA
dBBA <- td$BBA-eBBA
dHRA <- td$HRA-eHRA
dBIPH <- BIPH-eBIPH
##dE1 <- td$E1-eE1
##dE2 <- td$E2-eE2
##dE3 <- td$E3-eE3
##dE4 <- td$E4-eE4
##dE5 <- td$E5-eE5
##dE6 <- td$E6-eE6
##dE7 <- td$E7-eE7
##dE8 <- td$E8-eE8
##dE9 <- td$E9-eE9
##dA7 <- td$A7-eA7
##dA8 <- td$A8-eA8
##dA9 <- td$A9-eA9
##mod <- lm(dRA[ind]~0+dSOA[ind]+dBBA[ind]+dHRA[ind]+dBIPH[ind]+dBIPO[ind]+dSBA[ind]+dCSA[ind]+dE1[ind]+dE2[ind]+dE3[ind]+dE4[ind]+dE5[ind]+dE6[ind]+dE7[ind]+dE8[ind]+dE9[ind]+dA7[ind]+dA8[ind]+dA9[ind])
mod <- lm(dRA[ind]~0+dSOA[ind]+dBBA[ind]+dHRA[ind]+dBIPH[ind])
print(summary(mod))
beta3 <- mod$coef
names(beta3) <- gsub("[ind]","",names(beta3),fixed=T)
names(beta3) <- gsub("d","",names(beta3),fixed=T)

## Save
model <- c(beta1,beta2,beta3)
save(model,file="model.RData")

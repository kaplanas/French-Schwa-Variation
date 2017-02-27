library(lme4)

source("schwa_functions.R")

###########################
# LOAD AND CLEAN PFC DATA #
###########################

# load data
load_data()
# eliminate duplicate speakers
duplicate.speakers = names(table(speakers$Speaker)[table(speakers$Speaker) > 1])
speakers = speakers[!is.element(speakers$Speaker, duplicate.speakers),]
tokens = tokens[!is.element(tokens$Speaker, duplicate.speakers),]
# eliminate speakers who produced no tokens
speakers.no.tokens = names(table(speakers$Speaker))[!is.element(names(table(speakers$Speaker)), names(table(tokens$Speaker)))]
speakers = speakers[!is.element(speakers$Speaker, speakers.no.tokens),]
# eliminate tokens from speakers with no data
tokens.no.speakers = names(table(tokens$Speaker))[!is.element(names(table(tokens$Speaker)), names(table(speakers$Speaker)))]
tokens.no.speakers.cities = unique(tokens$City[is.element(tokens$Speaker, tokens.no.speakers)])
tokens = tokens[!is.element(tokens$Speaker, tokens.no.speakers),]

# make sure everything is a factor
speakers$Speaker = factor(speakers$Speaker)
tokens$Speaker = factor(tokens$Speaker)
tokens$Discourse = factor(tokens$Discourse)
tokens$Left = factor(tokens$Left)
tokens$Right = factor(tokens$Right)
tokens$Word = factor(tokens$Word)

# add some speaker data to token dataframe
rownames(speakers) = speakers$Speaker
tokens$Sex = speakers[as.character(tokens$Speaker),"Sex"]
tokens$City = factor(speakers[as.character(tokens$Speaker),"City"])
tokens$Region = factor(speakers[as.character(tokens$Speaker),"Region"])
tokens$Zone = factor(speakers[as.character(tokens$Speaker),"Zone"])

# clean the left and right contexts
tokens$LeftContext = as.character(tokens$LeftContext)
tokens$LeftContext = ifelse(substr(tokens$LeftContext, 1, 1) == " " & nchar(tokens$LeftContext) > 1, substr(tokens$LeftContext, 2, nchar(tokens$LeftContext)), tokens$LeftContext)
tokens$LeftContext = ifelse(substr(tokens$LeftContext, nchar(tokens$LeftContext), nchar(tokens$LeftContext)) == " ", substr(tokens$LeftContext, 1, nchar(tokens$LeftContext) - 1), tokens$LeftContext)
tokens$RightContext = as.character(tokens$RightContext)
tokens$RightContext = ifelse(substr(tokens$RightContext, 1, 1) == " ", substr(tokens$RightContext, 2, nchar(tokens$RightContext)), tokens$RightContext)
tokens$RightContext = ifelse(substr(tokens$RightContext, nchar(tokens$RightContext), nchar(tokens$RightContext)) == " ", substr(tokens$RightContext, 1, nchar(tokens$RightContext) - 1), tokens$RightContext)

# set up Paris- and Canada-only dataframes
speakers.paris = speakers[speakers$Region == "Ile de France",]
speakers.paris$Speaker = factor(speakers.paris$Speaker)
speakers.paris$City = factor(speakers.paris$City)
tokens.paris = tokens[tokens$Region == "Ile de France",]
tokens.paris$Speaker = factor(tokens.paris$Speaker)
tokens.paris$City = factor(tokens.paris$City)
tokens.paris$SchwaBin = ifelse(tokens.paris$Schwa == "yes", 1, 0)
speakers.canada = speakers[speakers$Zone == "Canada",]
speakers.canada$Speaker = factor(speakers.canada$Speaker)
speakers.canada$City = factor(speakers.canada$City)
tokens.canada = tokens[tokens$Zone == "Canada",]
tokens.canada$Speaker = factor(tokens.canada$Speaker)
tokens.canada$City = factor(tokens.canada$City)
tokens.canada$SchwaBin = ifelse(tokens.canada$Schwa == "yes", 1, 0)
speakers.pc = rbind(speakers.paris, speakers.canada)
tokens.pc = rbind(tokens.paris, tokens.canada)

# remove "unclear" and "NA" schwa tokens
tokens.pc = tokens.pc[!is.element(tokens.pc$Schwa, c("unclear", "NA")),]

# remove "metathesis" tokens
tokens.pc = tokens.pc[tokens.pc$Word != "metathesis",]

# remove tokens with left contexts that we can't analyze
tokens.pc = tokens.pc[!is.element(tokens.pc$Left, c("CC", "uncertain")),]
tokens.pc = tokens.pc[!(tokens.pc$Left == "boundary" & (tokens.pc$Word == "polysyl2" | tokens.pc$Word == "polysylend")),]

# remove tokens with various "NA" contexts
tokens.pc = tokens.pc[tokens.pc$Left != "NA" & tokens.pc$Right != "NA" & tokens.pc$Word != "NA",]

########################
# PARISIAN TWO-C SCHWA #
########################

# get speaker-specific frequences for Parisian two-C schwa
paris.two.c.data = tokens.pc[tokens.pc$Region == "Ile de France" & tokens.pc$Left == "V" & tokens.pc$Right == "C" & !grepl("[Pp]arce$", tokens.pc$LeftContext),]
paris.two.c.data = paris.two.c.data[!is.element(paris.two.c.data$Word, c("polysyl2", "polysylend")),]
paris.two.c = table(paris.two.c.data$Speaker, paris.two.c.data$Schwa)
paris.two.c.free = table(paris.two.c.data$Speaker[paris.two.c.data$Discourse == "free"], paris.two.c.data$Schwa[paris.two.c.data$Discourse == "free"])
paris.two.c.guided = table(paris.two.c.data$Speaker[paris.two.c.data$Discourse == "guided"], paris.two.c.data$Schwa[paris.two.c.data$Discourse == "guided"])
paris.two.c.read = table(paris.two.c.data$Speaker[paris.two.c.data$Discourse == "read"], paris.two.c.data$Schwa[paris.two.c.data$Discourse == "read"])
paris.two.c = data.frame(Yes = paris.two.c[,"yes"], No = paris.two.c[,"no"], Prop = paris.two.c[,"yes"] / (paris.two.c[,"yes"] + paris.two.c[,"no"]))
paris.two.c.free = data.frame(Yes = paris.two.c.free[,"yes"], No = paris.two.c.free[,"no"], Prop = paris.two.c.free[,"yes"] / (paris.two.c.free[,"yes"] + paris.two.c.free[,"no"]))
paris.two.c.guided = data.frame(Yes = paris.two.c.guided[,"yes"], No = paris.two.c.guided[,"no"], Prop = paris.two.c.guided[,"yes"] / (paris.two.c.guided[,"yes"] + paris.two.c.guided[,"no"]))
paris.two.c.read = data.frame(Yes = paris.two.c.read[,"yes"], No = paris.two.c.read[,"no"], Prop = paris.two.c.read[,"yes"] / (paris.two.c.read[,"yes"] + paris.two.c.read[,"no"]))

# plot speaker-specific frequencies
postscript("paris_two_c.ps", paper = "special", horizontal = F, width = 4, height = 2, pointsize = 8)
par(mar = c(4, 4, 1, 1))
hist(paris.two.c$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), main = "")
dev.off()
postscript("paris_two_c_discourse.ps", paper = "special", horizontal = F, width = 4.5, height = 1.5, pointsize = 10)
par(mar = c(4, 3, 1, 2), mfrow = c(1, 3))
hist(paris.two.c.free$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Free conversation")
hist(paris.two.c.guided$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Guided conversation")
hist(paris.two.c.read$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Read speech")
dev.off()

# fit models and do model comparison
fit.paris.two.c.1 = glmer(SchwaBin ~ Word + (1 | Discourse), data = paris.two.c.data, family = "binomial")
fit.paris.two.c.2 = glmer(SchwaBin ~ Word + (1 | Discourse) + (1 | Speaker), data = paris.two.c.data, family = "binomial")
anova.paris.two.c.1.2 = anova(fit.paris.two.c.1, fit.paris.two.c.2)
fit.paris.two.c.3 = glmer(SchwaBin ~ Word + (1 | Discourse) + (1 | City), data = paris.two.c.data, family = "binomial")
anova.paris.two.c.1.3 = anova(fit.paris.two.c.1, fit.paris.two.c.3)
fit.paris.two.c.4 = glmer(SchwaBin ~ Word + (1 | Discourse) + (1 | Speaker) + (1 | City), data = paris.two.c.data, family = "binomial")
anova.paris.two.c.2.4 = anova(fit.paris.two.c.2, fit.paris.two.c.4)
anova.paris.two.c.3.4 = anova(fit.paris.two.c.3, fit.paris.two.c.4)
fit.paris.two.c.final = fit.paris.two.c.2

#########################
# QUEBECOIS TWO-C SCHWA #
#########################

# get speaker-specific frequences for Quebecois two-C schwa
quebec.two.c.data = tokens.pc[tokens.pc$Region == "Quebec" & tokens.pc$Left == "V" & tokens.pc$Right == "C" & !grepl("[Pp]arce$", tokens.pc$LeftContext),]
quebec.two.c.data = quebec.two.c.data[!is.element(quebec.two.c.data$Word, c("polysyl2", "polysylend")),]
quebec.two.c = table(quebec.two.c.data$Speaker, quebec.two.c.data$Schwa)
quebec.two.c.free = table(quebec.two.c.data$Speaker[quebec.two.c.data$Discourse == "free"], quebec.two.c.data$Schwa[quebec.two.c.data$Discourse == "free"])
quebec.two.c.guided = table(quebec.two.c.data$Speaker[quebec.two.c.data$Discourse == "guided"], quebec.two.c.data$Schwa[quebec.two.c.data$Discourse == "guided"])
quebec.two.c.read = table(quebec.two.c.data$Speaker[quebec.two.c.data$Discourse == "read"], quebec.two.c.data$Schwa[quebec.two.c.data$Discourse == "read"])
quebec.two.c = data.frame(Yes = quebec.two.c[,"yes"], No = quebec.two.c[,"no"], Prop = quebec.two.c[,"yes"] / (quebec.two.c[,"yes"] + quebec.two.c[,"no"]))
quebec.two.c.free = data.frame(Yes = quebec.two.c.free[,"yes"], No = quebec.two.c.free[,"no"], Prop = quebec.two.c.free[,"yes"] / (quebec.two.c.free[,"yes"] + quebec.two.c.free[,"no"]))
quebec.two.c.guided = data.frame(Yes = quebec.two.c.guided[,"yes"], No = quebec.two.c.guided[,"no"], Prop = quebec.two.c.guided[,"yes"] / (quebec.two.c.guided[,"yes"] + quebec.two.c.guided[,"no"]))
quebec.two.c.read = data.frame(Yes = quebec.two.c.read[,"yes"], No = quebec.two.c.read[,"no"], Prop = quebec.two.c.read[,"yes"] / (quebec.two.c.read[,"yes"] + quebec.two.c.read[,"no"]))

# plot speaker-specific frequencies
postscript("quebec_two_c.ps", paper = "special", horizontal = F, width = 4, height = 2, pointsize = 8)
par(mar = c(4, 4, 1, 1))
hist(quebec.two.c$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), main = "")
dev.off()
postscript("quebec_two_c_discourse.ps", paper = "special", horizontal = F, width = 4.5, height = 1.5, pointsize = 10)
par(mar = c(4, 3, 1, 2), mfrow = c(1, 3))
hist(quebec.two.c.free$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Free conversation")
hist(quebec.two.c.guided$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Guided conversation")
hist(quebec.two.c.read$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Read speech", axes = F)
axis(1, seq(0, 1, .2))
axis(2, 0:3, labels = c("0", "1", "2", "3"))
dev.off()

# fit models and do model comparison
fit.quebec.two.c.1 = glmer(SchwaBin ~ Word + (1 | Discourse), data = quebec.two.c.data, family = "binomial")
fit.quebec.two.c.2 = glmer(SchwaBin ~ Word + (1 | Discourse) + (1 | Speaker), data = quebec.two.c.data, family = "binomial")
anova.quebec.two.c.1.2 = anova(fit.quebec.two.c.1, fit.quebec.two.c.2)

###################
# HAND-CODED DATA #
###################

# load hand-coded data
hand_coding_1_paris = read.csv("hand_coding.csv", header = T, sep = ",")
hand_coding_1_paris = hand_coding_1_paris[as.character(hand_coding_1_paris$Speaker) != "75cac1",]
hand_coding_1_paris = hand_coding_1_paris[,c(1:10,16:18)]
names(hand_coding_1_paris)[c(6,7,9,10,11,12,13)] = c("C2", "C3", "S2", "S3", "C1alt", "C2alt", "C3alt")
hand_coding_1_paris$Discourse = ifelse(as.character(hand_coding_1_paris$Speaker) == "75cac1", "NA", as.character(tokens[as.character(hand_coding_1_paris$Reference),]$Discourse))
hand_coding_1_quebec = read.csv("hand_coding_quebec.csv", header = T, sep = ",")
hand_coding_1_quebec = data.frame(
  Reference = hand_coding_1_quebec$SearchID,
  Speaker = hand_coding_1_quebec$Speaker,
  Codes = hand_coding_1_quebec$Codes,
  Context = paste(hand_coding_1_quebec$LeftContext, hand_coding_1_quebec$RightContext),
  C1 = hand_coding_1_quebec$C1,
  C2 = hand_coding_1_quebec$C2,
  C3 = hand_coding_1_quebec$C3,
  S1 = ifelse(is.element(hand_coding_1_quebec$C1, c("p", "t", "k", "b", "d", "g")), 0, ifelse(is.element(hand_coding_1_quebec$C1, c("f", "v", "s", "z", "ʃ", "ʒ", "R")), 1, ifelse(is.element(hand_coding_1_quebec$C1, c("m", "n")), 2, ifelse(hand_coding_1_quebec$C1 == "l", 3, "?")))),
  S2 = ifelse(is.element(hand_coding_1_quebec$C1, c("p", "t", "k", "b", "d", "g")), 0, ifelse(is.element(hand_coding_1_quebec$C1, c("f", "v", "s", "z", "ʃ", "ʒ", "R")), 1, ifelse(is.element(hand_coding_1_quebec$C1, c("m", "n")), 2, ifelse(hand_coding_1_quebec$C1 == "l", 3, "?")))),
  S3 = ifelse(is.element(hand_coding_1_quebec$C1, c("p", "t", "k", "b", "d", "g")), 0, ifelse(is.element(hand_coding_1_quebec$C1, c("f", "v", "s", "z", "ʃ", "ʒ", "R")), 1, ifelse(is.element(hand_coding_1_quebec$C1, c("m", "n")), 2, ifelse(hand_coding_1_quebec$C1 == "l", 3, "?")))),
  C1alt = "",
  C2alt = "",
  C3alt = "",
  Discourse = hand_coding_1_quebec$Discourse
)
hand_coding_1 = rbind(hand_coding_1_paris, hand_coding_1_quebec)
hand_coding_2 = read.csv("hand_coding_2.csv", header = T, sep = ",")
names(hand_coding_2)[c(7,8,10,11)] = c("C2", "C3", "S2", "S3")
hand_coding = rbind(hand_coding_1[,c(1:10, 14)], hand_coding_2[,c(1:3, 5:11, 4)])
hand_coding = hand_coding[hand_coding$C1 != "?" & hand_coding$C1 != "f!" & hand_coding$C1 != "p*" & hand_coding$C2 != "?" & hand_coding$C3 != "?" & hand_coding$C3 != "p!",]

# get the agreement between the hand coders
hand_coding_agreement = hand_coding_1[hand_coding_1$C1 != "" & hand_coding_1$C2 != "" & hand_coding_1$C3 != "" & hand_coding_1$C1alt != "" & hand_coding_1$C2alt != "" & hand_coding_1$C3alt != "" & as.character(hand_coding_1$C1) == as.character(hand_coding_1$C1alt) & as.character(hand_coding_1$C2) == as.character(hand_coding_1$C2alt) & as.character(hand_coding_1$C3) == as.character(hand_coding_1$C3alt),]

# clean codes in the hand-coding data
hand_coding$SchwaBin = ifelse(nchar(as.character(hand_coding$Codes)) == 3, 0, 1)
hand_coding$Codes = ifelse(nchar(hand_coding$Codes) == 3, paste("0", hand_coding$Codes, sep = ""), hand_coding$Codes)
temp = substr(hand_coding$Codes, start = 2, stop = 2)
hand_coding$Word = ifelse(temp == "1", "monosyl", ifelse(temp == "2", "polysyl1", ifelse(temp == "3", "polysyl2", ifelse(temp == "4", "polysylend", ifelse(temp == "5", "metathesis", "NA")))))
temp = substr(hand_coding$Codes, start = 3, stop = 3)
hand_coding$Left = ifelse(temp == "1", "V", ifelse(temp == "2", "C", ifelse(temp == "3", "boundary", ifelse(temp == "4", "uncertain", ifelse(temp == "5", "CC", "NA")))))
temp = substr(hand_coding$Codes, start = 4, stop = 4)
hand_coding$Right = ifelse(temp == "1", "V", ifelse(temp == "2", "C", ifelse(temp == "3", "boundarystrong", ifelse(temp == "4", "boundaryweak", "NA"))))
hand_coding$City = factor(speakers[as.character(hand_coding$Speaker),"City"])
hand_coding$Zone = factor(speakers[as.character(hand_coding$Speaker),"Zone"])
hand_coding$S1 = ifelse(hand_coding$C1 == "R", 4, as.character(hand_coding$S1))
hand_coding$S2 = ifelse(hand_coding$C2 == "R", 4, as.character(hand_coding$S2))
hand_coding$S3 = ifelse(hand_coding$C3 == "R", 1, as.character(hand_coding$S3))

# remove tokens from the hand-coding data that we're not going to analyze
hand_coding = hand_coding[is.element(hand_coding$Word, c("monosyl", "polysyl1")) & hand_coding$Left != "boundary" & !is.element(hand_coding$Right, c("boundaryweak", "boundarystrong")),]
hand_coding = hand_coding[!(hand_coding$S2 > 1 & hand_coding$S2 > hand_coding$S1 & hand_coding$S2 > hand_coding$S3),]
hand_coding = hand_coding[!(hand_coding$S2 == 0 & !((hand_coding$S3 == 1 | hand_coding$S3 == 3 | hand_coding$S3 == 4) | hand_coding$Word == "monosyl" | hand_coding$C1 == "R")),]

# get and plot speaker-specific frequencies
hand = table(hand_coding$Speaker, hand_coding$Schwa)
hand.guided = table(hand_coding$Speaker[hand_coding$Discourse == "guided"], hand_coding$Schwa[hand_coding$Discourse == "guided"])
hand.free = table(hand_coding$Speaker[hand_coding$Discourse == "free"], hand_coding$Schwa[hand_coding$Discourse == "free"])
hand.read = table(hand_coding$Speaker[hand_coding$Discourse == "read"], hand_coding$Schwa[hand_coding$Discourse == "read"])
hand = data.frame(Yes = hand[,"1"], No = hand[,"0"], Prop = hand[,"1"] / (hand[,"1"] + hand[,"0"]))
hand.free = data.frame(Yes = hand.free[,"1"], No = hand.free[,"0"], Prop = hand.free[,"1"] / (hand.free[,"1"] + hand.free[,"0"]))
hand.guided = data.frame(Yes = hand.guided[,"1"], No = hand.guided[,"0"], Prop = hand.guided[,"1"] / (hand.guided[,"1"] + hand.guided[,"0"]))
hand.read = data.frame(Yes = hand.read[,"1"], No = hand.read[,"0"], Prop = hand.read[,"1"] / (hand.read[,"1"] + hand.read[,"0"]))
postscript("hand.ps", paper = "special", horizontal = F, width = 4.5, height = 1.5, pointsize = 10)
par(mar = c(4, 3, 1, 2), mfrow = c(1, 3))
hist(hand.free$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Free conversation")
hist(hand.guided$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Guided conversation")
hist(hand.read$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), breaks = seq(0, 1, .1), main = "Read speech")
dev.off()

# fit models for hand-coded data and do model comparison
fit.hand.1 = glmer(SchwaBin ~ Word + (1 | Discourse), data = hand_coding, family = "binomial")
fit.hand.2 = glmer(SchwaBin ~ Word + (1 | Discourse) + (1 | Speaker), data = hand_coding, family = "binomial")
anova.hand.1.2 = anova(fit.hand.1, fit.hand.2)
fit.hand.3 = glmer(SchwaBin ~ Word + (1 | Discourse) + (1 | City), data = hand_coding, family = "binomial")
anova.hand.1.3 = anova(fit.hand.1, fit.hand.3)
fit.hand.4 = glmer(SchwaBin ~ Word + (1 | Discourse) + (1 | Speaker) + (1 | City), data = hand_coding, family = "binomial")
anova.hand.2.4 = anova(fit.hand.2, fit.hand.4)
anova.hand.3.4 = anova(fit.hand.3, fit.hand.4)
fit.hand.final = fit.hand.1

###########
# CLITICS #
###########

# plot speaker-specific frequencies
v.c.mono.data = tokens.pc[tokens.pc$Left == "V" & tokens.pc$Right == "C" & tokens.pc$Word == "monosyl",]
v.c.mono = table(v.c.mono.data$Speaker, v.c.mono.data$Schwa)
v.c.mono.guided = table(v.c.mono.data$Speaker[v.c.mono.data$Discourse == "guided"], v.c.mono.data$Schwa[v.c.mono.data$Discourse == "guided"])
v.c.mono.free = table(v.c.mono.data$Speaker[v.c.mono.data$Discourse == "free"], v.c.mono.data$Schwa[v.c.mono.data$Discourse == "free"])
v.c.mono.read = table(v.c.mono.data$Speaker[v.c.mono.data$Discourse == "read"], v.c.mono.data$Schwa[v.c.mono.data$Discourse == "read"])
v.c.mono = data.frame(Yes = v.c.mono[,"yes"], No = v.c.mono[,"no"], Prop = v.c.mono[,"yes"] / (v.c.mono[,"yes"] + v.c.mono[,"no"]))
v.c.mono.free = data.frame(Yes = v.c.mono.free[,"yes"], No = v.c.mono.free[,"no"], Prop = v.c.mono.free[,"yes"] / (v.c.mono.free[,"yes"] + v.c.mono.free[,"no"]))
v.c.mono.guided = data.frame(Yes = v.c.mono.guided[,"yes"], No = v.c.mono.guided[,"no"], Prop = v.c.mono.guided[,"yes"] / (v.c.mono.guided[,"yes"] + v.c.mono.guided[,"no"]))
v.c.mono.read = data.frame(Yes = v.c.mono.read[,"yes"], No = v.c.mono.read[,"no"], Prop = v.c.mono.read[,"yes"] / (v.c.mono.read[,"yes"] + v.c.mono.read[,"no"]))
postscript("v_c_mono.ps", paper = "special", horizontal = F, width = 4.5, height = 1.5, pointsize = 10)
par(mar = c(4, 3, 1, 2), mfrow = c(1, 3))
hist(v.c.mono.free$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), main = "Free conversation")
hist(v.c.mono.guided$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), main = "Guided conversation")
hist(v.c.mono.read$Prop, xlab = "Proportion schwa use", ylab = "Number of subjects", xlim = c(0, 1), main = "Read speech")
dev.off()

# fit models and do model comparison
fit.v.c.mono.1 = glmer(SchwaBin ~ (1 | Discourse) + (1 | Speaker), data = v.c.mono.data, family = "binomial")
fit.v.c.mono.2 = glmer(SchwaBin ~ (1 | Discourse), data = v.c.mono.data, family = "binomial")
anova.v.c.mono.1.2 = anova(fit.v.c.mono.1, fit.v.c.mono.2)
fit.v.c.mono.3 = glmer(SchwaBin ~ (1 | Discourse) + (1 | Speaker) + (1 | City), data = v.c.mono.data, family = "binomial")
fit.v.c.mono.4 = glmer(SchwaBin ~ (1 | Discourse) + (1 | City), data = v.c.mono.data, family = "binomial")
anova.v.c.mono.2.4 = anova(fit.v.c.mono.2, fit.v.c.mono.4)
anova.v.c.mono.1.3 = anova(fit.v.c.mono.1, fit.v.c.mono.3)
anova.v.c.mono.3.4 = anova(fit.v.c.mono.4, fit.v.c.mono.3)
anova.v.c.mono.1.4 = anova(fit.v.c.mono.1, fit.v.c.mono.4)
anova.v.c.mono.1.4 = anova(fit.v.c.mono.1, fit.v.c.mono.4)
fit.v.c.mono.final = fit.v.c.mono.3

#PLSC 501
#Spring 2024
#panel data
#dave clark



rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(faux)
library(mvtnorm)
library(sjPlot)
library(patchwork)
library(haven)
library(modelsummary)
library(kableExtra)
library(plm)
library(gganimate)



#simulate panel data ----
set.seed(8675309)
X <- data.frame(matrix(NA, nrow = 5, ncol = 0))
X <- X %>% mutate(ind = row_number()*10)
expand_r <- function(df, ...) {
  as.data.frame(lapply(df, rep, ...))
}
X <- expand_r(X, times = 4)
X <- data.frame(ind= sort(X$ind)) 

X <- X %>% mutate(x= row_number(), e=rnorm(nrow(X)))

X <- X %>% mutate(y = ind + x  + e)
pool <- lm(y ~ x , data = X) 
#summary(pool)
intercepts <- lm(y ~ x + as.factor(ind), data = X) 
#summary(intercepts)

poolfit <- predict(pool, X)
predictions <- data.frame(X, poolfit, predict(intercepts, interval = "confidence"))

#shared intercept ----
ggplot(predictions, aes(x = x, y = poolfit)) +
  geom_line(color="blue") + theme_minimal() + labs(title = "Pooled Data", subtitle = "All cross-sections share the same intercept", x="x", y="y") 



#different intercepts  ----
ggplot(predictions, aes(x = x, y = poolfit)) +
  geom_line(color="blue") + geom_point(aes(x=x, y=fit, group=ind)) + theme_minimal() + labs(title = "Pooled Data", subtitle = "Different intercepts", x="x", y="y")



#simpson's paradox ----
X <- X %>% mutate(ind=ind*-1, y = ind + x  + e)
pool <- lm(y ~ x , data = X)
#summary(pool)
intercepts <- lm(y ~ x + as.factor(ind), data = X)
#summary(intercepts)

poolfit <- predict(pool, X)
predictions <- data.frame(X, poolfit, predict(intercepts, interval = "confidence"))

#different intercepts, wrong slope
ggplot(predictions, aes(x = x, y = poolfit)) +
  geom_line(color="blue") + geom_point(aes(x=x, y=fit, group=ind)) + theme_minimal() + labs(title = "Simpson's Paradox", subtitle = "Different intercepts, wrong slope", x="x", y="y")

################-


#comparing pooled, within, between ----

states <- read_dta("/Users/dave/Documents/teaching/501/2023/slides/L8_panel/code/USstatedata.dta")

est <- states %>% dplyr::select(id, year, murder, unemp, prcapinc, south, hsdip) 

est <- est %>% group_by(id) %>% mutate(dmurder = murder-mean(murder, na.rm=TRUE), dunemp=unemp-mean(unemp, na.rm=TRUE), dprcapinc=prcapinc-mean(prcapinc, na.rm=TRUE), dhsdip=hsdip-mean(hsdip, na.rm=TRUE), dsouth = south-mean(south, na.rm=TRUE))

estT <- est %>% group_by(year) %>% mutate(dmurder = murder-mean(murder, na.rm=TRUE), dunemp=unemp-mean(unemp, na.rm=TRUE), dprcapinc=prcapinc-mean(prcapinc, na.rm=TRUE), dhsdip=hsdip-mean(hsdip, na.rm=TRUE), dsouth = south-mean(south, na.rm=TRUE))


pool <- lm(murder ~ unemp + prcapinc , data=states)
fe <- lm(murder ~ unemp + prcapinc +  factor(year) , data=states)
de <- lm(dmurder ~ dunemp + dprcapinc , data= estT)

modelsummary(list(fe,de))


#using plm library ; 
#structure - assumes first 2 columns are i,t and stacked; state explicitly using index=c(i,t). Effects/models - within is FE, default effect is demean units, equiv to LSDV on units; within, effect=time is one way FE as if between effects, demean time units, equiv to LSDV on time; within, effect =twoway is 2 way FE, equiv to 2 way LSDV

plmP <- plm::plm(murder ~ unemp + prcapinc, model="pooling" , data=est)
plmI <- plm::plm(murder ~ unemp + prcapinc, model="within" ,index=c("id", "year"), data=est)
plmIt <- plm::plm(murder ~ unemp + prcapinc,effect="time", model="within" ,index=c("id", "year"), data=est)
plmT <- plm::plm(murder ~ unemp + prcapinc, model="between" ,index=c("id", "year"),data=est)

modelsummary::modelsummary(list(plmIt, plmT, de))

# within - xs means
within <- states %>% group_by(id) %>% mutate(murdermean=mean(murder, na.rm=TRUE), unempmean=mean(unemp, na.rm=TRUE), prcapincmean=mean(prcapinc, na.rm=TRUE))

w1 <- lm(murdermean ~ unempmean+prcapincmean, data=within)
 
# kableExtra::kable(broom::tidy(de), digits=3, 
#       caption="Fixed effects in a subsample")



# separating effects ---- 

# built on https://nickch-k.github.io/EconometricsSlides/Week_06/Week_06_1_Within_Variation_and_Fixed_Effects.html; rmd at https://github.com/NickCH-K/EconometricsSlides/blob/master/Week_06/Week_06_1_Within_Variation_and_Fixed_Effects.Rmd

states %>%  ## quartiles: MA; WV; IL; NV
  filter(id %in% c(21,48,13, 28),
         assault < 500) %>%
  group_by(id) %>%
  mutate(label = case_when(
    murder == max(murder) ~ paste('State', id),
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x =  assault, y = murder, color = factor(id), label = label)) + 
  geom_point() + 
  geom_text(hjust = -.1, size = 14/.pt) + 
  theme_minimal() + 
  labs(x = 'Assaults per million', 
       y = 'Murder Rate') + 
  #scale_x_continuous(limits = c(.15, 2.5)) + 
  guides(color = FALSE, label = FALSE) + 
  scale_color_manual(values = c('black','blue','red','purple')) + 
  geom_smooth(method = 'lm', aes(color = NULL, label = NULL), se = FALSE)

#animate ----
stanimate <- states %>% 
  filter(id %in% c(21,48,13, 28),
         assault < 500) %>%
  mutate(allmur = mean(murder),
         allass = mean(assault)) %>%
  group_by(id) %>%
  mutate(label = case_when(
    murder == max(murder) ~ paste('State', id),
    TRUE ~ NA_character_
  ),
  mmur = mean(murder),
  mass = mean(assault),
  stage = '1. Raw Data')
stanimate <- stanimate %>%
  bind_rows(stanimate %>% 
              mutate(murder = murder - mmur + allmur,
                     assault = assault - mass + allass,
                     mmur = allmur,
                     mass = allass,
                     stage = '2. Remove all between variation'))

p <- ggplot(stanimate, aes(x =  assault, y = murder, color = factor(id))) + 
  geom_point() + 
  #geom_text(hjust = -.1, size = 14/.pt)  + 
  labs(x = 'Assaults per million', 
       y = 'Murder rate')+ 
  #scale_x_continuous(limits = c(.15, 2.5)) + 
  guides(color = FALSE, label = FALSE) + 
  scale_color_manual(values = c('black','blue','red','purple')) + 
  geom_smooth(aes(color = NULL),method = 'lm', se = FALSE)+
  geom_point(aes(x = mass, y = mmur), size = 20, shape = 3, color = 'darkorange') + 
  transition_states(stage) + 
  theme_minimal()
p
animate(p, nframes = 180)



# between removing all within variation  ----
xstates <- states %>% group_by(statename) %>% summarise(bmurder=mean(murder, na.rm=TRUE), bassault=mean(assault, na.rm=TRUE))

ggplot(data=xstates, aes(x=bassault, y=bmurder)) +
  geom_point()+
  # guides(color = FALSE) + 
  # scale_color_manual(values = c('black','blue','red','purple')) + 
  geom_smooth(method = 'lm', aes(color = NULL, label = NULL), se = FALSE)+    
  geom_text_repel(aes(label=statename),size=2.5, force=5) +
  labs(x="Assaults per 100,000", y="Murders per 100,000")+
  ggtitle("Between model")

# within removing all between
tstates <- states %>% group_by(year) %>% summarise(wmurder=mean(murder, na.rm=TRUE), wassault=mean(assault, na.rm=TRUE))

ggplot(data=tstates, aes(x=wassault, y=wmurder)) +
  geom_point()+
  # guides(color = FALSE) + 
  # scale_color_manual(values = c('black','blue','red','purple')) + 
  geom_smooth(method = 'lm', aes(color = NULL, label = NULL), se = FALSE)+    
  geom_text_repel(aes(label=year),size=2.5, force=5) +
  labs(x="Assaults per 100,000", y="Murders per 100,000")+
  ggtitle("Within model")



#notes ----

# clarify between/within language in slides; the "within" model is FE; it's account for timewise variation by controlling out xsectional variation correlated with the X variables either by intercepts (estimation) or by demeaning. Remaining variation is timewise. Note that time FE would be opposite, but terms remain same as "within" model. 

# show pool, LSDV, FE, demeaning for comparison. 

# look at Nick's animation of between and within - it's really good - https://nickch-k.github.io/EconometricsSlides/Week_06/Week_06_1_Within_Variation_and_Fixed_Effects.html; rmd at https://github.com/NickCH-K/EconometricsSlides/blob/master/Week_06/Week_06_1_Within_Variation_and_Fixed_Effects.Rmd

data(crime4)
crime4 %>%
  select(county, year, crmrte, prbarr) %>%
  rename(County = county,
         Year = year,
         CrimeRate = crmrte,
         ProbofArrest = prbarr) %>%
  slice(1:9) %>%
  knitr::kable(note = '...') %>%
  kableExtra::add_footnote('9 rows out of 630. "Prob. of Arrest" is estimated probability of being arrested when you commit a crime', notation = 'none')

crime4 %>% 
  filter(county %in% c(1,3,7, 23),
         prbarr < .5) %>%
  group_by(county) %>%
  mutate(label = case_when(
    crmrte == max(crmrte) ~ paste('County',county),
    TRUE ~ NA_character_
  ),
  mcrm = mean(crmrte),
  mpr = mean(prbarr)) %>%
  ggplot(aes(x =  prbarr, y = crmrte, color = factor(county), label = label)) + 
  #geom_point() + 
  #geom_text(hjust = -.1, size = 14/.pt) + 
  #theme_metro_regtitle() + 
  labs(x = 'Probability of Arrest', 
       y = 'Crime Rate',
       caption = 'One outlier eliminated in County 7.') + 
  #scale_x_continuous(limits = c(.15, 2.5)) + 
  guides(color = FALSE, label = FALSE) + 
  scale_color_manual(values = c('black','blue','red','purple')) + 
  geom_point(aes(x = mpr, y = mcrm), size = 20, shape = 3, color = 'darkorange') + 
  geom_smooth(aes(color = NULL), method = 'lm', se = FALSE)+
  annotate(geom = 'text', x = .3, y = .02, label = 'OLS Fit on These Four Points', color = 'blue', size = 14/.pt)


crime4 %>% 
  filter(county %in% c(1,3,7, 23),
         prbarr < .5) %>%
  group_by(county) %>%
  mutate(label = case_when(
    crmrte == max(crmrte) ~ paste('County',county),
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x =  prbarr, y = crmrte, color = factor(county), label = label)) + 
  geom_point() + 
  geom_text(hjust = -.1, size = 14/.pt) + 
 # theme_metro_regtitle() + 
  labs(x = 'Probability of Arrest', 
       y = 'Crime Rate',
       caption = 'One outlier eliminated in County 7.') + 
  #scale_x_continuous(limits = c(.15, 2.5)) + 
  guides(color = FALSE, label = FALSE) + 
  scale_color_manual(values = c('black','blue','red','purple')) + 
  geom_smooth(method = 'lm', aes(color = NULL, label = NULL), se = FALSE)




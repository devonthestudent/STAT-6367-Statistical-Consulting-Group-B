confirmed <- read.csv('United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv')

# 50 states' name 
state_name <- state.abb
state_data <- list()


for (i in 1:50){
  data_one <- confirmed[confirmed$state == state_name[i],  ]
  
  data_one$submission_date <- as.Date(data_one$submission_date, "%m/%d/%Y")
  
  data_sorted_one <- data_one[order(data_one$submission_date), ]
  
  data_one_filter <- data_sorted_one[data_sorted_one$tot_cases >=200, ]
  
  data_one_filter$death_rate <- data_one_filter$tot_death/data_one_filter$tot_cases
  
  state_data[[state_name[i]]] <- data_one_filter
  
  print(dim(data_one_filter)[1])
}

# plot state's total cases, death cases and death rate
plot_state <- 'CA'
plot(state_data[[plot_state]]$tot_cases, type = 'l', main=plot_state, ylab = 'total confirmed cases', xaxt = 'n')
axis(1, at=1:dim(state_data[[plot_state]])[1], labels=state_data[[plot_state]]$submission_date)
plot(state_data[[plot_state]]$tot_death, type = 'l', main=plot_state, ylab = 'total death cases', xaxt = 'n')
axis(1, at=1:dim(state_data[[plot_state]])[1], labels=state_data[[plot_state]]$submission_date)
plot(state_data[[plot_state]]$death_rate, type = 'l', main=plot_state, ylab = 'death rate',  xaxt = 'n')
axis(1, at=1:dim(state_data[[plot_state]])[1], labels=state_data[[plot_state]]$submission_date)

# average every 7 days
death_rate_average <- matrix(NA, ncol=50, nrow = 50)
begin_date <- numeric(50)
names(begin_date) <- state_name

for (i in 1:50){
  for (j in 1:floor(dim(state_data[[state_name[i]]])[1] /7)){
    death_rate_average[i, j] <- mean(state_data[[state_name[i]]]$death_rate[(7*(j-1)+1) : (7*j)])
  }
  begin_date[state_name[i]] <- state_data[[state_name[i]]]$submission_date[1]
}

rownames(death_rate_average) <- state_name
plot(death_rate_average[plot_state, ], ylab= 'death rate', xlab = 'week', type = 'l')

# Calculate ACF and PACF
state_acf <- matrix(0, nrow = 50, ncol = 17)
state_pacf <- matrix(0, nrow = 50, ncol = 16)
for (i in 1:50){
  state_acf[i, ] <- acf(death_rate_average[i,], plot = FALSE, na.action = na.pass)$acf
  state_pacf[i, ] <- pacf(death_rate_average[i,], plot = FALSE, na.action = na.pass)$acf
}

# plot ACF and PACF
plot(abs(state_pacf[1,]), type='o', ylim = c(0, 1))
for (i in 2:50){
  lines(1:16, abs(state_pacf[i,]),  type='o')
}
abline(h=2/sqrt(50), col = 'red')

library(plotly)

## Add trace directly here, since plotly adds a blank trace otherwise
p <- plot_ly(type = "scatter",
             mode = "lines", 
             x = 1:16,
             y = rep(2/sqrt(50), 16),
             visible = T)
p <- p %>% add_lines(x = 1:16, y = -rep(2/sqrt(50), 16), visible = T)
## Add arbitrary number of traces
## Ignore first col as it has already been added
for (i in 1:50) {
  p <- p %>% add_lines(x = 1:16, y = state_pacf[i,], name = state_name[i], visible = T)
}

mybutton <- function(i){
  if (i > 0){
  list(method="restyle", 
       args = list(list( y = list(state_pacf[i,]), mode = list("lines+markers"))),
       label = state_name[i])}
  else {
    list(method="restyle", 
         args = list(list( y = lapply(1:50, function(i){state_pacf[i,]}), mode = list("lines"))),
         label = 'All')
  }
  # args = list( list( y = list(myData$Y1), mode = list("lines")))
}

p <- p %>%
  layout(
    title = "pACF",
    xaxis = list(title = "lag"),
    yaxis = list(title = "pACF"),
    updatemenus = list(
      list(
        y = 0.7,
        ## Add all buttons at once
        buttons = lapply(0:50, mybutton)
      )
    )
  )
p

# hospital capacity data
hospital <- read.csv('covid19-NatEst.csv')[2:5033,]
hospital$collectionDate <- as.Date(hospital$collectionDate, "%d%b%Y")
for (i in colnames(hospital)[4:24]){
  hospital[, i] <- as.numeric(hospital[, i])
}
hospital_CA <- hospital[hospital$state=='CA',]
hospital <- hospital[,c('state','collectionDate', 'InBedsOccCOVID__Numbeds_Est', 'ICUBeds_Occ_AnyPat_Est')]

# calculate correlation
hospital_average <- matrix(NA, nrow = 50, ncol = 50) 
hospital_average2 <- matrix(NA, nrow = 50, ncol = 50) 
rownames(hospital_average) <- state_name
rownames(hospital_average2) <- state_name
for (i in 1:50){
  mdate <- begin_date[state_name[i]] 
  hospital_state <-  hospital[hospital$state==state_name[i],]
  for (j in 1:length(death_rate_average[state_name[i],])){
    if (mdate >= min(hospital_state$collectionDate) & mdate < max(hospital_state$collectionDate)){
      print(mdate)
      mm <- min(mdate + 6, max(hospital_state$collectionDate))
      hospital_average[state_name[i], j] <- mean(hospital_state$InBedsOccCOVID__Numbeds_Est[hospital_state$collectionDate >= mdate & hospital_state$collectionDate <= mm])
      hospital_average2[state_name[i], j] <- mean(hospital_state$ICUBeds_Occ_AnyPat_Est[hospital_state$collectionDate >= mdate & hospital_state$collectionDate <= mm])
    }
    mdate <- mdate + 7
  }
}

# other covariates
covarites <- read.csv('project_covariates.csv')
covarites <- covarites[-1,]
rownames(covarites) <- covarites$NAME
covarites <- covarites[state.name,-1]

for (i in 1:dim(covarites)[2]){
  covarites[, i] <- as.numeric(as.character(covarites[, i]))
}
covarites <- scale(covarites)

# health insurance
health <- read.csv('E://health_insurance.csv')
rownames(health) <- health$state
health <- health[state.name, 'health']

# AR(p) model
y <- death_rate_average
nn <- numeric(50)
for (i in 1:50){
  nn[i] <- min(which(is.na(y[i,])==TRUE)) -1
  
}


#nn[i] <- length(y[is.na(y) == FALSE])
xx <- array(0, dim=c(50, 50, 18))
for (i in 1:50){
  
  x <- covarites[rep(state.name[i], 50), ] #  matrix(rnorm(dim(covarites[rep(state.name[i], n), -1])[2]*dim(covarites[rep(state.name[i], n), -1])[1], 0,1), ncol=dim(covarites[rep(state.name[i], n), -1])[2], nrow= dim(covarites[rep(state.name[i], n), -1])[1])
  x <- cbind(x, matrix(hospital_average[state_name[i],1:50], ncol = 1),  matrix(hospital_average2[state_name[i],1:50], ncol = 1), matrix(health[i], nrow=50, ncol = 1))
  x[is.na(x)] <- 0
  x <- as.matrix(x)
  xx[i, ,] <- x
}

# rjags
coeff <- matrix(0, ncol = 18, nrow = 50)
rownames(coeff) <- state_name


model1 <- 'model {
 for (i in 1:50){
    for (w in 1:W[i]){ # W weeks
      y[i,w] ~ dnorm(theta[i,w], tau)
    
      mu[i,w] <- inprod(beta[i,], x[i,w,])
    }
    theta[i,1]<-mu[i,1] # week 0
    for (w in 2:W[i]) {
      theta[i,w] <- mu[i,w]+gamma*(y[i,w-1]-mu[i,w-1])
    }}
  # Priors
  #alpha ~ dnorm(0, 0.001)
  for (i in 1:18){
  for (j in 1:50){beta[j,i] ~ dnorm(eta[i],tau_i[i])}}
  for (i in 1:18){
  eta[i] ~ dnorm(0,0.001)
  tau_i[i] ~ dgamma(0.001,0.001)
  }
  gamma ~ dnorm(0,0.001)
  tau ~ dgamma(0.001,0.001)
  sigma.sq <- 1/tau # Normal errors
}'


W <- nn

model_2 <- jags.model(textConnection(model1), data = list(y = y, x = xx, W= W), n.chains = 3)

update(model_2, 5000, progress.bar="none"); # Burnin for 10000 samples
samp_2 <- coda.samples(model_2, 
                       variable.names=c('eta'), 
    
                                         n.iter=10000, progress.bar="none")

k <- 0
for (j in 1:18){
for (i in 1:50){
 k <- k +1
 coeff[i,j] <-mean(c(samp_2[[3]][,k], samp_2[[2]][,k], samp_2[[1]][,k]))
}
}

write.csv(data.frame(coeff), 'coeff.csv')



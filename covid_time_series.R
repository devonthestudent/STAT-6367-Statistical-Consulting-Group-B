confirmed <- read.csv('desktop/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv')

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

for (i in 1:50){
  for (j in 1:floor(dim(state_data[[state_name[i]]])[1] /7)){
    death_rate_average[i, j] <- mean(state_data[[state_name[i]]]$death_rate[(7*(j-1)+1) : (7*j)])
  }
  begin_date <- state_data[[state_name[i]]]$submission_date[1]
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
hospital <- read.csv('desktop/covid19-NatEst.csv')
hospital$collectionDate <- as.Date(hospital$collectionDate, "%d%b%Y")
for (i in colnames(hospital)[4:25]){
  hospital[, i] <- as.numeric(hospital[, i])
}
hospital_CA <- hospital[hospital$state=='CA',]
hospital <- hospital[,c('state','collectionDate', 'InBedsOccCOVID__Numbeds_Est', 'ICUBeds_Occ_AnyPat_Est')]

library(corrplot)
library(RColorBrewer)
M <-cor(hospital[, c("InpatBeds_Occ_AnyPat_Est" ,"InpatBeds_Occ_AnyPat_Est_Avail","InBedsOccAnyPat__Numbeds_Est", "InpatBeds_Occ_COVID_Est", "InBedsOccCOVID__Numbeds_Est", "ICUBeds_Occ_AnyPat_Est" , "ICUBeds_Occ_AnyPat_Est_Avail")])
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

hospital <- hospital[,c('state','collectionDate', 'InBedsOccCOVID__Numbeds_Est', 'ICUBeds_Occ_AnyPat_Est')]

# calculate correlation
corr1 <- numeric(50)
corr2 <- numeric(50)
for (i in 1:50){
  mydate <- min(hospital$collectionDate[hospital$state == state_name[i]])
  ii <- min(which(state_data[[state_name[i]]]$submission_date == mydate), 1)
  print(state_name[i])
  jj <- dim(hospital[hospital$state == state_name[i],])[1]
  
  x1 <- state_data[[state_name[i]]]$death_rate[ii:(ii + jj - 1)]
  y1 <- hospital$InBedsOccCOVID__Numbeds_Est[hospital$state == state_name[i]]
  y2 <- hospital$ICUBeds_Occ_AnyPat_Est[hospital$state == state_name[i]]
  corr1[i] <- cor(x1, y1)
  corr2[i] <- cor(x1, y2)
}
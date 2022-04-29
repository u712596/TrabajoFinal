install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(gapminder)
library(ggplot2)
library(DBI)
######################################################## Connection to MYSQL Section #############################################
#db <- dbConnect(RMySQL::MySQL(),
#                dbname = "mibd",
#                host = "ec2-54-211-72-127.compute-1.amazonaws.com
#                user = "usuario",
#                password = rstudioapi::askForPassword("Database password"),
#                Port = 3306)

#dfec2 <- dbGetQuery(db, 'SELECT * FROM alcohol')
#dfec2

#Nota el host podria cambiar debido a la naturaleza dinamica del servicio DNS de EC2 de AWS Academy
#####################################################Conecction to MYSQL Section###################################################
url <- "https://github.com/imsharvanj/Dr.-Semmelweis-and-the-discovery-of-handwashing/raw/master/notebook%20and%20datasets/datasets/yearly_deaths_by_clinic.csv"

yearly <- read_csv(url)
yearly <- yearly %>% mutate(proportion_deaths = deaths/births)
yearly

# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)

# Plot yearly proportion of deaths at the two clinics
# .... YOUR CODE FOR TASK 3 ....

ggplot(yearly,aes(x=year,y=proportion_deaths,color=clinic)) + geom_line()


monthly = read_csv("https://raw.githubusercontent.com/islamtaha/DataCamp/master/Dr.%20Semmelweis%20and%20the%20discovery%20of%20handwashing/datasets/monthly_deaths.csv")
monthly

monthly <- monthly %>% mutate(proportion_deaths = deaths/births)
print(monthly)

# Plot monthly proportion of deaths
# ... YOUR CODE FOR TASK 5 ...
ggplot(monthly, aes(x=date,y =proportion_deaths)) + geom_line() + labs(x="Year", y="Proportion_Deaths")


# From this date handwashing was made mandatory
handwashing_start = as.Date('1847-06-01')

# Add a TRUE/FALSE column to monthly called handwashing_started
# .... YOUR CODE FOR TASK 6 ....
monthly <- monthly %>% mutate(handwashing_started = (date >= handwashing_start))


# Plot monthly proportion of deaths before and after handwashing
# .... YOUR CODE FOR TASK 6 ....
ggplot(monthly, aes(x=date, y = proportion_deaths,color=handwashing_started)) + geom_line() + labs(x = "Year", y = "Proportion of Deaths")

# Calculating the mean proportion of deaths 
# before and after handwashing.

monthly_summary <- monthly %>% group_by(handwashing_started) %>% summarize(mean_proportion_deaths = mean(proportion_deaths))
# .... YOUR CODE FOR TASK 7 HERE ....

# Printing out the summary.
monthly_summary
print(monthly_summary)

# Calculating a 95% Confidence interval using t.test 
test_result <- t.test( proportion_deaths ~ handwashing_started, data = monthly)
test_result

# The data Semmelweis collected points to that:
doctors_should_wash_their_hands <- TRUE
doctors_should_wash_their_hands

#Alcohol Consumption in Russia Dataset
#Project Brief
#Your company owns a chain of stores across Russia that sell a variety of types of alcohol. 
#The company recently ran a wine promotion in Saint Petersburg that was very successful. 
#Due to the cost to the business, it is not possible to run the promotion in all regions. 
#The marketing team would like to target 10 other regions that have similar buying habits to Saint Petersburg where they would expect the promotion to be similarly successful and need help determining which regions they should select.

#Data
#Dataset has 1615 rows and 7 columns. Keys for columns:
  
# "year" - year (1998-2016)

#"region" - name of a federal subject of Russia. It could be oblast, republic, krai, autonomous okrug, federal city and a single autonomous oblast

#"wine" - sale of wine in litres by year per capita

#"beer" - sale of beer in litres by year per capita
#"vodka" - sale of vodka in litres by year per capita

#"champagne" - sale of champagne in litres by year per capita

#"brandy" - sale of brandy in litres by year per capita

alcohol <- read_csv("https://raw.githubusercontent.com/datacamp/careerhub-data/master/Alcohol%20Consumption%20in%20Russia/alcohol-consumption-in-russia.csv")
alcohol
str(alcohol)
summary(alcohol)
ndatos <- nrow(alcohol)
ndatos
#===============================================================================
#  DATOS PERDIDOS
#===============================================================================

# install.packages("VIM")
library(VIM)

# Mostrar cuales columnas tienen valores perdidos
cidx_perd <- which(colSums(is.na(alcohol))!=0)
cidx_perd

# Cantidad de valores perdidos en las columnas
nperdidos <- colSums(is.na(alcohol[,cidx_perd]))
nperdidos

# Porcentaje de valores perdidos en las columnas
pperdidos <- 100*nperdidos/ndatos
pperdidos

# Grafico de agregacionn: "aggregation plot"
agreg <- aggr(alcohol, numbers=TRUE)
agreg
summary(agreg)
# Ejemplo de visualizacion diferente: ordenado segun valores faltantes
aggr(alcohol, numbers=TRUE, sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)

# Grafico de Matriz ("Matrix plot") ... En RStudio previamente usar: x11()
matrixplot(alcohol) 


# Boxplots paralelos ("Parallel boxplots")
VIM::pbox(alcohol[3:7], pos=1)    # pos=1 indica que se desea mostrar la variable 1

### Al tener un total de datos perdidos para cada producto menor al 4% la opcion de eliminar los datos perdidos tiene 
### conformidad con las buenas practicas de imputacion de datos 

alcohol <- na.omit(alcohol)
summary(alcohol)

##San Petersburg Alcohol Sales 

ventaSaintPetersburg <- alcohol %>% filter(region == "Saint Petersburg") %>% group_by(year) %>% select(region,wine,beer,vodka,champagne,brandy)
ventaSaintPetersburg

View(ventaSaintPetersburg)

#Evolucion de Ventas Sant Petersburg

wine <-  ggplot(ventaSaintPetersburg,aes(x=year,y= wine )) + geom_line()
beer <-   ggplot(ventaSaintPetersburg,aes(x=year,y= beer )) + geom_line()
vodka <-   ggplot(ventaSaintPetersburg,aes(x=year,y= vodka )) + geom_line()
champagne <-  ggplot(ventaSaintPetersburg,aes(x=year,y= champagne )) + geom_line()
brandy <-  ggplot(ventaSaintPetersburg,aes(x=year,y= brandy )) + geom_line()

# install.packages('ggpubr')
library(ggpubr)

final_plot <- annotate_figure(
  ggarrange(wine, beer, vodka, champagne,brandy, ncol=2, nrow=3),
  top = text_grob("Venta Saint Petersburgo Products by Year", size = 20))
final_plot

##Wine Sales Analysis

winesalesyearly <- alcohol %>% group_by(year) %>% select(year,region,wine)  %>%top_n(1,wine)
winesalesyearly

topwinesalesyearly<- ggplot(winesalesyearly,aes(x=year,y= wine, color = winesalesyearly$region )) +labs(title = "Top Region Sales of Wine Per year ") + geom_point()
topwinesalesyearly  

df1 <- alcohol %>% group_by(year) %>% select(year,region,wine)
df1
df1yearly <- ggplot(df1,aes(x=year,y= wine )) + geom_col() + labs(title ="Top Wine sales per Year")
df1yearly

##beer Sales Analysis
beersalesyearly <- alcohol %>% group_by(year) %>% select(year,region,beer)  %>%top_n(1,beer) 
beersalesyearly

topbeersalesyearly<- ggplot(beersalesyearly,aes(x=year,y= beer, color = beersalesyearly$region )) +labs(title = "Top Region Sales of beer Per year ") + geom_point()
topbeersalesyearly  

df2 <- alcohol %>% group_by(year) %>% select(year,region,beer)
df2
df2yearly <- ggplot(df2,aes(x=year,y= beer )) + geom_col() + labs(title ="Top Beer sales per Year")
df2yearly
##vodka Sales Analysis

vodkasalesyearly <- alcohol %>% group_by(year) %>% select(year,region,vodka)  %>%top_n(1,vodka) 
vodkasalesyearly

topvodkasalesyearly<- ggplot(vodkasalesyearly,aes(x=year,y= vodka, color = vodkasalesyearly$region )) +labs(title = "Top Region Sales of vodka Per year ") + geom_point()
topvodkasalesyearly  

df3 <- alcohol %>% group_by(year) %>% select(year,region,vodka)
df3
df3yearly <- ggplot(df3,aes(x=year,y= vodka )) + geom_col() + labs(title ="Top Vodka sales per Year")
df3yearly

##Champagne sales Analysis

champagnesalesyearly <- alcohol %>% group_by(year) %>% select(year,region,champagne)  %>%top_n(1,champagne) 
champagnesalesyearly

topchampagnesalesyearly<- ggplot(champagnesalesyearly,aes(x=year,y= champagne, color = champagnesalesyearly$region )) +labs(title = "Top Region Sales of champagne Per year ") + geom_point()
topchampagnesalesyearly  

df4 <- alcohol %>% group_by(year) %>% select(year,region,champagne)
df4
df4yearly <- ggplot(df4,aes(x=year,y= champagne )) + geom_col() + labs(title ="Top Champagne sales per Year")
df4yearly

##Brandy Sales Analysis


brandysalesyearly <- alcohol %>% group_by(year) %>% select(year,region,brandy)  %>%top_n(1,brandy) 
brandysalesyearly

topbrandysalesyearly<- ggplot(brandysalesyearly,aes(x=year,y= brandy, color = brandysalesyearly$region )) +labs(title = "Top Region Sales of brandy Per year ") + geom_point()
topbrandysalesyearly  

df5 <- alcohol %>% group_by(year) %>% select(year,region,brandy)
df5
df5yearly <- ggplot(df5,aes(x=year,y= brandy )) + geom_col() + labs(title ="Top Brandy sales per Year")
df5yearly

################
# GRAHPS SECTION
################

par(mfrow = c(1,1))

boxplot(alcohol[c(3:7)])
pairs(alcohol[c(3:7)]) 

# Outliers usando la Puntuación Z
# """""""""""""""""""""""""""""""

is.outlier_z <- function(x, k=2) {
  return(abs(scale(x)) > k)           # scale: (x-media)/desv_est
}


# Boxplots para 5 producto (de manera independiente)

# Índices (T/F) de los wine atípicos
idx_outliers_z <- is.outlier_z(alcohol$wine, k=3)
which(idx_outliers_z)


# wine atípicos
alcohol$wine[idx_outliers_z]

# Registros asociados con los wine atípicos
alcohol[idx_outliers_z, ]
alcoholdata <- alcohol %>% select(wine,beer,vodka,champagne,brandy)
alcoholdata

####################################################### Section to be Reviewed ######################################################
p1 <- alcoholdata %>%
  mutate(outlier = ifelse(is.outlier_z(wine), Type, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = wine)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
p1

# Índices (T/F) de los wine atípicos
idx_outliers_z <- is.outlier_z(alcohol$beer, k=3)
which(idx_outliers_z)

# beer atípicos
alcohol$beer[idx_outliers_z]

# Registros asociados con los wine atípicos
alcohol[idx_outliers_z, ]

p2 <- alcoholdata %>%
  mutate(outlier = ifelse(is.outlier_z(beer), Type, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = beer)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
p2

# Índices (T/F) de los wine atípicos
idx_outliers_z <- is.outlier_z(df$Na, k=3)
which(idx_outliers_z)

# wine atípicos
df$Na[idx_outliers_z]

# Registros asociados con los wine atípicos
df[idx_outliers_z, ]

p3 <- df %>%
  mutate(outlier = ifelse(is.outlier_z(Mg), Type, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = Mg)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
p3

# Índices (T/F) de los Mg  atípicos
idx_outliers_z <- is.outlier_z(df$Mg, k=3)
which(idx_outliers_z)

# Mg atípicos
df$Mg[idx_outliers_z]

# Registros asociados con los Mg atípicos
df[idx_outliers_z, ]

p4 <- df %>%
  mutate(outlier = ifelse(is.outlier_z(Al), Type, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = Al)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p4

# Índices (T/F) de los Al  atípicos
idx_outliers_z <- is.outlier_z(df$Al, k=3)
which(idx_outliers_z)

# Al atípicos
df$Al[idx_outliers_z]

# Registros asociados con los Al atípicos
df[idx_outliers_z, ]

p5 <- df %>%
  mutate(outlier = ifelse(is.outlier_z(Si), Type, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = Si)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p5

# Índices (T/F) de los Si  atípicos
idx_outliers_z <- is.outlier_z(df$Si, k=3)
which(idx_outliers_z)

# Si atípicos
df$Si[idx_outliers_z]

# Registros asociados con los Si atípicos
df[idx_outliers_z, ]

##################################################### Section to be reviewed ################################################

######################################################Analysis de Atypical Values ##########################################

# Selección de algunas columnas de interés
dfa  <- alcohol[c(3:7)]
dfa
# Distancia de Mahalanobis
dm2 <- mahalanobis(dfa, colMeans(dfa), cov(dfa))
barplot(dm2, main="Mahalanobis")
which.max(dm2)

# Distribución Chi-Cuadrado: Punto de Corte 
p <- 1-0.001
dof = ncol(dfa)
k <- (qchisq(p, dof))
print("el valor de k es: ")
k
idx_outliers <- which(dm2 > k)
idx_outliers
dfa[idx_outliers,]         # Registros con valores atípicos

# Gráfico de Ojiva
plot(sort(dm2), ppoints(nrow(dfa)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p,dof), col = "red")

# QQ-plot:
x <- qchisq(ppoints(nrow(dfa)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot para"~~{chi^2}[nu==6]))
abline(0, 1, col="red")

# Exclusión de valores atípicos (un valor atípico)
idx_excluido <- which.max(dm2)    
dfa[idx_excluido, ]
dfa_clean <- dfa[-idx_excluido, ]

# Distancia de Mahalanobis
dm2 <- mahalanobis(dfa_clean, colMeans(dfa_clean), cov(dfa_clean))
plot(sort(dm2), ppoints(nrow(dfa_clean)), xlab="DM al cuadrado ordenada",
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p,dof), col = "red")

idx_outliers <- which(dm2 > k)
idx_outliers

# QQ-plot dfa_clean:
x <- qchisq(ppoints(nrow(dfa_clean)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot para"~~{chi^2}[nu==6]))
abline(0, 1, col="red")


###########################################Shiny Section###################################################################
library(shiny)
library(tidyverse)

# Leer datos
dfs <- read_csv("alcohol_union.csv")
# Eliminar datos faltantes
dfs = drop_na(dfs)


# Interfaz de usuario
ui <- fluidPage(
  
  # Titulo de la aplicacion
  titlePanel("Consumo de Alcohol - Rusia"),
  
  # Barra con una barra deslizadora para ingresar el número de intervalos
  sidebarLayout(
    sidebarPanel(
      sliderInput("nanio", "Anio de consumo:", value = 1998, min = 1998, max = 2016)
    ),
    
    # Grafico de la distribucion general
    mainPanel( plotOutput("plot_congreso") )
  )
  
)

# Logica del servidor
server <- function(input, output) {
  
  output$plot_congreso <- renderPlot({
    
    ggplot(
      filter(dfs, anio == input$nanio),
      aes(x = consumo, color = marca, fill=marca))+
      geom_density(alpha = 0.5)+
      xlim(-1.5, 1.5)+
      xlab("Consumo - Valor nominal")+
      ylab("Densidad")+
      scale_fill_manual(values = c("gray", "green", "purple", "orange", "blue"))+
      scale_color_manual(values = c("gray", "green", "purple", "orange", "blue"))
  })
  
}


shinyApp(ui, server)
## Anova Test and Tukey

## Pacotes
pacotes <- c("viridis","ggplot2","dplyr","tidyverse",
             "data.table","tidyr","tibble",
             "ggpubr","openxlsx","viridisLite","plotly","hrbrthemes","dgof",
             "agricolae","scales","cli","powerjoin",
             "multcompView","psych","ggpubr","rstatix",
             "ggrepel","magrittr","FSA","mosaic","vctrs")

pak::pkg_install("tidyverse")

## Instalador automatico de pacotes

if(sum(as.numeric(!pacotes %in% installed.packages()))!= 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in 1: length(instalador)) {
    install.packages(instalador,dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes,require,character = T)
}


## Bring the data
dados <- read.csv(file = "C:\\Users\\thale\\Desktop\\20221230_Anova.csv"
                  ,header = TRUE, sep = ";")

## Check the dispersion per State
names(dados) <- c("UF","Preco")
dataset <- dados %>%
  dplyr::filter(Preco > 0)
dados %>%
  ggplot(aes(x = dados$UF, y = dados$Preco)) +
  #coord_cartesian(ylim = c(0,150)) +
  scale_y_continuous() +
  geom_point(aes(fill=UF, alpha = 0.9), size=1, shape=21, colour="grey20",
             position=position_jitter(width=0.2, height=0)) + 
  geom_boxplot(alpha=0.5 , outlier.shape = NA, colour = "orange") + # , scale = "width" 
  #geom_boxplot(aes(fill = Segmento), alpha = 0.8, outlier.shape = NA) + # , scale = "width" 
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  theme(axis.title=element_text(size=10),
        axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 10),
        strip.text.y = element_text(size = 10), legend.position="")+
  #labs(title = "Dispersao de precos por Segmento") +
  ylab("Preco por UF") + xlab("")


## Remove outliers
df <- dados

result <- by(df,df$UF,function(df) {
  qnt <- quantile(df$Preco, probs=c(.25, .75))
  H <- 1.5 * IQR(df$Preco)
  outlierCheck <- (df$Preco) > qnt[1]-H & (df$Preco<qnt[2]+H)
  noOutliers <- df[outlierCheck,]
})

newdf <- do.call("rbind",result)
newdf

newdf$UF <- as.factor(newdf$UF)
str(newdf)

meanpreco <- aggregate(newdf$Preco, list(newdf$UF), FUN=mean)
sdpreco <- aggregate(newdf$Preco, list(newdf$UF), FUN=sd)
countprice <- aggregate(newdf$Preco, list(newdf$UF), FUN=length)

## Add empty column and rename columns
meanpreco <- cbind(meanpreco, empty1=NA,empty2=NA)
meanpreco$empty1 <- sdpreco$x
meanpreco$empty2 <- countprice$x 
            
summarytable <- meanpreco

colnames(summarytable)[1] ="UF"
colnames(summarytable)[2] ="AvgPrice"
colnames(summarytable)[3] ="SDPrice"
colnames(summarytable)[4] ="CountObs"
  
## Summary da anova
## Teste de hipotese da anova
##H0: Nao existe diferenca entre as medias p valor > 0.05
##H1: H1 pelo menos um grupo com media diferente p valor < 0.05
anova <- aov(newdf$Preco~newdf$UF, data = newdf)
anova
summary(anova)

## F value 2.48
## Need to check in the table how much is F value(F critico)
qf(0.95,df1 = 3, df2 = 26)

##Pressupostos da anova
##As observalcoes em cada grupo devem possuir uma distribuicao, aproximadamente normal;
##As variancias em cada grupo devem ser aproximadamente iguais.
shapiro.test(resid(anova))

##Result > 0.05 we can assume that the normality

### Testando a homogeneidade das variâncias através do teste de
### Bartlett. 
bartlett.test(newdf$Preco ~ newdf$UF, newdf)

##Result > 0.05 we can assume that there is homogenity in the variance

## Tukey Test
TUKEY <- TukeyHSD(anova)
print(TUKEY)
plot(TukeyHSD(anova, conf.level=.95), las = 2)
tukey.test2 <- HSD.test(anova, trt = 'newdf$UF')
tukey.test2

# letras do resultado da anova
letras_tukey <- multcompLetters4(anova,TUKEY)
print(letras_tukey)

## Criando tabela com os resultados do tukey
tb_tukey <- summarytable
letras_tukey <- as.data.frame.list(letras_tukey$`newdf$UF`)
tb_tukey2 <- letras_tukey$Letters
print(tb_tukey2)

## Generate labels
generate_label_df <- function(TUKEY, variable){
Tukey.levels <- TUKEY[[variable]][,4]
Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
Tukey.labels$treatment=rownames(Tukey.labels)
Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
return(Tukey.labels)
}

LABELS <- generate_label_df(TUKEY , "newdf$UF")

my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

a <- boxplot(newdf$Preco~newdf$UF,ylim=c(min(newdf$Preco) , 1.1*max(newdf$Preco)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="Price" , main="")
over <- 0.1*max( a$stats[nrow(a$stats),] )
text( c(1:nlevels(newdf$UF)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )

## Summary de kruskal wallis
## Teste de hipotese de kruskal wallis
##H0: Nao existe diferenca entre as medias p valor > 0.05
##H1: H1 pelo menos um grupo com media diferente p valor < 0.05
kruskal.test(newdf$Preco~newdf$UF,
             data = newdf
)

## Summary de  DunnTest
## Teste de hipotese de Dunn
##H0: Nao existe diferenca entre as medias p valor > 0.05
##H1: H1 pelo menos um grupo com media diferente p valor < 0.05
dunnTest(newdf$Preco~newdf$UF,
         data = newdf,
         method = "holm"
)

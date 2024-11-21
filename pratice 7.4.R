rairuoho<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt', 
                     header=T, sep="\t", dec=".", na.strings = "n/a")
pearson<-function(x,y){
  n<-length(x)
  r=sum((x-mean(x))*(y-mean(y)))/
    sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2))
  t=r*sqrt((n-2)/(1-r^2))
  df=n-2
  p<- 2 * pt(-abs(t), df)
  result<-list(
    correlation = r,
    t_value = t,
    degrees_of_freedom = df,
    p_value = p
  )
  print(result)
}
day3_n<-rairuoho %>% filter(treatment=="nutrient") %>% pull(day3)
day4_n<-rairuoho %>% filter(treatment=="nutrient") %>% pull(day4)
day8_n<-rairuoho %>% filter(treatment=="nutrient") %>% pull(day8)
pearson(day3_n, day4_n)
pearson(day3_n, day8_n)

day3_w<-rairuoho %>% filter(treatment=="water") %>% pull(day3)
day4_w<-rairuoho %>% filter(treatment=="water") %>% pull(day4)
day8_w<-rairuoho %>% filter(treatment=="water") %>% pull(day8)
pearson(day3_w, day4_w)
pearson(day3_w, day8_w)

#nihaoma

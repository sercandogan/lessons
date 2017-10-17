# Ki-Kare testi belli bir dağılıma uygun olup olmamayı araştırmak için kullanılır
# Gözlenen değerler ile beklenen frekanslar arasındaki uyuma dayalı bir test.
library(tidyverse)
# Anket çalışması yapan 200 kişilik bir ekipte, her bir anketörün yaptığı
# anket sayısı aşağıda verilmiştir. Dağılımın Poisson dağılımına uygun olup olmadığını
# ki-kare testi ile araştırınız.

anket <- tribble(
  ~anket_sayisi, ~anketor,
              0,       44,
              1,       64,
              2,       48,
              3,       23,
              4,       12,
              5,       5,
              6,       2, 
              7,       1,
              8,       1
) 


# H0 : Dağılım Poisson Dağılımına Uygundur.
# H1 : Dağılım Poisson Dağılımına Uygun değildir.




anket <- anket %>% 
          mutate(total = anket_sayisi * anketor)


lambda <- sum(anket$total) / 200


#way 1
expected <- sapply(anket$anket_sayisi, function(x) dpois(x,lambda = lambda) * 200)
#way 2
anket$expected <- expected

anket <- anket %>%
        mutate(chisq = (anketor - expected) ^ 2 / expected)

sum(anket$chisq)



# EXAMPLE 2---------------------------------------------------------------------
#Toplamda 1334 ailede ilk 7 çocuk arasında doğan erkek çocukların sayısı
#kaydedilmiştir. Erkek çocuk sayısının binom dağılımına uygun olup olmadığını
#Ki Kare testi ile bulunuz.


children <- tribble(
  ~cocuksayisi, ~ailesayisi,
  0,6,
  1,57,
  2,206,
  3,362,
  4,365,
  5,256,
  6,69,
  7,13
)


children <- table(rep(children$cocuksayisi,children$ailesayisi))


expected <- dbinom(0:7,7,0.5) * 1334

sum((children - expected) ^ 2 / expected)
#13,29618


#Chi-Square Table: 14.067	> 13,29!  So, we don't reject H0! 





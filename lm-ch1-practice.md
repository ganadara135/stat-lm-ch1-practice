회귀분석 1장 연습문제 풀이, 방송대 2022년 교재
================

## 1번

어떤 공장에서 동일한 기계들의 정비기록에 관한 표본자료를 취하였다. 이는
기계의 사용연도와 정비기록간에 어떤 관계가 있는가를 밝혀 내기 위한
것이로 그 자료는 다음과 같다(표본크기 n = 14)

``` r
mage = c(3,1,5,8,1,4,2,6,9,3,5,7,2,6)
mcost= c(39,24,115,105,50,86,67,90,140,112,70,186,43,126)
dataF= data.frame(mage,mcost)
```

<br> 1) 이 데이터의 산점도를 그려라.

``` r
dataF.lm = lm(mcost~mage, data=dataF)
plot(dataF,pch=19)
abline(h=100,v=5, lty=5)
abline(coef = coef(dataF.lm), lty=5)
abline(coef = dataF.lm$coefficients+1, col="RED", lty=1)
```

![](lm-ch1-practice_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> <br>
2) 최소제곱법에 의한 회귀직선을 적합시키라

``` r
dataF.lm = lm(mcost~mage, data=dataF)
plot(dataF$mage,dataF$mcost, xlab="기계사용기간",ylab="유지비용",pch=18)
title("기계사용연수에 따른 유지비용")
abline(dataF.lm, lty=1, col="RED")
```

![](lm-ch1-practice_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> <br>
3) 추정치의 표준오차 Sy.x 를 구하라 <br> 추정값의 표준오차:
![formular](https://latex.codecogs.com/svg.image?S_%7Byx%7D%20=%20%5Csqrt%7BMSE%7D%20=%20%5Csqrt%7B%5Cfrac%7BSSE%7D%7Bn-2%7D%7D%20=%20%5Csqrt%7B%5Csum%5Cfrac%7B(Y_i%20-%20%5Chat%7BY_i%7D)%5E2%7D%7Bn-2%7D%7D)
<br> Residual standard error: 29.11 가 추정값의 표준오차임<br>

``` r
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = mcost ~ mage, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -33.204 -20.383  -4.748  13.957  61.433 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   29.107     15.969   1.823 0.093341 .  
    ## mage          13.637      3.149   4.330 0.000978 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 29.11 on 12 degrees of freedom
    ## Multiple R-squared:  0.6098, Adjusted R-squared:  0.5773 
    ## F-statistic: 18.75 on 1 and 12 DF,  p-value: 0.0009779

<br> 4) 결정계수와 상관계수를 구하라 <br> 결정계수는 0\~1 사이에서
상관계수가 높을수록 1에 가까움, 단점은 음, 양을 알수없음<br> 상관계수는
두 변수 간의 선형관계를 재는 측도 <br> 단순회귀분석에서 상관계수 r :
![formular](https://latex.codecogs.com/svg.image?r=%5Cpm%20%5Csqrt%7BR%5E2%7D)
결정계수: 0.6098 <br> 상관계수는 회귀선의 기울기가 양,음인지 확인후 반영
<br>

``` r
sqrt(0.6098)
```

    ## [1] 0.7808969

<br> 5) 분산분석표를 작성하고 회귀직선의 유의 여부를 검정하라(a=0.05)
<br> 회귀방정식 유의 여부는 분산분석표의 F-검정으로 처리 <br> F = 18.753
이고, p-값: 0.0009779 로 매우 낮으므로 유의함 <br>

``` r
anova(dataF.lm)
```

    ## Analysis of Variance Table
    ## 
    ## Response: mcost
    ##           Df Sum Sq Mean Sq F value    Pr(>F)    
    ## mage       1  15887 15887.2  18.753 0.0009779 ***
    ## Residuals 12  10166   847.2                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

<br> 6) 사용연도가 4년인 기계의 평균정비비용은 어느 정도인가를 추정하라
<br> 추정식 : mcost = 29.107 + 13.637 x mage <br> 대입결과:

``` r
mcost_result = 29.107+(13.637*4)
print(mcost_result)
```

    ## [1] 83.655

<br> 7) 잔차 를 구하여 잔차의 합이 영임을 확인하라 <br>
![formular](https://latex.codecogs.com/svg.image?e_i=y_i%20-%20%5Chat%7By_i%7D)
<br>

``` r
names(dataF.lm$residuals)
```

    ##  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14"

``` r
plot(mage,dataF.lm$residuals, pch=19)
```

![](lm-ch1-practice_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
sum(dataF.lm$residuals)
```

    ## [1] 0

<br> 8) 잔차들의 Xi 에 대한 가중합 를 구하라 <br> 가중잔차의 합:
![formular](https://latex.codecogs.com/svg.image?%5Csum%20w_i(Y_i%20-%20b_0%20-%20b_1X_i))

``` r
sum(mage,dataF.lm$residuals)
```

    ## [1] 62

<br> 9) 잔차들의 Yi에 대한 가중합 를 구하라 <br>

``` r
sum(mcost,dataF.lm$residuals)
```

    ## [1] 1253

<br> 10) 두 변수 x, y 를 표준화된 변수로 고친 후 회귀직선을 적합시키고,
그 회귀계수가 두 변수 x, y 간의 상관계수와 같음을 밝혀라. <br>

## 5번 문제

1번 문제에 대하여 B1, B0, 뮤yx(x=8)의 90% 신뢰구간을 구하라. 또한 다음의
가설검정을 a=0.01 에서 실시하라 <br>
![formular](https://latex.codecogs.com/svg.image?H_0%20:%20%5Cbeta_1%20=%2010)
<br>
![formular](https://latex.codecogs.com/svg.image?H_1%20:%20%5Cbeta_1%20%5Cneq%2010)
<br> B1의 신뢰계수100(1-a)% 신뢰구간 :
![formular](https://latex.codecogs.com/svg.image?b_1%5Cpm%20t(n-2;%5Calpha%20/2)%5Csqrt%5Cfrac%7BMSE%7D%7BS_x_x%7D)
<br> 절편 B0의 신뢰구간 :
![formular](https://latex.codecogs.com/svg.image?b_0%5Cpm%20t(n-2;%5Calpha%20/2)%5Csqrt%20MSE(%5Cfrac%7B1%7D%7Bn%7D+%5Cfrac%7B%5Coverline%7BX%7D%5E2%7D%7BS_x_x%7D))

``` r
pred.frame = data.frame(X=seq(1.3, 9.5, 0.6))
pc = predict(dataF.lm, int="c", newdata = pred.frame)
pp = predict(dataF.lm, int="p", newdata = pred.frame)

pred.X = pred.frame$X
pred.X
```

    ##  [1] 1.3 1.9 2.5 3.1 3.7 4.3 4.9 5.5 6.1 6.7 7.3 7.9 8.5 9.1

``` r
plot(mage,mcost, ylim=range(mcost,pp))
matlines(pred.X, pc, lty=c(1,2,2), col="BLUE")
matlines(pred.X, pp, lty=c(1,3,3), col="RED")
```

![](lm-ch1-practice_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
summary(dataF.lm)
```

    ## 
    ## Call:
    ## lm(formula = mcost ~ mage, data = dataF)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -33.204 -20.383  -4.748  13.957  61.433 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   29.107     15.969   1.823 0.093341 .  
    ## mage          13.637      3.149   4.330 0.000978 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 29.11 on 12 degrees of freedom
    ## Multiple R-squared:  0.6098, Adjusted R-squared:  0.5773 
    ## F-statistic: 18.75 on 1 and 12 DF,  p-value: 0.0009779

``` r
#qt(0.99,14-2)
```

mage p-값이 0.000978 로 0.01 보다 낮음

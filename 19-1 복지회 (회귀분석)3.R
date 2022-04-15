data<-read.csv("매점 매출데이터 +주차.csv")
weather<-read.csv("weather.csv")
week<-read.csv("week.csv")
weather$X<-NULL
weather$판매일<-NULL
event<-cbind(week,weather)
event$num<-NULL
colnames(event)=c("판매일", "주차","행사","날씨")

for(i in length(event)){
  event$판매일<-paste0(event$판매일[i],".")
}
View(event)
week$num<-NULL
week$주차<-week$Week
week[(week$Week==week$행사),c("행사")] <- c("")
View(week)
#######################여기부터 다시#########
data<-merge(data, event, by='판매일')]

View(data)
write.csv(data,'merge.csv')
'
https://rfriend.tistory.com/51 # 참조
'

# 주차와 요일 데이터는 행 길이가 같기 때문에 그냥 cbind() 해주면 된다.


############################# 회귀분석 교육자료 ################################


lp <- read.csv("LinearProgramming.csv")
lp <- lp[,-1]
head(lp)


# dim(dataframe): 몇행 몇열인지 반환해주는 함수
dim(lp) # 409행 12열


# 결측값(NA값 제거)
lp <- lp[complete.cases(lp), ]
dim(lp) # 408 12

####################***********************회기분석하는 코드 중요함 별표별표별표***************************##########################
# lm(y~x, data): data에서 x(변수)를 통해 y(예측할 값) 회귀분석
# 여기서 .은 data의 (가격을 제외한) 모든 변수를 의미
fit <- lm(가격~., data=lp)
summary(fit)


# 잔차(Residuals) : median을 가운데로 두고 다른 분위수가 대칭적으로 나타나는지 정도만 봐도 무방하다. boxplot 의미
# 추정치(Estimate) : (Intercept)는 회귀직선의 y절편, 나머지는 변수마다의 기울기(상관계수)를 나타낸다.
# 표준오차(Std. Error): 추정치의 표준오차로써, 이를 사용해서 회귀계수의 신뢰구간을 구할 수 있다.
# t value : 추정치를 표준오차로 나눈 값, 이를 통해 회귀계수가 통계적으로 유의한지 가설검정을 할 수 있다.

# p-value pr(>|t|) : 이게 작다는 것은 회귀계수가 유의하다는 뜻, 상관관계가 있다는 것을 보이고 싶다면 값이 작을수록 좋다.
# 수정설명력(Adjusted R-squared): 변수의 개수에 비해 데이터를 얼마나 잘 설명하는가를 나타내는 척도로써, 높을수록 좋다.
# r squared(상관계수)가 높아야 설명력이 높음, p value는 낮아야 설득력 있음 -> ***는 상관이 아주 많다는 뜻
# r-squared = 0.8이상, p-value = 0.05이하이면 좋은 회귀모델이라고 볼 수 있음



# predict(lm모형, 예측할 데이터): 만들어둔 lm모형을 가지고 새로운 데이터를 통해 회귀분석을 진행한다.
pre <- predict(fit, newdata = lp)
pre <- as.data.frame(pre)
head(pre)



# predict(lm모형, 예측할 데이터, interval): 예측범위를 구해주는 함수
pre <- predict(fit, newdata = lp, interval = "predict")#오차범위까지 예측해줌.
pre <- as.data.frame(pre)
head(pre)


# 잘 예측 되었는지 실제값과 비교
pre <- cbind(pre, lp$가격)


# 'lwr <= price <= upr' 사이(예측범위 안)에 있으면 예측가격 안에 포함된 것
tf <- NA
pre <- cbind(pre, tf)
colnames(pre) <- c('fit', 'lwr', 'upr', 'price', 'tf')

#lwr<= price<=upr 사이(예측범위 안)에 있으면 예측가격 안에 포함된 것
pre$tf[pre$price>=pre$lwr&pre$price<=pre$upr]<-T
# 나머지 값은 F 처리
pre$tf[is.na(pre$tf)] <- F
head(pre)

# 예측률 = 예측성공 / 전체 행 개수
sum(pre$tf=="TRUE")/dim(pre)[1]


############################# 매점 매출데이터 회귀분석 ###################################
#매점 매출 데이터 전처리

library(plyr)

# 회귀분석을 위해 일일 매출액을 도출
# ddply(data, 기준 변수, 수행할 함수): 기준 변수가 같은 데이터들에 함수를 적용
sumsales <- ddply(store, c("판매일","날씨","최저온도", "최고온도", "주차", "요일"),
                  function(df1)sum(df1$매출합))
colnames(sumsales) <- c('판매일', '날씨', '최저온도', '최고온도', '주차', '요일', '장바구니매출')
str(sumsales)

# 회귀모형: 외부변수를 추가할 경우 '+외부변수'를 해주면 됨

fit <- lm(장바구니매출~날씨+최저온도+최고온도+주차+요일, data=sumsales)
summary(fit)


# 예측
pre <- predict(fit, newdata = sumsales, interval = "predict")
pre <- as.data.frame(pre)

# 앞 내용과 동일
# 회귀모델 시각화는 생략
#시각화
# plot(x=, y= ): 기본 형태
# xlab, ylab: x축, y축 제목
# type: 그래프의 출력 형태 -> "p":점으로 / "l":선으로 / "o":점과 선 동시에 / "h":히스토그램 / ...
# col: 색깔 -> 2: 빨강, 4:파랑
# format(data, scientific=F): 지수 표기를 숫자 표기로 바꿔준다.
# 여기서는 지수값 표기로 인해 실제 매출 그래프가 나타나지 않아서 쓰임 ex) 8e+06 -> 8000000

# 실제매출
plot(x = sumsales$판매일, y = format(pre$price, scientific=F)
     , ylab = "매출액", xlab ="날짜", type = "o", cex = 1, col = 2)

# 예상매출
# points(x=, y= ): plot위에 그래프를 또 그려줌
points(x = sumsales$판매일, y = pre$fit, type = "o", cex = 1, col = 4)

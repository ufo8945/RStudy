#ggplot2 Practice
library(ggplot2)
library(plyr)

#Basic - ggplot()
#ggplot2의 장점 : 레이어 추가 방식으로 그래프를 꾸미거나 추가할 수 있다.
#바탕만 그려질 뿐이다.
ggplot(data=iris,
       mapping=aes(x=Sepal.Length,
                   y=Sepal.Width))

#어떤 그래프를 그릴지 +로 연결해 작성한다.
#ggplot() + geom_point()와 같이 +기호로 연결해 구문을 작성한다.
#geom_point() : 산포도
#geom_line() : 선 그래프
#geom_boxplot() : 박스플롯
#geom_histogram() : 히스토그램
#geom_bar() : 막대그래프

ggplot(data=iris,
       mapping=aes(x=Sepal.Length,
                   y=Sepal.Width)) +
  geom_point()

#그래프 그리기 함수에 들어가는 주요 옵션에는
#그래프에 대한 색상/모양/크기/넓이 등이 있다.
#color : 색상, pch : 모양, size : 크기기
ggplot(data=iris,
       mapping=aes(x=Sepal.Length,
                   y=Sepal.Width)) +
  geom_point(color="purple", pch=2, size=2)

#그래프 옵션의 경우, 그룹별로 색상, 모양, 크기를 다르게 할 수 있다.
#단, factor형이어야 한다.
ggplot(data=iris,
       mapping=aes(x=Sepal.Length,
                   y=Sepal.Width)) +
  geom_point(color=c("purple","blue","red")[iris$Species], 
             pch=c(0,2,20)[iris$Species], 
             size=c(1,2,3)[iris$Species])

#도형 그리기 함수 - geom_도형 계열
#그래프를 그리고서 도형(사각형)으로 특정 구역을 표기하거나
#선으로 임계치를 표시하고 싶을 때 사용
#geom_abline() : 선
#geom_hline() : 평행선
#geom_vline() : 수직선
#geom_rect() : 사각형
#geom_text() : 텍스트
g <- ggplot(data=iris,
            mapping=aes(x=Sepal.Length,
                        y=Sepal.Width)) +
     geom_point(color=c("purple","blue","red")[iris$Species], 
               pch=c(0,2,20)[iris$Species], 
               size=c(1,2,3)[iris$Species])

tmp <- ddply(iris, .(Species), summarise,
             min_x = min(Sepal.Length),
             max_x = max(Sepal.Length),
             min_y = min(Sepal.Width),
             max_y = max(Sepal.Width))

start_x <- max(tmp$min_x)
end_x <- min(tmp$max_x)
start_y <- max(tmp$min_y)
end_y <- min(tmp$max_y)

#annotate() 함수 geom 옵션으로 어떤 도형을 그릴 것인지 결정
#사각형으로 모든 iris$Species가 겹치는 구역을 표시해보자,
g + annotate(geom="rect",
             xmin= start_x,
             xmax= end_x,
             ymin = start_y,
             ymax = end_y,
             fill = "red",
             alpha=0.2,
             color="black",
             lty=2)

#행 번호로 라벨을 붙여본다.
#geom : 도형종류 (text)
#x, y : x, y 좌표
#hjust : x축 영점 조절
#vjust : y축 영점 조절
g + annotate(geom = "text",
             x = iris$Sepal.Length,
             y = iris$Sepal.Width,
             label = rownames(iris),
             color = "brown",
             alpha = 0.7,
             size = 3,
             hjust = 0.5,
             vjust = -1)

#외부 옵션 함수 - coord, labs
# 축 변환 : coord_flip()
# 축 범위 : coord_cartesian()
# 라벨링 : labs()
g + coord_flip()
g + coord_cartesian(xlim = c(start_x, end_x),
                    ylim = c(start_y, end_y))
g + labs(title = "제목",
         subtitle = "부제목",
         caption = "주석",
         x = "x축 이름",
         y = "y축 이름")

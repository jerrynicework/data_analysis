library(tidyverse)
library(ggplot2)
game <- read_csv("/Users/jerry/Desktop/R실습/mobile_game_ABtest/cookie_cats.csv")
head(game)
tail(game)
str(game)
summary(game)

#결측치
colSums(is.na(game))
game %>%
  filter(duplicated(game)) %>%
  head()

#이상치
summary(game)
game %>%
  group_by(version)%>%
  count()

game %>%
  ggplot(aes(x=version,y=sum_gamerounds)) +
  geom_boxplot()

#이상치제거
game %>%
  group_by(version) %>%
  summarize(avg = mean(sum_gamerounds),
            qt90 = quantile(sum_gamerounds,0.9),
            qt99 = quantile(sum_gamerounds,0.99),
            qt999 = quantile(sum_gamerounds,0.999),
            max = max(sum_gamerounds))

new_game <- game %>%
  filter(sum_gamerounds < 49854) %>%
  arrange(desc(sum_gamerounds))

new_game %>%
  ggplot(aes(x=version, y=sum_gamerounds)) +
  geom_boxplot()

##EDA
# 버전별 플레이수 히스토그램
game %>%
  ggplot(aes(x=sum_gamerounds,fill=version))+
  geom_histogram(postion='identity',
                 alpha=0.4)+
  xlim(0,100)

# 버전별 총 인원수, 평균 게임 플레이 횟수
new_game %>%
  group_by(version) %>%
  summarize(player = n(),
            avg = mean(sum_gamerounds),
            median = median(sum_gamerounds),
            max = max(sum_gamerounds))

# 버전별 설치후 게임 한번도 안한 유저 
new_game %>%
  group_by(version, retention_1, retention_7) %>%
  filter(sum_gamerounds == 0) %>%
  summarize(player = n())


# 설치 하루 후 게임하지 않은 플레이어 55%
game %>%
  mutate(total_count = n()) %>%
  group_by(retention_1, total_count) %>%
  summarize(count = n(),
            avg = mean(sum_gamerounds)) %>%
  mutate(pct = count / total_count)

#설치 일주일 후 게임하지 않은 플레이어 81%
game %>%
  mutate(total_count = n()) %>%
  group_by(retention_7, total_count) %>%
  summarize(count = n(),
            avg = mean(sum_gamerounds)) %>%
  mutate(pct = count / total_count)

###버전별

# 1일차, 7일차 두번 다 들어온 플레이어 VS 아닌 플레이어 - 비슷한 결과
# 14.9%, 14.3% 앞으로도 게임을 계속할 플레이어
game %>%
  group_by(version) %>%
  mutate(total_count = n()) %>%
  group_by(version, retention_1, retention_7, total_count) %>%
  summarize(count = n(),
            avg = mean(sum_gamerounds)) %>%
  mutate(pct = count / total_count)


## AB TEST
gate30 <- new_game %>%
  filter(version == 'gate_30')

gate40 <- new_game %>%
  filter(version == 'gate_40')

#정규성 가정 - 샤피로
shapiro.test(gate30$sum_gamerounds)


# QQ plot
ggplot(gate30, aes(sample = sum_gamerounds)) +
  geom_qq() +
  geom_qq_line()+
  labs(title = "Gate 30 : QQ Plot for Normality Check")


ggplot(gate40, aes(sample = sum_gamerounds)) +
  geom_qq() +
  geom_qq_line()+
  labs(title = "Gate 40 : QQ Plot for Normality Check")

# 비모수검정 - 윌콕슨
wilcox.test(gate30$sum_gamerounds, gate40$sum_gamerounds)




















# 4��. ���м�
# 3��. �ٺ��� �߸����м�
# 4. �л�м�

# �Ͽ��л�м�
# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���.
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav",
use.value.labels = FALSE, to.data.frame = TRUE)
# ���輳�迡 ���� ������ �����Ϳ� �Բ� ÷�ε� ������ ������ ��.
# �����ڱ��� �� 24���̸�, ������ ���� 1~12������ ���� �̹����� �޾Ұ�, ������ ����
# 13~24������ ���� �̹����� �޾Ҵ�. ���õ� �̹����� ������ ��� ���γ���(1,5,9��),
# ���γ���(2,6,10��), �����дг���(3,7,11��), �ƽþ��γ���(4,8,12��)�̴�. ������
# ���ο���(13,17,21��), ���ο���(14,18,22��), �����дп���(15,19,23��),
# �ƻ����ο���(16,20,24��)�̴�. ���⼭�� ���õ� �̹����� ���� 8���� �����ڱ����� �Ͽ���.
table(tess$XTESS089, useNA = 'always')
tess$target.race <- tess$XTESS089
tess$target.race[tess$XTESS089 == 1| tess$XTESS089 == 5| tess$XTESS089 == 9] <- 1
tess$target.race[tess$XTESS089 == 2| tess$XTESS089 == 6| tess$XTESS089 == 10] <- 2
tess$target.race[tess$XTESS089 == 3| tess$XTESS089 == 7| tess$XTESS089 == 11] <- 3
tess$target.race[tess$XTESS089 == 4| tess$XTESS089 == 8| tess$XTESS089 == 12] <- 4
tess$target.race[tess$XTESS089 == 13| tess$XTESS089 == 17| tess$XTESS089 == 21] <- 5
tess$target.race[tess$XTESS089 == 14| tess$XTESS089 == 18| tess$XTESS089 == 22] <- 6
tess$target.race[tess$XTESS089 == 15| tess$XTESS089 == 19| tess$XTESS089 == 23] <- 7
tess$target.race[tess$XTESS089 == 16| tess$XTESS089 == 20| tess$XTESS089 == 24] <- 8
tess$target.race <- as.factor(tess$target.race)
# ���ڵ��� �� �Ǿ����� Ȯ���Ͽ���.
table(tess$XTESS089, tess$target.race, useNA = 'always')
# ��������δ� Q8�� �̿��Ͽ���. Q8�� ���ؼ��� �տ��� ������ �� �ִ�. -1�� ��� ������ó���Ͽ���.
table(tess$Q8, useNA = 'always')
tess$pain.other <- tess$Q8
tess$pain.other[tess$Q8 == -1] <- NA
table(tess$Q8, tess$pain.other, useNA = 'always')
# aov()�� �л�м��� �ǽ��ϴ� R��ɹ��̴�. ������� ~ ���������� ���¸� ���.
# data���� �м���� �����͸� �����ϸ� �ȴ�. �ش����� obj.aov��� �̸��� ������Ʈ�� �����ߴ�.
obj.aov <- aov(pain.other ~ target.race, data = tess)
# obj.aov�� ����� �����Ѵ�. summary()�� ���� ���� ���¿� ���� ���� summary() ������ �� �� �ִ�.
summary(obj.aov)

# ���ߺ�(��Ű�� Honest Significant Difference ���)�� �ǽ��Ͽ���.
# which �ɼǿ����� ���ϴ� ������ �̸��� ������ �ȴ�.
TukeyHSD(obj.aov, which = 'target.race', ordered = FALSE)

# ���� �� 8���� ������ �ֱ� ������ �׸��� ���� ȿ�����̴�. cex.axis�� ���� ���ڸ� �⺻�ɼ���
# ��(0.5)���� �Ѵٴ� �ǹ��̴�.
plot(TukeyHSD(obj.aov, which = 'target.race'), cex.axis = 0.5)

# �̿��л�м�
# �տ��� �ݺ��� ���α׷����� �Ұ����� �ʾҴ�.
# �տ����� ������ 8���� �ߴ�. �׷��� ���⼭�� �̹����� ����(����;����), �׸���
# ����(����, ����, �����д�, �ƽþ���)���� 2���� �����Ͽ���.
table(tess$XTESS089, useNA = 'always')
tess$target.race <- tess$XTESS089
tess$target.race[tess$XTESS089 == 1| tess$XTESS089 == 5| tess$XTESS089 == 9] <- 1
tess$target.race[tess$XTESS089 == 2| tess$XTESS089 == 6| tess$XTESS089 == 10] <- 2
tess$target.race[tess$XTESS089 == 3| tess$XTESS089 == 7| tess$XTESS089 == 11] <- 3
tess$target.race[tess$XTESS089 == 4| tess$XTESS089 == 8| tess$XTESS089 == 12] <- 4
tess$target.race[tess$XTESS089 == 13| tess$XTESS089 == 17| tess$XTESS089 == 21] <- 1
tess$target.race[tess$XTESS089 == 14| tess$XTESS089 == 18| tess$XTESS089 == 22] <- 2
tess$target.race[tess$XTESS089 == 15| tess$XTESS089 == 19| tess$XTESS089 == 23] <- 3
tess$target.race[tess$XTESS089 == 16| tess$XTESS089 == 20| tess$XTESS089 == 24] <- 4
tess$target.race <- as.factor(tess$target.race)
tess$taget.female <- tess$XTESS089
tess$target.female <- 0
tess$target.female[tess$XTESS089 >= 13] <- 1
tess$target.female <- as.factor(tess$target.female)
# ���ڵ��� �� �Ǿ����� Ȯ���Ͽ���.
table(tess$XTESS089, tess$target.race, useNA = 'always')
table(tess$XTESS089, tess$target.female, useNA = 'always')
# ��������δ� Q8�� �̿��Ͽ���. Q8�� ���ؼ��� �տ��� ������ �� �ִ�. -1�� ��� ������ó���Ͽ���.
table(tess$Q8, useNA = 'always')
tess$pain.other <- tess$Q8
tess$pain.other[tess$Q8 == -1] <- NA
table(tess$Q8, tess$pain.other, useNA = 'always')
# aov()�� �Ȱ�����, ������ ǥ���κ��� �ٸ���. �켱�� ��ȿ����(main effect terms)���� �����ߴ�.
# A + B�� ��ȿ���׸��� �����Ѵٴ� �ǹ��̴�.
obj.aov.m1 <- aov(pain.other ~ target.race + target.female, data = tess)
# ������ ��ȿ���װ� ���Ҿ� ��ȣ�ۿ�ȿ����(interaction effect term)�� �߰��Ͽ���.
# A * B��� ǥ���� A�� B�� ��ȿ���� ��ȣ�ۿ� ȿ���� ��θ� �����Ѵ�.
obj.aov.m2 <- aov(pain.other ~ target.race * target.female, data = tess)
# ������ ���캻 �� ������Ʈ�� ����� �����ش�.
summary(obj.aov.m1)
summary(obj.aov.m2)

# ���ߺ�(��Ű�� Honest Significant Difference ���)�� �ǽ��Ͽ���.
# which �ɼǿ����� ���ϴ� ������ �̸��� ������ �Ǵµ�, ����� �̿��л�м��̱� ������
# target.race�� target.female�� �� ������ �� ���� �� �ִ�. �׷��� target.female�� ���
# 2�� ����(level)�̱� ������ TukeyHSD�� �ǹ̰� ����.
TukeyHSD(obj.aov.m1, which = 'target.race', ordered = FALSE)
TukeyHSD(obj.aov.m2, which = 'target.race', ordered = FALSE)
TukeyHSD(obj.aov.m1, which = 'target.female', ordered = FALSE)
TukeyHSD(obj.aov.m2, which = 'target.female', ordered = FALSE)

# 2���л�м��� ��ȣ�ۿ��� ǥ���ϴ� �׷����� �Ʒ��� ���� �׸� �� �ִ�.
# x.factor�� ���� X�࿡ ���̰� �� ����, trace.factor�� X�࿡ ���� ������ �ƴ� �ٸ� ����,
# response�� ���� �������, fun�� ���� �̿��� �Լ��̴�. ��հ��� �����Ͽ��µ�, ��������
# �ֱ� ������ function(x) {mean(x, na.rm = TRUE) }�� �̿��Ͽ� �������� �����ϴ� �ɼ��� �ֵ����Ͽ���.
# type�� �׸��� ���� ���� ���ذ� ���� ���̴�. �׸��� ylab�� Y���� �̸���, xlab�� X���� �̸���
# �׸��� trace.label�� ���еǴ� ������ �̸��� ���� ���̴�. (myrecode.race / myrecode.gender�� ���
# ���� ���ڵ� �κп��� ������ �� �ִ�. 
myrecode.race <- c('White','Black','Hispanic','Asian')
myrecode.gender <- c('Male','Female')
interaction.plot(x.factor = myrecode.race[tess$target.race], 
trace.factor = myrecode.gender[tess$target.female], 
response = tess$pain.other, fun = function(x) {mean(x,na.rm = TRUE) }, type = 'b',
ylab = "Mean of subjective others' pain", xlab = "target's race",
trace.label = "target's gender")

# �����ڱؿ����� ��ȣ�ۿ�ȿ���� �߰ߵ��� �ʾ�����, �Ʒ��� ���� �������� ���缱ȣ���� ������ ����
# �������� ������ Ư���� �������� ������ ���, ��ȣ�ۿ� ȿ���� �߰��� �� �ִ�.
# �������� ���缱ȣ���� ��ȭ��������, ������, ���ִ������ķ� �����Ѵ�.
table(tess$XPARTY7, useNA = 'always')
tess$party3 <- tess$XPARTY7
tess$party3[tess$XPARTY7 <= 2] <- 'Republican'
tess$party3[tess$XPARTY7 <= 5 & tess$XPARTY7 >= 3] <- 'Independent'
tess$party3[tess$XPARTY7 >= 6] <- 'Democrat'
tess$party3 <- factor(tess$party3,levels=c('Republican','Independent','Democrat'))
table(tess$XPARTY7, tess$party3, useNA = 'always')
# �������� ������ ����, ����, �����д�, �ƽþư��� 4�������� �����Ѵ�.
tess$own.race <- NA
tess$own.race[tess$PPETHM == 1] <- 'White'
tess$own.race[tess$PPETHM == 2] <- 'Black'
tess$own.race[tess$PPETHM == 4] <- 'Hispanic'
tess$own.race[tess$PPETHM == 3] <- 'Asian'
tess$own.race <- factor(tess$own.race,c('White','Black','Hispanic','Asian'))
# �켱�� ��ȿ����(main effect terms)���� �����ߴ�.
obj.aov.m3 <- aov(pain.other ~ own.race + party3, data = tess)
# ������ ��ȿ���װ� ���Ҿ� ��ȣ�ۿ�ȿ����(interaction effect term)�� �߰��Ͽ���.
obj.aov.m4 <- aov(pain.other ~ own.race * party3, data = tess)
# ������ ���캻 �� ������Ʈ�� ����� �����ش�.
summary(obj.aov.m3)
summary(obj.aov.m4)

# ��ȣ�ۿ� ȿ���� �׷����� ���� ��ȣ�ۿ� ȿ���� Ȯ���ϰ� ��Ÿ����.
interaction.plot(x.factor = tess$own.race, trace.factor = tess$party3,
response = tess$pain.other, fun = function(x) {mean(x,na.rm = TRUE) }, type = 'b',
ylab = "Mean of subjective others' pain", xlab = "respondent's race",
trace.label = "Party affiliation")

# anova()�� ������ ������ ���յ��� ���Ѵ�. obj.aov.m3�� obj.aov.m4�� ���ϸ�, ��ȣ�ۿ� ȿ����
# �߰������ϴ� ���� ������ ������ ���ǹ��ϰ� ���̴����� �׽�Ʈ�� �� �ִ�.
anova(obj.aov.m3, obj.aov.m4)

table(tess$Q4, useNA = 'always')
tess$pain.own <- tess$Q4
tess$pain.own[tess$Q4 == -1 ] <- NA
# pain.own�� pain.other ������ �������� ������ ���踦 ������ ���� ���캸�Ҵ�. 
table(tess$pain.own, tess$pain.other, useNA='always')
# �� ������ �ϳ��� �������� �־ �м����� �����Ͽ���. 
tess.subset <- tess[complete.cases(tess[,c('pain.other','pain.own')]),]
# aov()�� �Ȱ�����, ������ ǥ���κ��� �ٸ���. �켱�� �������� �������� ���� �̿��л�м� ����� �����Ͽ���.
obj.aov.m5 <- aov(pain.other ~ target.race * target.female, data = tess.subset)
# ������ �������� ������ �̿����л�м��� �ǽ��Ͽ���.
obj.aov.m6 <- aov(pain.other ~ target.race * target.female + pain.own, data = tess.subset)
# ������ ���캻 �� ������Ʈ�� ����� �����ش�.
summary(obj.aov.m5)
summary(obj.aov.m6)
# anova()�� ������ ������ ���յ��� ���Ѵ�. �̿��л�м�����ó�� obj.aov.m5�� obj.aov.m6�� ���غ���.
anova(obj.aov.m5, obj.aov.m6)

# �ݺ����� �л�м�
# ��������δ� Q5~Q8���� �̿��Ͽ���. �� 4���� ������ �ϻ��Ȱ���� ������ ������ �Ǵ� ��Ȳ�̴�.
# 4���� ���� ��� -1�� ��� ������ó���Ͽ���.
recoding.Q5Q8 <- c(NA,0:10)
tess$q5r <- recoding.Q5Q8[tess$Q5+2]
tess$q6r <- recoding.Q5Q8[tess$Q6+2]
tess$q7r <- recoding.Q5Q8[tess$Q7+2]
tess$q8r <- recoding.Q5Q8[tess$Q8+2]
# �ݺ����� �л�м��� ���� �����ϱ� ���ؼ� car ���̺귯���� �����Ѵ�. car ���̺귯���� �տ��� ������ �� �ִ�
library('car')
# ��ü�� ���ο� �ش�Ǵ� �װ��� ������ ���� �ϳ��� ������Ʈȭ�Ѵ�.
wsfactor <- as.matrix(tess[,c('q5r','q6r','q7r','q8r')])
# ��ü�� ������ ���غ��� �̸�(label)�� ���δ�. ������ ��Ȳ�� ��¡�ϴ� �ܾ �־���.
wsfname <- expand.grid(pains = c('disinfect','tongue','hand','toe'))
wsfname
# �Ʒ��� �װ��� �����Ȳ���� ������������ �����м�(�л�м��� ����)�� �� ���̴�
# lm()�� ȸ�ͺм� ���ǿ��� ���� �ڼ��� ���캸��� �Ѵ�. "~ 1"�� �ǹ̴� �������� �����Ѵٴ�
# �ǹ��̴�. �� ��� ���������� �������� �ʴ´�.
ws.aov.1 <- lm(wsfactor ~ 1, data = tess)
# Anova() ��ɹ��� ��ü�� ������ �̿��Ͽ� �л�м��� �ǽ��� �� ���ȴ�.
# ws.aov.1�� �տ��� �̹� ����Ǿ���. idata�� ��ü�� ������ ���غ� �̸��� �����Ѵ�.
# idesign�� ��ü�� ������ ��� ������ �������� �����ش�. ����(intercept)���� pains�� �ִ´ٴ� �ǹ��̴�
# type�� ������(sum of squares)�� Ÿ���� �����Ѵ�. �Ϲ������� type 3�� ���� �������̴�.
ws.Anova.1 <- Anova(ws.aov.1, idata = wsfname, idesign = ~1 + pains, type='III')
# ����� ����� �����ش�. ����� �ؼ��ϴ� ���� ������ �ؼ��ϴ� �κп� ���� �ڼ��ϰ� ����ߴ�.
summary(ws.Anova.1)

# ��ü�� ���ΰ� ��ü�� ������ �л�м�
# �տ��� ������ �κ��� �ٽ� �ݺ����� �ʴ´�.
# �����м��� �ǽ��ϵ�, ��ü�� �������� � ������ �����ϴ����� �����Ѵ�(party3)
ws.aov.2 <- lm(wsfactor ~ party3, data = tess)
# Anova()�� ws.aov.1�� ws.aov.2�� ��ü�Ѵ�. �������� �����ϴ�.
ws.Anova.2 <- Anova(ws.aov.2, idata=wsfname, idesign = ~1 + pains, type='III')
# ����� ����� �����ش�. ����� �ؼ��ϴ� ���� ������ �ؼ��ϴ� �κп� ���� �ڼ��ϰ� ����ߴ�.
summary(ws.Anova.2)

# �ݺ����� �л�м�
# ��������δ� Q5~Q8���� �̿��Ͽ���. �� 4���� ������ �ϻ��Ȱ���� ������ ������ �Ǵ� ��Ȳ�̴�.
# 4���� ���� ��� -1�� ��� ������ó���Ͽ���.
recoding.Q5Q8 <- c(NA,0:10)
tess$q5r <- recoding.Q5Q8[tess$Q5+2]
tess$q6r <- recoding.Q5Q8[tess$Q6+2]
tess$q7r <- recoding.Q5Q8[tess$Q7+2]
tess$q8r <- recoding.Q5Q8[tess$Q8+2]
# �ݺ����� �л�м��� ���� �����ϱ� ���ؼ� car ���̺귯���� �����Ѵ�. car ���̺귯���� �տ��� ������ �� �ִ�
library('car')
# ��ü�� ���ο� �ش�Ǵ� �װ��� ������ ���� �ϳ��� ������Ʈȭ�Ѵ�.
wsfactor <- as.matrix(tess[,c('q5r','q6r','q7r','q8r')])
# ��ü�� ������ ���غ��� �̸�(label)�� ���δ�. ������ ��Ȳ�� ��¡�ϴ� �ܾ �־���.
wsfname <- expand.grid(pains = c('disinfect','tongue','hand','toe'))
wsfname
# �Ʒ��� �װ��� �����Ȳ���� ������������ �����м�(�л�м��� ����)�� �� ���̴�
# lm()�� ȸ�ͺм� ���ǿ��� ���� �ڼ��� ���캸��� �Ѵ�. "~ 1"�� �ǹ̴� �������� �����Ѵٴ�
# �ǹ��̴�. �� ��� ���������� �������� �ʴ´�.
ws.aov.1 <- lm(wsfactor ~ 1, data = tess)
# Anova() ��ɹ��� ��ü�� ������ �̿��Ͽ� �л�м��� �ǽ��� �� ���ȴ�.
# ws.aov.1�� �տ��� �̹� ����Ǿ���. idata�� ��ü�� ������ ���غ� �̸��� �����Ѵ�.
# idesign�� ��ü�� ������ ��� ������ �������� �����ش�. ����(intercept)���� pains�� �ִ´ٴ� �ǹ��̴�
# type�� ������(sum of squares)�� Ÿ���� �����Ѵ�. �Ϲ������� type 3�� ���� �������̴�.
ws.Anova.1 <- Anova(ws.aov.1, idata = wsfname, idesign = ~1 + pains, type='III')
# ����� ����� �����ش�. ����� �ؼ��ϴ� ���� ������ �ؼ��ϴ� �κп� ���� �ڼ��ϰ� ����ߴ�.
summary(ws.Anova.1)

####################################################----------����
# ��ȹ��(Planned Comparison)
# �տ��� �ݺ��� �κ��� ��� ������ �����Ͽ���.
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav",
use.value.labels = FALSE, to.data.frame = TRUE)
# ���輳�迡 ���� ������ �����Ϳ� �Բ� ÷�ε� ������ ������ ��.
# ��������δ� Q8�� �̿��Ͽ���. Q8�� ���ؼ��� �տ��� ������ �� �ִ�. -1�� ��� ������ ó���Ͽ���.
table(tess$Q8, useNA = 'always')
tess$pain.other <- tess$Q8
tess$pain.other[tess$Q8 == -1] <- NA
# �����ڱ��� �� 24���̸�, ������ ������� �������� 4������ �����Ͽ���.
table(tess$XTESS089, useNA = 'always')
tess$target.race <- tess$XTESS089
tess$target.race[tess$XTESS089 == 1| tess$XTESS089 == 5| tess$XTESS089 == 9] <- 1
tess$target.race[tess$XTESS089 == 2| tess$XTESS089 == 6| tess$XTESS089 == 10] <- 2
tess$target.race[tess$XTESS089 == 3| tess$XTESS089 == 7| tess$XTESS089 == 11] <- 3
tess$target.race[tess$XTESS089 == 4| tess$XTESS089 == 8| tess$XTESS089 == 12] <- 4
tess$target.race[tess$XTESS089 == 13| tess$XTESS089 == 17| tess$XTESS089 == 21] <- 1
tess$target.race[tess$XTESS089 == 14| tess$XTESS089 == 18| tess$XTESS089 == 22] <- 2
tess$target.race[tess$XTESS089 == 15| tess$XTESS089 == 19| tess$XTESS089 == 23] <- 3
tess$target.race[tess$XTESS089 == 16| tess$XTESS089 == 20| tess$XTESS089 == 24] <- 4
# Planned Comparison�� ���ϰ� �ϱ� ���� multcomp ���̺귯���� �ʿ��ϴ�. multcomp �� ������
# install.packages()�� ���ؼ� ��ġ�� �� ����ϸ� �ȴ�.
library('multcomp')
# ���������� ������ ��ü�� �ƴ϶� ���θ��� �ش�ȴ�. �� �������� ������ Ȯ���Ͽ��� �Ѵ�.
# ���⼭�� ����, ����, �����д�, �ƽþư��� 4�������� �����Ѵ�.
tess$own.race <- NA
tess$own.race[tess$PPETHM == 1] <- 1
tess$own.race[tess$PPETHM == 2] <- 2
tess$own.race[tess$PPETHM == 4] <- 3
tess$own.race[tess$PPETHM == 3] <- 4
# ���� ���� ������ �ڽ��� ������ ������Ű�� �� 16���� ��Ȳ�� ����ȴ�.
table(tess$own.race, tess$target.race, useNA = 'always')
# 16���� ������ 16���� ������ ���� �ϳ��� �������� �ٲ۴�.
# ��, 10�� �ڸ��� ���ڰ� ���ϴ� ���� ������, 1�� �ڸ��� ���ڰ� ���ϴ� ���� Ÿ���� ������ �ȴ�.
tess$own.target.race <- as.factor(10*tess$own.race + tess$target.race)
table(tess$own.target.race, useNA = 'always')
# ������������ ���ϰ��� �ϴ� ������ �����̸鼭 ������ ���ϴ� ���� �����̸鼭
# ������ ���ϴ� ����̴�. �� '11'�� '12'�� �񱳵Ǹ� ������ ���ܵ��� ������ ����� �ƴϴ�.
# �񱳰� �Ǵ� ������ 1�� -1��, �׸��� ������ ���ܵ鿡�� 0�� ����ϴ� ���͸� �ۼ��Ѵ�.
plan.compare <- c('WW v. WB' = c(1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
obj.aov <- aov(pain.other ~ own.target.race, data=tess)
summary(obj.aov)
obj.aov.compare <- glht(obj.aov,linfct=mcp(own.target.race = plan.compare),
alternative = 'two.sided')
summary(obj.aov.compare)


effectsize_function <- function(aov_object,mydigit) {
	  mycomponent <- unlist(summary(aov_object))
	  myaovlength <- length(mycomponent)/5
	  myaovtable <- data.frame(cbind(mycomponent[(0*myaovlength+1):myaovlength],
	  mycomponent[(1*myaovlength+1):(2*myaovlength)],
	  mycomponent[(2*myaovlength+1):(3*myaovlength)],
	  mycomponent[(3*myaovlength+1):(4*myaovlength)],
	  mycomponent[(4*myaovlength+1):(5*myaovlength)]))
	  rownames(myaovtable) <- c(paste('source',1:(myaovlength-1)),'residual')
	  colnames(myaovtable) <- c('df','SS','MS','F','p')
	  mySSeffect <- myaovtable[1:(myaovlength-1),2]
	  mypartialeta <- mySSeffect/(mySSeffect+myaovtable[myaovlength,2])
	  mydfeffect <- myaovtable[1:(myaovlength-1),1]
	  myMSerror <- myaovtable[myaovlength,3]
	  mySStotal <- myaovtable[myaovlength,2]
	  mypartialomega <- (mySSeffect - mydfeffect*myMSerror)/(mySStotal+myMSerror)
	  mypartialeta <- c(mypartialeta,NA)
	  mypartialomega <- c(mypartialomega,NA)
	  myeffectsize <- data.frame(mypartialeta,mypartialomega)
	  rownames(myeffectsize) <- c(paste('source',1:(myaovlength-1)),'Residual')
	  round(myeffectsize,mydigit)
  }
  

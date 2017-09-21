# 2��. ������ ����
# 2��. ������ �Է��ϱ� �� �ҷ�����
# 1. �������� �����Է�

## �����Է¹� 1: ������ ����
# year ��� �̸��� ������ ������
# <- ��� ǥ�ô� ������ ��� ���� �ο��ϴ°� �����ϴ� ������ �Ѵ�. ���� assignment(��ġ) ��� �Ҹ���.
# c(): �޸��� ���е� �Ϸ��� ��ȣ ���� �Է°�(value)�� ǥ���Ѵ�.
year <- c(1987, 1992, 1997, 2002, 2007, 2012)
# president(������̸�)��� �̸��� ���� ������ �����Ѵ�.
# <- �� c()�� ���� ������ ������ �����Ѵ�.
president <- c('TW Roh','YS Kim','DJ Kim','MH Roh','MB Lee','GH Park')
# rate(��ǥ��)��� �̸��� ������ �����Ͽ���.
rate <- c(89.2, 81.9, 80.7, 70.8, 63.0, 75.8)
# cbind()��� ���ɾ ���� �� ���� ����(year, president, & rate)�� ���´�.
# cbind�� column bind(�÷��� ���´�)�� �����̴�.
# data.frame()�̶�� ���ɹ��� ��ȣ ���� ������Ʈ�� �����ͼ� ���·� ��ȯ���� �ǹ��Ѵ�.
# �����ʰ� ���� ��ȯ�� �����ͼ��� national.election�̶�� �����ͼ� �̸����� ����ȴ�.
national.election <- data.frame(cbind(year, president, rate))
# national.election�̶�� �̸��� ������Ʈ�� PCȭ�鿡 �����ش�.
national.election

## �����Է¹� 2: ���������� ����
# <- �� c()�� ������ ������ ������ ������ �����Ѵ�.
# �� 6���� ��������(�� ����)�� r1, r2, ��, r6��� �̸��� ������Ʈ�� ���� �����Ѵ�.
r1 <- c(1987, 'TW Roh', 89.2)
r2 <- c(1992, 'YS Kim', 81.9)
r3 <- c(1997, 'DJ Kim', 80.7)
r4 <- c(2002, 'MH Roh', 70.8)
r5 <- c(2007, 'MB Lee', 63.0)
r6 <- c(2012, 'GH Park', 75.8)
# rbind()�� ��ȣ���� ������Ʈ�� ���� ������ �Ѵ�. cbind�� ������(column)�� �߽����� ���´ٸ�,
# rbind�� ������(row)�� �߽����� ���´ٴ� ������ ���̰� �ִ�.
# data.frame(rbind())�� ���� ����� �����ͼ��� national.election�̶�� ������Ʈ�� ����ȴ�.
national.election <- data.frame(rbind(r1,r2,r3,r4,r5,r6))
# colnames()�� ��ȣ���� �����ͼ� ������ ������Ʈ�� ������ �̸��� ��Ÿ���� ���ɹ��̴�.
# national.election�̶�� �����ͼ��� �� ������ �̸��� ������� year, president, rate��� �ٿ��ش�.
colnames(national.election) <- c('year','president','rate')
national.election

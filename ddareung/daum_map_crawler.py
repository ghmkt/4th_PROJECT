import pandas as pd
from selenium.webdriver.common.keys import Keys
from selenium import webdriver
import pickle
from selenium.common.exceptions import UnexpectedAlertPresentException
import time

df = pd.read_csv(r'station1.csv')
st_address = df['대여소 주소'].values.tolist()

path = 'chromedriver.exe'

# 133 x 1 벡터를 만들어서 한 row에 133개의 list를 추가해서 결과적으로 133 x 133 x 4의 3차원 array를 만듦
result = [[] * len(st_address) for row in range(len(st_address))]

# 각각의 주소 i에 대해서 똑같은 주소 j에 대한 경로를 찾을거임
for x, i in enumerate(st_address):
    if (x < 49) and (x > 84):
    # 컴퓨터 두 대에서 크롬 드라이버 여러 개로 나눠서 돌리느라 enumerate로 index 지정해주고 구간 나눠줌
        pass
    else:
        driver = webdriver.Chrome(path)

        # 다음 지도 켜고 길찾기로 넘어가는 동작
        driver.get('http://map.daum.net')
        time.sleep(1.5)
        findroad = driver.find_element_by_xpath('//*[@id="search.tab2"]/a')
        findroad.click()
        time.sleep(1.5)
        print(i)

        # '출발' 에 주소 넣는 동작
        input_fromadd = driver.find_element_by_xpath('//*[@id="info.route.waypointSuggest.input0"]')
        input_fromadd.send_keys(i)
        time.sleep(1.5)
        input_fromadd.send_keys(Keys.ENTER)

        for y, j in enumerate(st_address):
            if y < 0:
                pass
            else:

                if (y != 0) and (y % 20 == 0):
                    driver.close()
                    driver = webdriver.Chrome(path)
                    driver.get('http://map.daum.net')
                    findroad = driver.find_element_by_xpath('//*[@id="search.tab2"]/a')
                    findroad.click()
                    time.sleep(1.5)
                    input_fromadd = driver.find_element_by_xpath('//*[@id="info.route.waypointSuggest.input0"]')
                    input_fromadd.send_keys(i)
                    time.sleep(1.5)
                    input_fromadd.send_keys(Keys.ENTER)
                    time.sleep(1.5)

                print(j)
                # 133 x 1 벡터의 한 원소에 1 x 4 벡터를 133개 넣어주는 것. --> 133 x 133 x 4


                # 자기 자신의 경로는 안 찾음
                temp = []
                if i == j:
                    result[x].append(temp)
                    print(temp)

                if i != j:

                    # while 1 --> break에 도착할 때까지 코드 무한 반복
                    # 왜냐면 코드 속도에 비해서 크롬 속도가 느려서 element가 로딩이 안되면 alert나 invisible exception이 발생
                    # 그거를 막기 위해서 while, try, except 사용
                    while 1:
                        # '도착' 에 주소 넣고 경로 찾은 다음에 자전거 경로 찾는 동작
                        try:
                            input_toadd = driver.find_element_by_xpath('//*[@id="info.route.waypointSuggest.input1"]')
                            input_toadd.send_keys(j)
                            time.sleep(1.5)
                            input_toadd.send_keys(Keys.ENTER)
                            time.sleep(1.5)
                            driver.find_element_by_xpath('//*[@id="biketab"]').click()
                            time.sleep(1.5)
                            # print('1')
                            break

                        # 코드가 너무 빨리 동작해서 '도착'에 주소 넣고 엔터누른게 잘 인식이 안 된 경우
                        except UnexpectedAlertPresentException:
                            alert = driver.switch_to.alert
                            alert.accept()
                            # print('2')
                            continue

                        # 기타 등등. 뒤에서 나오는 exception의 경우가 다양해서 except: 로 통일
                        except:
                            driver.find_element_by_xpath('//*[@id="biketab"]').click()
                            time.sleep(1.5)
                            # print('3')
                            continue


                    # 시간
                    while 1:
                        # 시간이 숫자가 아니라 이미지 하나하나로 구성되어 있어서 시간에 해당하는 이미지들을 리스트로 받고
                        # 이미지 각각의 이름에서 숫자 추출 후 int로 변환
                        try:
                            timelst = driver.find_elements_by_css_selector(
                                'li.BikeRouteItem.BikeRouteItem-HOVER > div.BikeRouteSummaryView > div.contents > p > span.time > span.ImageNumberView.minute > span')
                            a = ''
                            for k in timelst:
                                a += k.get_attribute('class')[-1]
                            a = int(a)
                            # print('1')
                            break

                        except UnexpectedAlertPresentException:
                            alert = driver.switch_to.alert
                            alert.accept()
                            time.sleep(1.5)
                            # print('5')
                            continue

                        except:
                            driver.find_element_by_xpath('//*[@id="biketab"]').click()
                            time.sleep(1.5)
                            try:
                                hourlst = driver.find_elements_by_css_selector(
                                    'li.BikeRouteItem.BikeRouteItem-HOVER > div.BikeRouteSummaryView > div.contents > p > span.time > span.ImageNumberView.hours > span')
                                a = ''
                                b = ''
                                for k in hourlst:
                                    a += k.get_attribute('class')[-1]
                                a = 60 * int(a)
                                try:
                                    minutelst = driver.find_elements_by_css_selector(
                                        'li.BikeRouteItem.BikeRouteItem-HOVER > div.BikeRouteSummaryView > div.contents > p > span.time > span.ImageNumberView.minuteMargin > span')
                                    for k in minutelst:
                                        b += k.get_attribute('class')[-1]
                                    b = int(b)
                                    a += b
                                except:
                                    break
                                # print('2')
                                break
                            except:
                                continue
                    print(a)



                    # 거리
                    while 1:
                        # 거리도 숫자가 아니라 이미지와 점으로 구성되어 있어서 파일명에서 숫자 추출 후 float으로 변환
                        try:
                            distancelist = driver.find_elements_by_css_selector(
                                'li.BikeRouteItem.BikeRouteItem-HOVER > div.BikeRouteSummaryView > div.contents > p > span.distance > span.ImageNumberView.distanceImage > span')
                            b = ''
                            unit = driver.find_element_by_xpath(
                                '//*[@id="info.bikeRoute"]/div[1]/ul/li[1]/div[1]/div[1]/p/span[2]/span[2]').text
                            for k in distancelist:
                                if k.get_attribute('class') == 'dot':
                                    b += '.'
                                else:
                                    b += k.get_attribute('class')[-1]
                            b = float(b)
                            # print(unit)
                            if b > 40:
                                b = round(b / 1000, 1)
                            break
                            # print('7')

                        except UnexpectedAlertPresentException:
                            alert = driver.switch_to.alert
                            alert.accept()
                            time.sleep(1)
                            # print('8')
                            continue

                        except:
                            driver.find_element_by_xpath('//*[@id="biketab"]').click()
                            time.sleep(1)
                            # print('9')
                            continue
                    print(b)

                            # 고도차
                    while 1:
                        # str 형태로 들어가있는 최대, 최소 고도 int로 변환 후 (-) 연산
                        try:
                            altitude = driver.find_element_by_xpath(
                                '//*[@id="info.bikeRoute"]/div[1]/ul/li[1]/div[1]/div[1]/div/span[1]').text
                            lsta = altitude.split()
                            altdif = int(lsta[-1][:-1]) - int(lsta[2][:-2])
                            # print('10')
                            break

                        except UnexpectedAlertPresentException:
                            alert = driver.switch_to.alert
                            alert.accept()
                            time.sleep(1.5)
                            # print('11')
                            continue

                        except:
                            driver.find_element_by_xpath('//*[@id="biketab"]').click()
                            time.sleep(1.5)
                            # print('12')
                            continue
                    print(altdif)

                            # 칼로리
                    while 1:
                        try:
                            # str 형태의 칼로리 int로 변환
                            calorie = driver.find_element_by_xpath(
                                '//*[@id="info.bikeRoute"]/div[1]/ul/li[1]/div[1]/div[1]/div/span[2]').text[1:-4]
                            calorie = int(calorie)
                            # print('13')
                            break
                        except UnexpectedAlertPresentException:
                            alert = driver.switch_to.alert
                            alert.accept()
                            time.sleep(1.5)
                            # print('14')
                            continue

                        except:
                            driver.find_element_by_xpath('//*[@id="biketab"]').click()
                            time.sleep(1.5)
                            # print('15')
                            continue
                    print(calorie)

                            # temp 리스트에 정보 저장 후 result의 x행에 추가. 이걸 133번하면 열이 133개 생기고
                    # 각 원소는 4개의 정보를 갖는 리스트 형태
                    temp.append(a)
                    temp.append(b)
                    temp.append(altdif)
                    temp.append(calorie)
                    result[x].append(temp)
                    print(temp)


                    # print('다음y로')
                    # 다음 도착지로 넘어가기 위해서 주소 지워주는 코드
                    driver.find_element_by_xpath('//*[@id="info.route.searchBox"]/div[2]/span[1]/a').click()
                    time.sleep(1.5)

        # print('다음x로')
        # 다음 출발지로 넘어가기 위해서 출발 및 도착 주소 지워주는 코드
        # 를 짰었는데 크롬이 메모리를 몇 기가씩 차지해서 그냥 닫았다가 새로 열려고 바꿈
        driver.close()


        # result 리스트를 pickle 형태로 저장
        with open('ddarrung{0}.pickle'.format(x), 'wb') as f:
            pickle.dump(result, f)
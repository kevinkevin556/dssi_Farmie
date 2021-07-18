from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException
from time import sleep

def get_all_regions_month_price_html(year, month, product_class=u"蔬菜類", product=18, long_delay=7, short_delay=5):
    driver = webdriver.Chrome()
    driver.get("https://apis.afa.gov.tw/pagepub/AppContentPage.aspx?itemNo=PRI095")
    
    # 價格選項：月平均價格
    try:
        driver.find_element_by_id("WR1_1_Q_AvgPriceType_C1_1").click()
    except: 
        sleep(long_delay)
        driver.find_element_by_id("WR1_1_Q_AvgPriceType_C1_1").click()

    # 年度
    try:
        driver.find_element_by_id("WR1_1_Q_PRSR_Year_C1").click()
        Select(driver.find_element_by_id("WR1_1_Q_PRSR_Year_C1")).select_by_visible_text(str(year))
        driver.find_element_by_id("WR1_1_Q_PRSR_Year_C1").click()
    except:
        sleep(long_delay)
        driver.find_element_by_id("WR1_1_Q_PRSR_Year_C1").click()
        Select(driver.find_element_by_id("WR1_1_Q_PRSR_Year_C1")).select_by_visible_text(str(year))
        driver.find_element_by_id("WR1_1_Q_PRSR_Year_C1").click()
    finally:
    # 月份
        driver.find_element_by_id("WR1_1_Q_PRSR_Month_C1").click()
        Select(driver.find_element_by_id("WR1_1_Q_PRSR_Month_C1")).select_by_visible_text(str(month))
        driver.find_element_by_id("WR1_1_Q_PRSR_Month_C1").click()
    # 作物類別
        driver.find_element_by_id("WR1_1_Q_GroupCode_XX_C1").click()
        Select(driver.find_element_by_id("WR1_1_Q_GroupCode_XX_C1")).select_by_visible_text(product_class)
    
    # 作物種類
    try:
        driver.find_element_by_id("WR1_1_PRMG_01_"+str(product)).click()
    except: 
        sleep(short_delay)
        driver.find_element_by_id("WR1_1_PRMG_01_"+str(product)).click()
    
    # 回傳查詢結果 html
    driver.find_element_by_link_text(u"查詢").click()
    sleep(long_delay)
    driver.switch_to_window(driver.window_handles[-1])
    html = driver.page_source
    sleep(short_delay)
    driver.quit()  
    return html

test_html = get_all_regions_month_price_html(2010, 5)

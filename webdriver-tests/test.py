from selenium import webdriver
from selenium.webdriver import FirefoxOptions
from selenium.webdriver.common.keys import Keys

# chromeOptions = ChromeOptions()
# chromeOptions.add_argument("--headless")
# driver = webdriver.Chrome(options=chromeOptions)
options = FirefoxOptions()
options.headless = True;
driver = webdriver.Firefox(options=options)

driver.get("server:8001")
print(driver.title)

driver.close()

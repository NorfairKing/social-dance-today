import argparse
import time

from selenium import webdriver
from selenium.webdriver import FirefoxOptions
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

parser = argparse.ArgumentParser(description="Webdriver tests")
parser.add_argument(
    "--host",
    metavar="HOST",
    default="localhost:8000",
    type=str,
    help="The server to test",
)
parser.add_argument(
    "--headless",
    nargs="?",
    const=True,
    default=False,
    type=bool,
    help="Whether to run with display",
)

args = parser.parse_args()

host = args.host
options = FirefoxOptions()
options.headless = args.headless
driver = webdriver.Firefox(options=options)

print("Getting homepage")
driver.get(host)

# Do a real search
print("Doing a search")
element = driver.find_element_by_id("queryInput")
element.send_keys("Zürich")
element.submit()

time.sleep(1)

# Try to register
print("Registering")
driver.find_element_by_xpath('//a[contains(text(), "Sign up")]').click()
driver.find_element_by_name("email-address").send_keys("test@example.com")
driver.find_element_by_name("passphrase").send_keys("test-pass")
driver.find_element_by_name("passphrase-confirm").send_keys("test-pass")
driver.find_element_by_xpath('//button[contains(text(), "Sign up")]').click()

time.sleep(1)

# Try to delete account
print("Deleting account")
driver.find_element_by_xpath('//a[contains(text(), "Account")]').click()
driver.find_element_by_xpath('//button[contains(text(), "Delete account")]').click()
WebDriverWait(driver, 10).until(EC.alert_is_present())
driver.switch_to.alert.accept()

time.sleep(1)

driver.close()

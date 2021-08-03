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
parser.add_argument(
    "--wait",
    nargs="?",
    const=True,
    default=False,
    type=bool,
    help="Wether to wait between steps",
)


args = parser.parse_args()

host = args.host


def wait(seconds=1):
    if args.wait:
        time.sleep(seconds)


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

wait()

# Try to register
print("Registering")
driver.find_element_by_xpath('//a[contains(text(), "Sign up")]').click()
driver.find_element_by_name("email-address").send_keys("test@example.com")
driver.find_element_by_name("passphrase").send_keys("test")
driver.find_element_by_name("passphrase-confirm").send_keys("test")
driver.find_element_by_xpath('//button[contains(text(), "Sign up")]').click()

wait()

# Set up an organiser profile
print("Setting up an organiser profile")
driver.find_element_by_xpath('//a[contains(text(), "Organiser profile")]').click()
driver.find_element_by_name("name").send_keys("Test Organiser")
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
driver.find_element_by_xpath('//a[contains(text(), "View Public Profile")]').click()

wait()

# Submit a party
print("Submitting a party")
driver.find_element_by_xpath('//a[contains(text(), "My parties")]').click()
driver.find_element_by_xpath('//a[contains(text(), "Submit party")]').click()
driver.find_element_by_name("title").send_keys("Test Party")
driver.find_element_by_name("day").send_keys("2021-07-14")
# The address must be something we definitely have in cache
driver.find_element_by_name("address").send_keys("Zürich")
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
# Twice, just to make sure that editing without changing anything at least doesn't crash.
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
driver.find_element_by_xpath('//a[contains(text(), "Public Party Profile")]').click()

wait()

# Edit the party
print("Editing the party")
driver.find_element_by_xpath('//a[contains(text(), "My parties")]').click()
driver.find_element_by_xpath(
    '//a[contains(text(), "Edit")]'
).click()  # There's only one party so this should work.
driver.find_element_by_name("title").clear()
driver.find_element_by_name("title").send_keys("Test Party (Edited)")
driver.find_element_by_name("description").send_keys(
    "This is a Test\nDescription with\nnewlines."
)
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
# Twice, just to make sure that editing without changing anything still doesn't crash.
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
driver.find_element_by_xpath('//a[contains(text(), "Public Party Profile")]').click()

wait()

# Duplicate the party
print("Duplicating the party")
driver.find_element_by_xpath('//a[contains(text(), "My parties")]').click()
driver.find_element_by_xpath(
    '//a[contains(text(), "Duplicate")]'
).click()  # There's only one party so this should work.
driver.find_element_by_name("title").clear()
driver.find_element_by_name("title").send_keys("Test Party (Duplicated)")
driver.find_element_by_name("day").send_keys("2021-07-15")
driver.find_element_by_name("description").clear()
driver.find_element_by_name("description").send_keys(
    "This is the description of the duplicated party."
)
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
# Twice, just to make sure that editing without changing anything still doesn't crash.
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
driver.find_element_by_xpath('//a[contains(text(), "Public Party Profile")]').click()

wait()

# Cancel the party
print("Cancelling the party")
driver.find_element_by_xpath('//a[contains(text(), "My parties")]').click()
driver.find_element_by_xpath(
    '//button[contains(text(), "Cancel")]'
).click()  # There's only one party so this should work.

wait()

# Delete the party
print("Deleting the party")
driver.find_element_by_xpath('//a[contains(text(), "My parties")]').click()
driver.find_element_by_xpath(
    '//button[contains(text(), "Delete")]'
).click()  # There's only one party so this should work.
WebDriverWait(driver, 10).until(EC.alert_is_present())
driver.switch_to.alert.accept()
driver.refresh()

wait()

# Submit a schedule
print("Submitting a schedule")
driver.find_element_by_xpath('//a[contains(text(), "My party schedules")]').click()
driver.find_element_by_xpath('//a[contains(text(), "Submit a party schedule")]').click()
driver.find_element_by_name("title").send_keys("Test Schedule")
driver.find_element_by_xpath(
    "//select[@name='recurrence-day-of-week']/option[text()='Friday']"
).click()
# The address must be something we definitely have in cache
driver.find_element_by_name("address").send_keys("Zürich")
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
# Twice, just to make sure that editing without changing anything at least doesn't crash.
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()

wait()

# Edit the schedule
print("Editing the schedule")
driver.find_element_by_xpath('//a[contains(text(), "My party schedules")]').click()
driver.find_element_by_xpath(
    '//a[contains(text(), "Edit")]'
).click()  # There's only one schedule so this should work.
driver.find_element_by_name("title").clear()
driver.find_element_by_name("title").send_keys("Test Schedule (Edited)")
driver.find_element_by_name("description").send_keys(
    "This is a Test\nDescription with\nnewlines."
)
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()
# Twice, just to make sure that editing without changing anything still doesn't crash.
driver.find_element_by_xpath('//button[contains(text(), "Submit")]').click()

wait()

# Delete the schedule
print("Deleting the schedule")
driver.find_element_by_xpath('//a[contains(text(), "My party schedules")]').click()
driver.find_element_by_xpath(
    '//button[contains(text(), "Delete")]'
).click()  # There's only one party so this should work.
WebDriverWait(driver, 10).until(EC.alert_is_present())
driver.switch_to.alert.accept()
driver.refresh()

wait()

# Try to delete account
print("Deleting account")
driver.find_element_by_xpath('//a[contains(text(), "Account")]').click()
driver.find_element_by_xpath('//button[contains(text(), "Delete Account")]').click()
WebDriverWait(driver, 10).until(EC.alert_is_present())
driver.switch_to.alert.accept()

wait()

driver.close()

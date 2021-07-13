import argparse

from selenium import webdriver
from selenium.webdriver import FirefoxOptions
from selenium.webdriver.common.keys import Keys

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

driver.get(host)
print(driver.title)

driver.close()

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException
from utilities import *
import time


password = "parola"
mail = "un_mail"
link = "https://www.linkedin.com/jobs/search/?currentJobId=3325068125&f_E=1%2C2%2C3&f_TPR=r2592000&geoId=106670623&location=Romania&refresh=true&sortBy=DD"

chrome_driver_path = "C:\Development\chromedriver.exe"
custom_options = webdriver.ChromeOptions()
custom_options.add_argument('--lang=en')
#custom_options.add_argument("--kiosk")  #to maximize window

driver = webdriver.Chrome(executable_path=chrome_driver_path, options=custom_options)
driver.get(link)

#SIGN-IN
sign_in_button = driver.find_element(by=By.CLASS_NAME, value="nav__button-secondary")
sign_in_button.click()
time.sleep(2)

email_label = driver.find_element(by=By.ID, value="username")
email_label.send_keys(mail)

pass_label = driver.find_element(by=By.ID, value="password")
pass_label.send_keys(password)
pass_label.send_keys(Keys.ENTER)
time.sleep(5)

page = 0
count = 0
while page < 40:
    #COUNTING JOBS ON PAGE
    time.sleep(1)
    jobs = driver.find_elements_by_class_name("jobs-search-results__list-item") #find_element(by=By.CLASS_NAME, value="jobs-search-results__list-item")
    # print(len(jobs))

    for index in range(len(jobs)):
        try:
            driver.execute_script("arguments[0].scrollIntoView();", jobs[index - 1])

            time.sleep(1)
            jobs[index].click()
            time.sleep(1)

            role = driver.find_element_by_class_name("jobs-unified-top-card__job-title").text
            job_description = driver.find_element_by_id("job-details").text.replace(",", "")
            company = driver.find_element_by_class_name("jobs-unified-top-card__company-name").text.replace(",", "")
            location = driver.find_element_by_class_name("jobs-unified-top-card__bullet").text.replace(",", "")
            workplace_type = driver.find_element_by_class_name("jobs-unified-top-card__workplace-type").text.replace(",", "")
            duration_level = driver.find_element_by_class_name("jobs-unified-top-card__job-insight span").text.replace(",", "").split( '·')
            employees_industry = driver.find_elements_by_class_name("jobs-unified-top-card__job-insight span")[1].text.replace(",", "").split('·')

            #Formatting  time_level and employees_industry
            if len(duration_level) == 2:
                duration, level = duration_level[0][:-1], duration_level[1][1:]
            else:
                if 'time' in duration_level[0]:
                    duration, level = duration_level[0], ''
                else:
                    duration, level = '', duration_level[0]

            if len(employees_industry) == 2:
                employees, industry = employees_industry[0][:-1], employees_industry[1][1:]
            else:
                if any([char.isdigit() for char in employees_industry[0]]):
                    employees, industry = employees_industry[0], ''
                else:
                    employees, industry = '', employees_industry[0]

            #Translating into english
            info = [role, company, location, workplace_type, duration, level, employees, industry, job_description]
            info_en = [translate_if_needed(v) for v in info]
            print(str(page+1) + " " + str(index) + " ", info_en)

            count +=1
            write_to_csv(info_en[:-1])
            write_text_file(info_en[-1], count)


        except NoSuchElementException:
            print("Data not complete, skipped.")
            continue
        except StaleElementReferenceException:
            print("Something threw me off page.")
            time.sleep(5)
            driver.back()
            time.sleep(5)
            jobs = driver.find_elements_by_class_name("jobs-search-results__list-item")
            time.sleep(1)
            jobs[index + 1].click()
            time.sleep(1)
            continue


    page += 1
    print(page)

    page_buttons = driver.find_elements_by_class_name('artdeco-pagination__indicator')
    if page in [1, 2, 3, 4, 5, 6, 7, 8]:
        page_buttons[page].click()
    elif page in [33, 34, 35, 36, 37, 38, 39]:
        page_buttons[page - 30 - 1].click()
    elif page == 40:
        print("Gata")
    else:
        page_buttons[6].click()



driver.quit()




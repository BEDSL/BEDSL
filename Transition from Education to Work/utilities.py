import csv
from googletrans import Translator
import os
#pip3 install googletrans==3.1.0a0

def translate_if_needed(x):
    translator = Translator()
    if translator.detect(x).lang == "ro":
        x = translator.translate(x).text
    return x


def write_text_file(x, index):
    name_of_file = str(index) + ".txt"
    location = os.path.join("files", name_of_file)
    with open(location, 'w',  encoding="utf-8") as f:
        f.write(x)


def write_to_csv(list):
    with open('data.csv', 'a', newline="", encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerow(list)


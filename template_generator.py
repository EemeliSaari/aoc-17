######################################################################
#                                                                   #
#                                                                   #
#           Simple Advent of code template generator                #   
#           that makes all the folders with README.md               #
#           files attached. README.md files contain                 #
#           the UNIX styled chrismast calender that has             #
#           been generated with random numbers. Format              #
#           of is specifically for GitHub and might not             #
#           work in other platforms that use Markdown.              #
#                                                                   #
#                                                                   #
#####################################################################

import os
import random
import pickle


def make_calender(date, numbers):

    text = ""
    index = 0

    for row in range(0,19):

        if row % 3 == 0:
            text += "".join(["" + "+-----" for x in range(0,4)]) + "+<br>"

        if row % 2 == 0 and row % 3 != 0:    

            for i in range(0,4):

                if i == 0:
                    p = 0
                else:
                    p = 1

                if numbers[index] <= date:
                    number = numbers[index]
                    
                    start = "".join(["" + "&nbsp;" for x in range(0, 4 - (len(str(number))-1)-p)])
                    end = "".join(["" + "&nbsp;" for x in range(0, 4 - (len(str(number))-1))])
                    
                    text += "|{:}{:}{:}".format(start, number, end)

                else:

                    white = "" + "".join(["" + "&nbsp;" for x in range(0, 10-p)])
                    
                    text += "|" + white

                index += 1

            text += "|<br>"

    return text


def make_shuffled_dates():

    numbers = [x for x in range(1, 24 + 1)]
    shuffled = []

    for i in range(0, 24):
        index = random.randrange(0, len(numbers))
        shuffled.append(numbers[index])
        numbers.pop(index)

    with open('dates.pickle', 'wb') as file:
        pickle.dump(shuffled, file)


def make_readme(day, calender):

    if(day > 0):
        packet = "# {:d}. Day\n\n{:s}\n### Task: \n\n### Method: \n\n##### Links:\n".format(day, calender)
        path = 'day{:}'.format(day)
        if not os.path.exists(path):
            os.mkdir(path)
    else:
        packet = "# Advent of Code 2017\n\n### Purpose: \n\n### Goals:"
        path = "."

    with open(path + '/README.md', 'w') as f:
        f.write(packet)


def main():

    if not os.path.exists('days_dump.pickle'):
        make_shuffled_dates()

    with open('dates.pickle', 'rb') as file:
        data = pickle.load(file)

    make_readme(15, make_calender(15,data))

    for i in range(0,24+1):
        make_readme(i, make_calender(i, data))

    os.remove('dates.pickle')

if __name__ == '__main__':
    main()

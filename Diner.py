from MenuItem import MenuItem

class Diner(object):
    # class variable: list of strings containing the possible statuses a diner might have
    STATUSES = ["seated", "ordering", "eating", "paying", "leaving"]

    # input: str diner's name
    # return: none
    def __init__(self, name):
        self.__name = name
        self.__order = []   # an empty list since the diner has not ordered any menu items yet
        self.__status = 0   # set it to 0 since the diner just seated

    # getters:
    # input: none
    # return: str diner's name
    def getName(self):
        return self.__name

    # input: none
    # return: list of the MenuItem objects ordered by the diner
    def getOrder(self):
        return self.__order

    # input: none
    # return: int that represents the diner's current dining status
    def getStatus(self):
        return self.__status

    # setters:
    # input: str newName
    # return: none
    def setName(self, newName):
        if newName.isalpha() == True:       # if the newName consists of the alphabet
            self.__name = newName
        else:
            print("Invalid name")

    # input: list newList
    # return: none
    def setOrder(self, newOrder):
        self.__order = newOrder


    # input: int newStatus
    # return: none
    def setStatus(self, newStatus):
        if newStatus.isdigit() == True:
            self.__status = newStatus
        else:
            print("Invalid integer for the diner's current dining status")

    # method updateStatus
    # increase the diner's status by 1
    # input: none
    # return: none
    def updateStatus(self):
        self.__status += 1

    # method addToOrder
    # adds the menu item to the end of the list of menu items (self.__order)
    # input: a MenuItem object
    # return: none
    def addToOrder(self, menuItem):
        self.__order.append(menuItem)       # append the menu item to the list of menu items


    # method printOrder
    # print a message containing all the menu items the diner ordered
    # input: none
    # return: none
    def printOrder(self):
        for menuItemOrderedObject in self.__order:     # print each MenuItem object in the list of menu items (self.__order)
            print("-", menuItemOrderedObject)

    # method calculateMealCost
    # calculate the total cost of menu items the diner ordered
    # input: none
    # return: float total cost
    def calculateMealCost(self):
        totalCost = 0.00   # start with 0
        for menuItemOrderedObject in self.__order:
            totalCost += float(menuItemOrderedObject.getPrice())     # every time we loop, get the price from the menu item object, convert it to the float, and add it to the total cost
        return totalCost

    # input: none
    # return: str msg
    def __str__(self):
        msg = "Diner " + self.__name + " is currently " + Diner.STATUSES[self.__status] + "."
        return msg


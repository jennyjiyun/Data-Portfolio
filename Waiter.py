from Menu import Menu
from Diner import Diner

class Waiter(object):
    # input: a Menu object
    # return: none
    def __init__(self, menu):
        self.__diners = []      # initialize the list of diners to an empty list
        self.__menu = menu

    # method addDiner
    # add the new Diner object to the waiter's list of diners
    # input: a Diner object
    # return: none
    def addDiner(self, diner):
        self.__diners.append(diner)

    # method getNumDiners
    # get the number of diners the waiter is currently taking care of
    # input: none
    # return: int the number of diners
    def getNumDiners(self):
        return len(self.__diners)       # the length of the list of diners is the same as the number of diners the waiter is currently taking care of

    # method printDinerStatuses
    # print all the diners the water is keeping track of, grouped by their statuses
    # input: none
    # return: none
    def printDinerStatuses(self):
        for status in Diner.STATUSES:   # loop through each of the possible statuses a diner can have
            print("Diners who are " + status + ":")
            for diner in self.__diners:         # loop through each of diner object in the list of diners (self.__diners)
                if status == Diner.STATUSES[diner.getStatus()]:     # get the string status of the diner (instead of integer) and see if it's equal to status(string)
                    print("\t" + str(diner))


    # method takeOrders
    # if the diner is ordering, print the menu items for each menu type, then ask the diner to order a menu item by selecting a number
    # add the item to the diner, then print the diner's order
    # input: none
    # return: none
    def takeOrders(self):
        for diner in self.__diners:     # loop through each diner in the list of Diner objects
            if diner.getStatus() == 1:      # 'ordering' is at the index position of 1 in STATUSES
                for menuType in Menu.MENU_ITEM_TYPES:   # loop through the different menu types in MENU_ITEM_TYPES
                    self.__menu.printMenuItemsByType(menuType)     # print the menu items of that type

                    # ask the diner to order a menu item by selecting a number
                    dinerChoice = int(input(diner.getName() + ", please select a " + menuType + " menu item number."))

                    # error checking: if the user put the invalid number, which is any number greater than or equal to the length of the list with key menuType OR any negative number
                    while dinerChoice >= self.__menu.getNumMenuItemsByType(menuType) or dinerChoice < 0:
                        dinerChoice = int(input("Please select a " + menuType + " menu item number again."))

                    food = self.__menu.getMenuItem(menuType, dinerChoice)       # use the dinerChoice as the integer for the indexPosition input in the getMenuItem method
                    diner.addToOrder(food)      # add the item to the order
                print(diner.getName() + " ordered: ")
                diner.printOrder()      # print the diner's order

    # method ringUpDiners
    # check if the diner is paying and if he/she is, calculate the diner's meal cost and print out the message
    # input: none
    # return: none
    def ringUpDiners(self):
        for diner in self.__diners:
            if diner.getStatus() == 3:      # 'paying' is at the index position of 3 in STATUSES
                totalCost = diner.calculateMealCost()           # calculate the diner's meal cost
                print(diner.getName() + ", your meal cost is $" + str(totalCost))       # print out the message

    # method removeDoneDiners
    # check if the diner is leaving, and if he/she is, print a message thanking the diner
    # input: none
    # return: none
    def removeDoneDiners(self):
        # loop through the list of diners backwards because the diner will have a different index number once the other diner gets removed
        for i in range(len(self.__diners)-1, -1, -1):       # start: the length of the list of diners-1, end: -1 (not 0), step: -1 (backwards)
            if self.__diners[i].getStatus() == 4:       # 'leaving' is at the index position of 4 in STATUSES
                print(self.__diners[i].getName() + ", thank you for dining with us! Come again soon!")      # print a message thanking the diner
                self.__diners.remove(self.__diners[i])      # remove the diner from the list

    # method advanceDiners
    # allow the waiter to attend to the diners at their various stages as well as move each diner on to the next stage
    # input: none
    # return: none
    def advanceDiners(self):
        self.printDinerStatuses()
        print()     # a blank line after printing out the diners' statuses to make it look nicer for readers to read
        self.takeOrders()
        self.ringUpDiners()
        self.removeDoneDiners()
        for diner in self.__diners:     # for each diner in the list of diners, update their status
            diner.updateStatus()


from MenuItem import MenuItem

class Menu(object):
    # class variable: list containing 4 strings
    MENU_ITEM_TYPES = ["Drink", "Appetizer", "Entree", "Dessert"]

    # input: str name of the csv file
    # return: none
    def __init__(self, fileName):
        self.__menuItemDictionary = {}  # initialize this instance attribute as an empty dictionary that would contain MenuItem objects
        dataList = []  # create an empty list
        inputFile = open(fileName, "r")     # open the csv file
        for line in inputFile:      # read the file
            line = line.strip()
            line.split(",")
            dataList.append(line)   # add each line to the empty list
            # dataList is a list of lists, where each mini-list is a list containing 4 pieces of information about each menu item
        for miniList in dataList:   # create a MenuItem object from each miniList   # the order of the miniList is the same as the order of MenuItem object's input
            miniList = miniList.split(",")
            menuItem = MenuItem(miniList[0], miniList[1], miniList[2], miniList[3])

            # add the new object to the dictionary (its type is the key)
            # check if the type of menu exists in the dictionary
            if menuItem.getType() in self.__menuItemDictionary:    # if the type of menu already exists in the dictionary, append a new MenuItem object to the list of objects under the key menuItem.getType()
                self.__menuItemDictionary[menuItem.getType()].append(menuItem)
            else:   # if the type of menu does not exist, add a new menuItem object to the dictionary by using menuItem.getType() as a key and by creating a list of menuItem objects for the value
                self.__menuItemDictionary[menuItem.getType()] = [menuItem]
        inputFile.close()           # close the file

    # method: getMenuItem
    # get the correct MenuItem object from the dictionary using its type and index position in the list of items
    # input: str a type of menu(menuType), int index position of a certain menu item
    # return: a MenuItem object from the dictionary
    def getMenuItem(self, menuType, indexPosition):
        menuItem = self.__menuItemDictionary[menuType][indexPosition]   # among the values that has a key MenuType, return an object positioned at the indexPosition
        return menuItem


    # method: printMenuItemsByType
    # print a header with the type of menu items, followed by a numbered list of all the menu items of that type
    # input: str a type of menu(menuType)
    # return: none
    def printMenuItemsByType(self, menuType):
        count = 0
        # check if the menuType is in the dictionary
        if menuType.capitalize() in self.__menuItemDictionary:
            print("\n-----" + menuType.capitalize() + "-----")        # print a header with the type of menu items
            for item in self.__menuItemDictionary[menuType.capitalize()]:       # for each value(menuitem) in dict[key]
                print(str(count), ")", item)
                count += 1


    # method: getNumMenuItemsByType
    # return the number of MenuItems of the input type
    # input: str a type of menu (menuType)
    # return: int the number of MenuItems of the input type
    def getNumMenuItemsByType(self, menuType):
        return len(self.__menuItemDictionary[menuType.capitalize()])        # return the length of the list of the MenuItem objects under the menuType (key)

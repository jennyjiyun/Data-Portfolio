class MenuItem(object):
    # inputs: str name, str type, float price, str description
    # return: none
    def __init__(self, name, typeOfItem, price, description):
        self.__name = name
        self.__typeOfItem = typeOfItem
        self.__price = price
        self.__description = description

    # getters:
    # input: none
    # return: str name
    def getName(self):
        return self.__name

    # input: none
    # return: str type
    def getType(self):
        return self.__typeOfItem

    # input: none
    # return: float price
    def getPrice(self):
        return self.__price

    # input: none
    # return: str description
    def getDescription(self):
        return self.__description

    # setters:
    # input: str newName
    # return: none
    def setName(self, newName):
        if newName.isalpha() == True:       # if the newName consists of the alphabet
            self.__name = newName
        else:
            print("Invalid name")

    # input: str newType
    # return: none
    def setType(self, newType):
        if newType.isalpha() == True:       # if the newType consists of the alphabet
            self.__typeOfItem = newType
        else:
            print("Invalid type")

    # input: float newPrice
    # return: none
    def setPrice(self, newPrice):
        if newPrice >= 0:       # if the newPrice is a non-negative float
            self.__price = newPrice
        else:
            print("Invalid price")

    # input: str description
    # return: none
    def setDescription(self, newDescr):
        if newDescr.isalpha() == True:  # if the newDescr consists of the alphabet
            self.__description = newDescr
        else:
            print("Invalid description")

    # input: none
    # return: str msg
    def __str__(self):
        # convert float to string
        msg = self.__name + " (" + self.__typeOfItem + "): $" + str(self.__price) + "\n\t\t" + self.__description
        return msg


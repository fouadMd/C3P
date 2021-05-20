import abc


def add(x,y) :
    return x + y

def sub(x,y) :
    return x - y

DictOp = {"+" : add , "-" : sub,"*": lambda x,y: x * y, "/": lambda x,y: x / y, "**" : lambda x,y: int(float(x)**float(y))}


def trouverOp(op) :
    return DictOp[op]


class Expr :
    def printme(self):
        ...
    

class Value(Expr) :

    def __init__(self, val) :
        self.valeur = val

    def evaluate(self) :
        return self.valeur


class UniOp(Expr) :

    def __init__(self, op , val) :
        self.operation = op
        self.valeur = val

    def evaluate(self) :
        return (trouverOp(self.operation)(0,self.valeur.evaluate()))


class BinOp(Expr) :

    def __init__(self, op , val1 , val2) :
        self.operation = op
        self.valeur1 = val1
        self.valeur2 = val2

    def evaluate(self) :
        return trouverOp(self.operation)(self.valeur1.evaluate(),self.valeur2.evaluate())

class Var(Expr) :

    def __init__(self, n , val) :
        self.nom = n
        self.valeur = val

    def evaluate(self) :
        return self.valeur.evaluate()

    

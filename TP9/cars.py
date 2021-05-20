from spec import specification,Attribute

@specification
class Engine(object):
    pass


@specification
class Vehicule(object):
    number_of_wheels = Attribute(int)
    engine = Attribute(Engine)

@specification
class Car(Vehicule):
    pass

def specification(cls):
    cls.specification = True
    return cls

class Attribute(object):

    def __init__(self, t):
        self.type = t



def gen_header_class(cls):
    return f"""class {cls.__name__}({', '.join(s.__name__ for s in cls.__bases__)}):"""

def get_attributs(cls):
    attributs = []
    for attribut in cls.__dict__ :
        if (isinstance(cls.__dict__[attribut],Attribute)):
            attributs.append(attribut)
    return attributs

def gen_init_class2(cls):
    param ="\n\t\t" + "\n\t\t".join('self.'+ s +' = ' + s  for s in get_attributs(cls))
    return f"""def __init__(self, att_{', '.join(s + '=None' for s in get_attributs(cls))}):{param}"""


def gen_init_class(cls):
    res = "def __init__(self"
    attributs = get_attributs(cls)
    for a in attributs :
        res += ", " + a + "=None"
    res += "):" +"\n" + "\t"
    for a in attributs :
        res += "self." + a + " = " + a +"\n" + "\t"
    return res

def gen_getter_class(cls):
    res = ""
    for attribut in get_attributs(cls):
        res += "\t" + "@property" + "\n" + "\t" + "def " + attribut + "(self):"
        res += "\n" + "\t" + "\t" + "return self." + attribut + "\n" + "\n"
    return res

def gen_setter_class(cls):
    res = ""
    for attribut in get_attributs(cls):
        res += "\t" + "@"+attribut+".setter"+"\n"+ "\t" +"def "+ attribut +"(self,value):"
        res += "\n"+"\t" + "\t" + "assert value is None or isinstance(value,"
        res += cls.__dict__[attribut].type.__name__ + ")"
        res += "\n"+"\t"+ "\t" +"self."+attribut+" = value"+"\n"+"\n"
    return res

def gen_body(cls):
    res = ""
    res +=  "\t" + gen_init_class2(cls) + "\n" + "\n"
    res +=  gen_getter_class(cls)
    res +=  gen_setter_class(cls)
    return res


def gen_code_class(cls):
    res = gen_header_class(cls) + "\n" + "\n"
    if len(get_attributs(cls))>0 :
        res +=  gen_body(cls)
    else :
        res += "\t" "pass" + "\n" + "\n"
    return res

def collect_specification(mdl):
    res = []
    for k, v in mdl.__dict__.items():
        if hasattr(v,'specification'):
            res.append(v)
    return res

def gen_module(mdl):
    classes = collect_specification(mdl)
    res = ""
    for c in classes :
        res += gen_code_class(c)
    fichier = open(mdl.__name__ + "_gen.py","a" )
    fichier.write(res)
    fichier.close()

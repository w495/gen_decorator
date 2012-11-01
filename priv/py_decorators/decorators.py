# -*- coding: utf-8 -*-



def mkb(fn):
    # выполнится 1 раз
    print "mkb"
    def wrapped():
        # будет выполнится каждый раз
        print "mkb wrapped"
        return "<b>" + fn() + "</b>"
    return wrapped

def mki(fn):
    # выполнится 1 раз
    print "mki"
    def wrapped():
        # будет выполнится каждый раз
        print "mki wrapped"
        return "<i>" + fn() + "</i>"
    return wrapped

def mkbx(fn):
    # выполнится 1 раз
    print "mkbx"
    def wrapped(X):
        # будет выполнится каждый раз
        print "mkbx wrapped"
        return "<b>" + fn(X) + "</b>"
    return wrapped

def mkix(fn):
    # выполнится 1 раз
    print "mkix"
    def wrapped(X):
        # будет выполнится каждый раз
        print "mkix wrapped"
        return "<i>" + fn(X) + "</i>"
    return wrapped
    
@mkb
@mki
def hello():
    return "hello habr"


@mkbx
@mkix
def hello1(X):
    return "hello habr %s"%(X) 


if __name__ == "__main__":
    print hello()
    print hello1("!")
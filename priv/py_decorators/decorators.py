# -*- coding: utf-8 -*-



def mkb(fn):
    # выполнится 1 раз
    print "mkb"
    def wrapped():
        # будет выполнится каждый раз
        print "mkb wrapped"
        return "<b>" + fn() + "</b>"
    return wrapped
    return wrapped()

def mki(fn):
    # выполнится 1 раз
    print "mki"
    def wrapped():
        # будет выполнится каждый раз
        print "mki wrapped"
        return "<i>" + fn() + "</i>"
    return wrapped
    return wrapped()

@mkb
@mki
def hello():
    return "hello habr"



if __name__ == "__main__":
    print hello()
    print hello()
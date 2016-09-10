import re
def isNumber(value):
    try:
        value + 1
        return True
    except TypeError:
        return False
    
def islegal(value):
    pattern = re.compile(r'(^-?\d+d\d+\.\d$)|(^-?\d+d\d+$)')
    result = pattern.match(value)
    if result:
        return True
    else:
        return False
    
def spliter(value):
    pattern = re.compile(r'-?\d+\.?\d?')
    result = pattern.findall(value)
    return result
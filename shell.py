import Interpreter as interpreter
while True:
    text = input("UrduBasic > ")
    result, error = interpreter.run('<stdin>', text)
    if text.strip() == "": continue
    if error: print(error.as_string())
    elif result and (not isinstance(result, interpreter.nullObject)):
        if len(result.elements) == 1:
            if not (isinstance(result.elements[0], interpreter.nullObject) or result.elements[0] == None):
                print(result.elements[0])
        else:
            print(result)

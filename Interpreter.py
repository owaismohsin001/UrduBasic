from Tokens import *
from Errors import RTError
from Lexer import *
from Nodes import *
from Parser import *
import os

#RTResult
class RTResult:
    def __init__(self):
        self.reset()

    def reset(self):
        self.value = None
        self.error = None
        self.func_return_value = None
        self.loop_should_continue = False
        self.loop_should_break = False

    def register(self, res):
        if res.error: self.error = res.error
        self.func_return_value = res.func_return_value
        self.loop_should_continue = res.loop_should_continue
        self.loop_should_break = res.loop_should_break
        return res.value

    def success(self, value):
        self.reset()
        self.value = value
        return self

    def success_return(self, value):
        self.reset()
        self.func_return_value = value
        return self

    def success_continue(self):
        self.reset()
        self.loop_should_continue = True
        return self

    def success_break(self):
        self.reset()
        self.loop_should_break = True
        return self

    def failure(self, error):
        self.reset()
        self.error = error
        return self

    def should_return(self):
        return (
            self.error or
            self.func_return_value or
            self.loop_should_continue or
            self.loop_should_break
        )

#Values
class Value:
    def __init__(self):
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        return None, self.illegal_operation(other)

    def subbed_by(self, other):
        return None, self.illegal_operation(other)

    def multed_by(self, other):
        return None, self.illegal_operation(other)

    def dived_by(self, other):
        return None, self.illegal_operation(other)

    def moded_by(self, other):
        return None, self.illegal_operation(other)

    def powed_by(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_eq(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_ne(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lte(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gte(self, other):
        return None, self.illegal_operation(other)

    def ored_by(self, other):
        return None, self.illegal_operation(other)

    def anded_by(self, other):
        return None, self.illegal_operation(other)

    def notted(self):
        return None, self.illegal_operation(self)

    def execute(self):
        return RTResult().failure(self.illegal_operation())

    def is_true(self):
        return False

    def copy(self):
        raise Exception('No copy method defined')

    def illegal_operation(self, other=None):
        if not other: other = self
        return RTError(
            self.pos_start, other.pos_end,
            self.context,
            'Illegal Operation'
        )

class Number(Value):
    def __init__(self, value):
        self.value = value
        super().__init__()

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, self)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                other.pos_start, other.pos_end,
                self.context,
                "Zero se division"
                )
            return Number(self.value / other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def moded_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Zero se division"
                    )
            return Number(self.value % other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value == other.value)).set_context(self.context), None
        else:
            return Boolean(0), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value != other.value)).set_context(self.context), None
        else:
            return Boolean(1), None

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def ored_by(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def anded_by(self, other):
        if isinstance(other, Number):
            return Boolean(int(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def notted(self):
            return Boolean(1 if self.value == 0 else 0).set_context(self.context), None

    def is_true(self):
        return self.value != 0

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return String(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def dived_by(self, other):
        if isinstance(other, Number):
            to_return = list(self.value)[other.value]
            return String(to_return), None
        else:
            return None, Value.illegal_operation(self, other)

    def moded_by(self, other):
        if isinstance(other, Number):
            if other.value < 0:
                return String(self.value[:other.value]), None
            else:
                return String(self.value[other.value:]), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_eq(self, other):
        if isinstance(other, String):
            return Boolean(int(self.value == other.value)).set_context(self.context), None
        else:
            return Boolean(0), None

    def get_comparison_ne(self, other):
        if isinstance(other, String):
            return Boolean(int(self.value != other.value)).set_context(self.context), None
        else:
            return Boolean(1), None

    def is_true(self):
        return len(self.value) > 0

    def notted(self):
        return Boolean(not self.is_true()), None

    def ored_by(self, other):
        if isinstance(other, String):
            result = self.is_true() or other.is_true()
            return Boolean(result).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def anded_by(self, other):
        if isinstance(other, String):
            result = self.is_true() and other.is_true()
            return Boolean(result).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f'{self.value}'

class List(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements

    def added_to(self, other):
        if isinstance(other, List):
            return self.multed_by(other)
        else:
            new_list = List(self.elements[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
            new_list.elements.append(other)
            return new_list, None

    def subbed_by(self, other):
        if isinstance(other, Number):
            new_list = List(self.elements[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
            try:
                new_list.elements.pop(other.value)
                return new_list, None
            except:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Ye index nahi nikal sake kyu ke ye list me mila hi nahi"
                )
        else:
            return None, Value.illegal_operation(self, other)


    def moded_by(self, other):
        if isinstance(other, Number):
            if other.value < 0:
                return List(self.elements[:other.value]), None
            else:
                return List(self.elements[other.value:]), None
        else:
            return None, Value.illegal_operation(self, other)

    def multed_by(self, other):
        if isinstance(other, List):
            new_list = List(self.elements[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
            new_list.elements.extend(other.elements)
            return new_list, None
        else:
            return self.added_to(other)

    def dived_by(self, other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value], None
            except:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Ye number ki cheez list me mila hi nahi"
                )
        else:
            return None, Value.illegal_operation(self, other)

    def truth_of_list(self, other):
        truth = []
        if len(other.elements) != len(self.elements): return False
        if not isinstance(other, List): return False
        for index, element in enumerate(self.elements):
            if (0 <= index) and (index < len(other.elements)):
                call = self.elements[index].get_comparison_eq(other.elements[index])[0]
                if call:
                    if call.state == 1:
                        truth.append(True)
                    else:
                        break
                else:
                    break
        return (len(truth) == len(self.elements)) and (len(truth) == len(other.elements))

    def get_comparison_eq(self, other):
        if isinstance(other, List):
            return Boolean(int(self.truth_of_list(other))).set_context(self.context), None
        else:
            return Boolean(0), None

    def get_comparison_ne(self, other):
        if isinstance(other, List):
            return Boolean(int(not self.truth_of_list(other))).set_context(self.context), None
        else:
            return Boolean(1), None

    def is_true(self):
        return len(self.elements)>0

    def notted(self):
        return Boolean(not self.is_true()), None

    def ored_by(self, other):
        if isinstance(other, List):
            result = self.is_true() or other.is_true()
            return Boolean(result).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def anded_by(self, other):
        if isinstance(other, List):
            result = self.is_true() and other.is_true()
            return Boolean(result).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def copy(self):
        copy = List(self.elements)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f"[{', '.join([str(x) for x in self.elements])}]"

class Boolean(Number):
    def __init__(self, state):
        super().__init__(state)
        self.state = state

    def __repr__(self):
        return 'sahi' if self.state else 'galat'

    def copy(self):
        copy = Boolean(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

class nullObject(Number):
    def __init__(self):
        super().__init__(0)

    def __repr__(self):
        return 'khali'

    def copy(self):
        copy = nullObject()
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

class BaseFunction(Value):
    def __init__(self, name):
        super().__init__()
        self.name = name or "<anonymous>"

    def generate_new_context(self):
        new_context = Context(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        return new_context

    def check_args(self, arg_names, args):
        res = RTResult()
        if len(args)>len(arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                self.context,
                f"{len(args) - len(arg_names)} too many args passed into '{self.name}'"
            ))
        elif len(args)<len(arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                self.context,
                f"{len(args) - len(arg_names)} too many args passed into '{self.name}'"
            ))

        return res.success(null)

    def populate_args(self, arg_names, args, exec_ctx):
        for i in range(len(args)):
            arg_name = arg_names[i]
            arg_value = args[i]
            arg_value.set_context(exec_ctx)
            exec_ctx.symbol_table.set(arg_name, arg_value)

    def check_and_populate_args(self, arg_names, args, exec_ctx):
        res = RTResult()
        res.register(self.check_args(arg_names, args))
        if res.should_return(): return res
        self.populate_args(arg_names, args, exec_ctx)
        return res.success(null)

class Function(BaseFunction):
    def __init__(self, name, body_node, arg_names, should_auto_return):
        super().__init__(name)
        self.name = name or "<anonymous>"
        self.body_node = body_node
        self.arg_names = arg_names
        self.should_auto_return = should_auto_return

    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        exec_ctx = self.generate_new_context()
        res.register(self.check_and_populate_args(self.arg_names, args, exec_ctx))
        if res.should_return(): return res
        value = res.register(interpreter.visit(self.body_node, exec_ctx))
        if res.should_return() and res.func_return_value == None: return res
        ret_value = (value if self.should_auto_return else None) or res.func_return_value or null
        return res.success(ret_value)

    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names, self.should_auto_return)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy

    def __repr__(self):
        return f'function <{self.name}>'

class BuiltInFunction(BaseFunction):
    def __init__(self, name):
        super().__init__(name)

    def execute(self, args):
        res = RTResult()
        exec_ctx = self.generate_new_context()
        method_name = f'execute_{self.name}'
        method = getattr(self, method_name, self.no_visit_method)
        res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
        if res.should_return(): return res
        return_value = res.register(method(exec_ctx))
        if res.should_return(): return res
        return res.success(return_value)

    def no_visit_method(self, node, context):
        raise Exception(f'No execute_{self.name} method defined')

    def copy(self):
        copy = BuiltInFunction(self.name)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy

    def __repr__(self):
        return f'<built-in function {self.name}>'

    def execute_print(self, exec_ctx):
        print(str(exec_ctx.symbol_table.get("value")), end="")
        return RTResult().success(null)
    execute_print.arg_names = ["value"]


    def execute_println(self, exec_ctx):
        print(str(exec_ctx.symbol_table.get("value")))
        return RTResult().success(null)
    execute_println.arg_names = ["value"]

    def execute_print_ret(self, exec_ctx):
        val = str(exec_ctx.symbol_table.get("value"))
        print(val, end='')
        return RTResult().success(String(val))
    execute_print_ret.arg_names = ["value"]

    def execute_input(self, exec_ctx):
        text = input(exec_ctx.symbol_table.get("value"))
        return RTResult().success(String(text))
    execute_input.arg_names = ["value"]

    def execute_input_int(self, exec_ctx):
        while True:
            text = input(exec_ctx.symbol_table.get("value"))
            try:
                number = int(text)
                break
            except ValueError:
                print(f"'{text}' must be an integer")
        return RTResult().success(Number(number))
    execute_input_int.arg_names = ["value"]

    def execute_clear(self, exec_ctx):
        os.system('cls' if os.name == 'nt' else 'clear')
        return RTResult().success(null)
    execute_clear.arg_names = []

    def execute_is_number(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), Number)
        return RTResult().success(sahi if is_number else galat)
    execute_is_number.arg_names = ['value']

    def execute_is_string(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), String)
        return RTResult().success(sahi if is_number else galat)
    execute_is_string.arg_names = ['value']

    def execute_is_list(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), List)
        return RTResult().success(sahi if is_number else galat)
    execute_is_list.arg_names = ['value']

    def execute_is_function(self, exec_ctx):
        is_number = isinstance(exec_ctx.symbol_table.get("value"), BaseFunction)
        return RTResult().success(sahi if is_number else galat)
    execute_is_function.arg_names = ['value']

    def execute_list_conv(self, exec_ctx):
        string = exec_ctx.symbol_table.get("value")
        if not isinstance(string, String):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek string honi chahiye"
            ))
        list_ = list(string.value)
        list_ = [String(x) for x in list_]
        ret = List(list_)
        return RTResult().success(ret)
    execute_list_conv.arg_names = ['value']

    def execute_append(self, exec_ctx):
        given_list = exec_ctx.symbol_table.get("list")
        value = exec_ctx.symbol_table.get("value")
        index = exec_ctx.symbol_table.get("index")
        if not isinstance(given_list, List):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek list honi chahiye"
            ))
        if not isinstance(index, Number):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Akhri arument ek list honi chahiye"
            ))
        if index.value == -1:
            given_list.elements.append(value)
        else:
            given_list.elements.insert(index.value, value)
        return RTResult().success(null)
    execute_append.arg_names = ['list', 'value', 'index']

    def execute_str_conv(self, exec_ctx):
        given = exec_ctx.symbol_table.get("value")
        string = String(str(given.elements if isinstance(given, List) else given.value))
        return RTResult().success(string)
    execute_str_conv.arg_names = ['value']

    def execute_number_conv(self, exec_ctx):
        given = exec_ctx.symbol_table.get("value")
        try:
            if isinstance(given, float):
                string = Number(int(given.elements if isinstance(given, List) else given.value))
            else:
                string = Number(float(given.elements if isinstance(given, List) else given.value))
            return RTResult().success(string)
        except:
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ko number nahi bana sakte"
            ))
    execute_number_conv.arg_names = ['value']


    def execute_join_list(self, exec_ctx):
        given = exec_ctx.symbol_table.get("value")
        joiner = exec_ctx.symbol_table.get("joiner")
        if not isinstance(joiner, String):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek string honi chahiye"
            ))
        if not isinstance(given, List):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Dusra arument ek list honi chahiye"
            ))
        pre_string = [str(x) for x in given.elements]
        string = String(joiner.value.join(pre_string))
        return RTResult().success(string)
    execute_join_list.arg_names = ['joiner', 'value']

    def execute_split_str(self, exec_ctx):
        splitter = exec_ctx.symbol_table.get("splitter")
        splitee = exec_ctx.symbol_table.get("splitee")
        if not isinstance(splitter, String):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek string honi chahiye"
            ))
        if not isinstance(splitee, String):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Dusra arument ek string honi chahiye"
            ))
        splitted = splitee.value.split(splitter.value)
        splitted = List([String(x) for x in splitted])
        return RTResult().success(splitted)
    execute_split_str.arg_names = ['splitee', 'splitter']

    def execute_pop(self, exec_ctx):
        given_list = exec_ctx.symbol_table.get("list")
        index = exec_ctx.symbol_table.get("index")
        if not isinstance(given_list, List):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek list honi chahiye"
            ))
        if not isinstance(index, Number):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Dusra arument ek number hona chahiye"
            ))
        try:
            element = given_list.elements.pop(index.value)
        except:
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Index list mein nahi hai"
            ))
        return RTResult().success(String(element))
    execute_pop.arg_names = ['list', 'index']

    def execute_extend(self, exec_ctx):
        listA = exec_ctx.symbol_table.get("listA")
        listB = exec_ctx.symbol_table.get("listB")
        if not isinstance(listA, List):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek list honi chahiye"
            ))
        if not isinstance(listB, List):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Dusra arument ek list honi chahiye"
            ))
        listA.elements.extend(listB.elements)
        return RTResult().success(null)
    execute_extend.arg_names = ['listA', 'listB']

    def execute_len(self, exec_ctx):
        given = exec_ctx.symbol_table.get("list")
        if not isinstance(given, List):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek list honi chahiye"
            ))
        return RTResult().success(Number(len(given.elements)))
    execute_len.arg_names = ['list']

    def execute_run(self, exec_ctx):
        fn = exec_ctx.symbol_table.get("fn")
        if not isinstance(fn, String):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek string honi chahiye"
            ))
        fn = fn.value
        try:
            with open(fn, 'r') as f:
                script = f.read()
        except:
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "File nahi mili"
            ))
        _, error = run(fn, script)
        if error != None:
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                f"script '{fn}' sahi se nahi chali kyu ke,\n"+
                error.as_string()
            ))
        return RTResult().success(null)
    execute_run.arg_names = ['fn']

    def execute_exec(self, exec_ctx):
        given = exec_ctx.symbol_table.get("string")
        if not isinstance(given, String):
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                "Pehla arument ek string honi chahiye"
            ))
        _, error = run('string', given.value)
        if error != None:
            return RTResult().failure(RTError(
                self.pos_start, self.pos_end,
                exec_ctx,
                f"string sahi se nahi chali kyu ke,\n"+
                error.as_string()
            ))
        return RTResult().success(null)
    execute_exec.arg_names = ['string']

BuiltInFunction.print = BuiltInFunction("print")
BuiltInFunction.println = BuiltInFunction("println")
BuiltInFunction.print_ret = BuiltInFunction("print_ret")
BuiltInFunction.input = BuiltInFunction("input")
BuiltInFunction.input_int = BuiltInFunction("input_int")
BuiltInFunction.clear = BuiltInFunction("clear")
BuiltInFunction.is_number = BuiltInFunction("is_number")
BuiltInFunction.is_string = BuiltInFunction("is_string")
BuiltInFunction.is_list = BuiltInFunction("is_list")
BuiltInFunction.is_function = BuiltInFunction("is_function")
BuiltInFunction.append = BuiltInFunction("append")
BuiltInFunction.pop = BuiltInFunction("pop")
BuiltInFunction.extend = BuiltInFunction("extend")
BuiltInFunction.list = BuiltInFunction("list_conv")
BuiltInFunction.str = BuiltInFunction("str_conv")
BuiltInFunction.num = BuiltInFunction("number_conv")
BuiltInFunction.join = BuiltInFunction("join_list")
BuiltInFunction.split = BuiltInFunction("split_str")
BuiltInFunction.len = BuiltInFunction("len")
BuiltInFunction.run = BuiltInFunction("run")
BuiltInFunction.exec = BuiltInFunction("exec")

#Context
class Context(object):
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None

#Symbol Table
class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.consts = []
        self.parent = parent

    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value, index=None):
        if name in self.consts:
            return None, RTError
        if index == None:
            self.symbols[name] = value
        else:
            key = self.get(name)
            if isinstance(key, List):
                if index.value < len(key.elements):
                    if isinstance(value, List):
                        key.elements[int(index.value)] = List(value.elements[:])
                    else:
                        key.elements[int(index.value)] = value
                    return key, None
                else:
                    return None, None
        return self.symbols.get(name, None), None

    def const_set(self, name, value, index=None):
        got = self.get(name)
        if name in self.consts:
            return None, RTError
        self.symbols[name] = value
        self.consts.append(name)
        return value, None

    def remove(self, name):
        del self.symbols[name]

#Interpreter
class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    def no_visit_method(self, node, context):
        raise Exception (f"visit_{type(node).__name__} method is undefined")

    def visit_NumberNode(self, node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_StringNode(self, node, context):
        return RTResult().success(
            String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_ListNode(self, node, context):
        res = RTResult()
        elements = []
        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node, context)))
            if res.should_return(): return res
        return res.success(
                List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
            )

    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)
        if not value:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"'{var_name}' defined nahi hai",
            ))
        value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(value)

    def visit_VarAssignNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        var_type = node.var_type.value
        index = node.index_node
        if index:
            index = res.register(self.visit(index, context))
            if res.should_return(): return res
        value = res.register(self.visit(node.value_node, context))
        if res.should_return(): return res
        if var_type == "ABSE":
            value, error = context.symbol_table.const_set(var_name, value)
        else:
            value, error = context.symbol_table.set(var_name, value, index)
        if error == RTError:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"Mustakil value change nahi kar sakte"
            ))

        if (value == None) or (error):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"'{var_name}' list form mein defined nahi hai"
            ))
        return res.success(value)

    def visit_BinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.left_node, context))
        if res.should_return(): return res
        right = res.register(self.visit(node.right_node, context))
        if res.should_return(): return res
        if node.op_tok.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result, error = left.dived_by(right)
        elif node.op_tok.type == TT_POW:
            result, error = left.powed_by(right)
        elif node.op_tok.type == TT_MOD:
            result, error = left.moded_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD, 'OR'):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TT_KEYWORD, 'YA'):
            result, error = left.ored_by(right)
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_IfNode(self, node, context):
        res = RTResult()
        for condition, expr, should_return_null in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.should_return(): return res
            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.should_return(): return res
                return res.success(null if should_return_null else expr_value)
        if node.else_case:
            expr, should_return_null = node.else_case
            else_value = res.register(self.visit(expr, context))
            if res.should_return(): return res
            return res.success(null if should_return_null else else_value)
        return res.success(null)

    def visit_ForNode(self, node, context):
        res = RTResult()
        elements = []
        start_value = res.register(self.visit(node.start_value_node, context))
        if res.should_return(): return res
        end_value = res.register(self.visit(node.end_value_node, context))
        if res.should_return(): return res
        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node, context))
            if res.should_return(): return res
        else:
            step_value = Number(1)
        i = start_value.value
        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value
        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value
            value = res.register(self.visit(node.body_node, context))
            if res.should_return() and res.loop_should_continue == False and res.loop_should_break == False: return res
            if res.loop_should_continue:
                continue
            if res.loop_should_break:
                break
            elements.append(value)
        return res.success(null if node.should_return_null else List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_WhileNode(self, node, context):
        res = RTResult()
        elements = []
        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.should_return(): return res
            if not condition.is_true(): break
            value = res.register(self.visit(node.body_node, context))
            if res.should_return() and res.loop_should_continue == False and res.loop_should_break == False: return res
            if res.loop_should_continue:
                continue
            if res.loop_should_break:
                break
            elements.append(value)
        return res.success(null if node.should_return_null else List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.should_return(): return res
        error = None
        if node.op_tok.type == TT_MINUS:
            number, error = number.multed_by(Number(-1))
        elif node.op_tok.matches(TT_KEYWORD, 'NAHI'):
            number, error = number.notted()
        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))

    def visit_FuncDefNode(self, node, context):
        res = RTResult()
        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names, node.should_auto_return).set_context(context).set_pos(node.pos_start, node.pos_end)
        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)
        return res.success(func_value)

    def visit_CallNode(self, node, context):
        res = RTResult()
        args = []
        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.should_return(): return res
        if isinstance(value_to_call, VarAccessNode):
            value_to_call = res.register(self.visit(node.node_to_call, context))
        value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)
        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.should_return(): return res
        return_value = res.register(value_to_call.execute(args))
        if res.should_return(): return res
        return_value = return_value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(return_value)

    def visit_ReturnNode(self, node, context):
        res = RTResult()
        if node.node_to_return:
            value = res.register(self.visit(node.node_to_return, context))
            if res.should_return(): return res
        else:
            value = null

        return res.success_return(value)

    def visit_ContinueNode(self, node, context):
        return RTResult().success_continue()

    def visit_BreakNode(self, node, context):
        return RTResult().success_break()

    def visit_AssertNode(self, node, context):
        res = RTResult()
        assertion = res.register(self.visit(node.node_to_assert, context))
        if res.should_return(): return res
        if not assertion.is_true():
            return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    "Jo ap ne kaha wo sahi nahi"
                ))
        return res.success(null)

#run
global_symbol_table = SymbolTable()
null = nullObject()
galat = Boolean(0)
sahi = Boolean(1)
global_symbol_table.set("khali", null)
global_symbol_table.set("galat", galat)
global_symbol_table.set("sahi", sahi)
global_symbol_table.set("LIKHO", BuiltInFunction.print)
global_symbol_table.set("LINE_LIKHO", BuiltInFunction.println)
global_symbol_table.set("LIKHO_WAPIS", BuiltInFunction.print_ret)
global_symbol_table.set("PUCHO", BuiltInFunction.input)
global_symbol_table.set("MANGO_INT", BuiltInFunction.input_int)
global_symbol_table.set("SAAF", BuiltInFunction.clear)
global_symbol_table.set("KYA_NUM", BuiltInFunction.is_number)
global_symbol_table.set("KYA_STR", BuiltInFunction.is_string)
global_symbol_table.set("KYA_LIST", BuiltInFunction.is_list)
global_symbol_table.set("KYA_KAM", BuiltInFunction.is_function)
global_symbol_table.set("DALO", BuiltInFunction.append)
global_symbol_table.set("NIKAL", BuiltInFunction.pop)
global_symbol_table.set("MILAO", BuiltInFunction.extend)
global_symbol_table.set("LIST", BuiltInFunction.list)
global_symbol_table.set("STR", BuiltInFunction.str)
global_symbol_table.set("NUM", BuiltInFunction.num)
global_symbol_table.set("ALAG", BuiltInFunction.split)
global_symbol_table.set("JODH", BuiltInFunction.join)
global_symbol_table.set("LAMBAI", BuiltInFunction.len)
global_symbol_table.set("CHALAO", BuiltInFunction.run)
global_symbol_table.set("STR_CHALAO", BuiltInFunction.exec)


def run(fn, text):
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error: return None, error
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error
    interpreter = Interpreter()
    context = Context('<main>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)

    return result.value, result.error

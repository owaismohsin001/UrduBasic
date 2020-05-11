from string import ascii_letters
from strings_with_arrows import *
from copy import deepcopy

LETTERS = ascii_letters
DIGITS = "0123456789"

#Tokems
TT_INT = "INT"
TT_FLOAT = "FLOAT"
TT_IDENTIFIER = "IDENTIFIER"
TT_KEYWORD = "KEYWORD"
TT_PLUS = "PLUS"
TT_MINUS = "MINUS"
TT_MUL = "MUL"
TT_DIV = "DIV"
TT_RPAREN = "RPAREN"
TT_LPAREN = "LPAREN"
TT_EE = "EE"
TT_NE = "NE"
TT_GT = "GT"
TT_LT = "LT"
TT_GTE = "GTE"
TT_LTE = "LTE"
TT_EOF = "EOF"
TT_NOT = "NOT"
TT_AND = "AND"
TT_OR = "OR"
TT_RSQUARE = "RSQUARE"
TT_LSQUARE = "LSQUARE"
TT_RCURLY = "RCURLY"
TT_LCURLY = "LCURLY"
TT_HASH = "HASH"
TT_STRING = "STRING"
TT_EQUALS = "EQUALS"
TT_SARROW = "SARROW"
TT_DARROW = "DARROW"
TT_COMMA = "COMMA"
TT_COLON = "COLON"
TT_EXCL = "EXCL"
TT_LAMBDA = "TT_LAMBDA"
TT_INFIX = "INFIX"
TT_PIPELINE = "PIPELINE"
TT_PATTERN = "PATTERN"
TT_DCOLON = "DCOLON"
TT_ARGPOW = "ARGPOW"
TT_DATAOR = "DATAOR"
TT_NEWLINE = "NEWLINE"

KEYWORDS = [
"if",
"then",
"elif",
"else",
"and",
"or",
"not",
"while",
"where",
"end",
"run",
"let",
"for",
"in",
"case",
"val",
"restart",
"when",
"inherits",
"class",
"default",
"stop",
"data"
]

class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value
        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end

    def matches(self, name, value):
        return ((self.type == name) and (self.value == value))

    def __repr__(self):
        if self.value: return f'[{self.type}:{self.value}]'
        return self.type


class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}, File {self.pos_start.fn} in line number {self.pos_start.ln + 1}'
        result += '\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result

class IllegalCharacterError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Illegal Character:', details)

class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Syntax Error:', details)

class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Expected Character:', details)

class RTError(Error):
    def __init__(self, pos_start, pos_end, context, details=''):
        super().__init__(pos_start, pos_end, 'Runtime Error', details)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.error_name}: {self.details}'
        result += '\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context
        while ctx:
            result = f'File: {pos.fn}, line: {str(pos.ln+1)} {ctx.display_name} main\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent
        return "Error Traceback, (Most recent call last):\n" + result

#Position
class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1
        if current_char == '\n':
            self.ln += 1
            self.col = 0
        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

#Lexer
class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, self.fn, self.text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    def make_number(self):
        number_str = ""
        pos_start = self.pos.copy()
        e_count = 0
        dot_count = 0
        while (self.current_char != None) and (self.current_char in DIGITS+"."+"e"):
            if self.current_char == ".": dot_count+=1
            if self.current_char == "e": e_count+=1
            if dot_count == 2: break
            if e_count == 2: break
            if (dot_count == 1) and (e_count == 1): break
            number_str += self.current_char
            self.advance()
        if (dot_count != 0) or (e_count != 0):
            return Token(TT_FLOAT, float(number_str), pos_start, self.pos)
        else:
            return Token(TT_INT, int(number_str), pos_start, self.pos)

    def make_identifier(self):
        id_str = ""
        pos_start = self.pos.copy()
        while (self.current_char != None) and (self.current_char in LETTERS+"_"+DIGITS):
            id_str += self.current_char
            self.advance()
        return Token(TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER, id_str, pos_start, self.pos)

    def make_gt_or_gte(self):
        pos_start = self.pos.copy()
        tok_type = TT_GT
        self.advance()
        if self.current_char == "=":
            tok_type = TT_GTE
            self.advance()
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos.copy())

    def make_colon_or_docoln(self):
        pos_start = self.pos.copy()
        tok_type = TT_COLON
        self.advance()
        if self.current_char == ":":
            tok_type = TT_DCOLON
            self.advance()
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos.copy())

    def make_lt_or_lte(self):
        pos_start = self.pos.copy()
        tok_type = TT_LT
        self.advance()
        if self.current_char == "=":
            tok_type = TT_LTE
            self.advance()
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos.copy())

    def make_equals_or_ee_or_darrow(self):
        pos_start = self.pos.copy()
        tok_type = TT_EQUALS
        self.advance()
        if self.current_char == "=":
            tok_type = TT_EE
            self.advance()
        elif self.current_char == ">":
            tok_type = TT_DARROW
            self.advance()
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos.copy())

    def make_ne_or_excl(self):
        tok_type = TT_EXCL
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == "=":
            tok_type = TT_NE
            self.advance()
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos.copy()), None

    def make_infix(self):
        id_str = ''
        pos_start = self.pos.copy()
        self.advance()
        while (self.current_char != None) and (self.current_char in LETTERS+"_"+DIGITS) and (self.current_char != '`'):
            id_str += self.current_char
            self.advance()
        if self.current_char != '`':
            return None, ExpectedCharError(pos_start, self.pos, "Unexpected Character in infix identifier")
        self.advance()
        return Token(TT_INFIX, id_str, pos_start, self.pos.copy()), None

    def make_string(self):
        string = ""
        escape_character = False
        escape_characters = {
            't': '\t',
            'n': '\n'
        }
        pos_start = self.pos.copy()
        self.advance()
        while self.current_char != '"' or escape_character:
            if escape_character:
                string += escape_characters.get(self.current_char, self.current_char)
                escape_character = False
            else:
                if self.current_char == '\\':
                    escape_character = True
                else:
                    string += self.current_char
            self.advance()
        self.advance()
        return Token(TT_STRING, string, pos_start=pos_start, pos_end=self.pos.copy())

    def make_pipeline_or_pattern_or_dataor(self):
        tok_type = TT_PATTERN
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == ">":
            tok_type = TT_PIPELINE
            self.advance()
        if self.current_char == "|":
            tok_type = TT_DATAOR
            self.advance()
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos.copy())

    def make_sarrow_or_minus(self):
        pos_start = self.pos.copy()
        tok_type = TT_MINUS
        self.advance()
        if self.current_char == ">":
            tok_type = TT_SARROW
            self.advance()
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos.copy())

    def skip_comment(self):
        self.advance()
        while not self.current_char == '\n':
            self.advance()
        self.advance()
        return

    def generate_tokens(self):
        tokens = []
        while self.current_char != None:
            if self.current_char in '\t ':
                self.advance()
            elif self.current_char in '$':
                self.skip_comment()
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(self.make_sarrow_or_minus())
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '|':
                tokens.append(self.make_pipeline_or_pattern_or_dataor())
            elif self.current_char == '(':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '[':
                tokens.append(Token(TT_RSQUARE, pos_start=self.pos))
                self.advance()
            elif self.current_char == ']':
                tokens.append(Token(TT_LSQUARE, pos_start=self.pos))
                self.advance()
            elif self.current_char == '{':
                tokens.append(Token(TT_RCURLY, pos_start=self.pos))
                self.advance()
            elif self.current_char == '}':
                tokens.append(Token(TT_LCURLY, pos_start=self.pos))
                self.advance()
            elif self.current_char == '#':
                tokens.append(Token(TT_HASH, pos_start=self.pos))
                self.advance()
            elif self.current_char == ',':
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()
            elif self.current_char == ':':
                tokens.append(self.make_colon_or_docoln())
            elif self.current_char == '^':
                tokens.append(Token(TT_ARGPOW, pos_start=self.pos))
                self.advance()
            elif self.current_char == '=':
                tokens.append(self.make_equals_or_ee_or_darrow())
            elif self.current_char == '!':
                token, error = self.make_ne_or_excl()
                if error: return [], error
                tokens.append(token)
            elif self.current_char == '"':
                tokens.append(self.make_string())
            elif self.current_char == '>':
                tokens.append(self.make_gt_or_gte())
            elif self.current_char == '<':
                tokens.append(self.make_lt_or_lte())
            elif self.current_char == '.':
                tokens.append(Token(TT_NEWLINE, pos_start=self.pos))
                self.advance()
            elif self.current_char == '\\':
                tokens.append(Token(TT_LAMBDA, pos_start=self.pos))
                self.advance()
            elif self.current_char in '`':
                tok, error = self.make_infix()
                if error: return [], error
                tokens.append(tok)
            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharacterError(pos_start, self.pos, f"'{char}'")
        tokens.append(Token(TT_EOF, pos_start = self.pos))
        return tokens, None


class NumberNode:
    def __init__(self, number, pos_start, pos_end):
        self.number = number
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{self.number}"

class StringNode:
    def __init__(self, string, pos_start, pos_end):
        self.string = string
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{self.string}"

class ListNode:
    def __init__(self, elements, isArg, pos_start, pos_end):
        self.elements = elements
        self.isArg = isArg
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return str(self.elements)

class SetNode:
    def __init__(self, elements, pos_start, pos_end):
        self.elements = elements
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return "{"+f"{str(self.elements)}"+"}"

class MapNode:
    def __init__(self, elements, pos_start, pos_end):
        self.elements = elements
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return str(self.elements)

class RecordNode:
    def __init__(self, type_, elements, extension, pos_start, pos_end):
        self.type = type_
        self.extension = extension
        self.elements = elements
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{str(self.elements)} inherits {self.extension}"

class RecordInstanceNode:
    def __init__(self, type_, types, elements, pos_start, pos_end):
        self.type = type_
        self.types = types
        self.elements = elements
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{self.type}: {str(self.elements)}"

class TupleNode:
    def __init__(self, elements, pos_start, pos_end):
        self.elements = elements
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return str(self.elements)

class UnaryNode:
    def __init__(self, unary, number, pos_start, pos_end):
        self.unary = unary
        self.number = number
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{self.number}"

class BinOpNode:
    def __init__(self, left, op_tok, right, pos_start, pos_end):
        self.left = left
        self.op = op_tok
        self.right = right
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"({self.left}, {self.op}, {self.right})"

class FuncDefNode:
    def __init__(self, identifier, args, expr, pos_start, pos_end):
        self.identifier = identifier
        self.args = args
        self.expr = expr
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"let {self.identifier} {self.args} = {self.expr}"

class FunCallNode:
    def __init__(self, id, args, pos_start, pos_end):
        self.identifier = id
        self.args = args
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{self.identifier}" "{" + str(self.args) + "}"

class FuncShowNode:
    def __init__(self, id, pos_start, pos_end):
        self.identifier = id
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{self.identifier}!"

class ValNode:
    def __init__(self, identifiers, exprs, pos_start, pos_end):
        self.identifiers = identifiers
        self.exprs = exprs
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"{str(self.identifiers)} = {str(self.exprs)}"


class IfNode:
    def __init__(self, cases, else_case, pos_start, pos_end):
        self.cases = cases
        self.else_case = else_case
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"if stmnt {self.cases} else {self.else_case}"

class PatternNode:
    def __init__(self, node, vars, map_context, pos_start, pos_end):
        self.node = node
        self.vars = vars
        self.map_context = map_context
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"Pattern Match: {repr(self.node)} where {self.vars}"

class ForNode:
    def __init__(self, iterator, identifier_expr, expr, filter, pos_start, pos_end):
        self.iterator = iterator
        self.identifier_expr = identifier_expr
        self.expr = expr
        self.filter = filter
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"for {self.iterator} in {self.identifier_expr} | {self.expr}"

class WhenNode:
    def __init__(self, statement, expr, pos_start, pos_end):
        self.statement = statement
        self.expr = expr
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"when {self.statement} then {self.expr}"

class AlgebraDefNode:
    def __init__(self, identifier, cases, default, pos_start, pos_end):
        self.identifier = identifier
        self.cases = cases
        self.default = default
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"data {self.identifier} = {self.cases} else {self.default}"

class AddCaseNode:
    def __init__(self, identifier, condition, expr, pos_start, pos_end):
        self.identifier = identifier
        self.condition = condition
        self.expr = expr
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"case {self.identifier} {self.condition} -> {self.expr}"

class AddAlgebraCaseNode:
    def __init__(self, identifier, condition, expr, pos_start, pos_end):
        self.identifier = identifier
        self.condition = condition
        self.expr = expr
        self.pos_start = pos_start
        self.pos_end = pos_end

    def __repr__(self):
        return f"case {self.identifier} || {self.expr} -> {self.condition}"

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advancement(self):
        self.advance_count += 1

    def register(self, res):
        self.last_registered_advance_count = res.advance_count
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node

    def try_register(self, res):
        if res.error:
            self.to_reverse_count = res.advance_count
            return None
        return self.register(res)

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.next_tok_idx = 0
        self.advance()

    def update_current_tok(self):
        if self.tok_idx >= 0 and self.tok_idx<len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        if self.next_tok_idx >= 0 and self.next_tok_idx<len(self.tokens):
            self.next_tok = self.tokens[self.next_tok_idx]

    def advance(self):
        self.tok_idx += 1
        self.next_tok_idx += 1
        self.update_current_tok()
        return self.current_tok

    def reverse(self, amount=1):
        self.tok_idx -= amount
        self.update_current_tok()
        return self.current_tok

    def parse(self):
        res = self.statements()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expecte '+', '-', '*' or, '/'"
			))
        return res

    def statements(self):
        res = ParseResult()
        statements = []
        pos_start = self.current_tok.pos_start.copy()
        while self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()
        statement = res.register(self.statement())
        if res.error: return res
        statements.append(statement)
        more_statements = True
        while True:
            newline_count = 0
            while self.current_tok.type == TT_NEWLINE:
                res.register_advancement()
                self.advance()
                newline_count += 1
            if newline_count == 0:
                more_statements = False

            if not more_statements: break
            statement = res.try_register(self.statement())
            if not statement:
                self.reverse(res.to_reverse_count)
                more_statements = False
                continue
            statements.append(statement)
        return res.success(ListNode(
            statements,
            False,
            pos_start,
            self.current_tok.pos_end.copy()
        ))

    def statement(self):
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.matches(TT_KEYWORD, "let"):
            self.advance()
            identifier = self.current_tok
            self.advance()
            args = []
            while self.current_tok.type == TT_IDENTIFIER:
                args.append(self.current_tok)
                self.advance()
            if self.current_tok.type != TT_EQUALS:
                return res.failure(InvalidSyntaxError(
            	   pos_start, self.current_tok.pos_end.copy(),
        		   "Expected '='"
                ))
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(FuncDefNode(identifier, args, expr, pos_start, self.current_tok.pos_end.copy()))
        elif self.current_tok.matches(TT_KEYWORD, "val"):
            identifiers = []
            exprs = []
            vars = []
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
            	   pos_start, self.current_tok.pos_end.copy(),
        		   "Expected IDENTIFIER"
                ))
            identifiers.append(self.current_tok)
            res.register_advancement()
            self.advance()
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                if self.current_tok.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(
                	   pos_start, self.current_tok.pos_end.copy(),
            		   "Expected IDENTIFIER"
                    ))
                identifiers.append(self.current_tok)
                res.register_advancement()
                self.advance()
            if self.current_tok.type != TT_EQUALS:
                return res.failure(InvalidSyntaxError(
            	   pos_start, self.current_tok.pos_end.copy(),
        		   "Expected '='"
                ))
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            exprs.append(expr)
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                expr = res.register(self.expr())
                if res.error: return res
                exprs.append(expr)
            return res.success(ValNode(identifiers, exprs, pos_start, self.current_tok.pos_end.copy()))
        elif self.current_tok.matches(TT_KEYWORD, "class"):
            expr = res.register(self.case_class())
            if res.error: return res
            return res.success(expr)
        elif self.current_tok.matches(TT_KEYWORD, "case"):
            expr = res.register(self.add_case())
            if res.error: return res
            return res.success(expr)
        elif self.current_tok.matches(TT_KEYWORD, "data"):
            expr = res.register(self.algebraic_data())
            if res.error: return res
            return res.success(expr)
        expr = res.register(self.expr())
        if res.error: return res
        return res.success(expr)

    def case_class(self):
        res = ParseResult()
        identifier = None
        body = None
        vars = []
        args = []
        map_pos = None
        map_context = None
        pos_start = self.current_tok.pos_start
        if not self.current_tok.matches(TT_KEYWORD, "class"):
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected 'class'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected IDENTIFIER"
            ))
        identifier = self.current_tok
        res.register_advancement()
        self.advance()
        while self.current_tok.type == TT_IDENTIFIER:
            args.append(self.current_tok)
            res.register_advancement()
            self.advance()
        body = IfNode([], FunCallNode(Token(TT_IDENTIFIER, "null"), [], pos_start, self.current_tok.pos_end), pos_start, self.current_tok.pos_end.copy())
        if self.current_tok.matches(TT_KEYWORD, "where"):
            identifiers = []
            exprs = []
            res.register_advancement()
            self.advance()
            i = 0
            if self.current_tok.type == TT_RCURLY:
                res.register_advancement()
                self.advance()
                map_pos = i
                if self.current_tok.type != TT_LCURLY:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected '}'"
                    ))
                res.register_advancement()
                self.advance()
            elif self.current_tok.type == TT_IDENTIFIER:
                identifiers.append(self.current_tok)
                res.register_advancement()
                self.advance()
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected IDENTIFIER or '{'"
                ))
            i += 1
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                if self.current_tok.type == TT_RCURLY:
                    res.register_advancement()
                    self.advance()
                    map_pos = i
                    res.register_advancement()
                    self.advance()
                else:
                    identifiers.append(self.current_tok)
                    res.register_advancement()
                    self.advance()
                i+=1
            if self.current_tok.type != TT_EQUALS:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '='"
                ))
            i = 0
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            if i == map_pos:
                map_context = expr
            else:
                expr = exprs.append(expr)
            i += 1
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                expr = res.register(self.expr())
                if res.error: return res
                if i == map_pos:
                    map_context = expr
                else:
                    expr = exprs.append(expr)
                i+=1
            if len(identifiers) != len(exprs):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "IDENTIFIERS and expression are unequal"
                ))
            vars = list(zip(identifiers, exprs))
        body = PatternNode(body, vars, map_context, pos_start, self.current_tok.pos_end.copy())
        return res.success(FuncDefNode(identifier, args, body, pos_start, self.current_tok.pos_end.copy()))

    def add_case(self):
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy()
        identifier = None
        condition = None
        expr = None
        if not self.current_tok.matches(TT_KEYWORD, "case"):
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected 'case'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected IDENTIFIER"
            ))
        identifier = FuncShowNode(self.current_tok, self.current_tok.pos_start, self.current_tok.pos_end)
        res.register_advancement()
        self.advance()
        if self.current_tok.type == TT_DATAOR:
            res.register_advancement()
            self.advance()
            record = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_SARROW:
                res.register_advancement()
                self.advance()
                condition = res.register(self.expr())
                if res.error: return res
            else:
                condition = FunCallNode(Token(TT_IDENTIFIER, "true"), [], self.current_tok.pos_start.copy(), self.current_tok.pos_end.copy())
            return res.success(AddAlgebraCaseNode(identifier, condition if condition else None, record, pos_start, self.current_tok.pos_end.copy()))
        if self.current_tok.matches(TT_KEYWORD, "default"):
            res.register_advancement()
            self.advance()
        else:
            condition = res.register(self.expr())
        if res.error: return res
        if self.current_tok.type != TT_SARROW:
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected '->'"
            ))
        res.register_advancement()
        self.advance()
        expr = res.register(self.expr())
        if res.error: return res
        return res.success(AddCaseNode(identifier, condition if condition else None, expr, pos_start, self.current_tok.pos_end.copy()))

    def algebraic_data(self):
        res = ParseResult()
        parent = None
        functions = []
        default = None
        pos_start = self.current_tok.pos_start.copy()
        def parse_true(pos_start, pos_end):
            return FunCallNode(Token(TT_IDENTIFIER, "true"), [], pos_start, pos_end)
        if not self.current_tok.matches(TT_KEYWORD, "data"):
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected 'case'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected IDENTIFIER"
            ))
        parent = RecordNode(self.current_tok, [], None, pos_start, self.current_tok.pos_end.copy())
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_EQUALS:
            return res.failure(InvalidSyntaxError(
        	   pos_start, self.current_tok.pos_end.copy(),
               "Expected '='"
            ))
        res.register_advancement()
        self.advance()
        record = res.register(self.expr())
        record.extension = FuncShowNode(parent.type, pos_start, self.current_tok.pos_end.copy())
        if self.current_tok.type == TT_SARROW:
            res.register_advancement()
            self.advance()
            condition = res.register(self.expr())
            functions.append((condition, record))
        else:
            functions.append((parse_true(self.current_tok.pos_start, self.current_tok.pos_end), record))
        while self.current_tok.type == TT_DATAOR:
            res.register_advancement()
            self.advance()
            if self.current_tok.matches(TT_KEYWORD, "default"):
                res.register_advancement()
                self.advance()
                record = res.register(self.expr())
                record.extension = FuncShowNode(parent.type, pos_start, self.current_tok.pos_end.copy())
                default = record
            else:
                record = res.register(self.expr())
                record.extension = FuncShowNode(parent.type, pos_start, self.current_tok.pos_end.copy())
                if self.current_tok.type == TT_SARROW:
                    res.register_advancement()
                    self.advance()
                    condition = res.register(self.expr())
                    functions.append((condition, record))
                else:
                    functions.append((parse_true(self.current_tok.pos_start, self.current_tok.pos_end), record))
        return res.success(AlgebraDefNode(parent, functions, default, pos_start, self.current_tok.pos_end.copy()))

    def bin_op(self, left_fun, toks, right_fun):
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy()
        left = res.register(left_fun())
        if res.error: return res
        while (self.current_tok.type in toks) or ((self.current_tok.type, self.current_tok.value) in toks):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(right_fun())
            if res.error: return res
            left = BinOpNode(left, op_tok, right, pos_start, self.current_tok.pos_end)
        return res.success(left)

    def expr(self):
        res = ParseResult()
        binary_operation = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, "and"), (TT_KEYWORD, "or")), self.comp_expr))
        if res.error: return res
        return res.success(binary_operation)

    def comp_expr(self):
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.matches(TT_KEYWORD, "not"):
            unary_tok = self.current_tok
            res.register_advancement()
            self.advance()
            comp_expr = res.register(self.comp_expr())
            if res.error: return res
            return res.success(UnaryNode(unary_tok, comp_expr, pos_start, self.current_tok.pos_end.copy()))
        binary_operation = res.register(self.bin_op(self.pipeline_expr, (TT_EE, TT_NE, TT_GT, TT_GTE, TT_LT, TT_LTE, TT_DCOLON), self.pipeline_expr))
        if res.error: return res
        return res.success(binary_operation)

    def pipeline_expr(self):
        res = ParseResult()
        binary_operation = res.register(self.bin_op(self.arith_expr, (TT_PIPELINE, ), self.arith_expr))
        if res.error: return res
        return res.success(binary_operation)

    def arith_expr(self):
        res = ParseResult()
        binary_operation = res.register(self.bin_op(self.term, (TT_PLUS, TT_MINUS), self.term))
        if res.error: return res
        return res.success(binary_operation)

    def term(self):
        res = ParseResult()
        binary_operation = res.register(self.bin_op(self.factor, (TT_DIV, TT_MUL, TT_INFIX), self.factor))
        if res.error: return res
        return res.success(binary_operation)

    def factor(self):
        res = ParseResult()
        binary_operation = res.register(self.bin_op(self.atom, (TT_DARROW, ), self.atom))
        if res.error: return res
        return res.success(binary_operation)

    def atom(self):
        atoms = [TT_FLOAT, TT_INT, TT_STRING, TT_IDENTIFIER, TT_RPAREN, TT_RCURLY, TT_RSQUARE, TT_ARGPOW, (TT_KEYWORD, "if")]
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.type in (TT_PLUS, TT_MINUS, TT_ARGPOW, TT_DCOLON):
            unary_tok = self.current_tok
            res.register_advancement()
            self.advance()
            expr = res.register(self.atom())
            if res.error: return res
            return res.success(UnaryNode(unary_tok, expr, pos_start, self.current_tok.pos_end.copy()))
        elif (self.current_tok.type == TT_FLOAT) or (self.current_tok.type == TT_INT):
            pos_end = self.current_tok.pos_end.copy()
            value = self.current_tok
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(int(value.value) if value.type == TT_INT else float(value.value), pos_start, pos_end))
        elif self.current_tok.type == TT_STRING:
            pos_end = self.current_tok.pos_end.copy()
            value = self.current_tok.value
            res.register_advancement()
            self.advance()
            return res.success(StringNode(value, pos_start, pos_end))
        elif ((self.current_tok.type == TT_IDENTIFIER) and (self.next_tok.type == TT_RCURLY)) or ((self.current_tok.type == TT_LAMBDA) and (self.next_tok.type == TT_RCURLY)):
            expr = res.register(self.record_expr())
            if res.error: return res
            return res.success(expr)
        elif (self.current_tok.type == TT_IDENTIFIER) or (self.current_tok.type == TT_EXCL):
            if self.current_tok.type == TT_EXCL:
                self.advance()
                id = res.register(self.atom())
                if res.error: return res
            else:
                id = self.current_tok
                self.advance()
            if self.current_tok.type == TT_EXCL:
                self.advance()
                return res.success(FuncShowNode(id, pos_start, self.current_tok.pos_end))
            args = []
            while (self.current_tok.type in atoms) or ((self.current_tok.type, self.current_tok.value) in atoms):
                expr = res.register(self.expr())
                if res.error: return res
                args.append(expr)
            return res.success(FunCallNode(id, args, pos_start, self.current_tok.pos_end))
        elif self.current_tok.type == TT_RPAREN:
            exprs = []
            res.register_advancement()
            self.advance()
            if self.current_tok.type == TT_LPAREN:
                res.register_advancement()
                self.advance()
                return res.success(TupleNode(tuple(exprs), pos_start, self.current_tok.pos_end.copy()))
            expr = res.register(self.expr())
            if res.error: return res
            exprs.append(expr)
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                exprs.append(res.register(self.expr()))
                if res.error: return res
            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
    				pos_start, self.current_tok.pos_end.copy(),
    				"Expecte ')'"
    			))
            res.register_advancement()
            self.advance()
            return res.success(exprs[0] if len(exprs) == 1 else TupleNode(tuple(exprs), pos_start, self.current_tok.pos_end.copy()))
        elif self.current_tok.type == TT_LAMBDA:
            self.advance()
            args = []
            while self.current_tok.type == TT_IDENTIFIER:
                args.append(self.current_tok)
                self.advance()
            if self.current_tok.type != TT_SARROW:
                return res.failure(InvalidSyntaxError(
            	   pos_start, self.current_tok.pos_end.copy(),
        		   "Expected '\\'"
                ))
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(FuncDefNode(None, args, expr, pos_start, self.current_tok.pos_end.copy()))
        elif self.current_tok.matches(TT_KEYWORD, "if"):
            expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(expr)
        elif (self.current_tok.type == TT_RSQUARE) or (self.current_tok.type == TT_ARGPOW and self.next_tok.type == TT_RSQUARE):
            expr = res.register(self.list_expr())
            if res.error: return res
            return res.success(expr)
        elif self.current_tok.type == TT_RCURLY:
            expr = res.register(self.map_expr())
            if res.error: return res
            return res.success(expr)
        elif self.current_tok.type == TT_HASH:
            expr = res.register(self.set_expr())
            if res.error: return res
            return res.success(expr)
        elif self.current_tok.matches(TT_KEYWORD, "case"):
            expr = res.register(self.case_match())
            if res.error: return res
            return res.success(expr)
        elif self.current_tok.matches(TT_KEYWORD, "when"):
            expr = res.register(self.when_expr())
            if res.error: return res
            return res.success(expr)
        elif self.current_tok.type == TT_PATTERN:
            expr = res.register(self.pattern_match())
            if res.error: return res
            return res.success(expr)
        return res.failure(InvalidSyntaxError(
    	   pos_start, self.current_tok.pos_end.copy(),
		   "Expecte NUMBER, IDENTIFIER, if, or '('"
        ))

    def when_expr(self):
        res = ParseResult()
        statement = None
        expr = None
        pos_start = self.current_tok.pos_start.copy()
        if not self.current_tok.matches(TT_KEYWORD, 'when'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'when'"
            ))
        res.register_advancement()
        self.advance()
        statement = res.register(self.statement())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD, 'then'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'then'"
            ))
        res.register_advancement()
        self.advance()
        expr = res.register(self.expr())
        if res.error: return res
        return res.success(WhenNode(statement, expr, pos_start, self.current_tok.pos_end.copy()))

    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None
        pos_start = self.current_tok.pos_start.copy()
        if not self.current_tok.matches(TT_KEYWORD, 'if'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'if'"
            ))
        res.register_advancement()
        self.advance()
        condition = res.register(self.expr())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD, 'then'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'then'"
            ))
        res.register_advancement()
        self.advance()
        expr = res.register(self.expr())
        if res.error: return res
        cases.append((condition, expr))
        while self.current_tok.matches(TT_KEYWORD, 'elif'):
            res.register_advancement()
            self.advance()
            condition = res.register(self.expr())
            if res.error: return res
            if not self.current_tok.matches(TT_KEYWORD, 'then'):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected 'then'"
                ))
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            cases.append((condition, expr))
        if self.current_tok.matches(TT_KEYWORD, 'else'):
            res.register_advancement()
            self.advance()
            else_case = res.register(self.expr())
        return res.success(IfNode(cases, else_case, pos_start, self.current_tok.pos_end.copy()))

    def for_expr(self):
        res = ParseResult()
        filter = None
        iterator = None
        identifier_expr = None
        expr = None
        pos_start = self.current_tok.pos_start.copy()
        if not self.current_tok.matches(TT_KEYWORD, "for"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'for'"
            ))
        res.register_advancement()
        self.advance()
        iterator = self.current_tok
        res.register_advancement()
        self.advance()
        if not self.current_tok.matches(TT_KEYWORD, "in"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'in'"
            ))
        res.register_advancement()
        self.advance()
        identifier_expr = res.register(self.expr())
        if res.error: return res
        if self.current_tok.type != TT_PATTERN:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '|'"
            ))
        res.register_advancement()
        self.advance()
        expr = res.register(self.expr())
        if res.error: return res
        if self.current_tok.type == TT_SARROW:
            res.register_advancement()
            self.advance()
            if not self.current_tok.matches(TT_KEYWORD, "if"):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected 'if'"
                ))
            res.register_advancement()
            self.advance()
            filter = res.register(self.expr())
            if res.error: return res
        return res.success(ForNode(iterator, identifier_expr, expr, filter, pos_start, self.current_tok.pos_end.copy()))


    def map_expr(self):
        res = ParseResult()
        elements = []
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.type != TT_RCURLY:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '{'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type == TT_LCURLY:
            res.register_advancement()
            self.advance()
            return res.success(MapNode(elements, pos_start, self.current_tok.pos_end.copy()))
        if (self.current_tok.type != TT_STRING) and (self.current_tok.type != TT_INT):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected INTEGER or STRING"
            ))
        key = res.register(self.atom())
        if res.error: return res
        if self.current_tok.type != TT_COLON:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected ':'"
            ))
        res.register_advancement()
        self.advance()
        value = res.register(self.expr())
        if res.error: return res
        elements.append((key, value))
        while self.current_tok.type == TT_COMMA:
            res.register_advancement()
            self.advance()
            if (self.current_tok.type != TT_STRING) and (self.current_tok.type != TT_INT):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected INTEGER or STRING"
                ))
            key = res.register(self.atom())
            if res.error: return res
            if self.current_tok.type != TT_COLON:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ':'"
                ))
            res.register_advancement()
            self.advance()
            value = res.register(self.expr())
            if res.error: return res
            elements.append((key, value))
        if self.current_tok.type != TT_LCURLY:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '}'"
            ))
        res.register_advancement()
        self.advance()
        return res.success(MapNode(elements, pos_start, self.current_tok.pos_end.copy()))

    def record_expr(self):
        res = ParseResult()
        elements = []
        record_type = Token(TT_IDENTIFIER, "anonymous")
        extension = None
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.type == TT_IDENTIFIER:
            record_type = self.current_tok
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_RCURLY:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '{'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type == TT_LCURLY:
            res.register_advancement()
            self.advance()
        else:
            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected IDENTIFIER"
                ))
            identifier = self.current_tok
            elements.append(identifier)
            res.register_advancement()
            self.advance()
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                identifier = self.current_tok
                elements.append(identifier)
                res.register_advancement()
                self.advance()
            if self.current_tok.type != TT_LCURLY:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '}'"
                ))
            res.register_advancement()
            self.advance()
        if self.current_tok.matches(TT_KEYWORD, "inherits"):
            res.register_advancement()
            self.advance()
            extension = res.register(self.expr())
            if res.error: return res
        return res.success(RecordNode(record_type, elements, extension, pos_start, self.current_tok.pos_end.copy()))

    def set_expr(self):
        res = ParseResult()
        elements = []
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.type != TT_HASH:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '[' or '^'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_RCURLY:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '{'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_tok.type == TT_LCURLY:
            res.register_advancement()
            self.advance()
            return res.success(SetNode(elements, pos_start, self.current_tok.pos_end.copy()))
        expr = res.register(self.for_expr() if self.current_tok.matches(TT_KEYWORD, "for") else self.expr())
        if res.error: return res
        elements.append(expr)
        while self.current_tok.type == TT_COMMA:
            res.register_advancement()
            self.advance()
            expr = res.register(self.for_expr() if self.current_tok.matches(TT_KEYWORD, "for") else self.expr())
            if res.error: return res
            elements.append(expr)
        if self.current_tok.type != TT_LCURLY:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '}'"
            ))
        res.register_advancement()
        self.advance()
        return res.success(SetNode(elements, pos_start, self.current_tok.pos_end.copy()))

    def list_expr(self):
        res = ParseResult()
        elements = []
        isArg = self.current_tok.type == TT_ARGPOW
        pos_start = self.current_tok.pos_start.copy()
        if (self.current_tok.type != TT_RSQUARE) and (self.current_tok.type != TT_ARGPOW):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '[' or '^'"
            ))
        res.register_advancement()
        self.advance()
        if isArg:
            res.register_advancement()
            self.advance()
        if self.current_tok.type == TT_LSQUARE:
            res.register_advancement()
            self.advance()
            return res.success(ListNode(elements, isArg, pos_start, self.current_tok.pos_end.copy()))
        expr = res.register(self.for_expr() if self.current_tok.matches(TT_KEYWORD, "for") else self.expr())
        if res.error: return res
        elements.append(expr)
        while self.current_tok.type == TT_COMMA:
            res.register_advancement()
            self.advance()
            expr = res.register(self.for_expr() if self.current_tok.matches(TT_KEYWORD, "for") else self.expr())
            if res.error: return res
            elements.append(expr)
        if self.current_tok.type != TT_LSQUARE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected ']'"
            ))
        res.register_advancement()
        self.advance()
        return res.success(ListNode(elements, isArg, pos_start, self.current_tok.pos_end.copy()))

    def pattern_match(self):
        res = ParseResult()
        cases = []
        else_case = None
        identifiers = []
        exprs = []
        vars = []
        map_context = None
        map_pos = None
        pos_start = self.current_tok.pos_start.copy()
        while self.current_tok.type == TT_PATTERN:
            res.register_advancement()
            self.advance()
            condition = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_SARROW:
                res.register_advancement()
                self.advance()
                expr = res.register(self.expr())
                if res.error: return res
                cases.append((condition, expr))
            else:
                else_case = condition
        ifstatement = IfNode(cases, else_case, pos_start, self.current_tok.pos_end.copy())
        if self.current_tok.matches(TT_KEYWORD, "where"):
            res.register_advancement()
            self.advance()
            i = 0
            if self.current_tok.type == TT_RCURLY:
                res.register_advancement()
                self.advance()
                map_pos = i
                if self.current_tok.type != TT_LCURLY:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected '}'"
                    ))
                res.register_advancement()
                self.advance()
            elif self.current_tok.type == TT_IDENTIFIER:
                identifiers.append(self.current_tok)
                res.register_advancement()
                self.advance()
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected IDENTIFIER or '{'"
                ))
            i += 1
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                if self.current_tok.type == TT_RCURLY:
                    res.register_advancement()
                    self.advance()
                    map_pos = i
                    res.register_advancement()
                    self.advance()
                else:
                    identifiers.append(self.current_tok)
                    res.register_advancement()
                    self.advance()
                i+=1
            if self.current_tok.type != TT_EQUALS:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '='"
                ))
            i = 0
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            if i == map_pos:
                map_context = expr
            else:
                expr = exprs.append(expr)
            i += 1
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                expr = res.register(self.expr())
                if res.error: return res
                if i == map_pos:
                    map_context = expr
                else:
                    expr = exprs.append(expr)
                i+=1
            if len(identifiers) != len(exprs):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "IDENTIFIERS and expression are unequal"
                ))
            vars = list(zip(identifiers, exprs))
        return res.success(PatternNode(ifstatement, vars, map_context, pos_start, self.current_tok.pos_end.copy()))

    def case_match(self):
        res = ParseResult()
        cases = []
        else_case = None
        identifiers = []
        exprs = []
        vars = []
        pos_start = self.current_tok.pos_start.copy()
        if not self.current_tok.matches(TT_KEYWORD, "case"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'case'"
            ))
        res.register_advancement()
        self.advance()
        first_value = res.register(self.expr())
        if res.error: return res
        if self.current_tok.type != TT_SARROW:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '->'"
            ))
        res.register_advancement()
        self.advance()
        while self.current_tok.type == TT_PATTERN:
            res.register_advancement()
            self.advance()
            condition = res.register(self.expr())
            if res.error: return res
            condition = BinOpNode(first_value, Token(TT_EE), condition, first_value.pos_start, condition.pos_end)
            if self.current_tok.type == TT_SARROW:
                res.register_advancement()
                self.advance()
                expr = res.register(self.expr())
                if res.error: return res
                cases.append((condition, expr))
            else:
                else_case = condition.right
        ifstatement = IfNode(cases, else_case, pos_start, self.current_tok.pos_end.copy())
        if self.current_tok.matches(TT_KEYWORD, "where"):
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected IDENTIFIER"
                ))
            identifiers.append(self.current_tok)
            res.register_advancement()
            self.advance()
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                identifiers.append(self.current_tok)
                res.register_advancement()
                self.advance()
            if self.current_tok.type != TT_EQUALS:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '='"
                ))
            res.register_advancement()
            self.advance()
            expr = exprs.append(res.register(self.expr()))
            if res.error: return res
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                expr = exprs.append(res.register(self.expr()))
                if res.error: return res
            if len(identifiers) != len(exprs):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "IDENTIFIERS and expression are unequal"
                ))
            vars = list(zip(identifiers, exprs))
        return res.success(PatternNode(ifstatement, vars, pos_start, self.current_tok.pos_end.copy()))

class Value:
    def __init__(self):
        self.value = None
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
        return None, self.IllegalOperationError(other)

    def subbed_by(self, other):
        return None, self.IllegalOperationError(other)

    def multed_by(self, other):
        return None, self.IllegalOperationError(other)

    def dived_by(self, other):
        return None, self.IllegalOperationError(other)

    def lt(self, other):
        return None, self.IllegalOperationError(other)

    def lte(self, other):
        return None, self.IllegalOperationError(other)

    def gt(self, other):
        return None, self.IllegalOperationError(other)

    def gte(self, other):
        return None, self.IllegalOperationError(other)

    def ee(self, other):
        return None, self.IllegalOperationError(other)

    def ne(self, other):
        return None, self.IllegalOperationError(other)

    def execute(self, args):
        return None, self.IllegalOperationError()

    def argpowed(self):
        return None, self.IllegalOperationError()

    def instance_return(self, other):
        predefined = self.predefined[other.value]
        if (isinstance(predefined, int) or isinstance(predefined, float)):
            return Number(predefined)
        elif isinstance(predefined, str):
            return String(predefined)
        else:
            return predefined.copy()

    def get_property(self, other):
        if isinstance(other, Token) and other.type == TT_IDENTIFIER:
            if other.value in self.predefined:
                return self.instance_return(other), None
            else:
                return null, None
        else:
            if other.value in self.predefined:
                return self.instance_return(other), None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    f"{type(self).__name__} has no Property named {other.value}"
                )

    def copy(self):
        raise Exception("No copy method defined")

    def IllegalOperationError(self, other=None):
        if not other: other = self
        return RTError(
            self.pos_start, self.pos_end,
            self.context,
            'Illegal Operation'
        )

class Number(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value
        self.predefined = {
            "type": "Number",
            "id": id(self),
            "value": self
        }

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    self.pos_start, other.pos_end,
                    self.context,
                    "Division by Zero"
                )
            return Number(self.value / other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def lt(self, other):
        if isinstance(other, Number):
            return Bool(self.value < other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def lte(self, other):
        if isinstance(other, Number):
            return Bool(self.value <= other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def gt(self, other):
        if isinstance(other, Number):
            return Bool(self.value > other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def gte(self, other):
        if isinstance(other, Number):
            return Bool(self.value >= other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def ee(self, other):
        if isinstance(other, Number):
            return Bool(self.value == other.value).set_context(self.context), None
        else:
            return Bool(0), None

    def ne(self, other):
        if isinstance(other, Number):
            return Bool(self.value != other.value).set_context(self.context), None
        else:
            return Bool(1), None

    def is_true(self):
        return self.value != 0

    def is_type(self, other):
        return Bool(int(other.value == "Number")), None

    def notted(self):
        return Bool(not self.is_true()).set_context(self.context), None

    def anded(self, other):
        return Bool((self.is_true()) and (other.is_true())).set_context(self.context), None

    def ored(self, other):
        return Bool((self.is_true()) or (other.is_true())).set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f"{self.value}"

class Bool(Number):
    def __init__(self, state):
        super().__init__(state)

    def is_type(self, other):
        return Bool(int(other.value == "Bool")), None

    def copy(self):
        copy = Bool(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return "true" if self.value else "false"

class NullType(Number):
    def __init__(self, state):
        super().__init__(0)

    def is_type(self, other):
        return Bool(int(other.value == "NullType")), None

    def copy(self):
        copy = NullType(0)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return "null"


class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = str(value)
        self.predefined = {
            "type": "String",
            "id": id(self),
            "value": self
        }

    def added_to(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return String(self.value[:int(other.value)]).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return String(self.value * other.value).set_context(self.context), None
        else:
            return None, self.IllegalOperationError(other)

    def dived_by(self, other):
        if isinstance(other, Number):
            if len(self.value) > other.value:
                return String(self.value[int(other.value)]), None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def ee(self, other):
        if isinstance(other, String):
            return Bool(self.value == other.value).set_context(self.context), None
        else:
            return Bool(0), None

    def ne(self, other):
        if isinstance(other, String):
            return Bool(self.value != other.value).set_context(self.context), None
        else:
            return Bool(1), None

    def is_true(self):
        return len(self.value) > 0

    def is_type(self, other):
        return Bool(int(other.value == "String")), None

    def notted(self):
        return Bool(not self.is_true()).set_context(self.context), None

    def anded(self, other):
        return Bool((self.is_true()) and (other.is_true())).set_context(self.context), None

    def ored(self, other):
        return Bool((self.is_true()) or (other.is_true())).set_context(self.context), None

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f"{self.value}"


class BaseFunction(Value):
    def __init__(self, name):
        super().__init__()
        self.name = name
        self.predefined = {
            "type": "Function",
            "id": id(self),
            "value": self
        }

    def is_type(self, other):
        return Bool(int(other.value == "Function")), None

    def generate_new_context(self):
        new_context = Context(self.name, self.scope, self.pos_start)
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
                f"{len(args) - len(arg_names)} too few args passed into '{self.name}'"
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
    def __init__(self, name, body, arg_names, scope):
        super().__init__(name)
        self.name = name or "anonymous"
        self.body = body
        self.arg_names = arg_names
        self.scope = scope

    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        exec_ctx = self.generate_new_context()
        res.register(self.check_and_populate_args(self.arg_names, args, exec_ctx))
        if res.should_return(): return res
        value = res.register(interpreter.visit(self.body, exec_ctx))
        if res.should_return(): return res
        return res.success(value)

    def copy(self):
        copy = Function(self.name, self.body, self.arg_names, self.scope)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f"{self.name}({self.arg_names})" + " " + "{" + f"{self.body}" + "}"

class FunContainer(BaseFunction):
    def __init__(self, name, functions, default):
        super().__init__(name)
        self.name = name
        self.functions = functions
        self.default = default

    def execute(self, args):
        res = RTResult()
        for function in self.functions:
            if len(function.arg_names) == len(args):
                to_return = res.register(function.execute(args))
                if res.should_return(): return res
                if isinstance(to_return, NullType):
                    continue
                return res.success(to_return)
        if self.default:
            if len(self.default.arg_names) == len(args):
                to_return = res.register(self.default.execute(args))
                if res.should_return(): return res
                return res.success(to_return)
            return res.failure(RTError(
                self.default.pos_start, self.default.pos_end,
                self.default.context,
                f"No Pattern including default pattern {self.default.name} matched"
            ))
        return res.success(null)

    def copy(self):
        new_container = FunContainer(self.name, [x.copy() for x in self.functions], self.default)
        return new_container

    def __repr__(self):
        return f"{self.name} " + "{" + f"{self.functions}" + "}" + f" else {self.default}"


class List(Value):
    def __init__(self, value, isarg=False):
        super().__init__()
        self.value = value
        self.isarg = isarg
        self.predefined = {
            "type": "List",
            "id": id(self),
            "value": self
        }

    def added_to(self, other):
        new_list = List(self.value[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        new_list.value.append(other)
        return new_list, None

    def subbed_by(self, other):
        if isinstance(other, Number):
            if len(self.value) > other.value:
                new_list = List(self.value[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
                new_list.value.pop(int(other.value))
                return new_list, None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return None, Value.IllegalOperationError(self, other)
        else:
            new_list = List(self.value[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
            new_list.value.extend(other.value)
            return new_list, None

    def dived_by(self, other):
        if isinstance(other, Number):
            if len(self.value) > other.value:
                return self.value[int(other.value)], None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def truth_of_list(self, other):
        truth = []
        if len(other.value) != len(self.value): return False
        if not isinstance(other, List): return False
        for index, element in enumerate(self.value):
            if (0 <= index) and (index < len(other.value)):
                call = self.value[index].ee(other.value[index])[0]
                if call:
                    if call.value == 1:
                        truth.append(True)
                    else:
                        break
                else:
                    break
        return (len(truth) == len(self.value)) and (len(truth) == len(other.value))

    def ee(self, other):
        if isinstance(other, List):
            return Bool(int(self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(0), None

    def ne(self, other):
        if isinstance(other, List):
            return Bool(int(not self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(1), None

    def is_true(self):
        return len(self.value)>0

    def is_type(self, other):
        return Bool(int(other.value == "List")), None

    def notted(self):
        return Bool(not self.is_true()), None

    def ored_by(self, other):
        if isinstance(other, List):
            result = self.is_true() or other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def argpowed(self):
        new_list = List(self.value[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        new_list.isarg = True
        return new_list, None

    def anded_by(self, other):
        if isinstance(other, List):
            result = self.is_true() and other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def copy(self):
        copy = List(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

class Tuple(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value
        self.predefined = {
            "type": "Tuple",
            "id": id(self),
            "value": self
        }

    def added_to(self, other):
        new_list = Tuple(self.value + (other, )).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        return new_list, None

    def subbed_by(self, other):
        if isinstance(other, Number):
            if len(self.value) > other.value:
                list_value = list(self.value)
                list_value.pop(int(other.value))
                new_list = Map(tuple(list_value)).set_pos(self.pos_start, self.pos_end).set_context(self.context)
                return new_list, None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def multed_by(self, other):
        if not isinstance(other, Tuple):
            return None, Value.IllegalOperationError(self, other)
        new_list = Tuple(self.value + other.value).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        return new_list, None

    def dived_by(self, other):
        if isinstance(other, Number):
            if len(self.value) > other.value:
                return self.value[int(other.value)], None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def truth_of_list(self, other):
        truth = []
        if len(other.value) != len(self.value): return False
        if not isinstance(other, List): return False
        for index, element in enumerate(self.value):
            if (0 <= index) and (index < len(other.value)):
                call = self.value[index].ee(other.value[index])[0]
                if call:
                    if call.value == 1:
                        truth.append(True)
                    else:
                        break
                else:
                    break
        return (len(truth) == len(self.value)) and (len(truth) == len(other.value))

    def ee(self, other):
        if isinstance(other, List):
            return Bool(int(self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(0), None

    def ne(self, other):
        if isinstance(other, List):
            return Bool(int(not self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(1), None

    def is_true(self):
        return len(self.value)>0

    def is_type(self, other):
        return Bool(int(other.value == "Tuple")), None

    def notted(self):
        return Bool(not self.is_true()), None

    def ored_by(self, other):
        if isinstance(other, List):
            result = self.is_true() or other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def argpowed(self):
        new_list = List(self.value[:]).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        new_list.isarg = True
        return new_list, None

    def anded_by(self, other):
        if isinstance(other, List):
            result = self.is_true() and other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def copy(self):
        copy = Tuple(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

class Set(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value
        self.representative = [str(value) for key, value in self.value.items()]
        self.predefined = {
            "type": "Set",
            "id": id(self),
            "value": self
        }

    def added_to(self, other):
        if not ((isinstance(other, Number)) or (isinstance(other, String))):
            return None, Value.IllegalOperationError(self, other)
        new_list = deepcopy(self.value)
        new_list[other.value] = other
        new_list = Set(new_list).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        return new_list, None

    def subbed_by(self, other):
        if isinstance(other, Number) or isinstance(other, String):
            if other.value in self.value:
                new_list = deepcopy(self.value)
                del new_list[other.value]
                new_list = Set(new_list).set_pos(self.pos_start, self.pos_end).set_context(self.context)
                return new_list, None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def multed_by(self, other):
        new_list = {}
        new_list.update(self.value)
        new_list.update(other.value)
        new_list = Set(new_list).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        return new_list, None

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value in self.value:
                return self.value[int(other.value)], None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def truth_of_list(self, other):
        truth = []
        if len(other.value) != len(self.value): return False
        if not isinstance(other, Set): return False
        for index, element in self.value.items():
            if index in other.value:
                call = element.ee(other.value[index])[0]
                if call:
                    if call.value == 1:
                        truth.append(True)
                    else:
                        break
                else:
                    break
        return (len(truth) == len(self.value)) and (len(truth) == len(other.value))

    def ee(self, other):
        if isinstance(other, Set):
            return Bool(int(self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(0), None

    def ne(self, other):
        if isinstance(other, Set):
            return Bool(int(not self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(1), None

    def is_true(self):
        return len(self.value)>0

    def is_type(self, other):
        return Bool(int(other.value == "Set")), None

    def notted(self):
        return Bool(not self.is_true()), None

    def ored_by(self, other):
        if isinstance(other, List):
            result = self.is_true() or other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def anded_by(self, other):
        if isinstance(other, List):
            result = self.is_true() and other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def copy(self):
        copy = Set(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return "{"+f"{', '.join(self.representative)}"+"}"

class Map(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value
        self.value_map = {key.value: value for key, value in self.value.items()}
        self.predefined = {
            "type": "Map",
            "id": id(self),
            "value": self
        }

    def added_to(self, other):
        new_map = {}
        if not (isinstance(other, Set) or isinstance(other, Map)):
            return None, Value.IllegalOperationError(self, other)
        new_map.update(self.value)
        new_map.update(other.value)
        new_map = Map(new_map).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        return new_map, None

    def subbed_by(self, other):
        if (isinstance(other, String)) or (isinstance(other, Number)):
            if other.value in self.value:
                new_map = deepcopy(self.value)
                new_map = Map(new_map).set_pos(self.pos_start, self.pos_end).set_context(self.context)
                del new_map[other.value]
                return new_list, None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def dived_by(self, other):
        if (isinstance(other, String)) or (isinstance(other, Number)):
            if other.value in self.value_map:
                return self.value_map[other.value], None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def get_property(self, other):
        if (isinstance(other, Token)) and other.type == TT_IDENTIFIER:
            val = self.value_map.get(other.value)
            if val is None:
                return null.set_context(self.context).set_pos(self.pos_start, other.pos_end), None
            return val, None
        else:
            return self.dived_by(other)

    def truth_of_list(self, other):
        truth = []
        if len(other.value) != len(self.value): return False
        if not isinstance(other, Map): return False
        for index, element in self.value.items():
            if index in other.value:
                call = self.value[index].ee(other.value[index])[0]
                if call:
                    if call.value == 1:
                        truth.append(True)
                    else:
                        break
                else:
                    break
        return (len(truth) == len(self.value)) and (len(truth) == len(other.value))

    def ee(self, other):
        if isinstance(other, List):
            return Bool(int(self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(0), None

    def ne(self, other):
        if isinstance(other, List):
            return Bool(int(not self.truth_of_list(other))).set_context(self.context), None
        else:
            return Bool(1), None

    def is_type(self, other):
        return Bool(int(other.value == "Map")), None

    def is_true(self):
        return len(self.value)>0

    def notted(self):
        return Bool(not self.is_true()), None

    def ored_by(self, other):
        if isinstance(other, List):
            result = self.is_true() or other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def anded_by(self, other):
        if isinstance(other, List):
            result = self.is_true() and other.is_true()
            return Bool(result).set_context(self.context), None
        else:
            return None, Value.IllegalOperationError(self, other)

    def copy(self):
        copy = Map(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

class Record(Map):
    def __init__(self, types, elements):
        super().__init__(elements)
        self.value = elements
        self.predefined = {
            "types": types[:],
            "id": id(elements)
        }

    def added_to(self, other):
        new_map = {}
        if not (isinstance(other, Set) or isinstance(other, Map)):
            return None, Value.IllegalOperationError(self, other)
        new_map.update(self.value)
        new_map.update(other.value)
        new_map = Record(self.predefined['types'], new_map).set_pos(self.pos_start, self.pos_end).set_context(self.context)
        return new_map, None

    def subbed_by(self, other):
        if (isinstance(other, String)) or (isinstance(other, Number)):
            if other.value in self.value:
                new_map = deepcopy(self.value)
                new_map = Record(self.predefined['types'], new_map).set_pos(self.pos_start, self.pos_end).set_context(self.context)
                del new_map[other.value]
                return new_list, None
            else:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context,
                    "Index out of bounds"
                )
        else:
            return None, Value.IllegalOperationError(self, other)

    def get_property(self, other):
        if (isinstance(other, Token)) and other.type == TT_IDENTIFIER:
            val = self.value_map.get(other.value)
            if val is None:
                if other.value in self.predefined:
                    return self.predefined[other.value], None
                elif other.value == "type":
                    return String(self.predefined['types'][-1]), None
                return null.set_context(self.context).set_pos(self.pos_start, other.pos_end), None
            return val, None
        else:
            return self.dived_by(other)

    def is_type(self, other):
        return Bool(int(other.predefined["types"][-1] in self.predefined["types"]) if isinstance(other, Record) else int(other.value in self.predefined["types"])), None

    def copy(self):
        copy = Record(self.predefined['types'], self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f"{self.predefined['types'][-1]} {str(self.value)}" if self.value else f"{self.predefined['types'][-1]}"


class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None

    def copy(self):
        new_context = Context(self.display_name, self.parent, self.parent_entry_pos)
        new_context.symbol_table = self.symbol_table.copy()
        return new_context

    def __repr__(self):
        return f"<{self.display_name}>"

class SymbolTable:
    def __init__(self, parent=None):
        self.symbol_table = {}
        self.parent = parent

    def get(self, name):
        out = self.symbol_table.get(name, None)
        if (out is None) and (self.parent):
            out = self.parent.get(name)
        return out

    def set(self, name, value):
        self.symbol_table[name] = value
        return value

    def copy(self):
        new_table = SymbolTable(self.parent)
        new_table.symbol_table = self.symbol_table
        return new_table

    def __repr__(self):
        return f"{str(self.symbol_table)} -> {str(self.parent)}"

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

class Interpreter:
    def visit(self, node, context):
        method_name = f"visit_{type(node).__name__}"
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    def no_visit_method(self, node, context):
        raise Exception(f"visit_{type(node).__name__} is undefined")

    def visit_NumberNode(self, node, context):
        return RTResult().success(
            Number(node.number).set_pos(node.pos_start, node.pos_end).set_context(context)
        )

    def visit_StringNode(self, node, context):
        return RTResult().success(
            String(node.string).set_pos(node.pos_start, node.pos_end).set_context(context)
        )

    def visit_ListNode(self, node, context):
        res = RTResult()
        elements = []
        for element in node.elements:
            element_value = res.register(self.visit(element, context))
            if isinstance(element_value, List) and element_value.isarg:
                elements.extend(element_value.value)
            else:
                elements.append(element_value)
            if res.should_return(): return res
        return res.success(
                List(elements, node.isArg).set_context(context).set_pos(node.pos_start, node.pos_end)
            )

    def visit_TupleNode(self, node, context):
        res = RTResult()
        elements = []
        for element in node.elements:
            elements.append(res.register(self.visit(element, context)))
            if res.should_return(): return res
        return res.success(
                Tuple(tuple(elements)).set_context(context).set_pos(node.pos_start, node.pos_end)
            )

    def visit_SetNode(self, node, context):
        res = RTResult()
        elements = []
        for element in node.elements:
            element_value = res.register(self.visit(element, context))
            if res.should_return(): return res
            if isinstance(element_value, List) and element_value.isarg:
                for elem in element_value.value:
                    if not ((isinstance(elem, String)) or (isinstance(elem, Number))):
                        return res.failure(RTError(
                            node.pos_start, node.pos_end,
                            context,
                            f"Unhashable element {elem}"
                        ))
                    elements.append(elem)
            else:
                if not ((isinstance(element_value, String)) or (isinstance(element_value, Number))):
                    return res.failure(RTError(
                        node.pos_start, node.pos_end,
                        context,
                        f"Unhashable element {element_value}"
                    ))
                elements.append(element_value)
        elements = {k.value: k for k in elements}
        return res.success(
                Set(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
            )

    def visit_MapNode(self, node, context):
        res = RTResult()
        elements = {}
        for key, value in node.elements:
            key_value = res.register(self.visit(key, context))
            if res.should_return(): return res
            elements[key_value] = res.register(self.visit(value, context))
            if res.should_return(): return res
        return res.success(
                Map(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
            )

    def visit_RecordInstanceNode(self, node, context):
        res = RTResult()
        elements = {}
        type_ = res.register(self.visit(node.type, context))
        types = node.types[:]
        if res.error: return res
        for key, value in node.elements:
            key_value = res.register(self.visit(key, context))
            if res.should_return(): return res
            elements[key_value] = res.register(self.visit(value, context))
            if res.should_return(): return res
        return res.success(
                Record(types, elements).set_context(context).set_pos(node.pos_start, node.pos_end)
            )

    def visit_UnaryNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.number, context))
        if res.should_return(): return res
        error = None
        if node.unary.type == TT_MINUS:
            number, error = number.multed_by(Number(-1))
        elif node.unary.matches(TT_KEYWORD, "not"):
            number, error = number.notted()
        elif node.unary.type == TT_ARGPOW:
            number, error = number.argpowed()
        elif node.unary.type == TT_DCOLON:
            number, error = number.get_property(Token(TT_IDENTIFIER, "type"))
        if error: return res.failure(error)
        return res.success(number.set_pos(node.pos_start, node.pos_end))

    def visit_FuncDefNode(self, node, context):
        res = RTResult()
        func_name = node.identifier.value if node.identifier else node.identifier
        func_args = [arg.value for arg in node.args]
        func_body = node.expr
        func_value = Function(func_name, func_body, func_args, context.copy()).set_context(context).set_pos(node.pos_start, node.pos_end)
        if node.identifier:
            context.symbol_table.set(func_name, func_value)
        return res.success(func_value)

    def visit_ValNode(self, node, context):
        res = RTResult()
        if len(node.identifiers) > len(node.exprs):
            expr = res.register(self.visit(node.exprs[0], context))
            if res.error: return res
            if isinstance(expr, Tuple):
                 if len(node.identifiers) != len(expr.value):
                    return res.failure(RTError(
                        node.pos_start, node.pos_end,
                        context,
                        f"Unequal expressions and identifiers"
                    ))
                 else:
                    vars = list(zip(node.identifiers, expr.value))
                    for identifier, expr in vars:
                        context.symbol_table.set(identifier.value, expr)
                    return res.success(null.set_context(context).set_pos(node.pos_start, node.pos_end))
            else:
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"Unequal expressions and identifiers"
                ))
        elif len(node.identifiers) == len(node.exprs):
            vars = list(zip(node.identifiers, node.exprs))
            for identifier, expr in vars:
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                context.symbol_table.set(identifier.value, expr_value)
            return res.success(null.set_context(context).set_pos(node.pos_start, node.pos_end))
        return res.failure(RTError(
            node.pos_start, node.pos_end,
            context,
            f"Unequal expressions and identifiers"
        ))


    def visit_RecordNode(self, node, context):
        res = RTResult()
        func_name = node.type.value
        types = []
        elements = []
        func_args = [arg.value for arg in node.elements]
        if node.extension:
            extension = res.register(self.visit(node.extension, context))
            if res.error: return res
            if not isinstance(extension, Function):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{extension} is not a record"
                ))
            extension_record = res.register(extension.execute([Number(1) for _ in range(len(extension.arg_names))]))
            if res.error:
                res.reset()
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{extension} is not a record"
                ))
            if not isinstance(extension_record, Record):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{extension} is not a record"
                ))
            extension_keys = [k for k, _ in extension_record.value.items()]
            args = [x.value for x in extension_keys]
            func_args = [*args, *func_args]
            types.extend(extension_record.predefined["types"])
            for key in extension_keys:
                element_name = StringNode(key.value, node.pos_start, node.pos_end)
                element_value = FuncShowNode(Token(TT_IDENTIFIER, key.value), node.pos_start, node.pos_end)
                elements.append((element_name, element_value))
        for element in node.elements:
            element_name = StringNode(element.value, node.pos_start, node.pos_end)
            element_value = FuncShowNode(element, node.pos_start, node.pos_end)
            elements.append((element_name, element_value))
        type_node = StringNode(node.type.value, node.pos_start, node.pos_end)
        type_value = res.register(self.visit(type_node, context))
        types.append(type_value.value)
        if res.error: return res
        func_body = RecordInstanceNode(type_node, types, elements, node.pos_start, node.pos_end)
        func_value = Function(func_name, func_body, func_args, context.copy()).set_context(context).set_pos(node.pos_start, node.pos_end)
        context.symbol_table.set(func_name, type_value)
        return res.success(func_value)

    def visit_FunCallNode(self, node, context):
        res = RTResult()
        args = []
        if (isinstance(node.identifier, Token)) and (node.identifier.type == TT_IDENTIFIER):
            value_to_call = context.symbol_table.get(node.identifier.value)
        else:
            value_to_call = res.register(self.visit(node.identifier, context))
            if res.error: return res
        if value_to_call is None:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{node.identifier.value} is undefined"
            ))
        if not isinstance(value_to_call, BaseFunction):
            return res.success(value_to_call)
        if isinstance(value_to_call, FunCallNode):
            value_to_call = res.register(self.visit(node.identifier, context))
        for arg_node in node.args:
            arg = res.register(self.visit(arg_node, context))
            if res.should_return(): return res
            if (isinstance(arg, List)) and (arg.isarg):
                args.extend(arg.value)
            else:
                args.append(arg)
        return_value = res.register(value_to_call.execute(args))
        if res.should_return(): return res
        return_value = return_value.copy().set_pos(node.pos_start, node.pos_end)
        return res.success(return_value)

    def visit_FuncShowNode(self, node, context):
        res = RTResult()
        if (isinstance(node.identifier, Token)) and (node.identifier.type == TT_IDENTIFIER):
            value_to_call = context.symbol_table.get(node.identifier.value)
        else:
            value_to_call = res.register(self.visit(node.identifier, context))
            if res.error: return res
        if value_to_call is None:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{node.identifier.value} is undefined"
            ))
        return res.success(value_to_call)

    def visit_IfNode(self, node, context):
        res = RTResult()
        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.should_return(): return res
            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.should_return(): return res
                return res.success(expr_value)
        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.should_return(): return res
            return res.success(else_value)
        return res.success(null)

    def visit_WhenNode(self, node, context):
        res = RTResult()
        new_interpreter = Interpreter()
        new_context = Context(id(new_interpreter), context, node.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        statement = res.register(new_interpreter.visit(node.statement, new_context))
        if res.should_return(): return res
        expr = res.register(new_interpreter.visit(node.expr, new_context))
        if res.should_return(): return res
        return res.success(expr)

    def visit_AlgebraDefNode(self, node, context):
        res = RTResult()
        functions = []
        default = None
        node.identifier.type.value = fWordList(node.identifier.type.value, "upper")
        identifier = res.register(self.visit(node.identifier, context))
        if res.error: return res
        if not isinstance(identifier, BaseFunction):
            non_record = fWordList(node.identifier.type.value, "lower")
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{non_record} is not a function"
            ))
        identifier.name = fWordList(identifier.name, "upper")
        new_context = Context(str(id(node)) + identifier.name, context, node.pos_end)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        new_context.symbol_table.set(identifier.name, identifier)
        for conditionNode, recordNode in node.cases:
            recordFunction = res.register(self.visit(recordNode, new_context))
            if res.error: return res
            if not isinstance(recordFunction, BaseFunction):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{recordFunction} is not a function"
                ))
            recordFunctionCopy = recordFunction.copy()
            if isinstance(recordFunctionCopy.body, RecordInstanceNode):
                recordFunction.body.types = [identifier.name, *recordFunction.body.types]
            recordFunctionCopy.body = IfNode([(conditionNode, recordFunction.body)], None, recordFunction.pos_start, recordFunction.pos_end)
            if fWordList(recordFunction.name, "lower") in context.symbol_table.symbol_table:
                context.symbol_table.set(fWordList(recordFunction.name, "lower"), recordFunction)
            else:
                context.symbol_table.set(fWordList(recordFunction.name, "lower"), recordFunctionCopy)
            functions.append(recordFunctionCopy)
        if node.default:
            defaultRecordFunction = res.register(self.visit(node.default, new_context))
            if res.error: return res
            if not isinstance(defaultRecordFunction, BaseFunction):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{defaultRecordFunction} is not a function"
                ))
            defaultRecordFunctionCopy = defaultRecordFunction.copy()
            if isinstance(defaultRecordFunction.body, RecordInstanceNode):
                defaultRecordFunction.body.types = [identifier.name, *recordFunction.body.types]
            if fWordList(defaultRecordFunction.name, "lower") in context.symbol_table.symbol_table:
                context.symbol_table.set(fWordList(defaultRecordFunction.name, "lower"), defaultRecordFunction)
            else:
                context.symbol_table.set(fWordList(defaultRecordFunction.name, "lower"), defaultRecordFunctionCopy)
            default = defaultRecordFunctionCopy
        for c_identifier, c_value in new_context.symbol_table.symbol_table.items():
            if c_identifier[0].isupper():
                context.symbol_table.set(c_identifier, c_value)
        context.symbol_table.set(identifier.name, String(identifier.name).set_context(context).set_pos(node.pos_start, node.pos_end))
        fun_name = fWordList(identifier.name, "lower")
        container = FunContainer(fun_name, functions, default).set_context(context).set_pos(node.pos_start, node.pos_end)
        context.symbol_table.set(fun_name, container)
        return res.success(container)

    def visit_ForNode(self, node, context):
        res = RTResult()
        elements = []
        identifier_expr = res.register(self.visit(node.identifier_expr, context))
        condition = Bool(1)
        if res.error: return res
        if not (isinstance(identifier_expr, List) or isinstance(identifier_expr, Tuple) or isinstance(identifier_expr, String)):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{identifier_expr} is not a List"
            ))
        new_interpreter = Interpreter()
        new_context = Context(id(new_interpreter), context, node.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        for element in identifier_expr.value:
            new_context.symbol_table.set(node.iterator.value, element)
            if node.filter:
                condition = res.register(self.visit(node.filter, new_context))
                if res.should_return(): return res
            if condition.is_true():
                elements.append(res.register(new_interpreter.visit(node.expr, new_context)))
                if res.should_return(): return res
        new_list = List(elements).set_pos(node.pos_start, node.pos_end).set_context(context)
        new_list.isarg = True
        return res.success(new_list)

    def visit_PatternNode(self, node, context):
        res = RTResult()
        new_interpreter = Interpreter()
        new_context = Context(id(new_interpreter), context, node.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        if node.map_context is not None:
            map_context = res.register(self.visit(node.map_context, context))
            if res.should_return(): return res
            if not isinstance(map_context, Map):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{map_context} is neither a map nor a Record Instance"
                ))
            new_context.symbol_table.symbol_table = map_context.copy().value_map
        for identifier, expr in node.vars:
            expr_value = res.register(new_interpreter.visit(expr, new_context))
            new_context.symbol_table.set(identifier.value, expr_value)
        endres = res.register(new_interpreter.visit(node.node, new_context))
        if res.error: return res
        return res.success(endres)

    def visit_AddCaseNode(self, node, context):
        res = RTResult()
        function = res.register(self.visit(node.identifier, context))
        if res.error: return res
        if not isinstance(function, Function):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{node.identifier.identifier.value} is not a caseable function"
            ))
        if not isinstance(function.body, PatternNode):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{node.identifier.identifier.value} is not a caseable function"
            ))
        if not isinstance(function.body.node, IfNode):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{node.identifier.identifier.value} is not a caseable function"
            ))
        if node.condition:
            function.body.node.cases.append((node.condition, node.expr))
        else:
            function.body.node.else_case = node.expr
        return res.success(null)

    def visit_AddAlgebraCaseNode(self, node, context):
        res = RTResult()
        funcontainer = res.register(self.visit(node.identifier, context))
        if res.error: return res
        function = res.register(self.visit(node.expr, context))
        if res.error: return res
        if not isinstance(funcontainer, FunContainer):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{node.identifier.identifier.value} is not a caseable function"
            ))
        if not isinstance(function, BaseFunction):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                context,
                f"{function} is not a function"
            ))
        upperFunContainerName = fWordList(funcontainer.name, "upper")
        upperFunctionName = fWordList(function.name, "upper")
        lowerFunctionName = fWordList(function.name, "lower")
        if isinstance(function.body, RecordInstanceNode):
            function.body.types = [upperFunContainerName, *function.body.types]
        functionCopy = function.copy()
        functionCopy.body = IfNode([(node.condition, function.body)], None, node.pos_start, node.pos_end)
        funcontainer.functions.append(functionCopy)
        context.symbol_table.set(upperFunctionName, String(upperFunctionName).set_context(context).set_pos(node.pos_start, node.pos_end))
        if lowerFunctionName in context.symbol_table.symbol_table:
            context.symbol_table.set(lowerFunctionName, function)
        else:
            context.symbol_table.set(lowerFunctionName, functionCopy)
        return res.success(null)

    def visit_BinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.left, context))
        if res.should_return(): return res
        if node.op.type == TT_DARROW and isinstance(node.right, FunCallNode):
            right = node.right
        else:
            right = res.register(self.visit(node.right, context))
        if res.should_return(): return res
        error = None
        result = None
        if node.op.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op.type == TT_DIV:
            result, error = left.dived_by(right)
        elif node.op.type == TT_GT:
            result, error = left.gt(right)
        elif node.op.type == TT_GTE:
            result, error = left.gte(right)
        elif node.op.type == TT_LT:
            result, error = left.lt(right)
        elif node.op.type == TT_LTE:
            result, error = left.lte(right)
        elif node.op.type == TT_EE:
            result, error = left.ee(right)
        elif node.op.type == TT_NE:
            result, error = left.ne(right)
        elif node.op.type == TT_INFIX:
            id = node.op.value
            fun = context.symbol_table.get(id)
            if isinstance(fun, BaseFunction):
                result = res.register(fun.execute([left, right]))
                if res.error: error = res.error
            else:
                error = RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{id} is not a function"
                )
        elif node.op.type == TT_PIPELINE:
            fun = right
            if isinstance(fun, BaseFunction):
                result = res.register(fun.execute(left.value if (isinstance(left, List) and left.isarg) else [left]))
                if res.error: error = res.error
            else:
                error = RTError(
                    node.pos_start, node.pos_end,
                    context,
                    f"{id} is not a function"
                )
        elif node.op.type == TT_DCOLON:
            result, error = left.is_type(right)
        elif node.op.type == TT_DARROW:
            if isinstance(right, FunCallNode):
                right = right.identifier
            result, error = left.get_property(right)
        elif node.op.matches(TT_KEYWORD, "and"):
            result, error = left.anded(right)
        elif node.op.matches(TT_KEYWORD, "or"):
            result, error = left.ored(right)
        if error: return res.failure(error)
        return res.success(result.set_pos(node.pos_start, node.pos_end))

false = Bool(0)
true = Bool(1)
null = NullType(0)
global_symbol_table = SymbolTable()
global_symbol_table.set('false', false)
global_symbol_table.set('true', true)
global_symbol_table.set('null', null)
global_symbol_table.set('String', String("String"))
global_symbol_table.set('Number', String("Number"))
global_symbol_table.set('List', String("List"))
global_symbol_table.set('Tuple', String("Tuple"))
global_symbol_table.set('Map', String("Map"))
global_symbol_table.set('Set', String("Set"))
global_symbol_table.set('Function', String("Function"))
global_symbol_table.set('Bool', String("Bool"))
global_symbol_table.set('NullType', String("NullType"))

def fWordList(name, todo):
    fun_name = list(name)
    fun_name[0] = fun_name[0].upper() if todo == "upper" else fun_name[0].lower()
    fun_name = "".join(fun_name)
    return fun_name

def run(fn, text):
    lexer = Lexer(fn, text)
    tokens, error = lexer.generate_tokens()
    if error: return None, error
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error
    interpreter = Interpreter()
    context = Context(fn)
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)
    return result.value, result.error

while True:
    inp = input("Dyha>> ")
    res, error = run("<module>", inp)
    if error:
        print(error.as_string())
    else:
        if len(res.value) > 1:
            print(res)
        else:
            print(res.value[0])

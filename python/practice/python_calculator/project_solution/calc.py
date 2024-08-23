# Константы, обозначающие тип токена
TOKEN_NUMBER = "number"
TOKEN_OPERATOR = "operator"
TOKEN_PARENTHESIS = "parenthesis"

# Константы, обозначающие класс символа
DIGIT = "digit"
POINT = "point"
OPERATOR = "operator"
PARENTHESIS = "parenthesis"
OTHER = "other"

# Константы, обозначающие состояние КА
NEW_TOKEN = "new_token"
NUMBER_INTEGER_PART = "number_integer_part"
NUMBER_FRACTIONAL_PART = "number_fractional_part"
ERROR = "error"

# Поддерживаемые математические операции в выражении вместе с их реализацией
# и приоритетом. Чем больше значение поля "priority", тем выше приоритет
OPERATORS = {
    "*": {"func": lambda a, b: a * b, "priority": 2},
    "/": {"func": lambda a, b: a / b, "priority": 2},
    "+": {"func": lambda a, b: a + b, "priority": 1},
    "-": {"func": lambda a, b: a - b, "priority": 1},
}


def save_token(token, tokens):
    if len(token) == 0:
        return

    if token["type"] == TOKEN_NUMBER:
        if token["value"].isnumeric():
            token["value"] = int(token["value"])
        else:
            token["value"] = float(token["value"])

    tokens.append(token.copy())
    token.clear()


def start_accumulating_number(char, token, tokens):
    save_token(token, tokens)
    token["type"] = TOKEN_NUMBER
    token["value"] = char


def accumulate_number(char, token, tokens):
    token["value"] += char


def accumulate_operator(char, token, tokens):
    save_token(token, tokens)
    token["type"] = TOKEN_OPERATOR
    token["value"] = char


def accumulate_parenthesis(char, token, tokens):
    save_token(token, tokens)
    token["type"] = TOKEN_PARENTHESIS
    token["value"] = char


# Конечный автомат (finite-state machine) для парсинга строки 
# с алгебраическим выражением
FSM = {
    # Символ
    DIGIT: {
        # Текущее состояние        Новое состояние         Действие
        NEW_TOKEN: (NUMBER_INTEGER_PART, start_accumulating_number),
        NUMBER_INTEGER_PART: (NUMBER_INTEGER_PART, accumulate_number),
        NUMBER_FRACTIONAL_PART: (NUMBER_FRACTIONAL_PART, accumulate_number),
    },
    POINT: {
        NEW_TOKEN: (NUMBER_FRACTIONAL_PART, start_accumulating_number),
        NUMBER_INTEGER_PART: (NUMBER_FRACTIONAL_PART, accumulate_number),
        NUMBER_FRACTIONAL_PART: (ERROR, None),
    },
    OPERATOR: {
        NEW_TOKEN: (NEW_TOKEN, accumulate_operator),
        NUMBER_INTEGER_PART: (NEW_TOKEN, accumulate_operator),
        NUMBER_FRACTIONAL_PART: (NEW_TOKEN, accumulate_operator),
    },
    PARENTHESIS: {
        NEW_TOKEN: (NEW_TOKEN, accumulate_parenthesis),
        NUMBER_INTEGER_PART: (NEW_TOKEN, accumulate_parenthesis),
        NUMBER_FRACTIONAL_PART: (NEW_TOKEN, accumulate_parenthesis),
    },
    OTHER: {
        NEW_TOKEN: (ERROR, None),
        NUMBER_INTEGER_PART: (ERROR, None),
        NUMBER_FRACTIONAL_PART: (ERROR, None),
    },
}


def parse_symbol(c):
    if c.isnumeric():
        return DIGIT
    if c == ".":
        return POINT
    if c in OPERATORS:
        return OPERATOR
    if c in ["(", ")"]:
        return PARENTHESIS

    return OTHER


def tokenize(s):
    tokens = []
    token = {}

    state = NEW_TOKEN

    for c in s:
        symbol = parse_symbol(c)
        data = FSM[symbol][state]
        state = data[0]
        action = data[1]

        if action is not None:
            action(c, token, tokens)

        if state == ERROR:
            return []

    save_token(token, tokens)
    return tokens


def has_lower_priority(op1, op2):
    return OPERATORS[op1]["priority"] < OPERATORS[op2]["priority"]


def convert_to_postfix_notation(tokens_infix):
    tokens_postfix = []
    stack = []

    for token in tokens_infix:
        if token["type"] == TOKEN_NUMBER:
            tokens_postfix.append(token)
            continue

        if token["type"] == TOKEN_PARENTHESIS:
            if token["value"] == "(":
                stack.append(token)
            elif token["value"] == ")":
                found_pair = False
                while len(stack) > 0:
                    stack_token = stack.pop()
                    if (
                        stack_token["type"] == TOKEN_PARENTHESIS
                        and stack_token["value"] == "("
                    ):
                        found_pair = True
                        break
                    tokens_postfix.append(stack_token)

                if not found_pair:
                    return None
            continue

        if token["type"] == TOKEN_OPERATOR:
            while (
                len(stack) > 0
                and stack[-1]["type"] == TOKEN_OPERATOR
                and not has_lower_priority(stack[-1]["value"], token["value"])
            ):
                tokens_postfix.append(stack.pop())

            stack.append(token)
            continue

        return None  # unknown token type

    while len(stack) > 0:
        stack_token = stack.pop()
        if stack_token["type"] != TOKEN_OPERATOR:
            return None
        tokens_postfix.append(stack_token)

    return tokens_postfix


def apply(op, a, b):
    return OPERATORS[op]["func"](a, b)


def calculate_expression(tokens_postfix):
    stack = []

    for token in tokens_postfix:
        if token["type"] == TOKEN_NUMBER:
            stack.append(token["value"])
            continue
        if token["type"] == TOKEN_OPERATOR:
            if len(stack) < 2:
                return None

            b = stack.pop()
            a = stack.pop()

            res = apply(token["value"], a, b)
            stack.append(res)
            continue

        return None  # unexpected token type

    if len(stack) != 1:
        return None  # logic error

    return stack.pop()


def calc(raw_expression):
    tokens = tokenize(raw_expression)
    if tokens is None:
        return None

    tokens_postfix = convert_to_postfix_notation(tokens)
    if tokens_postfix is None:
        return None

    return calculate_expression(tokens_postfix)


if __name__ == "__main__":
    expressions = [
        "2+3",              # 5
        "1-2*3",            # -5
        "(1-2)*3",          # -3
        "(1+(2/2))-(3-5)",  # 4
        "1/2-1/2",          # 0
    ]

    for expr in expressions:
        print(f"{expr} = {calc(expr)}")

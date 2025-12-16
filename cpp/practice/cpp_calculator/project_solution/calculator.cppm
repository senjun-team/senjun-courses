export module calculator;

import std;

export double calc(const std::string & expr);

enum class TokenType : std::uint8_t
{
    Number = 0,
    Operator,
    Parenthesis,
    Invalid
};

enum class SymbolType : std::uint8_t
{
    Digit = 0,
    Point,
    Operator,
    Parenthesis,
    Other
};

enum class State : std::uint8_t
{
    NewToken = 0,
    NumberIntegerPart,
    NumberFractionalPart,
    Error
};

struct Operator
{
    std::uint8_t priority = 0;
    std::function<double(double, double)> func;
};

const std::map<std::string, Operator> kOperators = {
    {"*", Operator{.priority=2, .func=std::multiplies<double>()}},
    {"/", Operator{.priority=2, .func=std::divides<double>()}},
    {"+", Operator{.priority=1, .func=std::plus<double>()}},
    {"-", Operator{.priority=1, .func=std::minus<double>()}},
};

struct Token
{
    TokenType tok_type = TokenType::Invalid;
    std::string value;
};

void save_token(Token & token, std::vector<Token> & tokens)
{
    if (token.value.empty())
        return;

    tokens.push_back(token);
    token = Token{};
}

void start_accumulating_number(char symbol, Token & token, std::vector<Token> & tokens)
{
    save_token(token, tokens);
    token.tok_type = TokenType::Number;
    token.value = std::string{symbol};
}

void accumulate_number(char symbol, Token & token, std::vector<Token> & tokens)
{
    token.value += symbol;
}

void accumulate_operator(char symbol, Token & token, std::vector<Token> & tokens)
{
    save_token(token, tokens);
    token.tok_type = TokenType::Operator;
    token.value = std::string{symbol};
}

void accumulate_parenthesis(char symbol, Token & token, std::vector<Token> & tokens)
{
    save_token(token, tokens);
    token.tok_type = TokenType::Parenthesis;
    token.value = std::string{symbol};
}

struct Transition
{
    State new_state;
    std::function<void(char, Token &, std::vector<Token> &)> action;
};

using Row = std::map<State, Transition>;

// Конечный автомат (finite-state machine) для парсинга строки
// с алгебраическим выражением
const std::map<SymbolType, Row> kFSM = {
    // Символ
    { SymbolType::Digit,
        {
            { State::NewToken, // Текущее состояние
            // Новое состояние                       Действие
            { .new_state = State::NumberIntegerPart, .action = start_accumulating_number} },

            { State::NumberIntegerPart,
            { .new_state = State::NumberIntegerPart, .action = accumulate_number} },

            { State::NumberFractionalPart,
            { .new_state = State::NumberFractionalPart, .action = accumulate_number} },
        }
    },

    { SymbolType::Point,
        {
            { State::NewToken,
            { .new_state = State::NumberFractionalPart, .action = start_accumulating_number} },

            { State::NumberIntegerPart,
            { .new_state = State::NumberFractionalPart, .action = accumulate_number} },

            { State::NumberFractionalPart,
            { .new_state = State::Error, .action = {} } },
        }
    },

    { SymbolType::Operator,
        {
            { State::NewToken,
            { .new_state = State::NewToken, .action = accumulate_operator} },

            { State::NumberIntegerPart,
            { .new_state = State::NewToken, .action = accumulate_operator} },

            { State::NumberFractionalPart,
            { .new_state = State::NewToken, .action = accumulate_operator } },
        }
    },

    { SymbolType::Parenthesis,
        {
            { State::NewToken,
            { .new_state = State::NewToken, .action = accumulate_parenthesis} },

            { State::NumberIntegerPart,
            { .new_state = State::NewToken, .action = accumulate_parenthesis} },

            { State::NumberFractionalPart,
            { .new_state = State::NewToken, .action = accumulate_parenthesis } },
        }
    },

    { SymbolType::Other,
        {
            { State::NewToken,
            { .new_state = State::Error, .action = {} } },

            { State::NumberIntegerPart,
            { .new_state = State::Error, .action = {} } },

            { State::NumberFractionalPart,
            { .new_state = State::Error, .action = {}  } },
        }
    },
};

SymbolType get_symbol_type(char c)
{
    if (std::isdigit(c))
        return SymbolType::Digit;
    if (c == '.')
        return SymbolType::Point;
    if (kOperators.contains(std::string{c}))
        return SymbolType::Operator;
    if (c == '(' || c == ')')
        return SymbolType::Parenthesis;
    return SymbolType::Other;
}

std::vector<Token> tokenize(const std::string & raw_expression)
{
    std::vector<Token> tokens;
    tokens.reserve(raw_expression.size());

    Token token;
    State state = State::NewToken;

    for(char sybmol: raw_expression)
    {
        const SymbolType symbol_type = get_symbol_type(sybmol);

        if (symbol_type == SymbolType::Other)
            throw std::invalid_argument("Invalid token in expression");

        const Transition transition = kFSM.at(symbol_type).at(state);

        if (transition.new_state == State::Error)
            throw std::invalid_argument("Couldn't split expression to tokens");

        state = transition.new_state;
        transition.action(sybmol, token, tokens);
    }

    save_token(token, tokens);
    return tokens;
}

bool has_lower_priority(const std::string & op_left, const std::string &  op_right)
{
    return kOperators.at(op_left).priority < kOperators.at(op_right).priority;
}

std::vector<Token> convert_to_postfix_notation(const std::vector<Token> & tokens_infix)
{
    std::vector<Token> tokens_postfix;
    tokens_postfix.reserve(tokens_infix.size());

    std::stack<Token> stack;

    for (const Token & token: tokens_infix)
    {
        if (token.tok_type == TokenType::Number)
        {
            tokens_postfix.push_back(token);
            continue;
        }

        if (token.tok_type == TokenType::Parenthesis)
        {
            if (token.value == "(")
                stack.push(token);
            else if (token.value == ")")
            {
                bool found_pair = false;
                while (!stack.empty())
                {
                    Token stack_token = stack.top();
                    stack.pop();
                    if (stack_token.tok_type == TokenType::Parenthesis && stack_token.value == "(")
                    {
                        found_pair = true;
                        break;
                    }
                    tokens_postfix.push_back(stack_token);
                }

                if (!found_pair)
                    throw std::invalid_argument("Parenthesis mismatch");
            }
            continue;
        }

        if (token.tok_type == TokenType::Operator)
        {
            while ( !stack.empty() && stack.top().tok_type == TokenType::Operator
                && !has_lower_priority(stack.top().value, token.value)
            )
            {
                 tokens_postfix.push_back(stack.top());
                 stack.pop();
            }

            stack.push(token);
            continue;
        }

        throw std::invalid_argument("Unknown token in expression");
    }

    while (!stack.empty())
    {
        Token stack_token = stack.top();
        if (stack_token.tok_type != TokenType::Operator)
            throw std::invalid_argument("Parentheses mismatch");
        stack.pop();
        tokens_postfix.push_back(stack_token);
    }

    return tokens_postfix;
}

double apply(const std::string & op, double a, double b)
{
    return kOperators.at(op).func(a, b);
}

double calculate_expression(const std::vector<Token> & tokens_postfix)
{
    std::stack<double> stack;

    for (const Token & token: tokens_postfix)
    {
        if (token.tok_type == TokenType::Number)
        {
            stack.push(std::stod(token.value));
            continue;
        }

        if (token.tok_type == TokenType::Operator)
        {
            if (stack.size() < 2)
                throw std::invalid_argument("Operators-to-operands count mismatch");

            double b = stack.top();
            stack.pop();

            double a = stack.top();
            stack.pop();

            double res = apply(token.value, a, b);
            stack.push(res);
            continue;
        }

        throw std::invalid_argument("Unexpected token type");
    }

    if (stack.size() != 1)
        throw std::invalid_argument("Bug in implementation of calculation of expression in postfix form");

    return stack.top();
}

double calc(const std::string & expr)
{
    return calculate_expression(convert_to_postfix_notation(tokenize(expr)));
}

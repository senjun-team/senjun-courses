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
};

struct Operator
{
    std::uint8_t priority = 0;
    std::function<double(double, double)> func;
};

const std::map<std::string, Operator> kOperators = {
    {"*", Operator{2, std::multiplies<double>()}},
    {"/", Operator{2, std::divides<double>()}},
    {"+", Operator{1, std::plus<double>()}},
    {"-", Operator{1, std::minus<double>()}},
};

struct Token
{
    TokenType tok_type = TokenType::Invalid;
    std::string value;
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

class Tokenizer
{
public:
    explicit Tokenizer(const std::string & raw_expression);
    std::vector<Token> get_tokens();

private:
    void save_token();
    void start_accumulating_number();
    void accumulate_number();
    void accumulate_operator();
    void accumulate_parenthesis();

    void switch_state();

    char symbol = 0;
    Token token;
    std::vector<Token> tokens;
    State state = State::NewToken;
};

Tokenizer::Tokenizer(const std::string & raw_expression)
{
    tokens.reserve(raw_expression.size());

    for(char c: raw_expression)
    {
        symbol = c;
        switch_state();
    }

    save_token();
}

std::vector<Token> Tokenizer::get_tokens()
{
    return tokens;
}

void Tokenizer::save_token()
{
    if (token.value.empty())
        return;

    tokens.push_back(token);
    token = Token{};
}

void Tokenizer::start_accumulating_number()
{
    save_token();
    token.tok_type = TokenType::Number;
    token.value = std::string{symbol};
}

void Tokenizer::accumulate_number()
{
    token.value += symbol;
}

void Tokenizer::accumulate_operator()
{
    save_token();
    token.tok_type = TokenType::Operator;
    token.value = std::string{symbol};
}

void Tokenizer::accumulate_parenthesis()
{
    save_token();
    token.tok_type = TokenType::Parenthesis;
    token.value = std::string{symbol};
}

void Tokenizer::switch_state()
{
    const SymbolType symbol_type = get_symbol_type(symbol); 
    switch(symbol_type)
    {
        case SymbolType::Digit:
        {
            switch(state)
            {
                case State::NewToken:
                    state = State::NumberIntegerPart;
                    start_accumulating_number();
                    break;
                case State::NumberIntegerPart:
                    state = State::NumberIntegerPart;
                    accumulate_number();
                    break;
                case State::NumberFractionalPart:
                    state = State::NumberFractionalPart;
                    accumulate_number();
                    break;

            }
            break;
        }
        
        case SymbolType::Point:
        {
            switch(state)
            {
                case State::NewToken:
                    state = State::NumberFractionalPart;
                    start_accumulating_number();
                    break;
                case State::NumberIntegerPart:
                    state = State::NumberFractionalPart;
                    accumulate_number();
                    break;
                case State::NumberFractionalPart:
                    throw std::invalid_argument("Floating-point number with multiple points");
            }
            break;
        }
        
        case SymbolType::Operator:
            state = State::NewToken;
            accumulate_operator();
            break;
        
        case SymbolType::Parenthesis:
            state = State::NewToken;
            accumulate_parenthesis();
            break; 

        default:
            throw std::invalid_argument("Couldn't split expression to tokens");
    }
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
    Tokenizer tokenizer{expr};
    return calculate_expression(convert_to_postfix_notation(tokenizer.get_tokens()));
}

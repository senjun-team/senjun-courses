import boost.ut;

import calculator;

using namespace boost::ut;

struct TestCase
{
    std::string expr;
    std::optional<double> plan_val;
    std::string descr;
};

bool eq(double a, double b)
{
    return std::abs(a - b) < 1e-6;
}

void check(const TestCase & c)
{
    try
    {

        const double fact_val = calc(c.expr);

        if (c.plan_val.has_value())
        {
            expect(eq(fact_val, c.plan_val.value())) << std::format(
                "Invalid result for expression: {}. Plan: {}. Fact: {}. Test short description: {}",
                c.expr, c.plan_val.value(), fact_val, c.descr
                ) << fatal;

        }
    }
    catch(const std::invalid_argument & ia)
    {
        if (c.plan_val.has_value()) // should not throw
        {
            expect(false) << std::format(
                "Throwed std::invalid_argument for correct expression: {}. Test short description: {}. Exception text: {}",
                c.expr, c.descr, ia.what()
                ) << fatal;
        }
    }
    catch(const std::exception & e)
    {
        if (c.plan_val.has_value()) // should not throw
        {
            expect(false) << std::format(
                "Throwed exception for correct expression: {}. Test short description: {}. Exception text: {}",
                c.expr, c.descr, e.what()
                ) << fatal;
        }
        else // invalid exception type
        {
            expect(false) << std::format(
                "Throwed wrong exception type (must throw std::invalid_argument). Expression: {}. Test short description: {}. Exception text: {}",
                c.expr, c.descr, e.what()
                ) << fatal;
        }
    }
}

void check_suite(const std::vector<TestCase> & cases)
{
    for(const auto & c: cases)
        check(c);
}

// Юнит-тесты
int main()
{
    "Test integer numbers"_test = [] {
        const std::vector<TestCase> cases = {
            {"0", 0, "zero"},
            {"8", 8, "non-zero single-digit integer number"},
            {"12", 12, "many-digit integer number"},
            {"0123", 123, "many-digit int number with leading zero"},
            {"008", 8, "many-digit int number with several leading zeros"},
            {"9876543210", 9876543210, "long integer number"},
            {"a5", std::nullopt, "non-digit at the beginning"},
            {"5$", std::nullopt, "non-digit at the end"},
            {"1_2", std::nullopt, "non-digit in the middle"},
            {"a", std::nullopt, "not a digit"},
            {"abc", std::nullopt, "several non-digits"},
            {"-5", std::nullopt, "negative number"},
            {"+5", std::nullopt, "unary plus"},
        };

        check_suite(cases);
    };

    "Test floating-point numbers"_test = [] {
        const std::vector<TestCase> cases = {
            {"1.", 1, "floating-point without the fractional part"},
            {".03", 0.03, "floating-point without integer part"},
            {"..03", std::nullopt, "no integer part and 2 dots"},
            {"1.2", 1.2, "floating-point with integer and fractional parts"},
            {
                "0.1",
                0.1,
                "floating-point with 0 integer and non-zero fractional part",
            },
            {"1.2345", 1.2345, "fractional part with 4 digits"},
            {"987654.321", 987654.321, "integer part with 6 digits"},
            {"00.1", 0.1, "floating-point with leading zeros"},
            {"1.1000", 1.1, "floating-point with trailing zeros"},
            {"1.a", std::nullopt, "floating-point with letter in fractional part"},
            {"1.2e+3", std::nullopt, "exponential representation"},
            {"1.2.3", std::nullopt, "floating-point with 2 dots"},
        };

        check_suite(cases);
    };

    "Test operators"_test = [] {
        const std::vector<TestCase> cases = {
            {"1+2", 3, "expression with single +"},
            {"1+22+333", 356, "expression with chain of + operators"},
            {"9-11", -2, "expression with single - and negative result"},
            {"11-9", 2, "expression with single - and positive result"},
            {
                "58-304-0",
                -246,
                "expression with chain of - operators (with 0)",
            },
            {"9-1-1-0.5-1.5", 5, "expression with chain of - operators"},
            {"3*11", 33, "expression with single *"},
            {
                "2*9*100*7",
                12600,
                "expression with chain of * operators (integers only)",
            },
            {
                "2.0*9.0*100.0*7.0",
                12600,
                "expression with chain of * operators (floating-points only)",
            },
            {"49/7", 7, "expression with single /"},
            {"28/2/2/2", 3.5, "expression with chain of / operators"},
            {
                "8/4/2",
                1,
                "expression with chain of / operators and integer result",
            },
            {"5/0.5", 10, "expression with / operator and floating-point"},
        };

        check_suite(cases);
    };

    "Test operators priority"_test = [] {
        const std::vector<TestCase> cases = {
            {"2+5-1", 6, "operators with equal priority: +, -"},
            {"2-5+1", -2, "operators with equal priority: -, +"},
            {"4*2*3/11", 2.18181818, "operators with equal priority: *, /"},
            {"4/2/3*11", 7.33333333, "operators with equal priority: /, *"},
            {"3-9+8*2", 10, "operators with mixed priority: -, +, *"},
            {"44*150-45/5", 6591, "operators with mixed priority: *, -, /"},
            {
                "6+400/500-1*1",
                5.8,
                "operators with mixed priority: +, /, -, *",
            },
            {"1/2-1/2", 0, "operations with mixed priority: /, -, /"},
            {"15+2*6-8", 19, "operations with mixed priority: +, *, -"},
            {
                "2-503*8+1/7",
                -4021.85714286,
                "operations with mixed priority: -, *, +, /",
            },
            {
                "5*2-6/3+5-7-9*0",
                6,
                "operators with mixed priority and multiplication by 0",
            },
        };

        check_suite(cases);
    };

    "Test parentheses"_test = [] {
        const std::vector<TestCase> cases = {
            {"()", std::nullopt, "empty parentheses"},
            {")()", std::nullopt, "empty invalid parentheses"},
            {"(45.7)", 45.7, "parentheses with single number"},
            {"[45.7]", std::nullopt, "square brackets"},
            {"{45.7}", std::nullopt, "curly braces"},
            {"(1)+(2)", 3, "numbers with parentheses"},
            {"((62))", 62, "nested parentheses with single number"},
            {"(1+5.6)", 6.6, "expression in parentheses"},
            {"(8+2*5)/(1+3*2-4)", 6, "2 expressions in parentheses"},
            {"3+4*2/(1-5)*2", -1, "expression in parentheses (in divider)"},
            {"3*(4+5)", 27, "multiplying the sum in parentheses by a number"},
            {"(1-2)*3", -3, "priority of - operator in parentheses"},
            {
                "(1+(2/2))-(3-5)",
                4,
                "priority of / and - operators in parentheses",
            },
            {
                "(2-503)*8.0+1/7",
                -4007.85714286,
                "parentheses around 1st operator",
            },
            {
                "2-(503*8)+1.0/7",
                -4021.85714286,
                "parentheses around 2nd operator",
            },
            {
                "2.0-503*(8+1)/7",
                -644.714285714,
                "parentheses around 3rd operator",
            },
            {
                "2-503.0*8+(1/7)",
                -4021.85714286,
                "parentheses around 4th operator",
            },
            {"((2-503)*8+1)/7", -572.428571429, "nested parentheses"},
            {
                "(((2)-((503)))*8+1)/(7)",
                -572.428571429,
                "redundant parentheses",
            },
        };

        check_suite(cases);
    };

    "Test simple invalid expressions"_test = [] {
        const std::vector<TestCase> cases = {
            {"", std::nullopt, "empty string"},
            {"a+b", std::nullopt, "invalid characters"},
            {"*", std::nullopt, "operator-only"},
            {"/*", std::nullopt, "two operators without numbers"},
            {"67-", std::nullopt, "incorrect placement of - operator"},
            {"67--", std::nullopt, "incorrect placement of two - operators"},
            {"5+-2", std::nullopt, "incorrect placement of +, - operators"},
            {"3(4+5)", std::nullopt, "absent multiply operator before parentheses"},
            {"(4+5)3", std::nullopt, "absent multiply operator after parentheses"},
        };

        check_suite(cases);
    };

    "Test long correct expressions"_test = [] {
        const std::vector<TestCase> cases = {
            {
                "15/(7-(1+1))*3-(2+(1+1))*15/(7-(200+1))*3-(2+(0.5+1.5))*(15/(7-(1+1))*3-(2+(1+1))+15/(7-(1+1))*3-(2+(1+1)))",
                -30.0721649485,
                "long expression with parentheses",
            },
        };

        check_suite(cases);
    };
}

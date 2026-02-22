import std;
import calculator;

struct SimpleTest
{
    std::string expression;
    double result = 0.0;
};

bool eq(double a, double b)
{
    return std::abs(a - b) < 1e-6;
}

int main()
{
    std::vector<SimpleTest> tests = {
        {"2+3",             5.0},
        {"1-2*3",          -5.0},
        {"(1-2)*3",        -3.0},
        {"(1+(2/2))-(3-5)", 4.0},
        {"1/2-1/2",         0.0}
    };

    for(const auto & t: tests)
    {
        const double fact = calc(t.expression);
        std::println("calc(\"{}\") returns {}. {}", 
                     t.expression,
                     fact, 
                     eq(t.result, fact) ? "OK" : "ERROR");
    }
}

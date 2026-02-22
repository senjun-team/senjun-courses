import boost.ut;
import TestHelpers;

import brainfuck;

import std;

using namespace boost::ut;

std::string squares_str();

template<class T>
void check_throws(const std::string & sample, const std::string & err_msg_prefix, const std::string & code)
{
    try
    {
        BrainfuckInterpreter{sample};
    }
    catch(const T & e)
    {
        const std::string text = e.what();
        expect(text.starts_with(err_msg_prefix)) << std::format("BrainfuckInterpreter() must throw {} with text starting with \"{}\".\nBrainfuck code: {}", typeid(T).name(), err_msg_prefix, code) << fatal;
        return;
    }
    catch(...)
    {
        expect(false) << std::format("BrainfuckInterpreter() throws an exception of wrong type in case of {}. It must throw {}.\nBrainfuck code: {}", err_msg_prefix, typeid(T).name(), code) << fatal;
        return;
    }

    expect(false) << std::format("BrainfuckInterpreter() doesn't throw an exception in case of {}. It must throw {}.\nBrainfuck code: {}", err_msg_prefix, typeid(T).name(), code) << fatal;
}

struct testcase
{
    std::string filename;
    std::string output;
};

std::vector<int> to_nums(const std::string & s)
{
    std::vector<int> res;
    res.reserve(s.size());
    for (char c: s)
        res.push_back(static_cast<int>(static_cast<unsigned char>(c)));
    return res;
}

void check_testcase(const testcase & tc)
{
    const std::string path = std::format("project/examples/{}", tc.filename);
    const std::string output = hlp::exec(std::format("./build/main {}", path));
    expect(output == tc.output) << std::format("Invalid program output. Program path: {}\nPlan output: {}\nPlan output in ASCII-codes: {}\nFact output: {}\nFact output in ASCII-codes: {}\n", path, tc.output, to_nums(tc.output), output, to_nums(output)) << fatal;
}

// Юнит-тесты
int main()
{
    // Конструктор должен кидать исключение, если индекс ячейки вышел за пределы
    // памяти
    "Constructor throws on pointer out of bound"_test = [] {
        const std::size_t mem_size = 30'000;
        const std::string overflow = std::string(mem_size + 1, '>');

        std::vector<std::string> samples {
            "<",                                // underflow cases
            ".<",
            "><<",
            "+<+++",
            ">+++++[>+++++++<-]<<<<<<<<<<",
            overflow,                           // overflow cases
            overflow + ">",
            overflow + ">>",
            overflow + "+++.",
        };

        for (const auto & sample: samples)
            check_throws<std::out_of_range>(sample, "Memory pointer out of bound", sample);
    };

    // Конструктор должен кидать исключение, если в коде на Brainfuck
    // есть несоответствие открывающих и закрывающих скобок
    "Constructor throws on brackets mismatch"_test = [] {
        std::vector<std::string> samples {
            "[",
            "]",
            "][",
            "].[.[",
            "]][",
            "[[]",
            "[]]",
            ">+++++>+++++++<-]>.<<++[>+++++[>+++++++<-]<-]>>.",
            ">+++++]>+++++++<-]>.<<++[>+++++[>+++++++<-]<-]>>.",
            ">+++++[>+++++++<-]]>.<<++[>+++++[>+++++++<-]<-]>>.",
            "[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[",
            "[-[-[<->[-]]]]]]]]]",
            "[[++[[[[[[[[[[-[-[<->[-]]]]]]]]]"
            
        };

        for (const auto & sample: samples)
            check_throws<std::runtime_error>(sample, "Brackets [] do not match", sample);
    };

    "Code samples without user input"_test = [] {
        std::vector<testcase> testcases {
            {.filename="print_b.b", .output="B"},
            {.filename="print_b_other_way.b", .output="B"},
            {.filename="perl_tradition.b", .output="Just another brainfuck hacker"},
            {.filename="squares.b", .output=squares_str()},
        };

        for (const auto & tc: testcases)
            check_testcase(tc);
    };

    "Code sample with user input"_test = [] {
        const std::string program = "cat project/examples/input_reverse_number.txt | ./build/main project/examples/reverse_number.b";
        const std::string output = hlp::exec(program);
        const std::string plan = "738";
        expect(output == plan) << std::format("Invalid program output. Program: {}\nPlan output: {}\nPlan output in ASCII-codes: {}\nFact output: {}\nFact output in ASCII-codes: {}\n", program, plan, to_nums(plan), output, to_nums(output)) << fatal;
    };
}

std::string squares_str()
{
return R"(0
1
4
9
16
25
36
49
64
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729
784
841
900
961
1024
1089
1156
1225
1296
1369
1444
1521
1600
1681
1764
1849
1936
2025
2116
2209
2304
2401
2500
2601
2704
2809
2916
3025
3136
3249
3364
3481
3600
3721
3844
3969
4096
4225
4356
4489
4624
4761
4900
5041
5184
5329
5476
5625
5776
5929
6084
6241
6400
6561
6724
6889
7056
7225
7396
7569
7744
7921
8100
8281
8464
8649
8836
9025
9216
9409
9604
9801
10000
)";
}
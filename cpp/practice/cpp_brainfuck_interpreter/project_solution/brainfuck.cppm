export module brainfuck;

import std;

export class BrainfuckInterpreter
{
public:
    BrainfuckInterpreter(std::string source)
    {
        parse_brackets(source);

        std::array<unsigned char, 30'000> mem; // Ячейки памяти
        std::size_t i = 0; // Индекс текущей ячейки в mem
        i_command = 0;
        
        while (i_command < source.size())
        {
            switch (source[i_command])
            {
                case '>':
                    if (i == mem.size()) err_out_of_bound();
                    ++i;
                    break;
                case '<':
                    if (i == 0) err_out_of_bound();
                    --i;
                    break;
                case '+':
                    ++mem[i];
                    break;
                case '-':
                    --mem[i];
                    break;
                case '.':
                    std::putchar(mem[i]);
                    break;
                case ',':
                    mem[i] = std::getchar();
                    break;
                case '[':
                    if (mem[i] == 0) i_command = bracket_pairs[i_command];
                    break;
                case ']':
                    if (mem[i] != 0) i_command = bracket_pairs[i_command];
                    break;
            }
            ++i_command;
        }
    }

private:
    void parse_brackets(std::string source)
    {
        std::stack<std::size_t> brackets;
 
        for (i_command = 0; i_command < source.size(); ++i_command)
        {
            if (char c = source[i_command]; c == '[')
            {
                brackets.push(i_command);
            }
            else if (c == ']')
            {
                if (brackets.empty()) err_brackets_mismatch();

                const std::size_t i_open_bracket = brackets.top();
                bracket_pairs[i_open_bracket] = i_command;
                bracket_pairs[i_command] = i_open_bracket;
                brackets.pop();
            }
        }
 
        if (!brackets.empty()) err_brackets_mismatch();
    }

    std::string text_index() { return std::format("Command index: {}", i_command); }

    void err_out_of_bound()
    {
       throw std::out_of_range(std::format(
            "Memory pointer out of bound. {}.", text_index()));
    }

    void err_brackets_mismatch()
    {
        throw std::runtime_error(std::format(
            "Brackets [] do not match. {}.", text_index()));
    }

    std::size_t i_command = 0; // Индекс обрабатываемой команды в коде
    std::unordered_map<std::size_t, std::size_t> bracket_pairs; // Индексы парных скобок
};

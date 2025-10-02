import brainfuck;
import std;

std::string read_file(const std::string & filepath);

int main(int argc, char* argv[])
{    
    if (argc < 2)
    {
        BrainfuckInterpreter run( // Вы можете изменять этот код:
                "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++"
                ".>+.+++++++..+++.>++.<<+++++++++++++++.>.+++."
                "------.--------.>+.>."
        );
        return 0;
    }

    // Либо вы можете передать путь к файлу с кодом в качестве
    // аргумента командной строки. Например, examples/print_b.bf
    const std::string path_to_source = argv[1];
    const std::string source = read_file(path_to_source);

    BrainfuckInterpreter run(source);
}

std::string read_file(const std::string & filepath)
{
    std::ifstream file(filepath);

    if (!file.is_open())
        throw std::runtime_error(std::format("Couldn't open file {}", filepath));

    return std::string((std::istreambuf_iterator<char>(file)),
                std::istreambuf_iterator<char>());

}
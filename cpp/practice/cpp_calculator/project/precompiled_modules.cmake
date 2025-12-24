# Путь к precompiled модулям включая *.pcm и *.o
set(MOD_DIR "/usr/local/lib")

# Поиск модулей
find_file(STD_MODULE_FILE std.pcm PATHS ${MOD_DIR} REQUIRED)
find_file(STD_COMPAT_MODULE_FILE std.compat.pcm PATHS ${MOD_DIR} REQUIRED)
find_file(UT_MODULE_FILE boost.ut.pcm PATHS ${MOD_DIR} REQUIRED)
find_file(TH_MODULE_FILE TestHelpers.pcm PATHS ${MOD_DIR} REQUIRED)

# Модуль std с объектным файлом
add_library(std_module INTERFACE)
target_sources(std_module INTERFACE ${MOD_DIR}/std.o)
target_compile_options(std_module INTERFACE
    -fmodule-file=std=${STD_MODULE_FILE}
)

# Модуль std.compat с объектным файлом
add_library(std_compat_module INTERFACE)
target_sources(std_compat_module INTERFACE ${MOD_DIR}/std.compat.o)
target_compile_options(std_compat_module INTERFACE
    -fmodule-file=std=${STD_COMPAT_MODULE_FILE}
)
target_link_libraries(std_compat_module INTERFACE std_module)

# Модуль boost.ut с объектным файлом
add_library(boost_ut_module INTERFACE)
target_sources(boost_ut_module INTERFACE ${MOD_DIR}/boost.ut.o)
target_compile_options(boost_ut_module INTERFACE
    -fmodule-file=boost.ut=${UT_MODULE_FILE}
)
target_link_libraries(boost_ut_module INTERFACE std_module)

# Модуль TestHelpers с объектным файлом
add_library(test_helpers_module INTERFACE)
target_sources(test_helpers_module INTERFACE ${MOD_DIR}/TestHelpers.o)
target_compile_options(test_helpers_module INTERFACE
    -fmodule-file=TestHelpers=${TH_MODULE_FILE}
)
target_link_libraries(test_helpers_module INTERFACE std_module)

# Общий интерфейс для тестов
add_library(test_modules INTERFACE)
target_link_libraries(test_modules INTERFACE
    std_module
    boost_ut_module
    test_helpers_module
)

#include <iostream>
#include <string>
#include <utility>
#include <vector>


template<class InputIt, class T>
T my_accumulate(InputIt first, InputIt last, T init)
{
    for (; first != last; ++first) {
        init = std::move(init) + *first; // std::move since C++20
    }
    return init;
}


template<typename T>
class Dummy
{
public:
    Dummy(std::string name, int size=20);
private:
    std::string m_name;
    std::vector<T> m_vec;
    T m_pivot;
};


template<typename T>
Dummy<T>::Dummy(std::string name, int size)
    : m_name(name)
{
    m_vec = (size > 0) ? std::vector<T>(size) : std::vector<T>();
    m_pivot = (size > 0) ? m_vec.back() : T();
}


int main(int argc, char* argv[])
{
    std::vector<int> v_int{1, 2, 3, 4, 5};
    std::vector<Dummy<int>> v_Dummy{ { "dummy1", 20 },
                                     { "dummy2", 25 },
                                     { "dummy3", 10 },
                                     { "dummy4", 30 },
                                     { "dummy5", 20 }};

    std::cout << my_accumulate(v_int.begin(), v_int.end(), 0)
              << std::endl;
}

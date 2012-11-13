#pragma once

#include <cstring>

#include <boost/archive/iterators/xml_escape.hpp>

template<class Base>
class JsonEscape 
    : public boost::archive::iterators::escape<JsonEscape<Base>, Base>
{
    friend class boost::iterator_core_access;

    typedef boost::archive::iterators::escape<JsonEscape<Base>, Base> super_t;

public:
    char fill(const char * & bstart, const char * & bend);
    wchar_t fill(const wchar_t * & bstart, const wchar_t * & bend);

    explicit JsonEscape(Base start) : super_t(start)
    {}
};

template<class Base>
char JsonEscape<Base>::fill(const char*& bstart, const char*& bend) {
#define ESC(match, str, len) \
    case match: \
        bstart = str; \
        bend = bstart + len; \
        break;

    char current_value = *this->base_reference();
    switch(current_value){
    default:
        return current_value;
        ESC('"', "\\\"", 2);
        ESC('\\', "\\\\", 2);
        ESC('\b', "\\b", 2);
        ESC('\f', "\\f", 2);
        ESC('\n', "\\n", 2);
        ESC('\r', "\\r", 2);
        ESC('\t', "\\t", 2);
    }
    return *bstart;
}

template<class Base>
wchar_t JsonEscape<Base>::fill(const wchar_t*& bstart, const wchar_t*& bend) {
    wchar_t current_value = * this->base_reference();
    switch(current_value){
    default:
        return current_value;
        ESC('"', L"\\\"", 2);
        ESC('\\', L"\\\\", 2);
        ESC('\b', L"\\b", 2);
        ESC('\f', L"\\f", 2);
        ESC('\n', L"\\n", 2);
        ESC('\r', L"\\r", 2);
        ESC('\t', L"\\t", 2);
    }
    return *bstart;
#undef ESC
}

template <class Iterator>
boost::archive::iterators::xml_escape<Iterator> xmlEscape(
        Iterator iterator) {
    return boost::archive::iterators::xml_escape<Iterator>(iterator);
}

template <class Iterator>
JsonEscape<Iterator> jsonEscape(Iterator iterator) {
    return JsonEscape<Iterator>(iterator);
}

template <class...>
struct DoFormat;

class FormatContext {
public:
    explicit FormatContext(const char* string) :
        begin_(string), end_(begin_ + strlen(string))
    {}

    FormatContext(const char* string, size_t length) :
        begin_(string), end_(begin_ + length)
    {}

    explicit FormatContext(const std::string& string) :
        begin_(string.c_str()), end_(begin_ + string.size())
    {}

    explicit FormatContext(int n) {
        begin_ = buffer_;
        end_ = buffer_ + snprintf(buffer_, 100, "%d", n);
    }

    explicit FormatContext(double d, int width=6) {
        begin_ = buffer_;
        end_ = buffer_ + snprintf(buffer_, 100, "%.*f", width, d);
    }

    template <class... T>
    explicit FormatContext(const T&... t) {
        begin_ = buffer_;
        end_ = 0;
        DoFormat<T...>()(t..., buffer_, &begin_, &end_);
        if (!end_) {
            end_ = begin_ + strlen(buffer_);
        }
    }

    const char* begin() { return begin_; }
    const char* end() { return end_; }

private:
    const char* begin_;
    const char* end_;
    char buffer_[100];
};

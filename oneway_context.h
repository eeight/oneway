#pragma once

#include <cstring>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <vector>

#include "format.h"

class WrongState : public std::exception {
public:
    explicit WrongState(std::string what) :
            what_(std::move(what))
    {}

    ~WrongState() throw() {}

    const char* what() const throw() {
        return what_.c_str();
    }

private:
    std::string what_;
};

class OnewayContext {
public:
    explicit OnewayContext() :
        state_(0)
    {}

    size_t size() const {
        return buffer_.size();
    }

    const char* data() const {
        return buffer_.data();
    }

    void put(const char* string, size_t size) {
        buffer_.insert(buffer_.end(), string, string + size);
    }

    template <class Iterator>
    void put(Iterator begin, Iterator end) {
        buffer_.insert(buffer_.end(), begin, end);
    }

    int state() const { return state_; }
    void setState(int state) { state_ = state; }

    void feedTo(std::ostream& out) {
        std::copy(
                buffer_.begin(),
                buffer_.end(),
                std::ostreambuf_iterator<char>(out));
    }

    static void wrongState(int stateFrom, int stateTo) {
        std::stringstream s;
        s << "Transition from state " << stateFrom <<
                " to " << stateTo << " is illegal";
        throw WrongState(s.str());
    }

    static void wrongNumber(const char* name, int maxIterations) {
        std::stringstream s;
        s << "Variable " << name <<
                " occured more times than allowed. " <<
                "The number should not have exceeded " << maxIterations;
        throw WrongState(s.str());
    }
private:
    int state_;
    std::vector<char> buffer_;
};

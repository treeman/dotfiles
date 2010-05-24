#pragma once

#include <string>

namespace Tree
{
    class BaseDator {
    public:
        virtual ~BaseDator() { }

        virtual std::string Get() = 0;
        virtual std::string Set( const std::string new_val ) = 0;
    };
}

#pragma once

#include <string>
#include <map>
#include <boost/shared_ptr.hpp>

namespace Tree
{
    class Tweaks;

    //get tweaks from game
    boost::shared_ptr<Tweaks> GetTweaks();

    class Tweaks {
    public:
        Tweaks();

        void Load( std::string path );
        double GetNum( std::string s );
        std::string GetString( std::string s );
    private:
        typedef std::map<std::string, double> DoubleMap;
        DoubleMap doubles;
        typedef std::map<std::string, std::string> StringMap;
        StringMap strings;
    };
}


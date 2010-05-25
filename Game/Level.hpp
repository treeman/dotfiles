#pragma once

#include <vector>
#include <string>

class LevelLoader;

class Level {
public:
    Level();

    void Reset();

    bool IsLast();
    bool IsFirst();

    Level &GetNext();
    Level &GetPrevious();

    std::string GetName() { return name; }
private:
    friend class LevelLoader;

    Level *next;
    Level *prev;

    std::string name;

    typedef std::vector<std::string> Layout;
    Layout layout;
};


#pragma once

#include <vector>
#include <string>

#include "Tree/Vec2.hpp"

class LevelLoader;

class Level {
public:
    Level();

    void Reset();

    bool IsLast();
    bool IsFirst();

    Tree::Vec2i GetGirlPos() { return girl_pos; }

    Level &GetNext();
    Level &GetPrevious();

    std::string GetName() { return name; }
private:
    friend class LevelLoader;

    Level *next;
    Level *prev;

    std::string name;
    std::string message;
    std::string help;

    typedef std::vector<std::string> Layout;
    Layout layout;

    Tree::Vec2i girl_pos;

    int lvl_num;
    float candle_decline;
};


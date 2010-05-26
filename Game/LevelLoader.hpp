#pragma once

#include <vector>
#include <string>

#include "Tree/ErrorHandling.hpp"
#include "LevelResources.hpp"
#include "Level.hpp"
#include "Tile.hpp"

class LevelLoader {
public:
    LevelLoader();

    bool IsThereALevel();
    Level &GetFirstLevel();

    LevelResources CreateResources( Level &lvl );

    void LoadLevelFile( std::string file ) throw( Error::lua_error & );
private:
    //linked list of levels
    Level *lvls;

    //store the allocated levels here
    //just for easier memory management
    typedef std::vector<boost::shared_ptr<Level> > Levels;
    Levels levels;
};


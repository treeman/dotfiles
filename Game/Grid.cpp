#include "Grid.hpp"

Grid::Grid( int x, int box_w, int columns, int y, int box_h, int rows )
{
    Set( x, box_w, columns, y, box_h, rows );
}

void Grid::Set( int _x, int _box_w, int _columns, int _y, int _box_h, int _rows )
{
    x = _x; box_w = _box_w; columns = _columns;
    y = _y; box_h = _box_h; rows = _rows;
}

float Grid::ConvertXToScreen( int x_pos ) const
{
    return x + x_pos * box_w;
}
float Grid::ConvertYToScreen( int y_pos ) const
{
    return y + y_pos * box_h;
}

Tree::Vec2i Grid::ConvertToScreen( GridPos p ) const
{
    return Tree::Vec2i( x + p.x * box_w, y + p.y * box_h );
}
GridPos Grid::ConvertToGrid( Tree::Vec2i p ) const
{
    return GridPos(
        math::clip( (int)(( p.x - x ) / box_w), 0, columns - 1 ),
        math::clip( (int)(( p.y - y ) / box_h), 0, rows - 1 )
    );
}

void Grid::GetBounds( float &x1, float &y1, float &x2, float &y2 ) const
{
    x1 = x; y1 = y;
    x2 = x + columns * box_w;
    y2 = y + rows * box_h;
}

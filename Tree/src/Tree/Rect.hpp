#pragma once

#include "Vec2.hpp"

namespace Tree
{
    class Rect {
    public:
        Rect( Vec2f pos, float width = 0, float height = 0, bool is_centered = false );
        Rect( float x = 0, float y = 0, float width = 0, float height = 0, bool is_centered = false );

        void SetSquare( Vec2f pos, float size, bool is_centered = false );
        void SetSquare( float x, float y, float size, bool is_centered = false );
        void Set( float x, float y, float width, float height, bool is_centered = false );
        void SetAnchors( float x1, float y1, float x2, float y2 );

        bool IsOver( const Vec2f &pos ) const;
        bool IsOver( float x, float y ) const;

        bool Intersects( const Rect &rect ) const;

        float Width() const { return x2 - x1; }
        float Height() const { return y2 - y1; }

        Vec2f GetCenter() const;

        float x1, y1, x2, y2;
    };
}

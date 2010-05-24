#include "Vec2D.hpp"
#include "Box2D/Box2D.h"

Vec2D::Vec2D( const b2Vec2 &v ) { x = v.x; y = v.y; }

b2Vec2 Vec2D::b2Vec() { return b2Vec2( x, y ); }

const Vec2D Vec2D::zero( 0, 0 );
const Vec2D Vec2D::left( -1, 0 );
const Vec2D Vec2D::right( 1, 0 );
const Vec2D Vec2D::up( 0, -1 );
const Vec2D Vec2D::down( 0, 1 );

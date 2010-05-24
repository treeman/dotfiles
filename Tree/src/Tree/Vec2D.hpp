#ifndef VEC2D_HPP_INCLUDED
#define VEC2D_HPP_INCLUDED

#include <cmath>
#include <ostream>

#include "Math.hpp"

class b2Vec2;

class Vec2D {
public:
    Vec2D( const float _x = 0, const float _y = 0 ) : x(_x), y(_y) { }
    Vec2D( const Vec2D &v ) { x = v.x; y = v.y; }
    Vec2D( const b2Vec2 &v );

    Vec2D operator + ( const Vec2D &v ) const { return Vec2D( x + v.x, y + v.y ); }
    Vec2D operator - ( const Vec2D &v ) const { return Vec2D( x - v.x, y - v.y ); }
    Vec2D operator - () const { return Vec2D( -x, -y ); }
    Vec2D operator * ( float t ) const { return Vec2D( t * x, t * y ); }
    Vec2D operator / ( float t ) const { return Vec2D( x / t, y / t ); }

    void operator += ( const Vec2D &v ) { x += v.x; y += v.y; }
    void operator -= ( const Vec2D &v ) { x -= v.x; y -= v.y; }
    void operator *= ( float t ) { x *= t; y *= t; }
    void operator /= ( float t ) { x /= t; y /= t; }

    void operator = ( const Vec2D &v ) { x = v.x; y = v.y; }

    bool operator == ( const Vec2D &v ) const { return x == v.x && y == v.y; }
    bool operator != ( const Vec2D &v ) const { return !(*this == v); }

    float Dot( const Vec2D &v ) const { return x * v.x + y * v.y; }

    float Magnitude() const { return fastmath::sqrt( x * x + y * y ); }
    float MagnitudeSq() const { return x * x + y * y; }

    void SetMagnitude( const float f ) {
        const float m = Magnitude();
        if( m == 0 ) return;
        x = f * x / m;
        y = f * y / m;
    }

    Vec2D Normalize() const {
        const float mag = Magnitude();
        if( mag == 0 ) return *this;
        else return (*this) / mag;
    }

    b2Vec2 b2Vec();

    float x, y;

    //names for common vectors
    static const Vec2D zero;
    static const Vec2D right;
    static const Vec2D left;
    static const Vec2D up;
    static const Vec2D down;
};

inline Vec2D operator * ( const float f, const Vec2D v ) {
    return f * v;
}

inline std::ostream &operator << ( std::ostream &o, const Vec2D &v ) {
    return o << "(" << v.x << "," << v.y << ")";
}

#endif

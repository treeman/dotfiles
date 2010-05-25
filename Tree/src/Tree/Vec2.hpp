#ifndef VEC2_HPP_INCLUDED
#define VEC2_HPP_INCLUDED

#include <cmath>
#include <ostream>

#include <SFML/System/Vector2.hpp>
#include "Box2D/Common/b2Math.h"
#include "Math.hpp"

namespace Tree
{
    template<typename T>
    class Vec2 {
    public:
        Vec2( const T _x = 0, const T _y = 0 ) : x(_x), y(_y) { }
        template<typename S>
        Vec2( const Vec2<S> &v ) { x = v.x; y = v.y; }
        Vec2( const Vec2 &v ) { x = v.x; y = v.y; }
        Vec2( const b2Vec2 &v ) { x = v.x; y = v.y; }
        Vec2( const sf::Vector2<T> &v ) { x = v.x; y = v.y; }

        Vec2 operator + ( const Vec2 &v ) const { return Vec2( x + v.x, y + v.y ); }
        Vec2 operator - ( const Vec2 &v ) const { return Vec2( x - v.x, y - v.y ); }
        Vec2 operator - () const { return Vec2( -x, -y ); }
        Vec2 operator * ( T t ) const { return Vec2( t * x, t * y ); }
        Vec2 operator / ( T t ) const { return Vec2( x / t, y / t ); }

        void operator += ( const Vec2 &v ) { x += v.x; y += v.y; }
        void operator -= ( const Vec2 &v ) { x -= v.x; y -= v.y; }
        void operator *= ( T t ) { x *= t; y *= t; }
        void operator /= ( T t ) { x /= t; y /= t; }

        void operator = ( const Vec2 &v ) { x = v.x; y = v.y; }

        bool operator == ( const Vec2 &v ) const { return x == v.x && y == v.y; }
        bool operator != ( const Vec2 &v ) const { return !(*this == v); }

        float Dot( const Vec2 &v ) const { return x * v.x + y * v.y; }

        float Magnitude() const { return fastmath::sqrt( x * x + y * y ); }
        float MagnitudeSq() const { return x * x + y * y; }

        void SetMagnitude( const T f ) {
            const T m = Magnitude();
            if( m == 0 ) return;
            x = f * x / m;
            y = f * y / m;
        }

        Vec2 Normalize() const {
            const T mag = Magnitude();
            if( mag == 0 ) return *this;
            else return (*this) / mag;
        }

        b2Vec2 b2Vec() const { return b2Vec2( x, y ); }
        sf::Vector2<T> sfVec() const { return sf::Vector2<T>( x, y ); }

        void Set( T _x, T _y ) { x = _x; y = _y; }

        float x, y;

        //names for common vectors
        static const Vec2 zero;
        static const Vec2 right;
        static const Vec2 left;
        static const Vec2 up;
        static const Vec2 down;
    };

    typedef Vec2<float> Vec2f;
    typedef Vec2<int> Vec2i;
}

template<typename T, typename S>
inline Tree::Vec2<T> operator * ( const S f, const Tree::Vec2<T> v ) {
    return f * v;
}

template<typename T>
inline std::ostream &operator << ( std::ostream &o, const Tree::Vec2<T> &v ) {
    return o << "(" << v.x << "," << v.y << ")";
}

//looking lovely today aren't we eh static template declarations?
template<typename T>
const Tree::Vec2<T> Tree::Vec2<T>::zero( 0, 0 );
template<typename T>
const Tree::Vec2<T> Tree::Vec2<T>::left( -1, 0 );
template<typename T>
const Tree::Vec2<T> Tree::Vec2<T>::right( 1, 0 );
template<typename T>
const Tree::Vec2<T> Tree::Vec2<T>::up( 0, -1 );
template<typename T>
const Tree::Vec2<T> Tree::Vec2<T>::down( 0, 1 );

#endif

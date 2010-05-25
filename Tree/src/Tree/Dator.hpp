#pragma once

#include <string>
#include <list>
#include <map>

#include <boost/function.hpp>
#include <boost/lexical_cast.hpp>

#include "BaseDator.hpp"

namespace Tree
{
    class CallDator : public BaseDator {
    public:
        CallDator( boost::function<std::string()> func ) :
            call_func( func ) { }

        std::string Get() { return ""; }
        std::string Set( const std::string ) {
            return call_func();
        }
    private:
        boost::function<std::string()> call_func;
    };

    class SilentDator : public BaseDator {
    public:
        SilentDator( boost::function<void()> func ) :
            call_func( func ) { }

        std::string Get() { return ""; }
        std::string Set( const std::string ) {
            call_func();
            return "";
        }
    private:
        boost::function<void()> call_func;
    };

    template<typename T>
    class Dator : public BaseDator {
    public:
        Dator( T new_val ) :
            val( new_val ), change_func(0)
        { }
        Dator( T new_val, boost::function<std::string( const T )> func ) :
            val( new_val ), change_func( func )
        { }
        virtual ~Dator() { }

        T Val() { return val; }
        std::string Get() {
            return boost::lexical_cast<std::string>( val );
        }
        std::string Set( const std::string new_val )
        {
            try { return Set( boost::lexical_cast<T>( new_val ) ); }
            catch( ... ) { return ""; }
        }
        std::string Set( T new_val )
        {
            val = new_val;
            if( change_func ) { return change_func( val ); }
            else { return ""; }
        }
    protected:
        T val;

        boost::function<std::string( const T )> change_func;
    };

    template<>
    class Dator<std::string> : public BaseDator {
    public:
        Dator( std::string new_val ) :
            val( new_val ), change_func(0)
        { }
        Dator( std::string new_val, boost::function<std::string( const std::string )> func ) :
            val( new_val ), change_func( func )
        { }
        virtual ~Dator() { }

        std::string Val() { return Get(); }
        std::string Get() { return val; }
        std::string Set( const std::string new_val )
        {
            val = new_val;
            if( change_func ) { return change_func( val ); }
            else { return ""; }
        }
    protected:
        std::string val;
        boost::function<std::string( const std::string )> change_func;
    };
}


#pragma once

#include <boost/shared_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/lexical_cast.hpp>

#include <string>
#include <vector>
#include <map>
#include <list>

#include "Tree/Errorhandling.hpp"
#include "Tree/Singleton.hpp"
#include "Tree/BaseDator.hpp"
#include "Tree/Dator.hpp"

namespace Tree
{
    class Settings;

    //get settings from game
    boost::shared_ptr<Settings> GetSettings();

    class SettingsListener {
    public:
        virtual ~SettingsListener() { }
        virtual void HearSetting( std::string setting, std::string value, std::string return_val ) = 0;
    };

    class Settings {
    public:
        Settings();
        virtual ~Settings();

        template<typename T>
        void Register( std::string name, T val );
        void RegisterVariable( std::string name, boost::weak_ptr<BaseDator> dator );
        void Unregister( std::string name );

        template<typename T>
        T GetValue( std::string name ) throw( Error::setting_not_found );
        template<typename T>
        void SetValue( std::string name, T value );

        std::string GetSetting( std::string name );

        std::vector<std::string> GetSettings();
        std::map<std::string, std::string> GetSettingsValues();

        void ParseFile( std::string path ) throw( Error::file_not_found );
        void ParseSetting( std::string str ) throw();

        void AddListener( SettingsListener *listener );
    private:
        typedef std::multimap<std::string, boost::weak_ptr<BaseDator> > DatorMap;
        DatorMap dator_map;

        //these are simply containers which hold a setting forever
        typedef std::vector<boost::shared_ptr<BaseDator> > MyDators;
        MyDators my_dators;

        typedef std::map<std::string, std::string> StringMap;
        StringMap unparsed_settings_map;

        void SetVariable( std::string name, std::string value );

        typedef std::list<SettingsListener*> ListenerList;
        ListenerList listener_list;

        void UpdateListeners( std::string setting, std::string value, std::string return_val );
    };
}

template<typename T>
void Tree::Settings::Register( std::string name, T val )
{
    boost::shared_ptr<BaseDator> dator( new Dator<T>( val ) );
    my_dators.push_back( dator );

    RegisterVariable( name, dator );
}

template<typename T>
T Tree::Settings::GetValue( std::string name ) throw( Error::setting_not_found )
{
    DatorMap::iterator it = dator_map.find( name );
    if( it != dator_map.end() ) {
        boost::shared_ptr<BaseDator> other_dator = it->second.lock();
        if( other_dator ) {
            return boost::lexical_cast<T>( other_dator->Get() );
        }
        else {
            dator_map.erase( it );
        }
    }
    std::string s = "Setting '" + name + "' not found";
    throw( Error::setting_not_found( s.c_str() ) );
}

template<typename T>
void Tree::Settings::SetValue( std::string name, T value )
{
    try {
        SetVariable( name, boost::lexical_cast<std::string>( value ) );
    }
    catch( boost::bad_lexical_cast &e ) { }
}


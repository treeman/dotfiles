#include <boost/algorithm/string/trim.hpp>
#include <fstream>

#include "Tree/Settings.hpp"
#include "Tree/Dator.hpp"
#include "Tree/Game.hpp"

using Tree::Settings;

boost::shared_ptr<Settings> Tree::GetSettings()
{
    return GAME->GetSettings();
}

Settings::Settings()
{

}
Settings::~Settings()
{

}
void Settings::RegisterVariable( std::string name, boost::weak_ptr<BaseDator> dator )
{
    boost::shared_ptr<BaseDator> real_dator = dator.lock();

    StringMap::iterator it = unparsed_settings_map.find( name );
    if( it != unparsed_settings_map.end() && real_dator )
    {
        std::string ret = real_dator->Set( it->second );
        UpdateListeners( name, it->second, ret );
        unparsed_settings_map.erase( it );
    }
    else {
        DatorMap::iterator it = dator_map.find( name );
        if( it != dator_map.end() && real_dator )
        {
            boost::shared_ptr<BaseDator> other_dator = it->second.lock();
            if( other_dator ) {
                std::string ret = real_dator->Set( other_dator->Get() );
                UpdateListeners( name, real_dator->Get(), ret );
            }
            else {
                dator_map.erase( it );
            }
        }
    }

    if( !dator.expired() ) {
        dator_map.insert( std::make_pair( name, dator ) );
    }
}

void Settings::RegisterPermVariable( std::string name, boost::shared_ptr<BaseDator> dator )
{
    my_dators.push_back( dator );

    RegisterVariable( name, dator );
}

void Settings::Unregister( std::string name )
{
    dator_map.erase( name );
}

std::string Settings::GetSetting( std::string name )
{
    DatorMap::iterator it = dator_map.find( name );
    if( it != dator_map.end() ) {
        boost::shared_ptr<BaseDator> other_dator = it->second.lock();
        if( other_dator ) {
            return other_dator->Get();
        }
        else {
            dator_map.erase( it );
        }
    }
    return "";
}

std::vector<std::string> Settings::GetSettings()
{
    std::vector<std::string> l;
    for( DatorMap::iterator it = dator_map.begin(); it != dator_map.end(); ++it ) {
        l.insert( l.begin(), it->first );
    }
    std::sort( l.begin(), l.end() );
    std::unique( l.begin(), l.end() );
    return l;
}
std::map<std::string, std::string> Settings::GetSettingsValues()
{
    std::map<std::string, std::string> m;
    for( DatorMap::iterator it = dator_map.begin(); it != dator_map.end(); ++it ) {
        boost::shared_ptr<BaseDator> other_dator = it->second.lock();
        if( other_dator ) {
            m.insert( std::make_pair( it->first, other_dator->Get() ) );
        }
        else {
            dator_map.erase( it );
        }
    }
    return m;
}

void Settings::ParseFile( std::string file ) throw( Error::file_not_found )
{
    std::ifstream in( file.c_str() );

    if( !in.is_open() ) {
        std::string s = "Error opening: " + file;
        throw( Error::file_not_found( s.c_str() ) );
    }

    while( !in.eof() )
    {
        std::string str;
        std::getline( in, str );
        if( str.size() > 0 ) {

            if( str.find( ';' ) != 0 ) {
                ParseSetting( str );
            }
        }
    }
}

void Settings::ParseSetting( std::string str ) throw()
{
    if( !str.size() ) {
        return; //"Parsing an empty setting? Pff...";
    }

    std::string name, value;
    int pos = str.find( ' ' );

    if( pos != 0 )
    {
        name = str.substr( 0, pos );
        value = str.substr( pos + 1 );
    }
    else {
        return; // "Setting malformed: bastard!";
    }

    boost::trim( name ); boost::trim( value );
    SetVariable( name, value );
}

void Settings::SetVariable( std::string name, std::string value )
{
    typedef std::pair<DatorMap::iterator, DatorMap::iterator> ItPair;
    ItPair ret = dator_map.equal_range( name );

    if( ret.first == ret.second ) {
        //no setting found
        StringMap::iterator it = unparsed_settings_map.find( name );
        if( it != unparsed_settings_map.end() ) {
            it->second = value;
        }
        else {
            unparsed_settings_map.insert( std::make_pair( name, value ) );
        }
    }
    else {

        for( DatorMap::iterator it = ret.first; it != ret.second; ++it )
        {
            boost::shared_ptr<BaseDator> other_dator = it->second.lock();
            if( other_dator ) {
                std::string ret = other_dator->Set( value );
                UpdateListeners( name, value, ret );
            }
            else {
                dator_map.erase( it );
            }
        }
    }
}

void Settings::AddListener( SettingsListener *listener )
{
    listener_list.insert( listener_list.end(), listener );
}

void Settings::UpdateListeners( std::string setting, std::string value, std::string return_val )
{
    for( ListenerList::iterator it = listener_list.begin(); it != listener_list.end(); ++it )
    {
        (*it)->HearSetting( setting, value, return_val );
    }
}


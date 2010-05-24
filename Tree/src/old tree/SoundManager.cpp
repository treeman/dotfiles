#include "Tree/SoundManager.hpp"
#include "Tree/Settings.hpp"

using Tree::SoundManager;

SoundManager::SoundManager()
{
    music_vol.reset( new Dator<int>( 50, boost::bind( &SoundManager::SetMusicVolume, this, _1 ) ) );
    effect_vol.reset( new Dator<int>( 50, boost::bind( &SoundManager::SetEffectVolume, this, _1 ) ) );
    use_sound.reset( new Dator<bool>( true, boost::bind( &SoundManager::SetUseSound, this, _1 ) ) );

    SETTINGS->RegisterVariable( "music_volume", boost::weak_ptr<BaseDator>( music_vol )) ;
    SETTINGS->RegisterVariable( "effect_volume", boost::weak_ptr<BaseDator>( effect_vol ) );
    SETTINGS->RegisterVariable( "sound_enabled", boost::weak_ptr<BaseDator>( use_sound ) );
}
SoundManager::~SoundManager()
{

}

std::string SoundManager::SetUseSound( bool predicate )
{
    hge->System_SetState( HGE_USESOUND, predicate );
    if( predicate ) {
        return "waah - I can hear the sound now";
    }
    else {
        return "huh? Did you say something? I'm deaf";
    }
}

std::string SoundManager::SetMusicVolume( int val )
{
    hge->System_SetState( HGE_MUSVOLUME, val );
    hge->System_SetState( HGE_STREAMVOLUME, val );
    return "";
}
std::string SoundManager::SetEffectVolume( int val )
{
    hge->System_SetState( HGE_FXVOLUME, val );
    return "";
}

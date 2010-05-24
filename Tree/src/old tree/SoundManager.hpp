#pragma once

#include <boost/shared_ptr.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>

#include <string>

#include "Hge/Hge.hpp"
#include "Dator.hpp"

namespace Tree
{
    class SoundManager {
    public:
        SoundManager();
        ~SoundManager();

        std::string SetUseSound( bool predicate );
        std::string SetMusicVolume( int val );
        std::string SetEffectVolume( int val );
    private:
        HgeObj hge;

        boost::shared_ptr<Dator<int> > music_vol;
        boost::shared_ptr<Dator<int> > effect_vol;
        boost::shared_ptr<Dator<bool> > use_sound;
    };
}

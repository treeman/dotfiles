#ifndef TRACKS_HPP_INCLUDED
#define TRACKS_HPP_INCLUDED

#include <string>
#include <vector>

#include <boost/shared_ptr.hpp>

#include "Hge/Hge.hpp"

struct SongInfo {
    SongInfo( std::string n, HSTREAM s ) : name(n), stream(s)
    { }
    std::string name;
    HSTREAM stream;
};

struct Song {
    Song( HCHANNEL c, SongInfo i ) : channel(c), info(i)
    { }
    HCHANNEL channel;
    SongInfo info;
};

class Tracks {
public:
    Tracks();
    ~Tracks();

    void Add( std::string path, std::string name );

    void Next();
    void Prev();
    void Play();
    void Stop();

    bool IsPlaying();

    std::string Name();

    void Update( float );
private:
    typedef std::vector<SongInfo> SongInfos;
    SongInfos songinfos;
    typedef std::vector<SongInfo>::iterator SongInfoIt;
    SongInfoIt songinfo_it;

    boost::shared_ptr<Song> curr_song;

    HgeObj hge;
};

#endif

#include "Tracks.hpp"

Tracks::Tracks()
{
    songinfo_it = songinfos.begin();
}

Tracks::~Tracks()
{
    for( SongInfos::iterator it = songinfos.begin(); it != songinfos.end(); ++it )
    {
        hge->Stream_Free( it->stream );
    }
}

void Tracks::Add( std::string path, std::string name )
{
    songinfos.push_back( SongInfo( name,
        hge->Stream_Load( path.c_str() ) ) );
}

void Tracks::Next()
{
    Stop();

    ++songinfo_it;
    if( songinfo_it == songinfos.end() ) {
        songinfo_it = songinfos.begin();
    }
    Play();
}
void Tracks::Prev()
{
    Stop();

    if( songinfo_it == songinfos.begin() ) {
        for( SongInfos::iterator it = songinfos.begin(); it != songinfos.end(); ++it ) {
            songinfo_it = it;
        }
    }
    else {
        --songinfo_it;
    }
    Play();
}
void Tracks::Play()
{
    if( IsPlaying() ) return;

    songinfo_it = songinfos.begin();

    curr_song.reset( new Song(
        hge->Stream_Play( songinfo_it->stream, false, hge->System_GetState( HGE_STREAMVOLUME ) ),
        *songinfo_it
    ) );
}
void Tracks::Stop()
{
    if( !IsPlaying() ) return;

    hge->Channel_Stop( curr_song->channel );
    curr_song.reset();
}

std::string Tracks::Name()
{
    return songinfo_it->name;
}

bool Tracks::IsPlaying()
{
    return curr_song && hge->Channel_IsPlaying( curr_song->channel );
}

void Tracks::Update( float )
{
    if( curr_song && ! hge->Channel_IsPlaying( curr_song->channel ) )
    {
        Next();
    }
}

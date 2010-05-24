#ifndef TIMER_HPP_INCLUDED
#define TIMER_HPP_INCLUDED

#include <SFML/System.hpp>

namespace Tree {

    class Timer {
    public:
        Timer();
        virtual ~Timer();

        void Start();
        void Pause();
        void Stop();
        void Restart();
        void Reset();

        float GetTime() const;
        void SetTime( float time );

        void SetSpeed( float multiplier );
        float GetSpeed() const;

        bool IsStarted() const;
        bool IsPaused() const;
    protected:
        virtual float GetTimeStep() const;
        virtual void UpdateTimeAcc();

        float start_tick;
        float pause_tick;
        float time;

        float speed;

        bool is_started;
        bool is_paused;

        sf::Clock clock;
    };

}

#endif


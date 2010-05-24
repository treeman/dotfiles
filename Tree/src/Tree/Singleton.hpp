#pragma once

namespace Tree
{
    template<class T>
    class Singleton {
    public:
        static T *Instance()
        {
            if( !instance ) instance = new T;
            return instance;
        }
        static void Destroy()
        {
            delete instance;
            instance = 0;
        }

    protected:
        Singleton() { }
        virtual ~Singleton() { }
    private:
        Singleton( const Singleton &s );
        Singleton &operator = ( const Singleton &s );

        static T *instance;
    };
}

template <class T>
T *Tree::Singleton<T>::instance = 0;

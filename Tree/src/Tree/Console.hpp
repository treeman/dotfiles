#ifndef CONSOLE_HPP_INCLUDED
#define CONSOLE_HPP_INCLUDED

#include <boost/shared_ptr.hpp>
#include <string>
#include <vector>

#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/ref.hpp>

#include "Tree/Graphics.hpp"
#include "Tree/InputHandler.hpp"
#include "Tree/Timer.hpp"
#include "Tree/Dator.hpp"
#include "Tree/Settings.hpp"

namespace Tree
{
    class Console : public InputHandler, public SettingsListener {
    public:
        Console();
        ~Console();

        bool HandleEvent( sf::Event &e );

        void HearSetting( std::string setting, std::string value, std::string return_val );

        void AddHistory( std::string str );

        std::string Clear();
        std::string ShowCommands();
        std::string ShowCommandsValues();

        void Update( float dt );
        void Render();

        std::vector<std::string> GetHistory() const;
    private:
        void Activate();
        void Deactivate();

        typedef std::vector<std::string> StrList;
        typedef std::map<std::string, std::string> StrMap;

        float GetStringWidth( std::string str );
        sf::String render_str;

        float x, y;
        float w, h;

        sf::Shape back;

        bool IsHistoryLocked();

        void MoveBackInHistory();
        void MoveForwardInHistory();

        void ResetHistoryPos();

        void PushCmd( std::string str );
        StrList cmd_history;
        int cmd_pos;

        void PushHistory( std::string str );
        StrList big_history;

        bool IsSuggestionLocked();

        void MoveUpInSuggestion();
        void MoveDownInSuggestion();

        void AutoCompleteSuggestion();

        void UpdateSuggestionList();
        void ResetSuggestionPos();

        StrMap suggestion_map;
        int sugg_pos;
        float suggestion_box_width;

        void InputLineSet( std::string str, bool update_suggestion = true );
        void InputLineOnInput();

        void InputLineSetBuff();
        void InputLineRestoreBuff();

        void InputLineInsert( std::string str, int pos );

        void InputLineDeleteChar( int pos );
        void InputLineAddChar( char ch, int pos );

        void InputLineExecute();
        void InputLineClear();

        void InputLineMoveLeft();
        void InputLineMoveRight();

        std::string input_line_buff;
        std::string input_line;
        int input_line_pos;

        bool SelectionIsActive();
        void SelectionAll();
        void SelectionMoveLeft();
        void SelectionMoveRight();
        void SelectionClear();
        void SelectionDelete();

        void SelectionCopy();
        void SelectionPaste( int pos );

        int sel_start, sel_length;
        std::string selection_copy;

        void RenderBones();
        void RenderHistory();
        void RenderInputLine();
        void RenderTypeSuggestions();

        float text_off_left;
        float text_off_down;
        float text_off_top;

        float line_height;

        Timer blink_timer;
        float blinkie_time;

        bool is_active;

        int opacity;
        int fnt_opacity;
        int blinkie_opacity;
        int selection_opacity;
        int delimiter_opacity;

        int some_stuff;

        sf::Color fnt_color;
        sf::Color fnt_history_color;
        sf::Color fnt_history_line_sign_color;
        sf::Color fnt_highlight_color;
        sf::Color fnt_suggestion_color;
        sf::Color blinkie_color;

        sf::Color large_back_color;
        sf::Color suggestion_back_color;
        sf::Color selection_color;
        sf::Color delimiter_color;

        void RenderDebug();

        boost::shared_ptr<Dator<bool> > showDebug;
        boost::shared_ptr<CallDator> clearHistory;
        boost::shared_ptr<CallDator> showCommands;
        boost::shared_ptr<CallDator> showCommandsValues;
    };
}

#endif


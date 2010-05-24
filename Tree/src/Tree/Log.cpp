#include "Log.hpp"

#include <boost/logging/format.hpp>

BOOST_DEFINE_LOG_FILTER( g_l_filter, filter_type )
BOOST_DEFINE_LOG( g_l, logger_type )

void Tree::init_logs()
{
    g_l()->writer().add_formatter( bl::formatter::time("$hh:$mm.$ss ") );
    g_l()->writer().add_formatter( bl::formatter::idx(), "[%] " );
    g_l()->writer().add_formatter( bl::formatter::append_newline() );

    g_l()->writer().add_destination( bl::destination::file( "tree_log.txt" ) );

    L_ << "log file initiated\n";
}


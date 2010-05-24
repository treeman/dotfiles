#pragma once

#include <boost/logging/format_fwd.hpp>
#include <boost/logging/logging.hpp>
#include <boost/logging/format/optimize.hpp>

#include <string>

namespace bl = boost::logging;
typedef bl::logger_format_write< > logger_type;
typedef bl::filter::no_ts filter_type;

BOOST_DECLARE_LOG_FILTER( g_l_filter, filter_type )
BOOST_DECLARE_LOG( g_l, logger_type )

#define L_ BOOST_LOG_USE_LOG_IF_FILTER(g_l(), g_l_filter()->is_enabled() )

namespace Tree
{
    void init_logs();
}


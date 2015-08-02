dofile("/home/tree/.conky/colors.lua")

function conky_hacker_top (num)
    site = conky_parse("${curl http://news.ycombinator.net/ 5}")
    time = os.date("*t")
    if time.min < 10 then time.min = "0"..time.min end
    if time.sec < 10 then time.sec = "0"..time.sec end

    update = time.hour .. ":" .. time.min

    if site ~= "" then
        conky_set_update_interval(5*60)
        ret_str = l_color().."news.ycombinator.net at " .. update .. "\n"
        for n = 1, num do
            if n > 1 then ret_str = ret_str .. "\n" end
            match = string.match(site, n .. "%.(.-)<tr style=\"height:5px\"></tr>")
            info = extract_details(match)
            format = format_details(info)
            if format ~= "" then ret_str = ret_str .. format
            else ret_str = string.sub(ret_str,0,string.len(ret_str)-1) end
        end
        return ret_str
    else
        conky_set_update_interval(1)
        return "no site at " .. update
    end
end

function extract_details (submission)
    --escape for conky
    submission = string.gsub(submission, "$", "\\$")
    submission = string.gsub(submission, '"', '\"')
    submission = string.gsub(submission, "#", "\\#")
    d = {}
    head, d.title = string.match(submission, "(<td class=\"title\"><a href=\".-\">(.-)</a>.-</td>)")
    if nil == head then d.domain = ""
    else d.domain = string.match(head, "<span class=\"comhead\">(.-)</span>") end
    if d.domain == nil then d.domain = "" end
    subtext = string.match(submission, "td colspan=2></td><td class=\"subtext\">(.-)</td>")
    d.score = string.match(subtext, "<span id=score_%d->(.-)</span>")
    d.user, d.time, d.comments = string.match(subtext, "<a href=\"user%?id=.-\">(.-)</a>%s(.-)%s|%s<a href=\".-\">(.-)</a>")
    return d
end

function format_details (info)
    if info.title == nil then return "" end

    str = h_color() .. info.title .. l_color()
    .. info.domain .. "\n"
    .. info.score .. " by " .. info.user .. " " .. info.time
    .. " " .. info.comments
    return str
end


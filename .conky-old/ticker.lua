dofile("/home/tree/.conky/colors.lua")
dofile("/home/tree/.conky/vods.lua")
dofile("/home/tree/.conky/manga.lua")

function conky_ticker(max)
    max = tonumber(max)

    vods = get_vods_info()
    manga = get_manga_info()

    infos = {}
    for k, i in pairs(vods) do table.insert(infos, i) end
    for k, i in pairs(manga) do table.insert(infos, i) end

    table.sort(infos, function(a,b) return a.time > b.time end)

    curr_time = ""
    str = ""
    n = 1
    for k, info in pairs(infos) do
        if max ~= nil and n > max then break
        else n = n + 1 end

        if info.time ~= curr_time or curr_time == "" then
            curr_time = info.time
            str = str..format_time(curr_time).."\n"
        end
        str = str..l_color()..info.str.."\n"
    end
    str = string.sub(str,0,string.len(str)-1)

    if str == "" then str = "nothing ticking" end
    return str
end

function format_time(time)
    date = os.date("*t", time)
    return h_color()..months[date.month].." "..date.day..", "..date.year
end

months = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
    "Sep", "Okt", "Nov", "Dec", }

rev_months = {}
for i,v in ipairs(months) do
    rev_months[v] = i
end

dofile("/home/tree/.conky/colors.lua")

blip_tvs = {
    "http://day9tv.blip.tv/",
}

function conky_temp()
    big_site = conky_parse("${curl "..blip_tvs[1].."posts?view=archive&nsfw=dc&skin=json}")
    if big_site ~= "" then
        info = extract_blip_details(big_site)
        str = ""
        for v, item in pairs(info.items) do
            str = str .. format_details(item) .. "\n"
        end
        return str
    else
        return "bah"
    end
end

function extract_blip_details(site)
    d = {}
    d.items = {}
    for item in string.gmatch(site, "{ \"Post\" : {(.-%b{}.-)}}") do

        i = {}
        i.title = string.match(item, "\"title\":\"(.-)\"")

        date = {}
        date.year, date.month, date.day = string.match(item,
            "\"datestampDate\":\"(%d%d%d%d)%-(%d%d)%-(%d%d)\"")
        i.time = os.time(date)
        i.name = string.match(item, "\"showName\":\"(.-)\"")
        i.domain = "blip.tv"
        i.url = string.match(item, "\"url\":\"(.-)\"")

        table.insert(d.items, i)
    end

    return d
end

function format_details(info)
    str = l_color()..info.name.." "..info.title.." "..format_time(info.time)
    str = string.gsub(str, "#", "\\#")
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


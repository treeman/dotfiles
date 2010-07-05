blip_tvs = {
    "http://day9tv.blip.tv/",
}

function get_vods_info()
    big_site = conky_parse("${curl "..blip_tvs[1].."posts?view=archive&nsfw=dc&skin=json}")
    if big_site ~= "" then
        infos = {}
        details = extract_blip_details(big_site,1)
        for k, item in pairs(details.items) do
            i = {}
            i.time = item.time
            i.str = format_details(item)
            table.insert(infos, i)
        end
        return infos
    else
        return {}
    end
end

function extract_blip_details(site,num)
    d = {}
    d.items = {}
    n = 1
    for item in string.gmatch(site, "{ \"Post\" : {(.-%b{}.-)}}") do
        if num ~= nil and n > num then return d
        else n = n + 1 end

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

function format_details(details)
    str = details.name.." "..details.title
    str = string.gsub(str, "#", "\\#")
    return str
end


local active = false

function toggle_chapters()
	if active then
		mp.command("script-message osc-chapterlist 0") -- hides it
		active = false
	else
		-- Shows the list with a massive duration (9999 seconds) so it stays open
		mp.command("script-message osc-chapterlist 9999")
		active = true
	end
end

mp.add_key_binding("C", "toggle-chapters", toggle_chapters)

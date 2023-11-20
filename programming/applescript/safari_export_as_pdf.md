
## Safari で開いているページを PDF として書き出すスクリプト

````applescript
-- generate file name as yyyy_mm_d_time.pdf
set y to year of (current date) as string
set m to month of (current date) as number
set d to day of (current date) as string
set t to time of (current date) as string
set n to y & "_" & m & "_" & d & "_" & t & ".pdf"

-- make the window as the front window
tell application "Safari" to activate

tell application "System Events"
  tell process "Safari"

    -- click `Export as PDF...` menu
    click menu item "PDFとして書き出す…" of menu "ファイル" of menu bar 1

    -- wait until the sheet gets opened
    repeat until exists sheet 1 of window 1
    end repeat

    -- set file name
    set value of text field 1 of sheet 1 of window 1 to n

    -- click `Save` button
    click button "保存" of sheet 1 of window 1
  end tell
end tell
````

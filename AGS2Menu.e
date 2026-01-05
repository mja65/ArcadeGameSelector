/* Arcade Game Selection version 2. */


OPT PREPROCESS


MODULE 'dos/dos'
MODULE 'exec/memory'
MODULE 'intuition/intuition'
MODULE 'intuition/screens'
MODULE 'graphics/modeid'
MODULE 'graphics/gfx'
MODULE 'graphics/rastport'
MODULE 'graphics/text'
MODULE 'graphics/view'
MODULE 'graphics/videocontrol'
MODULE 'lowlevel'
MODULE 'libraries/lowlevel'
MODULE '*ilbmloader'
MODULE '*agsil'
MODULE '*agsnav'
MODULE '*agsconf'
MODULE '*agsdefs'
MODULE '*agsmenupos'
MODULE '*palfade'


ENUM ERR_KICKSTART = 1,
     ERR_JOYSTICK,
     ERR_SCREEN,
     ERR_WINDOW,
     ERR_FONT,
     ERR_BACKGROUND,
     ERR_EMPTY,
     ERR_COPY_SRC,
     ERR_COPY_DST,
     ERR_COPY_WRITE


OBJECT ags
    conf:PTR TO agsconf
    nav:PTR TO agsnav
    loader:PTR TO agsil_master
    rport:PTR TO rastport
    font:PTR TO textfont
    menu_rastport:PTR TO rastport
    menu_bitmap:PTR TO bitmap
    menu_bm_width:INT
    menu_bm_height:INT
    current_item:INT
    height:INT
    width:INT
    offset:INT
    char_width:INT
ENDOBJECT

PROC init(conf, nav, loader, rport, font) OF ags
    self.conf := conf
    self.nav := nav
    self.loader := loader
    self.rport := rport
    self.font := font
    ->self.height := 0
    self.width := 26
    ->self.offset := 0
    self.char_width := TextLength(rport, 'A', 1)
ENDPROC

PROC end() OF ags
    self.discard_menu_bitmap()
ENDPROC

CONST COPY_BUF_SIZE = 128

PROC copy_file(src_path:PTR TO CHAR, dst_path:PTR TO CHAR) HANDLE
    DEF len
    DEF src_fh = NIL
    DEF dst_fh = NIL
    DEF buf[COPY_BUF_SIZE]:ARRAY OF CHAR

    IF (src_fh := Open(src_path, MODE_OLDFILE)) = NIL THEN Raise(ERR_COPY_SRC)
    IF (dst_fh := Open(dst_path, MODE_NEWFILE)) = NIL THEN Raise(ERR_COPY_DST)
    WHILE (len := Read(src_fh, buf, COPY_BUF_SIZE)) > 0
        IF Write(dst_fh, buf, len) <> len THEN Raise(ERR_COPY_WRITE)
    ENDWHILE
EXCEPT DO
    IF src_fh THEN Close(src_fh)
    IF dst_fh THEN Close(dst_fh)
    IF exception
        PrintF('Copying \s to \s: ')
    ENDIF
    ReThrow()
ENDPROC


CONST REPEAT_DELAY = 8
CONST RAWKEY_Q      = 16
CONST RAWKEY_ESC    = 69
CONST RAWKEY_UP     = 76
CONST RAWKEY_DOWN   = 77
CONST RAWKEY_RIGHT  = 78
CONST RAWKEY_LEFT   = 79
CONST RAWKEY_RETURN = 68
CONST RAWKEY_HOME   = 70

PROC select(init_offset, init_pos) OF ags
    DEF key
    DEF rawkey
    DEF quit = FALSE
    DEF portstate
    -> Counters to delay repeat and screenshot loading.
    DEF up_ctr = 0
    DEF down_ctr = 0
    DEF right_ctr = 0
    DEF left_ctr = 0
    DEF screenshot_ctr = 0

    DEF item:PTR TO agsnav_item
    DEF index
    DEF path[255]:STRING
    DEF prev_item[255]:STRING
    DEF pos, len
    DEF should_redraw = 0
    DEF menu_pos = NIL:PTR TO agsmenupos

    self.reload(init_offset, init_pos)

    IF (portstate := ReadJoyPort(1)) = JP_TYPE_NOTAVAIL THEN Raise(ERR_JOYSTICK)

    REPEAT
        WaitTOF()

        portstate := ReadJoyPort(1)

        key := GetKey()
        rawkey := key AND $ffff

        IF ((rawkey = RAWKEY_Q) AND (key AND LLKB_RAMIGA)) OR (rawkey = RAWKEY_ESC)
            quit := TRUE
        ENDIF
        IF (portstate AND JP_TYPE_MASK) = JP_TYPE_GAMECTLR
            IF portstate AND JPF_BUTTON_BLUE
                IF self.conf.blue_button_action = AGSCONF_ACTION_QUIT
                    quit := TRUE
                ENDIF
            ENDIF
        ENDIF

      -> Home

        IF rawkey = 70
            index := 0
            item := self.nav.items[index]
            IF item.type = AGSNAV_TYPE_DIR
                IF self.nav.depth AND (index = 0) AND (StrCmp(self.nav.path, 'AGS:Menu/') = FALSE)
                    StrCopy(path, self.nav.path)
                    pos := EstrLen(path) - 1
                    len := 0
                    REPEAT
                        DEC pos
                        INC len
                    UNTIL (path[pos] = "/") OR (path[pos] = ":")
                    INC pos
                    RightStr(prev_item, path, len)
                    SetStr(prev_item, len - 5)
                    path[pos] := 0
                    SetStr(path, pos)
                        self.current_item := 0
                        should_redraw := 1
                    self.nav.set_path(path)
                    IF self.nav.depth > 0 THEN self.nav.depth := self.nav.depth - 1
                    self.reload(0, 0, prev_item)
                    screenshot_ctr := 0
                ELSEIF self.nav.depth < 6
                    StrCopy(path, self.nav.path)
                    StrAdd(path, item.name)
                    StrAdd(path, '.ags/')
                    self.nav.set_path(path)
                    self.nav.depth := self.nav.depth + 1
                    self.reload()
                    screenshot_ctr := 0
                ENDIF
            ENDIF
            REPEAT
                rawkey := GetKey() AND $ffff
            UNTIL rawkey <> 70
        ENDIF       

        -> Up / Down
        IF (portstate AND JPF_JOY_UP) OR (rawkey = RAWKEY_UP)
            IF (up_ctr = 0) OR (up_ctr > REPEAT_DELAY)
                IF self.current_item > 0
                    self.current_item := self.current_item - 1
                    self.redraw(self.current_item, self.current_item + 1)
                    screenshot_ctr := 0
                ELSEIF self.offset > 0
                    self.offset := self.offset - 1
                    self.redraw()
                    screenshot_ctr := 0
                ENDIF
            ENDIF
            INC up_ctr
        ELSE
            up_ctr := 0
        ENDIF

        IF (portstate AND JPF_JOY_DOWN) OR (rawkey = RAWKEY_DOWN)
            IF (down_ctr = 0) OR (down_ctr > REPEAT_DELAY)
                IF self.current_item < (self.height - 1)
                    self.current_item := self.current_item + 1
                    self.redraw(self.current_item - 1, self.current_item)
                    screenshot_ctr := 0
                ELSEIF (self.current_item + self.offset) < (self.nav.num_items - 1)
                    self.offset := self.offset + 1
                    self.redraw()
                    screenshot_ctr := 0
                ENDIF
            ENDIF
            INC down_ctr
        ELSE
            down_ctr := 0
        ENDIF

        -> Page Up / Down
        IF (portstate AND JPF_JOY_RIGHT) OR (rawkey = RAWKEY_RIGHT)
            IF (right_ctr = 0) OR (right_ctr > REPEAT_DELAY)
                IF (self.nav.num_items < self.height)
                    IF (self.current_item < self.height)
                        self.current_item := self.nav.num_items
                        should_redraw := 1
                    ENDIF
                ELSE
                    IF (self.current_item < (self.height - 1))
                        self.current_item := self.height - 1
                        should_redraw := 1
                    ELSE
                        IF (self.offset < (self.nav.num_items - self.height))
                            self.offset := Min(self.nav.num_items - self.height, self.offset + self.height - 1)
                            should_redraw := 1
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            INC right_ctr
        ELSE
            right_ctr := 0
        ENDIF

        IF (portstate AND JPF_JOY_LEFT) OR (rawkey = RAWKEY_LEFT)
            IF (left_ctr = 0) OR (left_ctr > REPEAT_DELAY)
                IF (self.nav.num_items < self.height)
                    IF (self.current_item > 0)
                        self.current_item := 0
                        should_redraw := 1
                    ENDIF
                ELSE
                    IF (self.current_item > 0)
                        self.current_item := 0
                        should_redraw := 1
                    ELSE
                        IF (self.offset > 0)
                            self.offset := Max(0, self.offset - self.height + 1)
                            should_redraw := 1
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            INC left_ctr
        ELSE
            left_ctr := 0
        ENDIF

        IF (should_redraw <> 0)
            self.redraw()
            screenshot_ctr := 0
            should_redraw := 0
        ENDIF

        IF (portstate AND JPF_BUTTON_RED) OR (rawkey = RAWKEY_RETURN)
            index := self.current_item + self.offset
            item := self.nav.items[index]
            IF item.type = AGSNAV_TYPE_DIR
                IF self.nav.depth AND (index = 0) AND (StrCmp(self.nav.path, 'AGS:Menu/') = FALSE) -> Go to parent dir.
                    StrCopy(path, self.nav.path)
                    pos := EstrLen(path) - 1
                    len := 0
                    REPEAT
                        DEC pos
                        INC len
                    UNTIL (path[pos] = "/") OR (path[pos] = ":")
                    INC pos
                    RightStr(prev_item, path, len)
                    SetStr(prev_item, len - 5)
                    path[pos] := 0
                    SetStr(path, pos)
                    self.nav.set_path(path)
                    self.nav.depth := self.nav.depth - 1
                    self.reload(0, 0, prev_item)
                    screenshot_ctr := 0
                ELSEIF self.nav.depth < 6
                    StrCopy(path, self.nav.path)
                    StrAdd(path, item.name)
                    StrAdd(path, '.ags/')
                    self.nav.set_path(path)
                    self.nav.depth := self.nav.depth + 1
                    self.reload()
                    screenshot_ctr := 0
                ENDIF
            ELSE
                -> Serialize navigation state and copy run script
                NEW menu_pos.init()
                menu_pos.write(self.nav.path, self.nav.depth, self.offset, self.current_item)
                END menu_pos

                StrCopy(path, self.nav.path)
                StrAdd(path, item.name)
                StrAdd(path, '.run')
                copy_file(path, AGS_RUN_PATH)
                screenshot_ctr := (REPEAT_DELAY + 2) -> Avoid loading now.
                quit := TRUE
            ENDIF
            REPEAT
                portstate := ReadJoyPort(1)
                rawkey := GetKey() AND $ffff
            UNTIL ((portstate AND JPF_BUTTON_RED) = 0) AND (rawkey <> RAWKEY_RETURN)
        ENDIF

        IF screenshot_ctr++ = (REPEAT_DELAY + 1)
            self.load_screenshot()
            self.load_text()
            IF self.conf.text2_enabled
                self.load_text2()
            ENDIF
        ENDIF

    UNTIL quit

ENDPROC

PROC reload(offset = 0, pos = 0, select_item = NIL) OF ags
    DEF line
    DEF item:PTR TO agsnav_item

    IF self.nav.read_dir() = 0 THEN Raise(ERR_EMPTY)
    self.height := Min(self.nav.num_items, self.conf.menu_height)

    IF select_item
        FOR line := 0 TO self.nav.num_items - 1
            item := self.nav.items[line]
            IF StrCmp(select_item, item.name) THEN JUMP break
        ENDFOR
        line := 0
        break:
        WHILE line >= self.height
            DEC line
            INC offset
        ENDWHILE
        pos := line
    ENDIF

    self.offset := offset
    self.current_item := pos
    self.discard_menu_bitmap()
    self.create_menu_bitmap()
    self.redraw()
ENDPROC

PROC discard_menu_bitmap() OF ags
    DEF bm:PTR TO bitmap
    DEF rp:PTR TO rastport

    IF self.menu_bitmap <> NIL
        IF self.menu_bitmap.planes[0] <> NIL
            FreeRaster(self.menu_bitmap.planes[0], self.menu_bm_width, self.menu_bm_height)
        ENDIF
        bm := self.menu_bitmap
        END bm
        self.menu_bitmap := NIL
    ENDIF
    IF self.menu_rastport <> NIL
        rp := self.menu_rastport
        END rp
        self.menu_rastport := NIL
    ENDIF
ENDPROC

PROC create_menu_bitmap() OF ags HANDLE
    DEF bm = NIL:PTR TO bitmap
    DEF rp = NIL:PTR TO rastport
    DEF line
    DEF item:PTR TO agsnav_item
    DEF y
    -> DEF i

    -> Allocate a single bitplane bitmap for the menu text.
    self.menu_bm_width := (self.width + 2) * self.char_width
    self.menu_bm_height := self.nav.num_items * (self.font.ysize + self.conf.font_leading)

    NEW bm
    InitBitMap(bm, 1, self.menu_bm_width, self.menu_bm_height)
    bm.planes[0] := AllocRaster(self.menu_bm_width, self.menu_bm_height)
    IF bm.planes[0] = NIL THEN Raise("MEM")
    self.menu_bitmap := bm

    NEW rp
    InitRastPort(rp)
    rp.bitmap := bm
    self.menu_rastport := rp
    SetRast(self.menu_rastport, 0)
    SetFont(self.menu_rastport, self.font)

    ->SetABPenDrMd(self.menu_rastport, 1, 0, RP_JAM2)
    FOR line := 0 TO self.nav.num_items - 1
        item := self.nav.items[line]
        y := line * (self.font.ysize + self.conf.font_leading)
        Move(self.menu_rastport, self.menu_rastport.font.xsize, y + self.menu_rastport.font.baseline)
        Text(self.menu_rastport, item.name, item.length)
    ENDFOR
EXCEPT
    IF bm.planes[0] <> NIL THEN FreeRaster(bm.planes[0], self.menu_bm_width, self.menu_bm_height)
    END bm
    END rp
    ReThrow()
ENDPROC

PROC redraw(start=0, end=-1) OF ags
    DEF src_y
    DEF dest_y
    DEF height

    IF end = -1
        IF self.nav.num_items < self.conf.menu_height
            SetAPen(self.rport, self.conf.text_background)
            RectFill(self.rport,
                     self.conf.menu_x,
                     self.conf.menu_y + ((self.font.ysize + self.conf.font_leading) * self.nav.num_items),
                     self.conf.menu_x + self.menu_bm_width - 1,
                     self.conf.menu_y + ((self.font.ysize + self.conf.font_leading) * self.conf.menu_height) - 1)
        ENDIF
        end := Min(self.conf.menu_height, self.nav.num_items) - 1
    ENDIF

    src_y := (self.offset + start) * (self.font.ysize + self.conf.font_leading)
    dest_y := start * (self.font.ysize + self.conf.font_leading)
    height := (Min(end - start, self.nav.num_items) + 1) * (self.font.ysize + self.conf.font_leading)
    BltBitMapRastPort(self.menu_bitmap,
                      0,
                      src_y,
                      self.rport,
                      self.conf.menu_x,
                      self.conf.menu_y + dest_y,
                      self.menu_bm_width,
                      height,
                      $0c0)
    BltBitMapRastPort(self.menu_bitmap,
                      0,
                      0,
                      self.rport,
                      self.conf.menu_x,
                      self.conf.menu_y + (self.current_item * (self.font.ysize + self.conf.font_leading)),
                      self.menu_bm_width,
                      self.font.ysize,
                      $050)
ENDPROC

PROC get_item_path(path:LONG, suffix:PTR TO CHAR, dir_ptr:LONG) OF ags
    DEF item:PTR TO agsnav_item
    DEF first_char[2]:STRING

    item := self.nav.items[self.current_item + self.offset]
    first_char[0] := item.name[0]
    first_char[1] := 0
    SetStr(first_char, 1)

    StrCopy(path, dir_ptr)
    StrAdd(path, first_char)
    StrAdd(path, '/')
    StrAdd(path, item.name)
    StrAdd(path, suffix)
ENDPROC

PROC load_screenshot() OF ags
    DEF path[255]:STRING
    DEF run_path[255]:STRING
    DEF item:PTR TO agsnav_item

    item := self.nav.items[self.current_item + self.offset]

    /* 1. Build the path to the potential .run file */
    StrCopy(run_path, self.nav.path)
    StrAdd(run_path, item.name)
    StrAdd(run_path, '.run')

/* 2. Check if the .run file exists */
    IF FileLength(run_path) <> -1
    /* It is a game entry! Load the actual screenshot */

        self.get_item_path(path, '.iff', self.conf.screenshot_dir)
        PrintF('DEBUG: Path built for image: \s\n', path)
        IF FileLength(path) = -1
            PrintF('DEBUG: Fallback Path for image: \s\n', self.conf.missing_screenshot)
            self.loader.send_cmd(AGSIL_LOAD, self.conf.missing_screenshot)
        ELSE
            self.loader.send_cmd(AGSIL_LOAD, path)
        ENDIF
    ELSE
        /* It is a directory or non-game entry. Load the empty/background image */
        self.loader.send_cmd(AGSIL_LOAD, self.conf.empty_screenshot)
    ENDIF
ENDPROC

PROC load_text() OF ags HANDLE
    DEF path[255]:STRING
    DEF run_path[255]:STRING
    DEF item:PTR TO agsnav_item
    DEF len
    DEF line = NIL
    DEF bufsize = 0
    DEF adjust_read
    DEF fh = NIL
    DEF linenum = 0
    DEF y

    IF self.conf.text_height = 0 THEN Raise(0)
    
    /* 1. Clear the text area */
    SetAPen(self.rport, self.conf.text_background)
    RectFill(self.rport,
             self.conf.text_x,
             self.conf.text_y,
             self.conf.text_x + (self.conf.text_width * self.font.xsize) - 1,
             self.conf.text_y + (self.conf.text_height * (self.font.ysize + self.conf.font_leading)) - 1)

    item := self.nav.items[self.current_item + self.offset]

    /* 2. Check if it's a game (.run file exists) */
    StrCopy(run_path, self.nav.path)
    StrAdd(run_path, item.name)
    StrAdd(run_path, '.run')

    IF FileLength(run_path) <> -1
        /* CASE A: It's a game. Use the global GameText folder logic */
        self.get_item_path(path, '.txt', self.conf.text_dir)

    ELSE
        /* CASE B: It's a folder. Look for [ItemName].txt in the current menu folder */
        /* If item name is "Shooters", it looks for "AGS:Menu/Path/Shooters.txt" */
        StrCopy(path, self.nav.path)
        StrAdd(path, item.name)
        StrAdd(path, '.txt')
        PrintF('DEBUG: Path built for folder text: \s\n', path)
        /* If no description exists for this folder, just exit */
        IF FileLength(path) = -1 THEN RETURN
    ENDIF

    /* 3. Load the file found in 'path' */
    bufsize := self.conf.text_width + 2
    line := String(bufsize)

    -> Work around Fgets() bug in V36/V37.
    IF KickVersion(39) THEN adjust_read := 0 ELSE adjust_read := 1

    SetAPen(self.rport, self.conf.text_color)
    SetBPen(self.rport, self.conf.text_background)
    SetDrMd(self.rport, RP_JAM2)
    IF (fh := Open(path, OLDFILE)) = NIL THEN Raise(0)
    WHILE (linenum < self.conf.text_height) AND Fgets(fh, line, bufsize - adjust_read)
        len := StrLen(line)
        -> Trim trailing newline.
        IF len > 0
            IF (line[len - 1] = "\n")
                DEC len
                line[len] := 0
            ENDIF
        ENDIF
        IF len > self.conf.text_width THEN DEC len
        -> Fix estring length.
        SetStr(line, len)

        IF len > 0
            y := self.conf.text_y +
                 self.font.baseline +
                 ((self.font.ysize + self.conf.font_leading) * linenum)
            Move(self.rport, self.conf.text_x, y)
            Text(self.rport, line, len)
        ENDIF

        INC linenum
    ENDWHILE

EXCEPT DO
    IF line THEN DisposeLink(line)
    IF fh THEN Close(fh)
ENDPROC

PROC load_text2() OF ags HANDLE
    DEF path[255]:STRING, run_path[255]:STRING
    DEF item:PTR TO agsnav_item
    DEF len, line = NIL, fh = NIL, linenum = 0, y
    DEF bufsize, adjust_read

    IF self.conf.text2_height = 0 THEN RETURN
    
    SetAPen(self.rport, self.conf.text2_background)
    RectFill(self.rport, self.conf.text2_x, self.conf.text2_y,
             self.conf.text2_x + (self.conf.text2_width * self.font.xsize) - 1,
             self.conf.text2_y + (self.conf.text2_height * (self.font.ysize + self.conf.font_leading)) - 1)

    item := self.nav.items[self.current_item + self.offset]
    StrCopy(run_path, self.nav.path)
    StrAdd(run_path, item.name)
    StrAdd(run_path, '.run')

    IF FileLength(run_path) <> -1
        self.get_item_path(path, '.txt', self.conf.text2_dir)
    ELSE
        StrCopy(path, self.nav.path)
        StrAdd(path, item.name)
        StrAdd(path, '.txt2')  -> Folder descriptions for Area 2 use .txt2 extension
        IF FileLength(path) = -1 THEN RETURN
    ENDIF

    bufsize := self.conf.text2_width + 2
    line := String(bufsize)
    IF KickVersion(39) THEN adjust_read := 0 ELSE adjust_read := 1

    SetAPen(self.rport, self.conf.text2_color)
    SetBPen(self.rport, self.conf.text2_background)
    SetDrMd(self.rport, RP_JAM2)

   fh := Open(path, OLDFILE) /* Remove the IF from this line */
    IF fh
        WHILE (linenum < self.conf.text2_height) AND Fgets(fh, line, bufsize - adjust_read)
            len := StrLen(line)

            IF (len > 0) AND (line[len - 1] = "\n")
                len := len - 1
                line[len] := 0
            ENDIF

            SetStr(line, Min(len, self.conf.text2_width))
            IF EstrLen(line) > 0
                y := self.conf.text2_y + self.font.baseline + ((self.font.ysize + self.conf.font_leading) * linenum)
                Move(self.rport, self.conf.text2_x, y)
                Text(self.rport, line, EstrLen(line))
            ENDIF
            INC linenum
        ENDWHILE
        Close(fh)
        fh := NIL /* Set to NIL so the EXCEPT block doesn't try to close it again */
    ENDIF

EXCEPT DO
    IF line THEN DisposeLink(line)
    IF fh THEN Close(fh) /* This only runs if an actual crash/exception happened */
ENDPROC

PROC main() HANDLE
    DEF conf = NIL:PTR TO agsconf
    DEF il = NIL:PTR TO ilbmloader
    DEF s = NIL:PTR TO screen
    DEF w = NIL:PTR TO window
    DEF pointer = NIL:PTR TO INT
    DEF s_width = 640
    DEF s_height = 256
    DEF s_depth = 4
    DEF s_mode
    DEF ta:textattr
    DEF font = NIL:PTR TO textfont
    DEF loader = NIL:PTR TO agsil_master    -> Background image loader master object.
    DEF reply
    DEF nav = NIL:PTR TO agsnav             -> Menu directory navigator.
    DEF ags = NIL:PTR TO ags                -> Application controller.
    DEF menu_pos = NIL:PTR TO agsmenupos

    IF KickVersion(37) = FALSE THEN Raise(ERR_KICKSTART)

    IF (lowlevelbase := OpenLibrary('lowlevel.library', 0)) = NIL THEN Raise("LOWL")

    NEW conf.init()
    conf.read('AGS:AGS2.conf')

    IF SetJoyPortAttrsA(1, [SJA_TYPE, SJA_TYPE_JOYSTK, 0]) = FALSE
        Raise(ERR_JOYSTICK)
    ENDIF

    NEW loader.init()
    loader.start()

    NEW il.init()
    IF il.open(conf.background) = FALSE THEN Raise(ERR_BACKGROUND)
    IF il.parse_header() = FALSE THEN Raise(ERR_BACKGROUND)
    IF conf.mode = AGSCONF_AUTODETECT
        s_mode := IF il.mode THEN il.mode ELSE (PAL_MONITOR_ID OR HIRES_KEY)
    ELSE
        s_mode := conf.mode
    ENDIF
    s_width := il.width
    s_height := il.height
    IF conf.depth = AGSCONF_AUTODETECT
        s_depth := il.depth
    ELSE
        s_depth := conf.depth
    ENDIF

    IF (s := OpenScreenTagList(NIL, [
            SA_WIDTH, s_width,
            SA_HEIGHT, s_height,
            SA_DEPTH, s_depth,
            SA_DISPLAYID, s_mode,
            SA_DRAGGABLE, FALSE,
            SA_SHOWTITLE, FALSE,
            SA_BEHIND, TRUE,
            0])) = NIL THEN Raise(ERR_SCREEN)
    IF (w := OpenWindowTagList(NIL, [
            WA_CUSTOMSCREEN, s,
            WA_WIDTH, s_width,
            WA_HEIGHT, s_height,
            WA_TITLE, 0,
            WA_CLOSEGADGET, FALSE,
            WA_BORDERLESS, TRUE,
            WA_RMBTRAP, TRUE,
            WA_ACTIVATE, TRUE,
            0])) = NIL THEN Raise(ERR_WINDOW)

    pointer := NewM(4, MEMF_CHIP OR MEMF_CLEAR)
    SetPointer(w, pointer, 1, 1, 0, 0)

    ta.name := conf.font_name
    ta.ysize := conf.font_size
    ta.style := 0
    ta.flags := 0
    IF (font := OpenFont(ta)) = NIL THEN Raise(ERR_FONT)
    SetFont(w.rport, font)

    fade_out_vport(s.viewport, Shl(1, s_depth), 1) -> Clear palette to black.
    VideoControl(s.viewport.colormap, [VTAG_BORDERBLANK_SET, 0, 0])
    il.load_body(w.rport, 0, 0)
    ScreenToFront(s)
    fade_in_vport(il.colormap, s.viewport, Shl(1, s_depth), 10)
    il.close()
    END il

    loader.wait_port()
    reply := loader.send_cmd(AGSIL_SETRPORT, w.rport)
    reply := loader.send_cmd(AGSIL_SETVPORT, s.viewport)
    reply := loader.send_cmd(AGSIL_SETMAXCOLORS, Shl(1, s_depth) - conf.lock_colors)
    reply := loader.send_cmd(AGSIL_SETCONF, conf)
    /* We loaded the background image directly using ilbmloader instead.
    reply := loader.send_cmd(AGSIL_SETXY, 0)
    reply := loader.send_cmd(AGSIL_LOAD, bkg_img)
    IF reply < 0 THEN Raise(ERR_BACKGROUND)
    loader.wait_load(reply)
    */

    NEW menu_pos.init()
    menu_pos.read()

    NEW nav.init()

    /* If no saved path exists, or path is just root, force it to Menu/ */
    IF (StrLen(menu_pos.path) = 0) OR (StrCmp(menu_pos.path, 'AGS:'))
        nav.set_path('AGS:Menu/')
        nav.depth := 0
    ELSE
        nav.set_path(menu_pos.path)
        nav.depth := menu_pos.depth
    ENDIF
    
    NEW ags.init(conf, nav, loader, w.rport, font)
    ags.select(menu_pos.offset, menu_pos.pos)
    fade_out_vport(s.viewport, Shl(1, s_depth), 10)

    loader.stop()

EXCEPT DO
    /* 1. Stop the background process first while hardware is still locked */
    IF loader THEN loader.stop()

    /* 2. Free your custom bitmaps and objects */
    END ags
    END nav
    END loader
    END il
    END menu_pos
    
    /* 3. Close the UI */
    IF font THEN CloseFont(font)
    IF pointer THEN Dispose(pointer)
    IF w THEN CloseWindow(w)
    IF s THEN CloseScreen(s)

    /* 4. Release system libs */
    SetJoyPortAttrsA(1, [SJA_REINITIALIZE, 0, 0])
    END conf
    IF lowlevelbase THEN CloseLibrary(lowlevelbase)
    SELECT exception
        CASE "MEM"
            PrintF('Out of memory.\n')
        CASE AGSIL_ERROR
            PrintF('\s.\n', agsil_strerror(exceptioninfo))
        CASE AGSNAV_ERROR
            PrintF('\s.\n', agsnav_strerror(exceptioninfo))
        CASE AGSCONF_ERROR
            PrintF('\s.\n', agsconf_strerror(exceptioninfo))
        CASE ILBM_ERROR
            PrintF('\s\n', ilbm_strerror(exceptioninfo))
        CASE ERR_KICKSTART
            PrintF('Requires Kickstart 2.0+.\n')
        CASE ERR_JOYSTICK
            PrintF('Couldn''t read joystick.\n')
        CASE ERR_SCREEN
            PrintF('Couldn''t open screen.\n')
        CASE ERR_WINDOW
            PrintF('Couldn''t open window.\n')
        CASE ERR_FONT
            PrintF('Couldn''t open font.\n')
        CASE ERR_BACKGROUND
            PrintF('Couldn''t load background image.\n')
        CASE ERR_EMPTY
            PrintF('Menu is empty, nothing to select.\n')
        CASE ERR_COPY_SRC
            PrintF('Error opening source.\n')
        CASE ERR_COPY_DST
            PrintF('Error opening destination.\n')
        CASE ERR_COPY_WRITE
            PrintF('Error writing run script.\n')
        DEFAULT
            IF exception
                IF exception < 10000
                    PrintF('Unknown exception \d\n', exception)
                ELSE
                    PrintF('Unknown exception "\s"', [exception, 0])
                ENDIF
            ENDIF
    ENDSELECT
ENDPROC

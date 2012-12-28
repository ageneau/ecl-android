// Copyright (c) 2012 The Chromium OS Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

'use strict';

lib.rtdep('hterm.Keyboard.KeyActions');

/**
 * The default key map for hterm.
 *
 * Contains a mapping of keyCodes to keyDefs (aka key definitions).  The key
 * definition tells the hterm.Keyboard class how to handle keycodes.
 *
 * This should work for most cases, as the printable characters get handled
 * in the keypress event.  In that case, even if the keycap is wrong in the
 * key map, the correct character should be sent.
 *
 * Different layouts, such as Dvorak should work with this keymap, as those
 * layouts typically move keycodes around on the keyboard without disturbing
 * the actual keycaps.
 *
 * There may be issues with control keys on non-US keyboards or with keyboards
 * that very significantly from the expectations here, in which case we may
 * have to invent new key maps.
 *
 * The sequences defined in this key map come from [XTERM] as referenced in
 * vt.js, starting with the section titled "Alt and Meta Keys".
 */
hterm.Keyboard.KeyMap = function(keyboard) {
  this.keyboard = keyboard;
  this.keyDefs = {};
  this.reset();
};

/**
 * Add a single key definition.
 *
 * The definition is a hash containing the following keys: 'keyCap', 'normal',
 * 'control', and 'alt'.
 *
 *  - keyCap is a string identifying the key.  For printable
 *    keys, the key cap should be exactly two characters, starting with the
 *    unshifted version.  For example, 'aA', 'bB', '1!' and '=+'.  For
 *    non-printable the key cap should be surrounded in square braces, as in
 *    '[INS]', '[LEFT]'.  By convention, non-printable keycaps are in uppercase
 *    but this is not a strict requirement.
 *
 *  - Normal is the action that should be performed when they key is pressed
 *    in the absence of any modifier.  See below for the supported actions.
 *
 *  - Control is the action that should be performed when they key is pressed
 *    along with the control modifier.  See below for the supported actions.
 *
 *  - Alt is the action that should be performed when they key is pressed
 *    along with the alt modifier.  See below for the supported actions.
 *
 *  - Meta is the action that should be performed when they key is pressed
 *    along with the meta modifier.  See below for the supported actions.
 *
 * Actions can be one of the hterm.Keyboard.KeyActions as documented below,
 * a literal string, or an array.  If the action is a literal string then
 * the string is sent directly to the host.  If the action is an array it
 * is taken to be an escape sequence that may be altered by modifier keys.
 * The second-to-last element of the array will be overwritten with the
 * state of the modifier keys, as specified in the final table of "PC-Style
 * Function Keys" from [XTERM].
 */
hterm.Keyboard.KeyMap.prototype.addKeyDef = function(keyCode, def) {
  if (keyCode in this.keyDefs)
    console.warn('Duplicate keyCode: ' + keyCode);

  this.keyDefs[keyCode] = def;
};

/**
 * Add mutiple key definitions in a single call.
 *
 * This function takes the key definitions as variable argument list.  Each
 * argument is the key definition specified as an array.
 *
 * (If the function took everything as one big hash we couldn't detect
 * duplicates, and there would be a lot more typing involved.)
 *
 * Each key definition should have 6 elements: (keyCode, keyCap, normal action,
 * control action, alt action and meta action).  See KeyMap.addKeyDef for the
 * meaning of these elements.
 */
hterm.Keyboard.KeyMap.prototype.addKeyDefs = function(var_args) {
  for (var i = 0; i < arguments.length; i++) {
    this.addKeyDef(arguments[i][0],
                   { keyCap: arguments[i][1],
                     normal: arguments[i][2],
                     control: arguments[i][3],
                     alt: arguments[i][4],
                     meta: arguments[i][5]
                   });
  }
};

/**
 * Inherit from hterm.Keyboard.KeyMap, as defined in keyboard.js.
 */
hterm.Keyboard.KeyMap.prototype = {
  __proto__: hterm.Keyboard.KeyMap.prototype
};

/**
 * Set up the default state for this keymap.
 */
hterm.Keyboard.KeyMap.prototype.reset = function() {
  this.keyDefs = {};

  var self = this;

  // This function us used by the "macro" functions below.  It makes it
  // possible to use the call() macro as an argument to any other macro.
  function resolve(action, e, k) {
    if (typeof action == 'function')
      return action.apply(self, [e, k]);

    return action;
  }

  // If not application keypad a, else b.  The keys that care about
  // application keypad ignore it when the key is modified.
  function ak(a, b) {
    return function(e, k) {
      var action = (e.shiftKey || e.ctrlKey || e.altKey || e.metaKey ||
                    !self.keyboard.applicationKeypad) ? a : b;
      return resolve(action, e, k);
    }
  }

  // If mod or not application cursor a, else b.  The keys that care about
  // application cursor ignore it when the key is modified.
  function ac(a, b) {
    return function(e, k) {
      var action = (e.shiftKey || e.ctrlKey || e.altKey || e.metaKey ||
                    !self.keyboard.applicationCursor) ? a : b;
      return resolve(action, e, k);
    }
  }

  // If not backspace-sends-backspace keypad a, else b.
  function bs(a, b) {
    return function(e, k) {
      var action = !self.keyboard.backspaceSendsBackspace ? a : b
      return resolve(action, e, k);
    }
  }

  // If not e.shiftKey a, else b.
  function sh(a, b) {
    return function(e, k) {
      var action = !e.shiftKey ? a : b
      return resolve(action, e, k);
    }
  }

  // If no modifiers a, else b.
  function mod(a, b) {
    return function (e, k) {
      var action = !(e.shiftKey || e.ctrlKey || e.altKey || e.metaKey) ? a : b;
      return resolve(action, e, k);
    }
  }

  // Compute a control character for a given character.
  function ctl(ch) { return String.fromCharCode(ch.charCodeAt(0) - 64) }

  // Call a method on the keymap instance.
  function call(m) { return function (e, k) { return this[m](e, k) } }

  var ESC = '\x1b';
  var CSI = '\x1b[';
  var SS3 = '\x1bO';

  var CANCEL = hterm.Keyboard.KeyActions.CANCEL;
  var DEFAULT = hterm.Keyboard.KeyActions.DEFAULT;
  var PASS = hterm.Keyboard.KeyActions.PASS;
  var STRIP = hterm.Keyboard.KeyActions.STRIP;

  // On OS X, we want to PASS on Meta+KEY and DEFAULT on Meta+Shift+KEY for
  // certain browser accelerators.  Other platforms use Alt instead of Meta.
  var osx = window.navigator.userAgent.match(/Mac OS X/);
  var altAccel = osx ? DEFAULT : sh(PASS, DEFAULT);
  var metaAccel = osx ? sh(PASS, DEFAULT) : DEFAULT;

  this.addKeyDefs(
    // These fields are: [keycode, keycap, normal, control, alt, meta]

    // The browser sends the keycode 0 for some keys.  We'll just assume it's
    // going to do the right thing by default for those keys.
    [0,   '[UNKNOWN]', PASS, PASS, PASS, PASS],

    // First row.
    [27,  '[ESC]', ESC,                       DEFAULT, DEFAULT,     DEFAULT],
    [112, '[F1]',  mod(SS3 + 'P', CSI + 'P'), DEFAULT, CSI + "23~", DEFAULT],
    [113, '[F2]',  mod(SS3 + 'Q', CSI + 'Q'), DEFAULT, CSI + "24~", DEFAULT],
    [114, '[F3]',  mod(SS3 + 'R', CSI + 'R'), DEFAULT, CSI + "25~", DEFAULT],
    [115, '[F4]',  mod(SS3 + 'S', CSI + 'S'), DEFAULT, CSI + "26~", DEFAULT],
    [116, '[F5]',  CSI + '15~',               DEFAULT, CSI + "28~", DEFAULT],
    [117, '[F6]',  CSI + '17~',               DEFAULT, CSI + "29~", DEFAULT],
    [118, '[F7]',  CSI + '18~',               DEFAULT, CSI + "31~", DEFAULT],
    [119, '[F8]',  CSI + '19~',               DEFAULT, CSI + "32~", DEFAULT],
    [120, '[F9]',  CSI + '20~',               DEFAULT, CSI + "33~", DEFAULT],
    [121, '[F10]', CSI + '21~',               DEFAULT, CSI + "34~", DEFAULT],
    [122, '[F11]', CSI + '23~',               DEFAULT, CSI + "42~", DEFAULT],
    [123, '[F12]', CSI + '24~',               DEFAULT, CSI + "43~", DEFAULT],

    // Second row.
    [192, '`~', DEFAULT, sh(ctl('@'), ctl('^')),        DEFAULT,        PASS],
    [49,  '1!', DEFAULT, sh(PASS, STRIP),               altAccel,  metaAccel],
    [50,  '2@', DEFAULT, sh(PASS, ctl('@')),            altAccel,  metaAccel],
    [51,  '3#', DEFAULT, sh(PASS, ctl('[')),            altAccel,  metaAccel],
    [52,  '4$', DEFAULT, sh(PASS, ctl('\\')),           altAccel,  metaAccel],
    [53,  '5%', DEFAULT, sh(PASS, ctl(']')),            altAccel,  metaAccel],
    [54,  '6^', DEFAULT, sh(PASS, ctl('^')),            altAccel,  metaAccel],
    [55,  '7&', DEFAULT, sh(PASS, ctl('_')),            altAccel,  metaAccel],
    [56,  '8*', DEFAULT, sh(PASS, '*'),                 altAccel,  metaAccel],
    [57,  '9(', DEFAULT, sh(PASS, STRIP),               altAccel,  metaAccel],
    [48,  '0)', DEFAULT, call('onZoom_'),               DEFAULT,     DEFAULT],
    [189, '-_', DEFAULT, sh(call('onZoom_'), ctl('_')), DEFAULT,     DEFAULT],
    [187, '=+', DEFAULT, call('onZoom_'),               DEFAULT,     DEFAULT],
    [8,   '[BKSP]', bs('\x7f', '\b'), bs('\b', '\x7f'), DEFAULT,     DEFAULT],

    // Third row.
    [9,   '[TAB]', '\t',    STRIP,     PASS,    DEFAULT],
    [81,  'qQ',    DEFAULT, ctl('Q'),  DEFAULT, DEFAULT],
    [87,  'wW',    DEFAULT, ctl('W'),  DEFAULT, DEFAULT],
    [69,  'eE',    DEFAULT, ctl('E'),  DEFAULT, DEFAULT],
    [82,  'rR',    DEFAULT, ctl('R'),  DEFAULT, DEFAULT],
    [84,  'tT',    DEFAULT, ctl('T'),  DEFAULT, DEFAULT],
    [89,  'yY',    DEFAULT, ctl('Y'),  DEFAULT, DEFAULT],
    [85,  'uU',    DEFAULT, ctl('U'),  DEFAULT, DEFAULT],
    [73,  'iI',    DEFAULT, ctl('I'),  DEFAULT, DEFAULT],
    [79,  'oO',    DEFAULT, ctl('O'),  DEFAULT, DEFAULT],
    [80,  'pP',    DEFAULT, ctl('P'),  DEFAULT, DEFAULT],
    [219, '[{',    DEFAULT, ctl('['),  DEFAULT, DEFAULT],
    [221, ']}',    DEFAULT, ctl(']'),  DEFAULT, DEFAULT],
    [220, '\\|',   DEFAULT, ctl('\\'), DEFAULT, DEFAULT],

    // Fourth row. (We let Ctrl-Shift-J pass for Chrome DevTools.)
    [20,  '[CAPS]',  PASS,    PASS,               PASS,    DEFAULT],
    [65,  'aA',      DEFAULT, ctl('A'),           DEFAULT, DEFAULT],
    [83,  'sS',      DEFAULT, ctl('S'),           DEFAULT, DEFAULT],
    [68,  'dD',      DEFAULT, ctl('D'),           DEFAULT, DEFAULT],
    [70,  'fF',      DEFAULT, ctl('F'),           DEFAULT, DEFAULT],
    [71,  'gG',      DEFAULT, ctl('G'),           DEFAULT, DEFAULT],
    [72,  'hH',      DEFAULT, ctl('H'),           DEFAULT, DEFAULT],
    [74,  'jJ',      DEFAULT, sh(ctl('J'), PASS), DEFAULT, DEFAULT],
    [75,  'kK',      DEFAULT, ctl('K'),           DEFAULT, DEFAULT],
    [76,  'lL',      DEFAULT, ctl('L'),           DEFAULT, DEFAULT],
    [186, ';:',      DEFAULT, STRIP,              DEFAULT, DEFAULT],
    [222, '\'"',     DEFAULT, STRIP,              DEFAULT, DEFAULT],
    [13,  '[ENTER]', '\r',    CANCEL,             CANCEL,  DEFAULT],

    // Fifth row.  This includes the copy/paste shortcuts.  On some
    // platforms it's Ctrl-C/V, on others it's Meta-C/V.  We assume either
    // Ctrl-C/Meta-C should pass to the browser when there is a selection,
    // and Ctrl-Shift-V/Meta-*-V should always pass to the browser (since
    // these seem to be recognized as paste too).
    [16,  '[SHIFT]', PASS, PASS,                   PASS,    DEFAULT],
    [90,  'zZ',   DEFAULT, ctl('Z'),               DEFAULT, DEFAULT],
    [88,  'xX',   DEFAULT, ctl('X'),               DEFAULT, DEFAULT],
    [67,  'cC',   DEFAULT, call('onCtrlC_'),       DEFAULT, call('onMetaC_')],
    [86,  'vV',   DEFAULT, sh(ctl('V'), PASS),     DEFAULT, PASS],
    [66,  'bB',   DEFAULT, sh(ctl('B'), PASS),     DEFAULT, sh(DEFAULT, PASS)],
    [78,  'nN',   DEFAULT, call('onCtrlN_'),       DEFAULT, call('onMetaN_')],
    [77,  'mM',   DEFAULT, ctl('M'),               DEFAULT, DEFAULT],
    [188, ',<',   DEFAULT, STRIP,                  DEFAULT, DEFAULT],
    [190, '.>',   DEFAULT, STRIP,                  DEFAULT, DEFAULT],
    [191, '/?',   DEFAULT, sh(ctl('_'), ctl('?')), DEFAULT, DEFAULT],

    // Sixth and final row.
    [17,  '[CTRL]', PASS,    PASS,     PASS,    PASS],
    [18,  '[ALT]',  PASS,    PASS,     PASS,    PASS],
    [91,  '[LAPL]', PASS,    PASS,     PASS,    PASS],
    [32,  ' ',      DEFAULT, ctl('@'), DEFAULT, DEFAULT],
    [92,  '[RAPL]', PASS,    PASS,     PASS,    PASS],

    // These things.
    [42,  '[PRTSCR]', PASS, PASS, PASS, PASS],
    [145, '[SCRLK]',  PASS, PASS, PASS, PASS],
    [19,  '[BREAK]',  PASS, PASS, PASS, PASS],

    // The block of six keys above the arrows.
    [45,  '[INSERT]', call('onKeyInsert_'),   DEFAULT, DEFAULT, DEFAULT],
    [36,  '[HOME]',   call('onKeyHome_'),     DEFAULT, DEFAULT, DEFAULT],
    [33,  '[PGUP]',   call('onKeyPageUp_'),   DEFAULT, DEFAULT, DEFAULT],
    [46,  '[DEL]',    CSI + '3~',             DEFAULT, DEFAULT, DEFAULT],
    [35,  '[END]',    call('onKeyEnd_'),      DEFAULT, DEFAULT, DEFAULT],
    [34,  '[PGDOWN]', call('onKeyPageDown_'), DEFAULT, DEFAULT, DEFAULT],

    // Arrow keys.  When unmodified they respect the application cursor state,
    // otherwise they always send the CSI codes.
    [38, '[UP]',    ac(CSI + 'A', SS3 + 'A'), DEFAULT, DEFAULT, DEFAULT],
    [40, '[DOWN]',  ac(CSI + 'B', SS3 + 'B'), DEFAULT, DEFAULT, DEFAULT],
    [39, '[RIGHT]', ac(CSI + 'C', SS3 + 'C'), DEFAULT, DEFAULT, DEFAULT],
    [37, '[LEFT]',  ac(CSI + 'D', SS3 + 'D'), DEFAULT, DEFAULT, DEFAULT],

    [144, '[NUMLOCK]', PASS, PASS, PASS, PASS],

    // With numlock off, the keypad generates the same key codes as the arrows
    // and 'block of six' for some keys, and null key codes for the rest.

    // Keypad with numlock on generates unique key codes...
    [96,  '[KP0]', ak(DEFAULT, CSI + '2~'), DEFAULT, DEFAULT, DEFAULT],
    [97,  '[KP1]', ak(DEFAULT, SS3 + 'F'),  DEFAULT, DEFAULT, DEFAULT],
    [98,  '[KP2]', ak(DEFAULT, CSI + 'B'),  DEFAULT, DEFAULT, DEFAULT],
    [99,  '[KP3]', ak(DEFAULT, CSI + '6~'), DEFAULT, DEFAULT, DEFAULT],
    [100, '[KP4]', ak(DEFAULT, CSI + 'D'),  DEFAULT, DEFAULT, DEFAULT],
    [101, '[KP5]', ak(DEFAULT, CSI + 'E'),  DEFAULT, DEFAULT, DEFAULT],
    [102, '[KP6]', ak(DEFAULT, CSI + 'C'),  DEFAULT, DEFAULT, DEFAULT],
    [103, '[KP7]', ak(DEFAULT, SS3 + 'H'),  DEFAULT, DEFAULT, DEFAULT],
    [104, '[KP8]', ak(DEFAULT, CSI + 'A'),  DEFAULT, DEFAULT, DEFAULT],
    [105, '[KP9]', ak(DEFAULT, CSI + '5~'), DEFAULT, DEFAULT, DEFAULT],
    [107, '[KP+]', ak(DEFAULT, SS3 + 'k'),  DEFAULT, DEFAULT, DEFAULT],
    [109, '[KP-]', ak(DEFAULT, SS3 + 'm'),  DEFAULT, DEFAULT, DEFAULT],
    [106, '[KP*]', ak(DEFAULT, SS3 + 'j'),  DEFAULT, DEFAULT, DEFAULT],
    [111, '[KP/]', ak(DEFAULT, SS3 + 'o'),  DEFAULT, DEFAULT, DEFAULT],
    [110, '[KP.]', ak(DEFAULT, CSI + '3~'), DEFAULT, DEFAULT, DEFAULT]
  );
};

/**
 * Either allow the paste or send a key sequence.
 */
hterm.Keyboard.KeyMap.prototype.onKeyInsert_ = function(e) {
  if (this.keyboard.shiftInsertPaste && e.shiftKey)
    return hterm.Keyboard.KeyActions.PASS;

  return '\x1b[2~';
};

/**
 * Either scroll the scrollback buffer or send a key sequence.
 */
hterm.Keyboard.KeyMap.prototype.onKeyHome_ = function(e) {
  if (!this.keyboard.homeKeysScroll ^ e.shiftKey) {
    if ((e.altey || e.ctrlKey || e.shiftKey) ||
        !this.keyboard.applicationKeypad) {
      return '\x1b[H';
    }

    return '\x1bOH';
  }

  this.keyboard.terminal.scrollHome();
  return hterm.Keyboard.KeyActions.CANCEL;
};

/**
 * Either scroll the scrollback buffer or send a key sequence.
 */
hterm.Keyboard.KeyMap.prototype.onKeyEnd_ = function(e) {
  if (!this.keyboard.homeKeysScroll ^ e.shiftKey) {
    if ((e.altKey || e.ctrlKey || e.shiftKey) ||
        !this.keyboard.applicationKeypad) {
      return '\x1b[F';
    }

    return '\x1bOF';
  }

  this.keyboard.terminal.scrollEnd();
  return hterm.Keyboard.KeyActions.CANCEL;
};

/**
 * Either scroll the scrollback buffer or send a key sequence.
 */
hterm.Keyboard.KeyMap.prototype.onKeyPageUp_ = function(e) {
  if (!this.keyboard.pageKeysScroll ^ e.shiftKey)
    return '\x1b[5~';

  this.keyboard.terminal.scrollPageUp();
  return hterm.Keyboard.KeyActions.CANCEL;
};

/**
 * Either scroll the scrollback buffer or send a key sequence.
 */
hterm.Keyboard.KeyMap.prototype.onKeyPageDown_ = function(e) {
  if (!this.keyboard.pageKeysScroll ^ e.shiftKey)
    return '\x1b[6~';

  this.keyboard.terminal.scrollPageDown();
  return hterm.Keyboard.KeyActions.CANCEL;
};

/**
 * Either send a ^C or allow the browser to interpret the keystroke as a copy
 * command.
 *
 * If there is no selection, or if the user presses Ctrl-Shift-C, then we'll
 * transmit a ^C ('\x03').  If there is a selection, we defer to the
 * browser.  In this case we clear out the selection so the user knows we
 * heard them, and also to give them a chance to send a ^C by just hitting
 * the key again.
 */
hterm.Keyboard.KeyMap.prototype.onCtrlC_ = function(e, keyDef) {
  var selection = this.keyboard.terminal.getDocument().getSelection();
  if (e.shiftKey || selection.isCollapsed) {
    // If the shift key is being held or there is no document selection, then
    // send a ^C.
    return '\x03';
  }

  // Otherwise let the browser handle it as a copy command.  Clear the selection
  // soon after a Ctrl-C copy, so that it frees up Ctrl-C to send ^C.
  setTimeout(selection.collapseToEnd.bind(selection), 750);
  return hterm.Keyboard.KeyActions.PASS;
};

/**
 * Either send a ^N or open a new window to the same location.
 */
hterm.Keyboard.KeyMap.prototype.onCtrlN_ = function(e, keyDef) {
  if (e.shiftKey) {
    window.open(document.location.href, '',
                'chrome=no,close=yes,resize=yes,scrollbars=yes,' +
                'minimizable=yes');
    return hterm.Keyboard.KeyActions.CANCEL;
  }

  return '\x0e';
};

/**
 * Either the default action or open a new window to the same location.
 */
hterm.Keyboard.KeyMap.prototype.onMetaN_ = function(e, keyDef) {
  if (e.shiftKey) {
    window.open(document.location.href, '',
                'chrome=no,close=yes,resize=yes,scrollbars=yes,' +
                'minimizable=yes,width=' + window.outerWidth +
                ',height=' + window.outerHeight);
    return hterm.Keyboard.KeyActions.CANCEL;
  }

  return hterm.Keyboard.KeyActions.DEFAULT;
};

/**
 * Either send a Meta-C or allow the browser to interpret the keystroke as a
 * copy command.
 *
 * If there is no selection, or if the user presses Meta-Shift-C, then we'll
 * transmit an '\x1b' (if metaSendsEscape is on) followed by 'c' or 'C'.
 *
 * If there is a selection, we defer to the browser.  In this case we clear out
 * the selection so the user knows we heard them, and also to give them a
 * chance to send a Meta-C by just hitting the key again.
 */
hterm.Keyboard.KeyMap.prototype.onMetaC_ = function(e, keyDef) {
  var document = this.keyboard.terminal.getDocument();
  if (e.shiftKey || document.getSelection().isCollapsed) {
    // If the shift key is being held, or there is no document selection, send
    // a Meta-C.  The keyboard code will add the ESC if metaSendsEscape is true,
    // we just have to decide between 'c' and 'C'.
    return keyDef.keyCap.substr(e.shiftKey ? 1 : 0, 1);
  }

  // Otherwise let the browser handle it as a copy command.
  setTimeout(function() { document.getSelection().collapseToEnd() }, 50);
  return hterm.Keyboard.KeyActions.PASS;
};

/**
 * Handle font zooming.
 *
 * The browser's built-in zoom has a bit of an issue at certain zoom levels.
 * At some magnifications, the measured height of a row of text differs from
 * the height that was explicitly set.
 *
 * We override the browser zoom keys to change the ScrollPort's font size to
 * avoid the issue.
 */
hterm.Keyboard.KeyMap.prototype.onZoom_ = function(e, keyDef) {
  if (this.keyboard.terminal.getZoomFactor() != 1) {
    // If we're not at 1:1 zoom factor, let the Ctrl +/-/0 keys control the
    // browser zoom, so it's easier to for the user to get back to 100%.
    return hterm.Keyboard.KeyActions.PASS;
  }

  var cap = keyDef.keyCap.substr(0, 1);
  if (cap == '0') {
      this.keyboard.terminal.setFontSize(0);
  } else {
    var size = this.keyboard.terminal.getFontSize();

    if (cap == '-') {
      size -= 1;
    } else {
      size += 1;
    }

    this.keyboard.terminal.setFontSize(size);
  }

  return hterm.Keyboard.KeyActions.CANCEL;
};

// Copyright (c) 2012 The Chromium OS Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

'use strict';

lib.rtdep('lib.f',
          'hterm');

// CSP means that we can't kick off the initialization from the html file,
// so we do it like this instead.
window.onload = function() {
  lib.ensureRuntimeDependencies();
  hterm.init(Crosh.init);
};

/**
 * The Crosh-powered terminal command.
 *
 * This class defines a command that can be run in an hterm.Terminal instance.
 * The Crosh command uses terminalPrivate extension API to create and use crosh
 * process on ChromeOS machine.
 *
 *
 * @param {Object} argv The argument object passed in from the Terminal.
 */
function Crosh(argv) {
  this.argv_ = argv;
  this.io = null;
};

var nethackEmbed;

/**
 * Prefix for text from the pipe mount.
 *
 * @private
 */
Crosh.prefix_ = 'JSPipeMount:1:';


/**
 * Static initialier called from crosh.html.
 *
 * This constructs a new Terminal instance and instructs it to run the Crosh
 * command.
 */
Crosh.init = function() {
  var profileName = lib.f.parseQuery(document.location.search)['profile'];
  var terminal = new hterm.Terminal(profileName);
  terminal.decorate(document.querySelector('#terminal'));

  // Useful for console debugging.
  window.term_ = terminal;

  // Looks like there is a race between this and terminal initialization, thus
  // adding timeout.
  setTimeout(function() {
      terminal.setAutoCarriageReturn(true);
      terminal.setCursorPosition(0, 0);
      terminal.setCursorVisible(true);
      terminal.setInsertMode(true);
      terminal.runCommandClass(Crosh, document.location.hash.substr(1));
    }, 500);
  return true;
};

/**
 * Handle messages sent to us from NaCl.
 *
 * @private
 */
Crosh.prototype.handleMessage_ = function(e) {
  if (e.data.indexOf(Crosh.prefix_) != 0) return;
  var msg = e.data.substring(Crosh.prefix_.length);
  console.log("handleMessage:" + msg);
  term_.io.print(msg);  
}

function got(str) {
  term_.io.print(str);
  nethackEmbed.postMessage('JSPipeMount:0:' + str);
}


/**
 * The name of this command used in messages to the user.
 *
 * Perhaps this will also be used by the user to invoke this command, if we
 * build a shell command.
 */
Crosh.prototype.commandName = 'crosh';

/**
 * Called when an event from the crosh process is detected.
 *
 * @param pid Process id of the process the event came from.
 * @param type Type of the event.
 *             'stdout': Process output detected.
 *             'exit': Process has exited.
 * @param text Text that was detected on process output.
**/
Crosh.prototype.onProcessOutput_ = function(pid, type, text) {
  if (type == 'exit') {
    this.exit(0);
    return;
  }
  this.io.print(text);
}

Crosh.prototype.resize_ = function(width, height) {
  nethackEmbed.postMessage('WINCH:' + width + ':' + height);
}

/**
 * Start the crosh command.
 *
 * This is invoked by the terminal as a result of terminal.runCommandClass().
 */
Crosh.prototype.run = function() {
  this.io = this.argv_.io.push();

  // Create the object for Crosh.
  nethackEmbed = document.createElement('object');
  nethackEmbed.width = 0;
  nethackEmbed.height = 0;
  nethackEmbed.addEventListener('message', this.handleMessage_.bind(this));
  nethackEmbed.data = 'ecl_terminal.nmf';
  nethackEmbed.type = 'application/x-nacl';

  var param = document.createElement('param');
  param.name = 'windowtype';
  param.value = 'tty';
  nethackEmbed.appendChild(param);

  document.getElementById('listener').appendChild(nethackEmbed);

  this.io.onVTKeystroke = got;

  this.io.onTerminalResize = this.resize_.bind(this);
};

Crosh.prototype.onBeforeUnload_ = function(e) {
  var msg = 'Closing this tab will exit crosh.';
  e.returnValue = msg;
  return msg;
};

/**
 * Send a string to the crosh process.
 *
 * @param {string} string The string to send.
 */
Crosh.prototype.sendString_ = function(string) {
};

/**
 * Closes crosh terminal and exits the crosh command.
**/
Crosh.prototype.close_ = function() {
}

/**
 * Notify process about new terminal size.
 *
 * @param {string|integer} terminal width.
 * @param {string|integer} terminal height.
 */
Crosh.prototype.onTerminalResize_ = function(width, height) {
};

/**
 * Exit the crosh command.
 */
Crosh.prototype.exit = function(code) {
  this.close_();
  this.io.pop();
  window.onbeforeunload = null;

  if (this.argv_.onExit)
    this.argv_.onExit(code);
};

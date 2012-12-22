#pragma once


#include <ppapi/cpp/instance.h>


enum NaClMessageKind {
  NACL_MSG_INIT_NHWINDOWS=0,
  NACL_MSG_PLAYER_SELECTION,
  NACL_MSG_ASKNAME,
  NACL_MSG_GET_NH_EVENT,
  NACL_MSG_EXIT_NHWINDOWS,
  NACL_MSG_SUSPEND_NHWINDOWS,
  NACL_MSG_RESUME_NHWINDOWS,
  NACL_MSG_CREATE_NHWINDOW,
  NACL_MSG_CREATE_NHWINDOW_BY_ID,
  NACL_MSG_CLEAR_NHWINDOW,
  NACL_MSG_DISPLAY_NHWINDOW,
  NACL_MSG_DESTROY_NHWINDOW,
  NACL_MSG_CURS,
  NACL_MSG_PUTSTR,
  NACL_MSG_DISPLAY_FILE,
  NACL_MSG_START_MENU,
  NACL_MSG_ADD_MENU,
  NACL_MSG_END_MENU,
  NACL_MSG_SELECT_MENU,
  NACL_MSG_UPDATE_INVENTORY,
  NACL_MSG_MARK_SYNCH,
  NACL_MSG_WAIT_SYNCH,
  NACL_MSG_CLIPAROUND,
  NACL_MSG_CLIPAROUND_PROPER,
  NACL_MSG_PRINT_GLYPH,
  NACL_MSG_RAW_PRINT,
  NACL_MSG_RAW_PRINT_BOLD,
  NACL_MSG_NHGETCH,
  NACL_MSG_NH_POSKEY,
  NACL_MSG_NHBELL,
  NACL_MSG_DOPREV_MESSAGE,
  NACL_MSG_YN_FUNCTION,
  NACL_MSG_GETLIN,
  NACL_MSG_GET_EXT_CMD,
  NACL_MSG_NUMBER_PAD,
  NACL_MSG_DELAY_OUTPUT,
  NACL_MSG_START_SCREEN,
  NACL_MSG_END_SCREEN,
  NACL_MSG_OUTRIP,
  NACL_MSG_DELETE_NHWINDOW_BY_REFERENCE,
  NACL_MSG_UPDATE_STATS
};


class EndOfMessage {
 public:
  EndOfMessage() { junk = 0; }
 private:
  int junk; 
};


class NaClMessage {
 public:
  NaClMessage();
  NaClMessage& operator<<(int value);
  NaClMessage& operator<<(const std::string& value);
  NaClMessage& operator<<(const EndOfMessage& value);
  
  static void SetInstance(pp::Instance* instance);

  static const std::string GetReply();

 private:
  std::string data_;
  static pp::Instance* instance_;
};

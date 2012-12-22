// Copyright 2011 Google Inc. All Rights Reserved.
// Author: jeffbailey@google.com (Jeff Bailey)

#include "naclmsg.h"
#include <stdio.h>
#include <string>
#include <sstream>
#include <ppapi/cpp/var.h>


NaClMessage::NaClMessage() : data_("[") {
}

NaClMessage& NaClMessage::operator<<(int value) {
  std::stringstream out;
  out << value;
  if (data_.size() > 1) {
    data_ += ",";
  }
  data_ += out.str();
  return *this;
}

static std::string EscapeString(const std::string& s) {
  std::string ret;

  for (std::string::const_iterator pos = s.begin(); pos != s.end(); ++pos) {
    if (*pos == '"') {
      ret += "\\\"";
    } else if (*pos == '\\') {
      ret += "\\\\";
    } else {
      ret += *pos;
    }
  }
  return ret;
}

NaClMessage& NaClMessage::operator<<(const std::string& value) {
  if (data_.size() > 1) {
    data_ += ",";
  }
  data_ += "\"";
  data_ += EscapeString(value);
  data_ += "\"";
  return *this;
}

NaClMessage& NaClMessage::operator<<(const EndOfMessage& value) {
  data_ += "]";
  // fprintf(stderr, "PostMessage: %s\n", data_.c_str());
  write(3, data_.c_str(), data_.size());
  data_ = "[";
  return *this;
}

void NaClMessage::SetInstance(pp::Instance* instance) {
  instance_ = instance;
}

const std::string NaClMessage::GetReply() {
  char buffer[64000];
  int len;

  len = read(3, buffer, sizeof(buffer));
  return std::string(buffer, len);
}

pp::Instance* NaClMessage::instance_ = 0;

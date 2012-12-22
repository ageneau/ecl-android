/*
 * Copyright (c) 2011 The Native Client Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

#include <ppapi/cpp/module.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
#include <sstream>
#include <string>
#include <sys/stat.h>
#include <ppapi/cpp/instance.h>
#include "nacl-mounts/base/KernelProxy.h"
#include "nacl-mounts/base/MainThreadRunner.h"
#include "nacl-mounts/base/UrlLoaderJob.h"
#include "nacl-mounts/console/JSPipeMount.h"
#include "nacl-mounts/console/JSPostMessageBridge.h"
#include "nacl-mounts/pepper/PepperMount.h"
#include "naclmsg.h"


#define TARFILE "nethack.tar"
//define USE_PSEUDO_THREADS

extern int ecl_main(int argc, char **argv);

extern "C" {
int simple_tar_extract_to(const char *path, const char *dst);
	//void winch();

int nacl_CO;
int nacl_LI;
}

void
stdio_write(const char *out) {
  ssize_t size = strlen(out);
  ssize_t i = write(1, out, size);
  // fprintf(stderr, "%Zd chars written\n", i);
}

class NethackInstance : public pp::Instance {
 public:
  explicit NethackInstance(PP_Instance instance) : pp::Instance(instance) {
    NaClMessage::SetInstance(this);
    jspipe_ = NULL;
    jsbridge_ = NULL;
    fs_ = NULL;
    runner_ = NULL;
#ifdef USE_PSEUDO_THREADS
    runner_sigwinch_ = NULL;
    jsbridge_sigwinch_ = NULL;
#endif
  }

  virtual ~NethackInstance() {
    if (runner_) delete runner_;
#ifdef USE_PSEUDO_THREADS
    if (runner_sigwinch_) delete runner_sigwinch_;
    if (jsbridge_sigwinch_) delete jsbridge_sigwinch_;
#endif
    if (jspipe_) delete jspipe_;
    if (jsbridge_) delete jsbridge_;
    if (fs_) delete fs_;
    pthread_join(nethack_thread_, NULL);
  }

  static void *GameThread(void *arg) {
    NethackInstance *inst = static_cast<NethackInstance*>(arg);
    inst->Run();
    return 0;
  }

  static void *WinchThread(void *arg) {
	  //    winch();
    return 0;
  }

  void Run() {
    /* Setup home directory to a known location. */
    setenv("HOME", "/mnt/home", 1);
    /* Setup terminal type. */
    setenv("TERM", "xterm-256color", 1);
    /* Blank out USER and LOGNAME. */
    setenv("USER", "", 1);
    setenv("LOGNAME", "", 1);
    /* Indicate where we decompress things. */
    setenv("HACKDIR", "/nethack", 1);

    // Setup game directory.
    kp_->mkdir("/nethack", 0777);
    kp_->mkdir("/mnt", 0777);

    // Mount local storage.
    {
      PepperMount* pm = new PepperMount(runner_, fs_, 20 * 1024 * 1024);
      pm->SetPathPrefix("/nethack-userdata");
      int ret = kp_->mount("/mnt", pm);
      assert(ret == 0); 
    }

    kp_->mkdir("/mnt/home", 0777);
    kp_->mkdir("/mnt/playground", 0777);
    kp_->mkdir("/mnt/playground/nethack", 0777);
    kp_->mkdir("/mnt/playground/nethack/save", 0777);

    // {
    //   UrlLoaderJob *job = new UrlLoaderJob;
    //   job->set_url(TARFILE);
    //   std::vector<char> data;
    //   job->set_dst(&data);
    //   runner_->RunJob(job);
    //   int fh = open("/" TARFILE, O_CREAT | O_WRONLY);
    //   write(fh, &data[0], data.size());
    //   close(fh);
    // }

    // simple_tar_extract_to("/" TARFILE, "/nethack");

    const char *argv[] = {"nethack"
#ifdef DEBUG
      ,"-D"
#endif
    };
    size_t argc = (sizeof(argv)/sizeof(char *));
    
    ecl_main(argc, const_cast<char **>(argv));
  }

  virtual bool Init(uint32_t argc, const char* argn[], const char* argv[]) {
    fs_ = new pp::FileSystem(this, PP_FILESYSTEMTYPE_LOCALPERSISTENT);
    runner_ = new MainThreadRunner(this);
    kp_ = KernelProxy::KPInstance();

    std::stringstream options;
    int argcount = 0;
    const char**argnwalk = argn;
    const char**argvwalk = argv;
    uint32_t argcwalk = 0;
    while (argcwalk < argc) {
      if (!strcmp(*argnwalk, "width")) {
        // The browser adds on extra fields we don't need:
        // width, height, data, type, src, @dev.
        // TODO(jeffbailey): Find if there a definite way we can
        // filter these without relying on magic ordering.
        break;
      }
      // fprintf(stderr, "argn: %s, argv: %s\n", *argnwalk, *argvwalk);
      if (argcount > 0) {
        options << ',';
      }
      options << *argnwalk << ':' << *argvwalk;
      argnwalk++;
      argvwalk++;
      argcwalk++;
      argcount++;
    }
    const std::string& optstr = options.str();
    setenv("NETHACKOPTIONS", optstr.c_str(), 1);
    // fprintf(stderr, "%s\n", optstr.c_str());

    jsbridge_ = new JSPostMessageBridge(runner_);
    jspipe_ = new JSPipeMount();
    jspipe_->set_outbound_bridge(jsbridge_);
#ifdef USE_PSEUDO_THREADS
    jspipe_->set_using_pseudo_thread(true);
    runner_sigwinch_ = new MainThreadRunner(this);
    jsbridge_sigwinch_ = new JSPostMessageBridge(runner_sigwinch_);
#endif
    // Replace stdin, stdout with js console.
    kp_->mount("/jspipe", jspipe_);
    close(0);
    close(1);
    int fd;
    fd = open("/jspipe/0", O_RDONLY);
    assert(fd == 0);
    fd = open("/jspipe/1", O_WRONLY);
    assert(fd == 1);
    // Open pipe for messages.
    fd = open("/jspipe/3", O_RDWR);
    assert(fd == 3);

#ifdef USE_PSEUDO_THREADS
    MainThreadRunner::PseudoThreadFork(&GameThread, this);
#else
    pthread_create(&nethack_thread_, NULL, &GameThread, this);
#endif
    return true;
  }

  virtual void HandleMessage(const pp::Var& message) {
    std::string msg = message.AsString();
    const std::string BASE = "WINCH:";
    if (msg.compare(0, BASE.length(), BASE, 0, BASE.length()) == 0) {
      std::stringstream convertor(msg.substr(BASE.length()));
      char colon;
      int new_CO;
      int new_LI;
      convertor >> new_CO >> colon >> new_LI;
      if (nacl_CO == new_CO && nacl_LI == new_LI) {
        // fprintf(stderr, "Dupe resize!");
        return;
      }

      // Set minimum screen size to 20x20 to avoid cases where there's
      // a negative x value for the cursor.
      if (new_CO < 20) {
        new_CO = 20;
      }
      if (new_LI < 20) {
        new_LI = 20;
      }
      nacl_CO = new_CO;
      nacl_LI = new_LI;
      // fprintf(stderr, "Width: %d height: %d\n", nacl_CO, nacl_LI);
#ifdef USE_PSEUDO_THREADS
      jspipe_->set_outbound_bridge(jsbridge_sigwinch_);
      MainThreadRunner::PseudoThreadHeadroomFork(
          640 * 1024 * 2, &WinchThread, this);
      jspipe_->set_outbound_bridge(jsbridge_);
#else
      pthread_t id;
      pthread_attr_t attr;
      pthread_attr_init(&attr);
//      pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
      pthread_create(&id, &attr, &WinchThread, this);
      pthread_attr_destroy(&attr);
#endif
      return;
    }

    jspipe_->Receive(msg.c_str(), msg.size());
  }

 private:
  pthread_t nethack_thread_;
  JSPipeMount* jspipe_;
  JSPostMessageBridge* jsbridge_;
  MainThreadRunner *runner_;
#ifdef USE_PSEUDO_THREADS
  MainThreadRunner *runner_sigwinch_;
  JSPostMessageBridge* jsbridge_sigwinch_;
#endif
  KernelProxy *kp_;
  pp::FileSystem *fs_;
};


class NethackModule : public pp::Module {
 public:
  virtual pp::Instance* CreateInstance(PP_Instance instance) {
    return new NethackInstance(instance);
  }
  virtual ~NethackModule() { }
};


namespace pp {
  Module *CreateModule() {
    return new NethackModule();
  }
}

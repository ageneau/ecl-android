package org.lisp.ecl;

import android.util.Log;

public class EmbeddedCommonLisp {
    private static String TAG = "EmbeddedCommonLisp";
	
    public void start() {
    	start(System.getenv("user.dir"));
    }
    public native void start(String path);
    public native String exec(String string);
    // public native String botExec(String string);

    static {
	System.loadLibrary("ecl_android");
	Log.w(TAG,"Done loading library");
    }
}

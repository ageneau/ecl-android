/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.example.hellojni;

import android.app.Activity;
import android.content.res.AssetManager;
import android.content.res.AssetFileDescriptor;
import android.widget.TextView;
import android.os.Bundle;
import android.util.Log;

import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.RandomAccessFile;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileOutputStream;
import java.io.OutputStream;


public class HelloJni extends Activity
{
    private static String TAG = "HelloJni";
    private static String RESOURCES_DIR = "lisp";
    private static String APP_RESOURCES_DIR = "resources";
    private static boolean DEBUG = false;
    
    static AssetManager assetManager;
	static File uncompressedFilesDir;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        assetManager = getAssets();

        uncompressedFilesDir = getDir(APP_RESOURCES_DIR,MODE_PRIVATE);
        uncompressDir(RESOURCES_DIR,uncompressedFilesDir);

        Log.w(TAG,"ECL Starting...");        
        startECL();
        Log.w(TAG,"ECL Started");


        /* Create a TextView and set its content.
         * the text is retrieved by calling a native
         * function.
         */
        TextView  tv = new TextView(this);
        tv.setText("DONE");
        setContentView(tv);
    }

    public void uncompressDir(String in, File out)
    {
        try
		{
            String[] files = assetManager.list(in);
            Log.w(TAG,"Uncompressing: " + files.length + " files");
            for(int i=0; i<files.length; i++)
            {
                Log.w(TAG,"Uncompressing: " + files[i]);
                File fileIn = new File(in,files[i]);
                File fileOut = new File(out,files[i]);

                try
                {
                    uncompressFile(fileIn,fileOut);
                }
                catch(FileNotFoundException e)
                {
                    // fileIn is a directory, uncompress the subdir
                    if(!fileOut.isDirectory())
                    {
                        Log.w(TAG,"Creating dir: " + fileOut.getAbsolutePath());
                        fileOut.mkdir();
                    }
                    uncompressDir(fileIn.getPath(), fileOut);    
                }
            }
		}
		catch(IOException e)
		{
			e.printStackTrace();
		}
    }

	public static String getResourcesPath()
	{
		return uncompressedFilesDir.getAbsolutePath();
	}

	public static void uncompressFile(File fileIn,File fileOut)
        throws IOException
    {
        InputStream in = assetManager.open(fileIn.getPath(),
                                           android.content.res.AssetManager.ACCESS_RANDOM);
		OutputStream out = new FileOutputStream(fileOut);

		byte[] buf = new byte[1024];
		int len;
		while ((len = in.read(buf)) > 0)
		{
			out.write(buf, 0, len);
		}

		in.close();
		out.close();
		Log.i(TAG,"File copied.");
    }
    
    
    public native void startECL();
    
    static
    {
        System.loadLibrary("hello-jni");
        Log.w(TAG,"Done loading library");
    }
}

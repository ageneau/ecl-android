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
import android.widget.EditText;
import android.widget.TextView;
import android.widget.LinearLayout;
import android.widget.ImageButton;
import android.os.Bundle;
import android.util.Log;
import android.text.TextWatcher;
import android.text.Editable;
import android.view.KeyEvent;
import android.view.inputmethod.EditorInfo;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.view.View.OnClickListener;
import android.view.inputmethod.EditorInfo;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import android.view.Display;
import android.util.DisplayMetrics;


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

import com.example.hellojni.common.*;
import com.example.hellojni.gui.*;


public class HelloJni extends Activity
{
    private static String TAG = "HelloJni";

    static
    {
        System.loadLibrary("hello-jni");
        Log.i(TAG,"Done loading library");
    }

    // Layout Views
	private ConsoleScrollView scrollView;
	private ConsoleOutputTextView outputTextView;


    private static String RESOURCES_DIR = "lisp";
    private static String APP_RESOURCES_DIR = "resources";
    private static boolean DEBUG = true;
    
    static AssetManager assetManager;
	static File uncompressedFilesDir;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        // Set up the window layout
        setContentView(R.layout.main);

        assetManager = getAssets();
        uncompressedFilesDir = getDir(APP_RESOURCES_DIR,MODE_PRIVATE);

        // uncompress assets if not already done in a previous run
        if(!uncompressedFilesDir.isDirectory())
        {
            uncompressDir(RESOURCES_DIR,uncompressedFilesDir,true);
        }

        Log.i(TAG,"ECL Starting...");        
        startECL();
        Log.i(TAG,"ECL Started");


    	// init display text views 
    	Display display = this.getWindowManager().getDefaultDisplay();
    	DisplayMetrics metrics = new DisplayMetrics();
    	display.getMetrics(metrics);

    	LinearLayout ll = (LinearLayout)findViewById(R.id.verticalLayout);
    	outputTextView = new ConsoleOutputTextView(this, metrics.heightPixels);
    	ll.addView(outputTextView);
    	
    	scrollView = (ConsoleScrollView)findViewById(R.id.consoleScrollView);
    	scrollView.setScrollViewListener(outputTextView);

    	// init input button
    	ImageButton inputOK = (ImageButton) findViewById(R.id.inputOKButton);
    	inputOK.setOnClickListener(butClickListener);
    	
    	// init input text view
    	EditText inputView = (EditText)findViewById(R.id.inputBox);
    	inputView.setOnKeyListener(inputTextViewKeyListener);

    }

    @Override
    public void onStart()
    {
        super.onStart();
    }

    private void issueCommand()
    {
       EditText inputView = (EditText)findViewById(R.id.inputBox);
	   String s = inputView.getText().toString().trim();
	   String res = eclExec(s);

	   inputView.setText(""); // clear

	   writeln("> " + s);
	   writeln("");
	   writeln(res);
	}

    private View.OnClickListener butClickListener = new View.OnClickListener() 
    {
    	public void onClick(View v) {
    		issueCommand();
		}
    	
    };


    private EditText.OnKeyListener inputTextViewKeyListener = new EditText.OnKeyListener() 
    {
		public boolean onKey(View v, int keyCode, KeyEvent event) {
			if (keyCode == KeyEvent.KEYCODE_ENTER
				&& event.getAction() == KeyEvent.ACTION_UP) {
    		
    			issueCommand();
    			return true;
    		}
    		return false;
    	}
    	
    };

    private Runnable scrollToBottomAction = new Runnable()
    {
    	public void run() 
    	{ 
    		if (outputTextView.getViewableHeight() > scrollView.getHeight()) {
    			scrollView.scrollTo(0, outputTextView.getHeight());
    		} 
        } 
    };


    public void write(String msg)
    {
    	outputTextView.appendText(msg);
    	scrollView.post(scrollToBottomAction); // add scroll action to message queue
    }

    public void writeln(String msg)
    {
    	outputTextView.appendText(msg);
    	outputTextView.appendText(ConstantData.NEWLINE);
    	scrollView.post(scrollToBottomAction); // add scroll action to message queue
    }

    
    public void uncompressDir(String in, File out, boolean recursive)
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
                    if(recursive)
                    {
                        // fileIn is a directory, uncompress the subdir
                        if(!fileOut.isDirectory())
                        {
                            Log.w(TAG,"Creating dir: " + fileOut.getAbsolutePath());
                            fileOut.mkdir();
                        }
                        uncompressDir(fileIn.getPath(), fileOut, true);
                    }
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
    public native String eclExec(String s);
}

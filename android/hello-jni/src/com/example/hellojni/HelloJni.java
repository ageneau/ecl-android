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

    static
    {
        System.loadLibrary("hello-jni");
        Log.w(TAG,"Done loading library");
    }

    // Layout Views
    private ListView mConversationView;
    private EditText mOutEditText;
    private Button mSendButton;

    private static String RESOURCES_DIR = "lisp";
    private static String APP_RESOURCES_DIR = "resources";
    private static boolean DEBUG = true;

    // Array adapter for the conversation thread
    private ArrayAdapter<String> mConversationArrayAdapter;
    // String buffer for outgoing messages
    private StringBuffer mOutStringBuffer;

    
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

        Log.w(TAG,"ECL Starting...");        
        startECL();
        Log.w(TAG,"ECL Started");


        EditText tv = new EditText(this);

        TextWatcher textWatcher = new TextWatcher()
        {
            public void onTextChanged(CharSequence s, int start, int before, int count)
            {
                Log.w(TAG,"Text changed");
            }
            public void afterTextChanged(Editable arg0)
            {
            }
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {}
        };

        tv.addTextChangedListener(textWatcher);
        tv.setOnEditorActionListener(mWriteListener);
        tv.setText("DONE");
//        setContentView(tv);
    }

    @Override
    public void onStart()
    {
        super.onStart();
        if(DEBUG) Log.e(TAG, "++ ON START ++");
        setupChat();
    }


    private void setupChat() {
        Log.d(TAG, "setupChat()");

        // Initialize the array adapter for the conversation thread
        mConversationArrayAdapter = new ArrayAdapter<String>(this, R.layout.message);
        mConversationView = (ListView) findViewById(R.id.in);
        mConversationView.setAdapter(mConversationArrayAdapter);

        // Initialize the compose field with a listener for the return key
        mOutEditText = (EditText) findViewById(R.id.edit_text_out);
        mOutEditText.setOnEditorActionListener(mWriteListener);

        // Initialize the send button with a listener that for click events
        mSendButton = (Button) findViewById(R.id.button_send);
        mSendButton.setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
                // Send a message using content of the edit text widget
                TextView view = (TextView) findViewById(R.id.edit_text_out);
                String message = view.getText().toString();
                sendMessage(message);
            }
        });
        
        // Initialize the BluetoothChatService to perform bluetooth connections
//        mChatService = new BluetoothChatService(this, mHandler);

        // Initialize the buffer for outgoing messages
        mOutStringBuffer = new StringBuffer("");
    }

    private void sendMessage(String message) {
        // Check that there's actually something to send
        if (message.length() > 0) {
            mConversationArrayAdapter.add("IN:  " + message);
            String res = eclExec(message);
            
            // Reset out string buffer to zero and clear the edit text field
            mOutStringBuffer.setLength(0);
            mOutEditText.setText(mOutStringBuffer);

            mConversationArrayAdapter.add("OUT:  " + res);
        }

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

        // The action listener for the EditText widget, to listen for the return key
    private TextView.OnEditorActionListener mWriteListener =
        new TextView.OnEditorActionListener() {
        public boolean onEditorAction(TextView view, int actionId, KeyEvent event) {
            // If the action is a key-up event on the return key, send the message
            if (actionId == EditorInfo.IME_NULL && event.getAction() == KeyEvent.ACTION_UP) {
                String message = view.getText().toString();
                sendMessage(message);
            }
            if(DEBUG) Log.i(TAG, "END onEditorAction");
            return true;
        }
    };


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

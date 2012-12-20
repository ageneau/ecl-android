package org.lisp.ecl;

import android.app.Activity;
import android.content.Context;
import android.content.res.AssetManager;
import android.content.res.AssetFileDescriptor;
import android.widget.TextView;
import android.widget.Toast;
import android.os.Bundle;
import android.util.Log;
import android.content.SharedPreferences;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

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
import java.util.Date;


public class HelloEclActivity extends Activity
{
    private static String TAG = "HelloEcl";
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

        SharedPreferences settings = getPreferences(MODE_PRIVATE);
        boolean assetsUncompressed = settings.getBoolean("assetsUncompressed", false);
        uncompressedFilesDir = getDir(APP_RESOURCES_DIR,MODE_PRIVATE);

        if(!assetsUncompressed)
        {
	        uncompressDir(RESOURCES_DIR,uncompressedFilesDir);
	        SharedPreferences.Editor editor = settings.edit();
	        editor.putBoolean("assetsUncompressed", true);
	        editor.commit();
        }

        Log.w(TAG,"ECL Starting...");
        startECL();
        Log.w(TAG,"ECL Started");

        setContentView(R.layout.main);

		String result = eclExec("(format nil \"Hello from lisp\")");
		System.out.println("Result: " + result);
		
		Context context = getApplicationContext();
		int duration = Toast.LENGTH_LONG;
		
		Toast toast = Toast.makeText(context, result, duration);
		toast.show();
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

	// Initiating Menu XML file (menu.xml)
    @Override
    public boolean onCreateOptionsMenu(Menu menu)
    {
        MenuInflater menuInflater = getMenuInflater();
        menuInflater.inflate(R.layout.menu, menu);
        return true;
    }

	/**
     * Event Handling for Individual menu item selected
     * Identify single menu item by it's id
     * */
    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {
        switch (item.getItemId())
        {
        case R.id.menu_uncompress:
	        uncompressDir(RESOURCES_DIR,uncompressedFilesDir);
	        return true;
        default:
	        return super.onOptionsItemSelected(item);
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
    public native String eclExec(String string);
    
    static
    {
        System.loadLibrary("hello-ecl");
        Log.w(TAG,"Done loading library");
    }
}

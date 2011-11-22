/*
 *  Copyright 2011 Seto Chi Lap (setosoft@gmail.com)
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.example.hellojni.gui;

import android.util.Log;
import android.util.AttributeSet;
import android.widget.ScrollView;
import android.content.Context;

/**
 *  Derived a class from ScrollView just to get the current scrolled position
 *  at onScrollChanged since ScrollView itself does not provide public method for one
 *  to get it.
 */
public class ConsoleScrollView extends ScrollView
{
	private ScrollViewListener scrollViewListener = null;

    public ConsoleScrollView(Context context) 
    {
        super(context);
    }
    
    public ConsoleScrollView(Context context, AttributeSet attrs, int defStyle) 
    {
        super(context, attrs, defStyle);
    }

    public ConsoleScrollView(Context context, AttributeSet attrs) 
    {
        super(context, attrs);
    }
    
    public void setScrollViewListener(ScrollViewListener scrollViewListener) 
    {
        this.scrollViewListener = scrollViewListener;
    }
    
    @Override
    protected void onScrollChanged(int l, int t, int oldl, int oldt)
    {
    	super.onScrollChanged(l, t, oldl, oldt);
 	    if (scrollViewListener != null)
 	    	scrollViewListener.onViewScroll(l, t, oldl, oldt);
    }
    
    /**
     * Interface for party interested in the scrolling position
     */
    public interface ScrollViewListener
    {
    	void onViewScroll(int x, int y, int oldx, int oldy);
    }
}

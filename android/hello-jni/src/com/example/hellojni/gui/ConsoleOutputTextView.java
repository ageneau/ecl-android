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

import android.widget.ImageView;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Typeface;
import android.util.Log;
import java.util.LinkedList;
import java.util.ListIterator;

import com.example.hellojni.common.ConstantData;

public class ConsoleOutputTextView extends ImageView implements ConsoleScrollView.ScrollViewListener
{
   private static final String TAG = "ConsoleTextView";
   
   
   private static final String NEWLINE = "\n";
   private static final int BGCOLOR = 0xff000000;
   private static final int FGCOLOR = 0xffffffff;
   private static final int MAXLINE = 2048;
   private static final int DRAWYOFFSET = 16;
   private int TEXTSIZE = 12;
   private int LINEHEIGHT;
   private final int SCREENHEIGHT; 
   
   private Paint textPaint;                    ///< the paint to draw text
   private LinkedList<String> textBufferList;  ///< buffer to hold all lines of text
   private StringBuilder appendStringBuffer;   ///< buffer used during appending text
   private int lineLimitInView;   ///< maximum number of lines which can be viewed by user 
   private int curViewYPos;       ///< current view Y position (value callback by scrollview)
	
   public ConsoleOutputTextView(Context context, int screenHeight)
   {
	   super(context);
	   
	   textPaint = new Paint();
	   textPaint.setTypeface(Typeface.MONOSPACE);
	   textPaint.setColor(FGCOLOR);
	   
	   textBufferList = new LinkedList<String>();
	   appendStringBuffer = new StringBuilder(512);
	   
	   SCREENHEIGHT = screenHeight;
	   
	   setFontSize(TEXTSIZE);
	   
	   curViewYPos = 0;
   }
   
   /**
    * Add text to display in this view
    */
   public void appendText(String s)
   {
	   int index = 0;
	   float maxWidth = this.getWidth();
	   int searchStart = 0;
	   
	   appendStringBuffer.delete(0, appendStringBuffer.length());
	   
	   if (textBufferList.size() > 0) {
		   // recalculate the last line
		   appendStringBuffer.append(textBufferList.get(textBufferList.size() - 1));
		   // remove the last line as we will recalculate 
		   textBufferList.remove(textBufferList.size() - 1);
	   }
	   
	   appendStringBuffer.append(s);
	   
   
	   while (index < appendStringBuffer.length()) { 
		  int lineBreakPos = appendStringBuffer.indexOf(NEWLINE, searchStart);
		  int tempCount = 0; // num of char that will accomodate in a single line
		  
		  //Log.d(TAG, "lineBreakPos = " + lineBreakPos);
		  
		  if (lineBreakPos != -1) {
			  // new line is found
			  tempCount = textPaint.breakText(appendStringBuffer, index, lineBreakPos, 
                                              true, maxWidth, null);
			  if (tempCount < (lineBreakPos - index))  
				 lineBreakPos = -1;   // linebreak not reached, leave for next line
		  }
		  else {
			  tempCount = textPaint.breakText(appendStringBuffer, index, appendStringBuffer.length(), 
				                              true, maxWidth, null);
		  }
		  
		  //Log.d(TAG, "tempCount = " + tempCount);
	      
		  textBufferList.add(appendStringBuffer.substring(index, index + tempCount));
	      index += (tempCount + ((lineBreakPos != -1) ? 1 : 0));
	      searchStart = index;
	      
	      if (lineBreakPos != -1 && index == appendStringBuffer.length()) { // linebreak at last char
	    	  textBufferList.add("");  // add one blank line to it
	      }
	   }
	   
	   
	   // remove oldest line if too many
	   if (textBufferList.size() > MAXLINE) {
		   //Log.d(TAG, "TOO many lines ...");
		   int removeCount = textBufferList.size() - MAXLINE;
		   for (int i = 0; i < removeCount; ++i)
			  textBufferList.remove(0);
	   }
	   
	   // redraw itself
	   this.requestLayout();
	   this.invalidate();
   }
   
   /**
    *  Remove all text in this view
    */
   public void clearAllText()
   {
	   int removeCount = textBufferList.size();
	   for (int i = 0; i < removeCount; ++i)
		  textBufferList.remove(0);
	   
	   
	   // redraw itself
	   this.requestLayout();
	   this.invalidate();
   }
/*   
   public int getLastLineYPos()
   {
	   return DRAWYOFFSET + textBufferList.size() * LINEHEIGHT;
   }
   
   public int getCurViewYPos()
   {
	   return curViewYPos;
   }
*/ 
   
   /**
    *  The height of viewable area
    */
   public int getViewableHeight()
   {
	   return (DRAWYOFFSET + textBufferList.size() * LINEHEIGHT) - curViewYPos;
   }
   
   public int getFontSize()
   {
	   return (int)textPaint.getTextSize();
   }
   
   public void setFontSize(int fontSize)
   {
	   // check valid font size
	   boolean isValidFontSize = false;
	   for (ConstantData.ConsoleFontSize f : ConstantData.ConsoleFontSize.values()) {
		  if (fontSize == f.v()) {
			  isValidFontSize = true;
			  break;
		  }
	   }
	    
	   TEXTSIZE = (isValidFontSize) ? fontSize : ConstantData.ConsoleFontSize.SMALL.v();
	   textPaint.setTextSize(TEXTSIZE);
	   LINEHEIGHT = TEXTSIZE + 2;
	   lineLimitInView = (int)(SCREENHEIGHT / LINEHEIGHT);
   }
   
   
   @Override
   protected void onDraw(Canvas canvas)
   {
	   
	   // background colour
	   canvas.drawColor(BGCOLOR);

	   float YOffset = DRAWYOFFSET; // don't draw at pixel 0 as it can't be seen
	   
	   int startLine = curViewYPos / LINEHEIGHT;
	   //Log.d(TAG, "startline = " + startLine + ", curViewYPos = " + curViewYPos + ", text size = " + textBufferList.size());
	   
	   startLine = (startLine >= textBufferList.size()) ? (textBufferList.size() - 1) : startLine;
	   startLine = (startLine < 0) ? 0 : startLine;
	   float posY = YOffset + startLine * LINEHEIGHT; // starting Y pos to draw 
	   ListIterator<String> itr = textBufferList.listIterator(startLine);
	   int drawLineCount = 0;
	   
	   while (itr.hasNext()) { 
		   String s = itr.next();
		   
		   canvas.drawText(s, 0, posY, textPaint);
		   posY += LINEHEIGHT;
		   
		   ++drawLineCount;
		   if (drawLineCount > lineLimitInView)  // no need to draw if out of screen
			  break;
	   }
	   
	   //Log.d(TAG, "draw text *******");
   }
   
   /**
    *  Callback by parent scrollView to notify that user has triggered a scrolling action  
    */
   public void onViewScroll(int x, int y, int oldx, int oldy)
   {
	   //Log.d(TAG, "x = " + x + ", y = " + y + ", oldx = " + oldx + ", oldy = " + oldy);
	   curViewYPos = y;
	   //Log.d(TAG, "curViewYPos = " + curViewYPos);
   }
   
   
   /**
    *  Set the size of this view
    */
   @Override
   protected void onMeasure (int widthMeasureSpec, int heightMeasureSpec)
   {
	   int tempLineCount = textBufferList.size() < lineLimitInView ? lineLimitInView : textBufferList.size();
	   int height = getDefaultSize(tempLineCount * LINEHEIGHT, heightMeasureSpec);
	   int width = getDefaultSize(0, widthMeasureSpec);
	   
	   setMeasuredDimension(width, height);
   }
}

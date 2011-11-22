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
package com.example.hellojni.common;

public class ConstantData 
{
	private ConstantData() { }
	
	public static final String ROOTDIR = "/";
	public static final String DIRSEPARATOR = "/";
	public static final char DIRSEPCHAR = '/';
	public static final String NEWLINE = "\n";
	
	public static final String STREAMENCODING = "UTF-8";
	
	public enum MsgType
	{
	   STRING, // display string
	   CLEAR,  // clear screen
	   SHOWSRES, // show screen resolution
	   SETFONTSIZE, // set new font size 
	   SHOWFONTSIZE, // show current font size
	   EXIT    // quit console
	};
	
	/**
	 * Font size in the output console
	 */
	public enum ConsoleFontSize
	{
	   UNKNOWN(-1),
	   BIG(20),
	   MEDIUM(16),
	   SMALL(12);
	   
	   private final int size;   ///< actual font size
	   ConsoleFontSize(int s)
	   {
	      this.size = s;
	   }
	      
	   public int v()
	   {
	      return this.size;
	   }
	   
	   public static ConsoleFontSize toFontSize(int size)
	   {
		  for (ConsoleFontSize f : ConsoleFontSize.values()) {
			 if (f.v() == size)
				return f;
		  }
		  
		  return UNKNOWN;
	   }
	};
}

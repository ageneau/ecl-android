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

import android.content.Context;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.PackageInfo;
import android.app.Application;

/**
 * To hold common useful functions
 */
public class CFunc 
{
   private CFunc() { }
   
   private static Application appInst;
   
   public static void setAppInst(Application appInst)
   {
	   CFunc.appInst = appInst;
   }
   
   public static Application getAppInst()
   {
       return CFunc.appInst;	   
   }
   
   public static String getString(int resId)
   {
	   return CFunc.appInst.getString(resId);
   }
   
   /**
	 * 
	 * @param org
	 * @param max -- exclusive
	 * @param min -- inclusive
	 * @return
	 */
	public static int nextValue(int org, int max, int min)
	{
		++org;
		if (org >= max)
			org = min;
		
		return org;
	}
	
	/**
	 * 
	 * @param org
	 * @param max -- exclusive
	 * @param min -- inclusive
	 * @return
	 */
	public static int prevValue(int org, int max, int min)
	{
		--org;
		if (org < min)
			org = max - 1;
		
		return org;
	}
	
	public static int parseInt(String s)
	{
		try {
			return Integer.parseInt(s);
		}
		catch (Exception e) {
			
		}
		return 0;
	}
	
	public static long parseLong(String s)
	{
		try {
			return Long.parseLong(s);
		}
		catch (Exception e) {
			
		}
		return 0;
	}
	
	/**
	 * Trim '\r', '\n', ' ' and '\t' in a string
	 * @param s
	 * @return
	 */
	public static String trimString(String s)
	{
	   if (s == null)
		  return null;
	   
	   int end = s.length() - 1;
	   int start = 0;
	   
	   // trim beginning
	   while (start <= end) {
		   char c = s.charAt(start);
		   if (!(c == '\r' || c == '\n' || c == ' ' || c == '\t'))
			   break;
		   ++start;
	   }
	   
	   // trim trailing
	   while (start <= end) {
		   char c = s.charAt(end);
		   if (!(c == '\r' || c == '\n' || c == ' ' || c == '\t'))
			   break;
		   --end;
	   }
	   
	   if (start <= end)
		  return s.substring(start, end + 1);
	   
	   return "";
	}
	
	public static boolean isNullOrEmpty(String s)
	{
	   return (s == null || s.length() == 0);
	}
	
	public static String getVersion()
    {
       try {
    	  Context context = getAppInst();
          PackageManager pMan = context.getPackageManager();
          PackageInfo pInfo = pMan.getPackageInfo(context.getPackageName(), 0);
          return pInfo.versionName;
       }
       catch (NameNotFoundException e) {

       }
       return null;
	}
	
	public static String getContactMail()
	{
	   return "setosoft@gmail.com"; 
	}
	
	public static void sleep(long millsec)
	{
	   try { Thread.sleep(millsec); } catch (InterruptedException e) { }
	}
	
	public static String int2Ipv4(int netAddr)
	{
	   StringBuilder sb = new StringBuilder(15);
	   
	   sb.append(netAddr & 0xff).append(".")
	     .append((netAddr >> 8) & 0xff).append(".")
	     .append((netAddr >> 16) & 0xff).append(".")
	     .append((netAddr >> 24) & 0xff);
	   return sb.toString(); 
	}
	
	public static String int2Ipv4(byte[] netAddr)
	{
	   int addr = (netAddr[3] << 24)| (netAddr[2] << 16)|(netAddr[1] << 8)|(netAddr[0]);
	   return int2Ipv4(addr);
	}
}

/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.util;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

import java.util.*;

import org.jboss.tools.common.model.plugin.ModelPlugin;

public class OSHelper {
  static Properties env = null;

  public OSHelper() {
  }

  private static java.util.Properties getEnvironment()  {
    Process p = null;
    java.util.Properties envVars = new java.util.Properties();
    Runtime r = Runtime.getRuntime();
    String OS = System.getProperty("os.name").toLowerCase();
    try {
      // Get the Windows 95 environment variables
      if (OS.indexOf("windows 9") > -1) {
        p = r.exec( "command.com /c set" );
      }
      // Get the Windows NT environment variables
      else if (OS.indexOf("nt") > -1) {
        p = r.exec( "cmd.exe /c set" );
      }
      // Get the Windows 2000 environment variables
      else if (OS.indexOf("2000") > -1) {
        p = r.exec( "cmd.exe /c set" );
      }
      // Get the Windows XP environment variables
      else if (OS.indexOf("xp") > -1) {
        p = r.exec( "cmd.exe /c set" );
      }
      // Get the unix environment variables
      else if (OS.indexOf("linux") > -1) {
        p = r.exec( "env" );
      }
      // Get the unix environment variables
      else if (OS.indexOf("unix") > -1) {
        p = r.exec( "/bin/env" );
      }            // Get the unix environment variables
      else if (OS.indexOf("sunos") > -1) {
        p = r.exec( "/bin/env" );
      } else  {
          if(ModelPlugin.isDebugEnabled()) {
        	  ModelPlugin.getPluginLog().logInfo("OS not known: " + OS);
          }
      }
    } catch (java.io.IOException e) {
        if(ModelPlugin.isDebugEnabled()) {
        	ModelPlugin.getPluginLog().logError(e);
        }
    }
    java.io.BufferedReader br = new java.io.BufferedReader(new java.io.InputStreamReader(p.getInputStream()));
    String line;
    try {
      int idx;
      String key, value;
      while( (line = br.readLine()) != null ) {
        idx = line.indexOf('=');
        // if there is no equals sign on the line skip to the net line
        // this occurs when there are newline characters in the environment variable
        //
        if (idx < 0) continue;
        key = line.substring( 0, idx );
        value = line.substring( idx+1 );
        envVars.setProperty( key.toUpperCase(), value );
      }
    } catch (java.io.IOException e) {
    	ModelPlugin.getPluginLog().logError(e);
    }
    return envVars;
  }

  public static java.util.Properties get() {
    if (env == null) env = getEnvironment();
    return (java.util.Properties)env.clone();
  }

  public static String getProperty(String key) {
    if (env == null) env = getEnvironment();
    return env.getProperty(key);
  }

  public static String getProperty(String key, String defaultValue) {
    if (env == null) env = getEnvironment();
    String val = env.getProperty(key);
    return (val == null) ? defaultValue : val;
  }
}
